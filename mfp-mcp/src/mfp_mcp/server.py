"""MCP server: log what you eat through Claude, track macros, sync to MyFitnessPal."""

from __future__ import annotations

import json
import os
from datetime import date as date_cls, datetime
from zoneinfo import ZoneInfo

from mcp.server.mcpserver import MCPServer

from .mfp_client import MFPClient, MFPError
from .store import MACROS, MEALS, Entry, Store, normalize_meal

INSTRUCTIONS = """\
Food logging and macro tracking for the user, with optional MyFitnessPal (MFP) sync.

Be proactive - the point of this server is that every meal gets logged:
- Whenever the user mentions eating or drinking anything, log it with log_food
  without being asked. Ask only if the portion is genuinely ambiguous.
- At the start of a conversation (or when the user says hi / plans their day), call
  check_unlogged_meals for today. If earlier meals are missing for the time of day,
  ask the user what they had and log it.
- To log: call search_food first, pick the best match (prefer verified entries),
  choose the serving_index and servings that match what the user ate, then call
  log_food with mfp_food_id / mfp_food_version / serving_index. If nothing matches,
  call log_food with your best estimate of calories/protein/carbs/fat instead.
- After logging, briefly report the day's totals and what's left versus goals.
"""

store = Store(os.environ.get("MFP_MCP_DB", "~/.mfp-mcp/food_log.db"))
_mfp: MFPClient | None = None


def mfp() -> MFPClient | None:
    global _mfp
    if _mfp is None and os.environ.get("MFP_COOKIE"):
        _mfp = MFPClient(os.environ["MFP_COOKIE"])
    return _mfp


def today() -> str:
    tz = os.environ.get("MFP_TIMEZONE")
    return (datetime.now(ZoneInfo(tz)) if tz else datetime.now()).date().isoformat()


def resolve_date(d: str | None) -> str:
    if not d or d.lower() == "today":
        return today()
    return date_cls.fromisoformat(d).isoformat()


server = MCPServer("myfitnesspal-food-log", instructions=INSTRUCTIONS)


@server.tool()
async def search_food(query: str, limit: int = 8) -> dict:
    """Search the MyFitnessPal food database.

    Returns foods with serving options; each option shows macros for ONE of that
    serving. Use the mfp_food_id, mfp_food_version and serving_index with log_food.
    """
    client = mfp()
    if not client:
        return {"error": "MyFitnessPal is not configured (MFP_COOKIE unset). "
                         "Log with log_food using estimated macros instead."}
    try:
        foods = await client.search(query, min(max(limit, 1), 25))
    except MFPError as e:
        return {"error": str(e)}
    return {"query": query, "results": [f.to_result() for f in foods]}


@server.tool()
async def log_food(
    meal: str,
    description: str = "",
    servings: float = 1,
    date: str | None = None,
    mfp_food_id: str | None = None,
    mfp_food_version: str | None = None,
    serving_index: int = 0,
    calories: float | None = None,
    protein_g: float | None = None,
    carbs_g: float | None = None,
    fat_g: float | None = None,
) -> dict:
    """Log something the user ate.

    meal: Breakfast, Lunch, Dinner or Snacks. date: YYYY-MM-DD (default today).
    Either pass mfp_food_id (+ mfp_food_version, serving_index) from search_food,
    in which case macros are computed and the entry is pushed to the MFP diary,
    or pass calories/protein_g/carbs_g/fat_g totals for the whole portion
    (saved locally only - MFP cannot accept custom macros through this API).
    """
    meal = normalize_meal(meal)
    day = resolve_date(date)
    if servings <= 0:
        raise ValueError("servings must be positive")

    entry = Entry(date=day, meal=meal, description=description, servings=servings,
                  calories=0, protein_g=0, carbs_g=0, fat_g=0)
    client = mfp()
    note = None

    if mfp_food_id:
        if not client:
            raise ValueError("mfp_food_id given but MFP_COOKIE is not set; pass macros instead")
        try:
            food = await client.get_food(mfp_food_id)
        except MFPError as e:
            return {"error": str(e)}
        if not 0 <= serving_index < len(food.servings):
            raise ValueError(f"serving_index must be 0..{len(food.servings) - 1}")
        serving = food.servings[serving_index]
        for k, v in food.macros_for(serving_index, servings).items():
            setattr(entry, k, v)
        entry.description = description or (
            f"{food.name}{f' ({food.brand})' if food.brand else ''}, "
            f"{servings:g} x {serving.label}")
        entry.mfp_food_id = food.id
        entry.mfp_food_version = mfp_food_version or food.version
        entry.mfp_serving = json.dumps(serving.raw or {
            "value": serving.value, "unit": serving.unit,
            "nutrition_multiplier": serving.nutrition_multiplier})
        entry.sync_status = "pending"
    else:
        if calories is None:
            raise ValueError("Pass mfp_food_id from search_food, or at least calories")
        if not description:
            raise ValueError("description is required when logging manual macros")
        entry.calories = calories
        entry.protein_g, entry.carbs_g, entry.fat_g = protein_g or 0, carbs_g or 0, fat_g or 0
        if client:
            note = ("Saved locally only: this food has no MFP id, so it was not added "
                    "to the MFP diary.")

    store.add(entry)
    if entry.sync_status == "pending":
        await _push(entry)

    result = {
        "logged": {"id": entry.id, "date": day, "meal": meal, "description": entry.description,
                   **{k: getattr(entry, k) for k in MACROS}},
        "mfp_sync": entry.sync_status,
        "day": _brief(store.summary(day)),
    }
    if entry.sync_error:
        result["mfp_error"] = entry.sync_error
    if note:
        result["note"] = note
    return result


async def _push(entry: Entry) -> None:
    client = mfp()
    if not client:
        store.set_sync(entry.id, "pending", "MFP_COOKIE not set")
        return
    try:
        await client.add_diary_entry(
            date=entry.date, meal=entry.meal, food_id=entry.mfp_food_id,
            food_version=entry.mfp_food_version, serving=json.loads(entry.mfp_serving),
            servings=entry.servings)
        entry.sync_status, entry.sync_error = "synced", None
    except Exception as e:  # MFPError, network errors etc. - keep the local entry
        entry.sync_status, entry.sync_error = "failed", str(e)
    store.set_sync(entry.id, entry.sync_status, entry.sync_error)


def _brief(s: dict) -> dict:
    return {k: s[k] for k in ("totals", "goals", "remaining", "meals_not_logged")}


@server.tool()
def get_daily_summary(date: str | None = None) -> dict:
    """Macros eaten on a day (default today), per meal, with goals and what's remaining."""
    return store.summary(resolve_date(date))


@server.tool()
def check_unlogged_meals(date: str | None = None) -> dict:
    """Which meals have nothing logged yet for a day (default today), plus the current time,
    so you can ask the user about meals they forgot to log."""
    day = resolve_date(date)
    s = store.summary(day)
    tz = os.environ.get("MFP_TIMEZONE")
    now = datetime.now(ZoneInfo(tz)) if tz else datetime.now()
    return {"date": day, "local_time": now.strftime("%H:%M"),
            "meals_not_logged": s["meals_not_logged"], "totals": s["totals"],
            "remaining": s["remaining"]}


@server.tool()
def set_macro_goals(calories: float | None = None, protein_g: float | None = None,
                    carbs_g: float | None = None, fat_g: float | None = None) -> dict:
    """Set daily targets. Omitted values keep their current setting."""
    return {"goals": store.set_goals(calories=calories, protein_g=protein_g,
                                     carbs_g=carbs_g, fat_g=fat_g)}


@server.tool()
def delete_entry(entry_id: int) -> dict:
    """Delete an entry from the local log (by id from get_daily_summary).
    Does not remove it from MyFitnessPal - tell the user to delete it there if it was synced."""
    e = store.delete(entry_id)
    if not e:
        return {"error": f"No entry {entry_id}"}
    out = {"deleted": entry_id, "description": e.description}
    if e.sync_status == "synced":
        out["note"] = "This entry was synced to MyFitnessPal; remove it there manually."
    return out


@server.tool()
async def retry_mfp_sync() -> dict:
    """Retry pushing entries whose MyFitnessPal sync failed (e.g. after refreshing the cookie)."""
    results = []
    for e in store.unsynced():
        await _push(e)
        results.append({"id": e.id, "description": e.description,
                        "status": e.sync_status, "error": e.sync_error})
    return {"retried": results}


@server.prompt()
def log_meal(what_i_ate: str) -> str:
    """Log a meal from a plain-language description."""
    return (f"I just ate: {what_i_ate}\n\nLog it with the food tools (search MyFitnessPal "
            "first, estimate macros if there's no good match), then show my totals for "
            "today and what's left of my macro goals.")


@server.prompt()
def daily_checkin() -> str:
    """Catch up on any meals not logged today."""
    return ("Check which of my meals today aren't logged yet. Ask me what I had for each "
            "one that should have happened by now, log my answers, then summarize my "
            "macros for the day against my goals.")


def main() -> None:
    server.run()


if __name__ == "__main__":
    main()

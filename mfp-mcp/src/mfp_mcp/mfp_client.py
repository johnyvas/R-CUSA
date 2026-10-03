"""Minimal MyFitnessPal client.

MyFitnessPal has no public API for individuals. This uses the same private
endpoints the MFP web app calls, authenticated with your browser session
cookie. It can break whenever MFP changes their site; every endpoint is
overridable via environment variables so it can be patched without code
changes.
"""

from __future__ import annotations

import os
import time
from dataclasses import dataclass, field

import httpx

WEB_BASE = os.environ.get("MFP_WEB_BASE", "https://www.myfitnesspal.com")
API_BASE = os.environ.get("MFP_API_BASE", "https://api.myfitnesspal.com")
AUTH_TOKEN_PATH = os.environ.get("MFP_AUTH_TOKEN_PATH", "/user/auth_token?refresh=true")
SEARCH_PATH = os.environ.get("MFP_SEARCH_PATH", "/public/nutrition")
FOOD_PATH = os.environ.get("MFP_FOOD_PATH", "/v2/foods/{id}")
DIARY_PATH = os.environ.get("MFP_DIARY_PATH", "/v2/diary")

USER_AGENT = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
              "(KHTML, like Gecko) Chrome/129.0 Safari/537.36")

MEAL_POSITIONS = {"Breakfast": 0, "Lunch": 1, "Dinner": 2, "Snacks": 3}


class MFPError(Exception):
    pass


@dataclass
class Serving:
    value: float
    unit: str
    nutrition_multiplier: float
    raw: dict = field(default_factory=dict)

    @property
    def label(self) -> str:
        v = int(self.value) if float(self.value).is_integer() else self.value
        return f"{v} {self.unit}"


@dataclass
class Food:
    id: str
    version: str
    name: str
    brand: str | None
    # Nutrition for nutrition_multiplier == 1
    calories: float
    protein_g: float
    carbs_g: float
    fat_g: float
    servings: list[Serving]
    verified: bool = False

    def macros_for(self, serving_index: int, servings: float) -> dict:
        s = self.servings[serving_index]
        f = s.nutrition_multiplier * servings
        return {
            "calories": round(self.calories * f, 1),
            "protein_g": round(self.protein_g * f, 1),
            "carbs_g": round(self.carbs_g * f, 1),
            "fat_g": round(self.fat_g * f, 1),
        }

    def to_result(self) -> dict:
        return {
            "mfp_food_id": self.id,
            "mfp_food_version": self.version,
            "name": self.name,
            "brand": self.brand,
            "verified": self.verified,
            "serving_options": [
                {"serving_index": i, "serving": s.label, **self.macros_for(i, 1)}
                for i, s in enumerate(self.servings)
            ],
        }


def _nutrient(nc: dict, *keys: str) -> float:
    for k in keys:
        v = nc.get(k)
        if isinstance(v, dict):
            v = v.get("value")
        if v is not None:
            try:
                return float(v)
            except (TypeError, ValueError):
                pass
    return 0.0


def parse_food(item: dict) -> Food:
    nc = item.get("nutritional_contents") or {}
    servings = [
        Serving(value=float(s.get("value", 1)), unit=str(s.get("unit", "serving")),
                nutrition_multiplier=float(s.get("nutrition_multiplier", 1)), raw=s)
        for s in item.get("serving_sizes") or []
    ] or [Serving(1, "serving", 1.0, {"value": 1, "unit": "serving", "nutrition_multiplier": 1})]
    return Food(
        id=str(item["id"]),
        version=str(item.get("version", item["id"])),
        name=item.get("description") or item.get("name") or "Unknown food",
        brand=item.get("brand_name"),
        calories=_nutrient(nc, "energy", "calories"),
        protein_g=_nutrient(nc, "protein"),
        carbs_g=_nutrient(nc, "carbohydrates", "carbs"),
        fat_g=_nutrient(nc, "fat", "total_fat"),
        servings=servings,
        verified=bool(item.get("verified")),
    )


class MFPClient:
    def __init__(self, cookie: str, http: httpx.AsyncClient | None = None):
        if not cookie:
            raise MFPError("MFP_COOKIE is not set")
        self.cookie = cookie.strip()
        self.http = http or httpx.AsyncClient(timeout=20, headers={"User-Agent": USER_AGENT})
        self._token: str | None = None
        self._user_id: str | None = None
        self._expires = 0.0
        self.food_cache: dict[str, Food] = {}

    async def _auth_headers(self) -> dict:
        if not self._token or time.time() > self._expires - 60:
            r = await self.http.get(WEB_BASE + AUTH_TOKEN_PATH,
                                    headers={"Cookie": self.cookie, "Accept": "application/json"})
            if r.status_code != 200:
                raise MFPError(f"MFP auth failed (HTTP {r.status_code}). Your session cookie "
                               "has probably expired - copy a fresh one from your browser.")
            try:
                data = r.json()
            except ValueError:
                raise MFPError("MFP auth returned non-JSON (likely a login page). "
                               "Refresh MFP_COOKIE.") from None
            if not data.get("access_token"):
                raise MFPError("MFP auth response had no access_token. Refresh MFP_COOKIE.")
            self._token = data["access_token"]
            self._user_id = str(data.get("user_id", ""))
            self._expires = time.time() + float(data.get("expires_in", 3600))
        return {
            "Authorization": f"Bearer {self._token}",
            "mfp-client-id": "mfp-main-js",
            "mfp-user-id": self._user_id or "",
            "Accept": "application/json",
        }

    async def search(self, query: str, limit: int = 10) -> list[Food]:
        r = await self.http.get(
            API_BASE + SEARCH_PATH,
            params={"q": query, "page": 1, "per_page": limit},
            headers=await self._auth_headers(),
        )
        if r.status_code != 200:
            raise MFPError(f"MFP search failed (HTTP {r.status_code}): {r.text[:200]}")
        foods = []
        for it in r.json().get("items", []):
            item = it.get("item", it)
            if "id" not in item:
                continue
            food = parse_food(item)
            self.food_cache[food.id] = food
            foods.append(food)
        return foods[:limit]

    async def get_food(self, food_id: str) -> Food:
        if food_id in self.food_cache:
            return self.food_cache[food_id]
        r = await self.http.get(
            API_BASE + FOOD_PATH.format(id=food_id),
            params=[("fields[]", "nutritional_contents"), ("fields[]", "serving_sizes")],
            headers=await self._auth_headers(),
        )
        if r.status_code != 200:
            raise MFPError(f"MFP food lookup failed (HTTP {r.status_code}): {r.text[:200]}")
        body = r.json()
        food = parse_food(body.get("item", body))
        self.food_cache[food.id] = food
        return food

    async def add_diary_entry(self, *, date: str, meal: str, food_id: str, food_version: str,
                              serving: dict, servings: float) -> dict:
        payload = {"items": [{
            "type": "food_entry",
            "date": date,
            "meal_name": meal,
            "meal_position": MEAL_POSITIONS.get(meal, 3),
            "food": {"id": food_id, "version": food_version},
            "servings": servings,
            "serving_size": {k: serving[k] for k in ("value", "unit", "nutrition_multiplier")
                             if k in serving},
        }]}
        r = await self.http.post(API_BASE + DIARY_PATH, json=payload,
                                 headers=await self._auth_headers())
        if r.status_code not in (200, 201):
            raise MFPError(f"MFP diary write failed (HTTP {r.status_code}): {r.text[:200]}")
        return r.json() if r.content else {}

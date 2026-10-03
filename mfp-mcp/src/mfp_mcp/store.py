"""Local SQLite food log. This is the source of truth for macro tracking;
MyFitnessPal sync is best-effort on top of it."""

from __future__ import annotations

import sqlite3
from dataclasses import dataclass
from pathlib import Path

MEALS = ("Breakfast", "Lunch", "Dinner", "Snacks")

SCHEMA = """
CREATE TABLE IF NOT EXISTS entries (
    id            INTEGER PRIMARY KEY AUTOINCREMENT,
    date          TEXT NOT NULL,
    meal          TEXT NOT NULL,
    description   TEXT NOT NULL,
    servings      REAL NOT NULL DEFAULT 1,
    calories      REAL NOT NULL DEFAULT 0,
    protein_g     REAL NOT NULL DEFAULT 0,
    carbs_g       REAL NOT NULL DEFAULT 0,
    fat_g         REAL NOT NULL DEFAULT 0,
    mfp_food_id   TEXT,
    mfp_food_version TEXT,
    mfp_serving   TEXT,
    sync_status   TEXT NOT NULL DEFAULT 'local_only',
    sync_error    TEXT,
    created_at    TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS entries_date ON entries(date);
CREATE TABLE IF NOT EXISTS goals (
    id         INTEGER PRIMARY KEY CHECK (id = 1),
    calories   REAL,
    protein_g  REAL,
    carbs_g    REAL,
    fat_g      REAL
);
"""

MACROS = ("calories", "protein_g", "carbs_g", "fat_g")


def normalize_meal(meal: str) -> str:
    m = meal.strip().lower()
    aliases = {"snack": "Snacks", "snacks": "Snacks", "breakfast": "Breakfast",
               "lunch": "Lunch", "dinner": "Dinner", "supper": "Dinner"}
    if m not in aliases:
        raise ValueError(f"meal must be one of {', '.join(MEALS)} (got {meal!r})")
    return aliases[m]


@dataclass
class Entry:
    date: str
    meal: str
    description: str
    servings: float
    calories: float
    protein_g: float
    carbs_g: float
    fat_g: float
    mfp_food_id: str | None = None
    mfp_food_version: str | None = None
    mfp_serving: str | None = None  # JSON of the MFP serving_size object
    sync_status: str = "local_only"  # local_only | synced | pending | failed
    sync_error: str | None = None
    id: int | None = None


class Store:
    def __init__(self, path: str | Path):
        self.path = Path(path).expanduser()
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.db = sqlite3.connect(self.path)
        self.db.row_factory = sqlite3.Row
        self.db.executescript(SCHEMA)

    def add(self, e: Entry) -> Entry:
        cur = self.db.execute(
            """INSERT INTO entries (date, meal, description, servings, calories, protein_g,
               carbs_g, fat_g, mfp_food_id, mfp_food_version, mfp_serving, sync_status, sync_error)
               VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)""",
            (e.date, e.meal, e.description, e.servings, e.calories, e.protein_g, e.carbs_g,
             e.fat_g, e.mfp_food_id, e.mfp_food_version, e.mfp_serving, e.sync_status, e.sync_error),
        )
        self.db.commit()
        e.id = cur.lastrowid
        return e

    def set_sync(self, entry_id: int, status: str, error: str | None = None) -> None:
        self.db.execute("UPDATE entries SET sync_status=?, sync_error=? WHERE id=?",
                        (status, error, entry_id))
        self.db.commit()

    def delete(self, entry_id: int) -> Entry | None:
        e = self.get(entry_id)
        if e:
            self.db.execute("DELETE FROM entries WHERE id=?", (entry_id,))
            self.db.commit()
        return e

    def get(self, entry_id: int) -> Entry | None:
        row = self.db.execute("SELECT * FROM entries WHERE id=?", (entry_id,)).fetchone()
        return _row(row) if row else None

    def for_date(self, date: str) -> list[Entry]:
        rows = self.db.execute("SELECT * FROM entries WHERE date=? ORDER BY id", (date,))
        return [_row(r) for r in rows]

    def unsynced(self) -> list[Entry]:
        rows = self.db.execute(
            "SELECT * FROM entries WHERE sync_status IN ('pending','failed') ORDER BY id")
        return [_row(r) for r in rows]

    def set_goals(self, **goals: float | None) -> dict:
        current = self.goals()
        current.update({k: v for k, v in goals.items() if v is not None})
        self.db.execute(
            "INSERT OR REPLACE INTO goals (id, calories, protein_g, carbs_g, fat_g) VALUES (1,?,?,?,?)",
            tuple(current.get(k) for k in MACROS),
        )
        self.db.commit()
        return self.goals()

    def goals(self) -> dict:
        row = self.db.execute("SELECT * FROM goals WHERE id=1").fetchone()
        return {k: row[k] for k in MACROS} if row else {k: None for k in MACROS}

    def summary(self, date: str) -> dict:
        entries = self.for_date(date)
        by_meal = {m: {"entries": [], **{k: 0.0 for k in MACROS}} for m in MEALS}
        totals = {k: 0.0 for k in MACROS}
        for e in entries:
            meal = by_meal[e.meal]
            meal["entries"].append({
                "id": e.id, "description": e.description, "servings": e.servings,
                **{k: round(getattr(e, k), 1) for k in MACROS},
                "mfp_sync": e.sync_status,
            })
            for k in MACROS:
                meal[k] = round(meal[k] + getattr(e, k), 1)
                totals[k] += getattr(e, k)
        totals = {k: round(v, 1) for k, v in totals.items()}
        goals = self.goals()
        remaining = {k: (round(goals[k] - totals[k], 1) if goals[k] is not None else None)
                     for k in MACROS}
        return {
            "date": date,
            "totals": totals,
            "goals": goals,
            "remaining": remaining,
            "meals": by_meal,
            "meals_not_logged": [m for m in MEALS if not by_meal[m]["entries"]],
        }


def _row(r: sqlite3.Row) -> Entry:
    return Entry(**{k: r[k] for k in r.keys() if k != "created_at"})

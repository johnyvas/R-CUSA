import json

import httpx
import pytest

from mfp_mcp import mfp_client, server
from mfp_mcp.store import Store

FOOD = {
    "id": "123", "version": "456", "description": "Banana", "brand_name": None, "verified": True,
    "nutritional_contents": {"energy": {"unit": "calories", "value": 105},
                             "protein": 1.3, "carbohydrates": 27, "fat": 0.4},
    "serving_sizes": [
        {"value": 1, "unit": "medium", "nutrition_multiplier": 1},
        {"value": 100, "unit": "g", "nutrition_multiplier": 0.85},
    ],
}


class FakeMFP:
    def __init__(self, fail_diary=False):
        self.diary_posts = []
        self.fail_diary = fail_diary

    def handler(self, req: httpx.Request) -> httpx.Response:
        if req.url.path == "/user/auth_token":
            assert req.headers["cookie"] == "session=abc"
            return httpx.Response(200, json={"access_token": "tok", "user_id": "u1",
                                             "expires_in": 3600})
        assert req.headers["authorization"] == "Bearer tok"
        if req.url.path == "/public/nutrition":
            return httpx.Response(200, json={"items": [{"item": FOOD}]})
        if req.url.path == "/v2/foods/123":
            return httpx.Response(200, json={"item": FOOD})
        if req.url.path == "/v2/diary" and req.method == "POST":
            if self.fail_diary:
                return httpx.Response(500, text="boom")
            self.diary_posts.append(json.loads(req.content))
            return httpx.Response(201, json={"items": []})
        return httpx.Response(404)


@pytest.fixture
def env(tmp_path, monkeypatch):
    monkeypatch.setattr(server, "store", Store(tmp_path / "log.db"))
    monkeypatch.setattr(server, "today", lambda: "2026-10-03")

    def install(fake: FakeMFP | None):
        client = None
        if fake:
            client = mfp_client.MFPClient(
                "session=abc", httpx.AsyncClient(transport=httpx.MockTransport(fake.handler)))
        monkeypatch.setattr(server, "_mfp", client)
        monkeypatch.setattr(server, "mfp", lambda: client)
    return install


async def test_search_and_log_syncs_to_mfp(env):
    fake = FakeMFP()
    env(fake)
    res = await server.search_food("banana")
    opt = res["results"][0]["serving_options"][1]
    assert opt == {"serving_index": 1, "serving": "100 g", "calories": 89.2,
                   "protein_g": 1.1, "carbs_g": 22.9, "fat_g": 0.3}

    out = await server.log_food(meal="snack", mfp_food_id="123", mfp_food_version="456",
                                serving_index=0, servings=2)
    assert out["mfp_sync"] == "synced"
    assert out["logged"]["calories"] == 210
    assert out["logged"]["meal"] == "Snacks"
    item = fake.diary_posts[0]["items"][0]
    assert item["food"] == {"id": "123", "version": "456"}
    assert item["date"] == "2026-10-03" and item["meal_name"] == "Snacks"
    assert item["servings"] == 2
    assert item["serving_size"] == {"value": 1, "unit": "medium", "nutrition_multiplier": 1}


async def test_failed_sync_kept_locally_and_retried(env):
    fake = FakeMFP(fail_diary=True)
    env(fake)
    out = await server.log_food(meal="Breakfast", mfp_food_id="123")
    assert out["mfp_sync"] == "failed" and "500" in out["mfp_error"]
    assert server.get_daily_summary()["totals"]["calories"] == 105

    fake.fail_diary = False
    retried = await server.retry_mfp_sync()
    assert retried["retried"][0]["status"] == "synced"
    assert len(fake.diary_posts) == 1


async def test_manual_macros_without_mfp(env):
    env(None)
    server.set_macro_goals(calories=2000, protein_g=150)
    out = await server.log_food(meal="Lunch", description="Chicken burrito bowl",
                                calories=650, protein_g=45, carbs_g=70, fat_g=18)
    assert out["mfp_sync"] == "local_only"
    assert out["day"]["remaining"]["calories"] == 1350
    assert out["day"]["remaining"]["protein_g"] == 105
    assert out["day"]["remaining"]["carbs_g"] is None
    assert out["day"]["meals_not_logged"] == ["Breakfast", "Dinner", "Snacks"]

    check = server.check_unlogged_meals()
    assert check["meals_not_logged"] == ["Breakfast", "Dinner", "Snacks"]

    entry_id = out["logged"]["id"]
    assert server.delete_entry(entry_id)["deleted"] == entry_id
    assert server.get_daily_summary()["totals"]["calories"] == 0


async def test_validation(env):
    env(None)
    with pytest.raises(ValueError):
        await server.log_food(meal="brunch", description="x", calories=1)
    with pytest.raises(ValueError):
        await server.log_food(meal="Lunch", description="x")
    with pytest.raises(ValueError):
        await server.log_food(meal="Lunch", mfp_food_id="123")


async def test_expired_cookie_message(env, monkeypatch):
    def handler(req):
        return httpx.Response(200, text="<html>login</html>")
    client = mfp_client.MFPClient("session=old",
                                  httpx.AsyncClient(transport=httpx.MockTransport(handler)))
    monkeypatch.setattr(server, "mfp", lambda: client)
    res = await server.search_food("rice")
    assert "Refresh MFP_COOKIE" in res["error"]

# MyFitnessPal food log MCP server

Tell Claude what you ate in plain language ("2 eggs and toast for breakfast") and it gets
logged, macros get tracked against your goals, and the entry is pushed to your
MyFitnessPal diary.

## What it does

| Tool | Purpose |
|---|---|
| `search_food` | Search the MyFitnessPal food database (macros for each serving option) |
| `log_food` | Log a food, either an MFP food (synced to your MFP diary) or a manual macro estimate |
| `get_daily_summary` | Calories/protein/carbs/fat per meal, vs. goals, with what's remaining |
| `check_unlogged_meals` | Lists today's meals with nothing logged, so Claude can ask you about them |
| `set_macro_goals` | Set daily calorie and macro targets |
| `delete_entry` | Remove a local entry |
| `retry_mfp_sync` | Re-push entries whose MFP sync failed (e.g. after a cookie refresh) |

Prompts: `log_meal` (log from a description) and `daily_checkin` (catch up on missed meals).

**Making sure you actually log.** The server ships instructions telling Claude to log
food whenever you mention eating, without being asked, and to call
`check_unlogged_meals` at the start of a conversation and ask you about meals you
skipped. For a daily nudge, schedule a Claude task (e.g. 8pm: "run daily_checkin") or
add a calendar reminder.

Every entry is saved to a local SQLite log (`~/.mfp-mcp/food_log.db`), which is what
macro totals come from. MyFitnessPal sync is on top of that, so if MFP is down or your
cookie expires, nothing is lost.

## Install

Requires Python 3.10+ and [uv](https://docs.astral.sh/uv/).

```bash
git clone <this repo> && cd R-CUSA/mfp-mcp
uv sync
```

### Get your MyFitnessPal cookie

MyFitnessPal has no public API for individuals, so the server uses your logged-in
browser session (the same private endpoints the MFP website uses):

1. Log in at https://www.myfitnesspal.com in Chrome.
2. Open DevTools → **Network** tab, reload the page, click any request to
   `www.myfitnesspal.com`.
3. Under **Request Headers**, copy the full value of `Cookie:`.

Treat it like a password. It expires periodically; when sync starts failing with an
auth error, copy a fresh one and call `retry_mfp_sync`.

### Claude Desktop

Add this to `claude_desktop_config.json` (Settings → Developer → Edit Config):

```json
{
  "mcpServers": {
    "myfitnesspal": {
      "command": "uv",
      "args": ["--directory", "/ABSOLUTE/PATH/TO/R-CUSA/mfp-mcp", "run", "mfp-mcp"],
      "env": {
        "MFP_COOKIE": "paste cookie here",
        "MFP_TIMEZONE": "America/New_York"
      }
    }
  }
}
```

### Claude Code

```bash
claude mcp add myfitnesspal \
  -e MFP_COOKIE='paste cookie here' -e MFP_TIMEZONE=America/New_York \
  -- uv --directory /ABSOLUTE/PATH/TO/R-CUSA/mfp-mcp run mfp-mcp
```

Without `MFP_COOKIE` the server still works as a standalone macro tracker (Claude
estimates macros, nothing goes to MFP).

## Configuration

| Variable | Default | |
|---|---|---|
| `MFP_COOKIE` | (unset) | Browser cookie header for myfitnesspal.com; enables MFP search and sync |
| `MFP_TIMEZONE` | system | IANA timezone for "today", e.g. `Europe/Athens` |
| `MFP_MCP_DB` | `~/.mfp-mcp/food_log.db` | Local log location |
| `MFP_SEARCH_PATH`, `MFP_DIARY_PATH`, `MFP_FOOD_PATH`, `MFP_AUTH_TOKEN_PATH` | see `mfp_client.py` | Override MFP endpoints if MFP changes them |

## Limitations

- **Unofficial MFP integration.** It relies on undocumented endpoints and can break
  without notice. The endpoints are overridable via env vars so you can patch them
  without changing code.
- Only foods from MFP's database sync to the MFP diary. Manual macro estimates stay in
  the local log (MFP's custom "quick add" isn't available through this API).
- Deleting an entry only removes it locally; delete synced entries in MFP yourself.

## Development

```bash
uv sync --extra dev
uv run pytest
```

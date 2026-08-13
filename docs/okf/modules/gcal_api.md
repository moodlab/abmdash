---
type: Module
title: Google Calendar Reader
description: Effectful service-account OAuth2 read of Google Calendar events across one or many calendars.
resource: R/gcal_api.R
tags: [r, google, calendar, oauth2, httr2]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this module does: lists and fetches Google Calendar events using the same
service-account OAuth2 flow as [gsheet_api](gsheet_api.md) (calendar.readonly
scope), including a multi-calendar merge that tolerates per-calendar failures.

What it does NOT do: it is read-only — no event creation/updates; it does not
read Sheets and does not compute compliance.

# Interface

All 3 exported functions (NAMESPACE):

- `get_calendar_events(calendar_id = "primary", time_min = NULL, time_max = NULL, max_results = 10)` — list of event objects (raw Calendar API JSON)
- `list_calendars()` — calendars the service account can see (`users/me/calendarList`)
- `get_combined_calendar_events(calendar_ids, time_min = NULL, time_max = NULL, max_results = 100)` — list with `$items` merged from all calendars; one failing calendar degrades to a message, others still return

Internal helper: `get_google_access_token(service_account)`.

# Dependencies

- [gsheet_api](gsheet_api.md) — sibling sharing the JWT service-account pattern
- R packages: `httr2`, `jsonlite`, `jose`, `openssl`, `utils`
- Environment: `GOOGLE_SERVICE_ACCOUNT_JSON`

# Invariants

- Same defensive JSON parsing and `\\n` private-key unescaping as `gsheet_api`.
- `calendar_id` is `utils::URLencode`d (`reserved = TRUE`) before path interpolation.
- Tokens are requested per call with a 1-hour `exp` claim (no caching).

# Examples

```r
events <- get_calendar_events(
  time_min = paste0(Sys.Date(), "T00:00:00Z"),
  time_max = paste0(Sys.Date(), "T23:59:59Z")
)
combined <- get_combined_calendar_events(
  c("a@group.calendar.google.com", "b@group.calendar.google.com")
)
```

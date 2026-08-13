---
type: Module
title: Google Sheets Reader
description: Effectful service-account OAuth2 read of Google Sheets, plus recent-response/issue monitoring helpers.
resource: R/gsheet_api.R
tags: [r, google, sheets, oauth2, httr2]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this module does: reads Google Sheets data with a service-account OAuth2
access token (JWT-signed via `jose`/`openssl`), converting the Sheets `values`
payload into a data.frame, and provides monitoring conveniences for response
forms and the participant-issues sheet.

What it does NOT do: it is read-only (`spreadsheets.readonly` scope) — no
writes; it does not touch Google Calendar (see [gcal_api](gcal_api.md)) and
does not compute compliance (see [compliance_tracking](compliance_tracking.md)).

# Interface

All 4 exported functions (NAMESPACE):

- `read_google_sheet(sheet_url, sheet_name = NULL, range = NULL)` — data.frame; first row becomes column names, shorter rows padded with NA
- `print_sheet_head(sheet_url, n = 6, sheet_name = NULL)` — prints head, invisibly returns full data.frame
- `check_recent_responses(sheet_url, days_back = 14, timestamp_col = 1, sheet_name = NULL)` — list with `has_recent`, `recent_count`, `recent_data`, `all_data`, `cutoff_date`
- `check_participant_issues(days_back = 14, verbose = TRUE)` — recent-issues monitor against the hard-coded participant-issues sheet URL

Internal helpers: `extract_sheet_id(url)` (regex `/d/([a-zA-Z0-9_-]+)`), `get_google_sheets_access_token()` (JWT + token POST).

# Dependencies

- [compliance_tracking](compliance_tracking.md) — consumes `read_google_sheet()`
- [gcal_api](gcal_api.md) — sibling: same service-account pattern, calendar scope
- R packages: `httr2`, `jsonlite`, `jose` (JWT claim + sign), `openssl` (key read), `utils`
- Environment: `GOOGLE_SERVICE_ACCOUNT_JSON` (service-account JSON, possibly escaped in `.Renviron`)

# Invariants

- The service-account JSON is parsed defensively: outer quotes stripped, `\\"` unescaped.
- Private-key newlines arrive escaped (`\\n`) and are unescaped before `openssl::read_key`.
- Timestamp parsing tries three formats in order: `%m/%d/%Y %H:%M:%S`, `%Y-%m-%d %H:%M:%S`, then POSIX auto-detect.
- Empty sheets return `data.frame()` (with a warning), header-only sheets return a 0-row frame with the header columns.

# Examples

```r
data <- read_google_sheet(
  "https://docs.google.com/spreadsheets/d/SHEET_ID/edit",
  sheet_name = "Sheet1"
)
issues <- check_participant_issues(days_back = 7)
```

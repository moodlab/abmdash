---
type: Module
title: GABM Compliance Tracking
description: Computes expected-vs-actual weekly sessions from Google Sheets gameplay events for GABM participants.
resource: R/compliance_tracking.R
tags: [r, compliance, sheets, dashboard]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this module does: reads gameplay events from a Google Sheet via
[gsheet_api](gsheet_api.md), filters to completed sessions (180 turns), derives
week-1 start dates, builds week 1-4 study windows, and flags late participants
(`late` = `sess_cnt + 1 < expect_cnt`). Also surfaces 178-179-turn sessions as
suspiciously short.

What it does NOT do: it does not handle traditional ABM data from the ABS
portal (see [trad_compliance](trad_compliance.md)) and does not collapse to
per-participant rows (see [compliance_summary](compliance_summary.md)).

# Interface

Both exported functions (NAMESPACE):

- `get_compliance_report(sheet_url, sheet_name = NULL, exclude_ids = c("123456789", "12345678"), sessions_per_week = 4)` — list with `compliance` (active participant-weeks) and `short_sessions` (178-179 turns)
- `get_late_participants(sheet_url, sheet_name = NULL)` — data.frame of rows flagged `late` (invisible return)

# Dependencies

- [gsheet_api](gsheet_api.md) — consumes `read_google_sheet()`
- [compliance_summary](compliance_summary.md) — consumer of `get_compliance_report()`
- R packages: base R (`aggregate`, `merge`, `difftime`)

# Invariants

- Sheet columns are mapped by display name: `Referral ID`, `Date of Event UTC`, `Event Type`, `Week #`, `Session #`, `Turns Completed`; "N/A" string values are converted to NA BEFORE `as.numeric()`.
- A session counts only when `Turns Completed == 180`; `sess_cnt` is the max session number per id+week; missing counts default to 0.
- Expected rate: `sessions_per_week / 7` per day (default 4/week); compliance window is `0 < time_from_start < 5` weeks.
- Excluded: default test IDs, any id containing "test" (case-insensitive), non-Gameplay events.

# Examples

```r
result <- get_compliance_report(sheet_url)
head(result$compliance)
late <- get_late_participants(sheet_url)
```

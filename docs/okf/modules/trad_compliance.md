---
type: Module
title: Traditional ABM Compliance
description: Computes traditional-ABM compliance from the ABS portal CSV, mirroring the GABM summary output shape.
resource: R/trad_compliance.R
tags: [r, compliance, abs, dashboard]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this module does: logs into the ABS portal via [abs_login](abs_login.md),
downloads the session CSV, and computes a per-participant compliance summary
(current week, weeks from start, total sessions, expected sessions, sessions
behind, start date, On Track / Behind-by-N status) that mirrors the output
shape of `get_participant_summary()` from [compliance_summary](compliance_summary.md).

What it does NOT do: it does not read Google Sheets, does not touch REDCap,
and does not handle GABM gameplay events (see
[compliance_tracking](compliance_tracking.md)).

# Interface

Exported functions (NAMESPACE):

- `get_trad_compliance_summary(base_url = "https://abs.la.utexas.edu", csv_path = "/admin/test/download-csv-all", exclude_pattern = "test", sessions_per_week = 4, active_window_weeks = 5, verbose = FALSE)` — data.frame, one row per active participant, most-behind first

Internal helper: `process_trad_compliance_data(trad_data, ...)` — pure transform of the raw CSV (testable without the portal).

# Dependencies

- [abs_login](abs_login.md) — consumes `abs_login()` and `download_abs_csv()`
- R packages: base R only

# Invariants

- Requires columns `subject_id`, `session`, `start_time`; missing columns raise `stop()` before any computation.
- `start_time` parsed with `as.POSIXct(..., tz = "UTC")`; test ids filtered by `exclude_pattern` (default `"test"`, case-insensitive).
- Active window: `weeks_from_start <= active_window_weeks` (default 5); expected sessions capped at 16.
- Status: `"On Track"` when `sessions_behind <= 0`, else `"Behind by N"`.
- The empty result is returned as a 0-row frame with the full column schema (early returns never change shape).

# Examples

```r
summary <- get_trad_compliance_summary()
print(summary[, c("id", "current_week", "status")])
```

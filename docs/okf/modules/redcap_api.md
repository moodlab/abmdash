---
type: Module
title: REDCap API Shell
description: Effectful httr2 shell over the REDCap API — records, metadata, reports, logs, and derived eligibility/enrollment stats.
resource: R/redcap_api.R
tags: [r, redcap, api, httr2, invariant]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this module does: POSTs REDCap API calls to
`https://redcap.prc.utexas.edu/redcap/api/` using the token from the
`REDCAP_API_TOKEN` environment variable, and derives screening/eligibility,
weekly-screening, and enrollment statistics from REDCap reports 14081 and 13387.

What it does NOT do: it does not persist data, does not render anything, does
not touch Google Sheets/Calendar or the ABS portal, and does not own the
compliance calculations (those live in [compliance_tracking](compliance_tracking.md)
and [trad_compliance](trad_compliance.md)).

# Interface

All 9 exported functions (NAMESPACE):

- `call_redcap_api(content = "record", format = "json", ...)` — raw API call; parses JSON to list, CSV to data.frame
- `get_redcap_records(fields = NULL, forms = NULL, records = NULL, events = NULL, format = "json")` — records with field/form/record/event filters
- `get_redcap_metadata(format = "json")` — data dictionary
- `get_survey_completions(surveys = NULL, records = NULL, format = "json")` — long-format `record_id`/`survey_instrument`/`survey_timestamp`/`survey_complete` from `_timestamp`/`_complete` fields
- `get_redcap_logs(records = NULL, begin_time = NULL, end_time = NULL)` — logging data
- `get_redcap_report(report_id, format = "json", date_begin = NULL, date_end = NULL)` — report export by ID
- `get_eligible_participants()` — eligibility filter over report 14081 (past 30 days, `phq8score >= 17`, commute/austin/phone/computer/bpd/druguse/medchng criteria)
- `get_weekly_screening_stats()` — past-7-day screening totals, eligible and Hispanic counts from report 14081
- `get_enrollment_stats()` — enrollment counts from report 13387 keyed on a GUID field

Internal helpers: `get_redcap_token()` (env read), `` `%||%` `` (null coalescing).

# Dependencies

- [demographics](demographics.md) — consumes `get_redcap_report()` (report 13349)
- [week12_tracking](run_initial_function.md) — consumes `get_redcap_logs()` for follow-up scheduling (dependency mention; 1-export module, no own doc)
- R packages: `httr2` (HTTP), `jsonlite`, `utils` (CSV), plus `as.Date`/`aggregate` from base R

# Invariants

- REDCap API returns empty string `""` (NOT `NA`) for unset fields; `as.numeric("")` returns `NA`.
- Parse numeric REDCap fields ONCE with `suppressWarnings(as.numeric(...))` and guard on the PARSED value — never `!is.na(raw) & as.numeric(raw) >= N`. An NA leaking into a logical row index inserts an all-NA row, and `data.frame()` then raises "row names contain missing values".
- Use `sapply(..., USE.NAMES = FALSE)` so value vectors never become row names (see the `r01es_name` -> `first_name` extraction).
- **FLAG (L441/L563):** the phq8score raw-string guard (`!is.na(phq8score) & as.numeric(phq8score) >= 17`) that caused bug #38 sat at `redcap_api.R` L441 (`get_eligible_participants`) and L563 (`get_weekly_screening_stats`). Fix #39 (commit 90ef649) replaced both with parse-once guards (now at L446 / L575). The raw-guard STYLE (`!is.na(x) & x == "1"`) persists on sibling eligibility fields at L440-450 / L568-580 — benign for character equality, but flagged as the remaining sweep target for wave-3a / AC-3.9.
- All wrapper functions return zero-row data.frames / error-carried frames rather than raising on empty reports.

# Examples

```r
# Raw records
recs <- call_redcap_api("record", format = "csv")

# Eligibility over the past 30 days (report 14081)
eligible <- get_eligible_participants()

# Enrollment stats (report 13387)
enr <- get_enrollment_stats()
enr$total_enrolled
```

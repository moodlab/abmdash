---
type: Module
title: Bootstrap and Misc Helpers
description: Package entry stub, dashboard-encryption hook, Central-Time clock, enrollment-targets CSV reader, and home of the week12_tracking dependency mention.
resource: R/run_initial_function.R
tags: [r, bootstrap, dashboard, env]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this module does: hosts the package's small bootstrap/utility functions —
the `run_initial_function(n)` entry stub, the `encrypt_dashboard()` staticrypt
hook, a Central-Time clock for the dashboard header, and the enrollment-targets
CSV reader. It is also the anchor doc for `week12_tracking.R` (a 1-export module,
below the okf-bundle concept threshold, so it is documented as a dependency here).

What it does NOT do: it is not the data engine — no REDCap/ABS/Google calls
beyond the CSV read; no compliance logic.

# Interface

Exported functions (NAMESPACE):

- `run_initial_function(n)` — returns `n + n` (entry stub)
- `encrypt_dashboard()` — no-op unless `STATICRYPT_PASSWORD` is set; resolves the docs dir as `/app/docs` (Docker) → `../../docs` → `docs`
- `get_central_time()` — formatted Central Time string (`%Y-%m-%d %I:%M %p %Z`, tz `America/Chicago`)
- `get_enrollment_targets()` — data.frame from `enrollment_targets.csv`, probing 6 candidate paths (system.file extdata, `inst/extdata/`, `data/`, relative variants)

Related export defined in the sibling 1-export module week12_tracking (dependency):

- `get_upcoming_followups(days_ahead = 14)` — Week 12/16/28 follow-up scheduling from REDCap logs (source: `R/week12_tracking.R`)

# Dependencies

- week12_tracking — `get_upcoming_followups()` (dependency mention; consumes `get_redcap_logs()` from [redcap_api](redcap_api.md))
- [redcap_api](redcap_api.md) — transitively, via `get_upcoming_followups()`
- Files: `inst/extdata/enrollment_targets.csv`, `data/enrollment_targets.csv`
- R packages: `utils` (read.csv)

# Invariants

- `encrypt_dashboard()` must never fail the build when `STATICRYPT_PASSWORD` is unset — it returns invisibly.
- `get_enrollment_targets()` raises if none of the 6 candidate paths exist.
- `get_upcoming_followups()` skips withdrawn participants and already-completed follow-up types.

# Examples

```r
run_initial_function(5)          # 10
get_central_time()               # "2026-08-13 10:30 AM CDT"
targets <- get_enrollment_targets()
encrypt_dashboard()              # no-op without STATICRYPT_PASSWORD
```

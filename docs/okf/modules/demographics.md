---
type: Module
title: Demographics Summary
description: Summarizes REDCap demographic reports (race, ethnicity, sex, age) for enrolled participants.
resource: R/demographics.R
tags: [r, demographics, redcap, pure]
timestamp: 2026-08-13
pure: true
---

# Responsibility

What this module does: pulls a REDCap demographic report (default report 13349)
via [redcap_api](redcap_api.md), optionally filters to enrolled IDs, recodes
race/ethnicity/sex from REDCap checkbox/coded fields, computes age from
`interview_age`, and builds summary tables for dashboard display.

What it does NOT do: it does not compute compliance, does not handle
enrollment statistics (see `get_enrollment_stats()` in
[redcap_api](redcap_api.md)), and does not render. `summarize_demographics()`
is a pure transform; the only effectful boundary is the `get_redcap_report()`
fetch inside `get_demographic_summary()`.

# Interface

Both exported functions (NAMESPACE):

- `get_demographic_summary(report_id = "13349", enrolled_ids = NULL)` — data.frame with `record_id`, `race`, `ethnicity`, `sex`, `age`
- `summarize_demographics(demo_data)` — list with `overall`, `age_groups`, `sex`, `race`, `ethnicity` summary frames

# Dependencies

- [redcap_api](redcap_api.md) — consumes `get_redcap_report()`
- R packages: base R only

# Invariants

- REDCap event names are fixed: `eligibility_screen_arm_1` (race/sex/age) and `week_0_eligibility_arm_1` (ethnicity).
- Race recoding from checkbox fields `raceid___1`..`raceid___7` ("American Indian", "Asian", "Black", "Hawaiian", "White", "More than one race", "Unknown").
- Unknown values fall back to `"Unknown"` for categorical fields; missing age stays NA; `summarize_demographics()` drops "Unknown" and NA before counting.
- Age groups: `cut(breaks = c(0,18,25,35,45,55,100), right = FALSE)`.

# Examples

```r
demo <- get_demographic_summary()
summaries <- summarize_demographics(demo)
summaries$race
```

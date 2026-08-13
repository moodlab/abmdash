---
type: Module
title: Participant Compliance Summary
description: Pure per-participant view over the compliance report — one row per participant, current week, totals, and sessions behind.
resource: R/compliance_summary.R
tags: [r, compliance, pure, dashboard]
timestamp: 2026-08-13
pure: true
---

# Responsibility

What this module does: collapses the long-format compliance report from
[compliance_tracking](compliance_tracking.md) into one row per participant
(current week, weeks from start, total completed sessions, expected sessions,
sessions behind, start date), sorted most-behind-first, and a convenience
filter for behind participants.

What it does NOT do: it performs no network I/O itself (it delegates sheet
reading to `get_compliance_report()`), does not touch REDCap or the ABS portal,
and does not render.

# Interface

Both exported functions (NAMESPACE):

- `get_participant_summary(sheet_url, sheet_name = NULL, exclude_ids = c("123456789", "12345678"), sessions_per_week = 4)` — data.frame, one row per participant, sorted by `sessions_behind` descending
- `get_behind_participants(sheet_url, sheet_name = NULL, min_sessions_behind = 1)` — subset of the summary where `sessions_behind >= min_sessions_behind` (invisible return)

# Dependencies

- [compliance_tracking](compliance_tracking.md) — consumes `get_compliance_report()`
- R packages: base R only (no imports beyond what compliance_tracking pulls in)

# Invariants

- Expected sessions are capped at 16 (4 weeks x 4/week); `current_week` is capped at 4.
- `sessions_behind` negative means ahead of schedule; sorting is descending by `sessions_behind` (most behind first).
- `start_date` is back-derived from the latest week's start: `start - ((current_week - 1) * 7 days)`.

# Examples

```r
summary <- get_participant_summary(sheet_url)
behind <- get_behind_participants(sheet_url, min_sessions_behind = 2)
```

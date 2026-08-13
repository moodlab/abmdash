---
status: in-progress
feature: repo-usability
slice: AC-3.5
issue: 56
verification: code
---

# AC-3.5 — Refactor redcap_api.R for readability (zero behavior change)

## Spec (from issue #56)

Refactor the 844L R/redcap_api.R: extract helpers, kill nested loops
(L243/L257/L667), rename for intent. R-learner audience, base R only, no new
deps. 9 exports byte-identical NAMESPACE + \usage{} frozen (named-arg callers
demographics.R:18 / week12_tracking.R:21 break invisibly otherwise). Unpinned
stop/warning strings grep-preserved. Shadowing resolved, %||% kept with honest
comment. Atomic bisectable commits. Verified by 64-test redcap lock suite.

**Predicate (10 conjuncts, ALL must hold):**
- P1: NAMESPACE byte-identical (9 exports, no additions).
- P2: \usage{} FROZEN in regenerated man/*.Rd for the 9 exports (param
  names/order unchanged). CRITICAL: named-arg callers
  R/demographics.R:18 (get_redcap_report(report_id, format="csv")) +
  R/week12_tracking.R:21 (get_redcap_logs(begin_time=)) are NOT covered by
  the positional lock — param rename = runtime break the suite cannot see.
- P3: Full lock suite green: test-redcap-behavior-lock.R (64) +
  test-eligible-participants.R + test-faq-verbatim.R.
- P4: UNPINNED signal strings grep-preserved verbatim: stop L176 "Could not
  retrieve metadata from REDCap"; warning L206 "No survey instruments found";
  warning L229 "No survey data retrieved".
- P5: Nested loops eliminated from export bodies: L243, L257, L667 extracted
  into named helpers or one-level iteration.
- P6: Shadowing resolved: local `content <- resp_body_json(...)` shadowing
  param `content` renamed to `parsed_response`; lock test comment updated.
- P7: %||% guard (L502) preserved with honest comment.
- P8: No new deps; base R only.
- P9: roxygen idempotent: document() twice → byte-identical man/; helpers
  @keywords internal (no new @export).
- P10: Atomic bisectable commits: only R/redcap_api.R + man/ + NAMESPACE +
  lock-test COMMENTS touched; no commit >60 logic lines; git bisect run
  devtools::test() viable.

**Negative cases:** param rename with lock green; unpinned string altered;
warning→message swap; %||% deletion; shadowing left with stale comment;
row-order flip; &→&& NA guard reorder; factor coercion; error-frame column
rename; rewrite-not-refactor (>60 lines/commit); new @export on helper;
non-idempotent roxygen.

## Extraction map (from issue)

- call_redcap_api → build_request_body + parse_response (shadow fix)
- get_survey_completions → list_to_df, ensure_fields (L243), reshape_long
  (L257), activity_guard (+ metadata_to_df for the %||% metadata conversion)
- eligibility (get_eligible_participants + get_weekly_screening_stats share
  helpers) → report_to_df, filter_recent_dates, eligibility_mask,
  first_name_of. Keep `suppressWarnings(as.numeric(` + `USE.NAMES` verbatim
  (test-faq-verbatim.R L205-206 scans ALL R/ sources).
- enrollment → detect_guid_field (L667), group_enrolled,
  monthly_breakdown_counts. Preserve debug fields available_fields /
  date_range / valid_dates_count (lock pins; month_year must stay attached to
  enrolled_df before names() at return).

## Rename map (1:1)

- x → metadata_entry / survey_record / report_record (context)
- content (local) → parsed_response
- result_list → instrument_rows
- final_result → combined_survey_data

## Commit cadence (one logical change per commit, lock green after each)

1. build_request_body extraction
2. parse_response extraction + content→parsed_response (shadow fix) + lock
   test shadowing comment update
3. list_to_df + ensure_fields + metadata_to_df (L243 loop eliminated)
4. reshape_long + activity_guard + empty_survey_frame (L257 loop eliminated)
5. eligibility helpers + get_eligible_participants rewire
6. get_weekly_screening_stats rewire onto shared helpers
7. detect_guid_field (L667 loop eliminated)
8. group_enrolled extraction
9. monthly_breakdown_counts extraction + weekly/monthly section rewire
10. rename cluster + %||% honest comment + lock test %||% comment + stale
    line-ref fixes in lock test comments
11. roxygen regen ×2 (idempotent) + probe checks

## Progress

- [x] 2026-08-13: baseline lock suite green (64 + 25 + 62) before any change
- [ ] commit 1..11 (in progress)
- [ ] full suite + probes + PR

## Decision Log

- **Helpers documented with `@keywords internal`** (repo convention, matches
  gsheet_api.R/gcal_api.R) → roxygen2 generates man/<helper>.Rd files with
  their own \usage{} lines. These are ADDITIONS, not changes to the 9 export
  Rd files; probe "no \usage{} changes" = export Rd files byte-identical.
- **Cadence atomized to 11 commits** (8-step cadence is a logical grouping):
  enrollment split into detect_guid_field / group_enrolled /
  monthly_breakdown_counts so no commit exceeds 60 logic lines.
- **monthly_breakdown_counts(enrolled_df)** (helper name distinct from local
  var `monthly_breakdown`) — caller attaches month_year to enrolled_df
  BEFORE calling the helper so available_fields debug field keeps
  "record_id, parsed_interview_date, month_year" (lock-pinned).
- **metadata_to_df** added beyond the named map — the %||% metadata
  list→df conversion in get_survey_completions needs a name; list_to_df is
  the survey-data conversion (it pairs with ensure_fields L243).

## Surprises & Discoveries

- test-faq-verbatim.R scans the CONCATENATION of ALL R/ files, not just
  redcap_api.R — so suppressWarnings(as.numeric( + USE.NAMES must survive
  verbatim anywhere in R/ after extraction.
- detect_guid_field for-loop can be replaced by intersect(known, names)
  (intersect preserves first-arg order) — one-level iteration, no loop.
- Lock test comments cite original line numbers (L65, L271-276, L284, L440,
  L446, L470-477, L502, L551, L569, L575, L587-588, L690, L763, L786, L800,
  L826, L491-498, L599-607, L831-843) — these go stale as the file shrinks;
  refresh them in the rename commit so reviewers don't flag doc drift.

## Probe (from issue)

Rscript -e 'devtools::document(); devtools::document(); devtools::test(filter="redcap-behavior-lock"); devtools::test(filter="eligible-participants"); devtools::test(filter="faq-verbatim")' && grep -F the 3 unpinned strings && git diff man/ shows no \usage{} changes (on the 9 export Rd files).

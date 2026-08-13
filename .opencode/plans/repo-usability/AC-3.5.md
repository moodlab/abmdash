---
ac: 3.5
depends_on: AC-3.2 (lock suite)
risk: high
status: spec
---

# AC-3.5: Refactor redcap_api.R for readability (ZERO behavior change)

## Executable Spec
- **predicate (10 conjuncts, ALL must hold):**
  P1 NAMESPACE byte-identical (9 exports, no additions).
  P2 **\usage{} FROZEN in regenerated man/*.Rd for the 9 exports** — param names/order unchanged. CRITICAL: named-arg callers R/demographics.R:18 (get_redcap_report(report_id, format="csv")) + R/week12_tracking.R:21 (get_redcap_logs(begin_time=)) are NOT covered by the positional lock — param rename = runtime break the suite cannot see.
  P3 Full lock suite green: test-redcap-behavior-lock.R (64 tests) + test-eligible-participants.R + test-faq-verbatim.R.
  P4 **UNPINNED signal strings grep-preserved verbatim**: stop L176 "Could not retrieve metadata from REDCap"; warning L206 "No survey instruments found"; warning L229 "No survey data retrieved" (no test pins these — grep-check).
  P5 Nested loops eliminated from export bodies: L243, L257, L667 (corrected from AC's L655) extracted into named helpers or one-level iteration.
  P6 Shadowing resolved: local `content <- resp_body_json(...)` shadowing param `content` at L65 renamed to `parsed_response`; update lock test comment from "shadowing quirk locked as-is" → "shadowing resolved; output identical".
  P7 %||% guard (L502) preserved: keep with honest comment ("fixture has zero NULL fields; right branch untested by lock — guard retained for real metadata") OR explicit if(is.null). Removal FORBIDDEN (lock passes without it but real metadata breaks).
  P8 No new deps; base R only.
  P9 roxygen idempotent: document() twice → byte-identical man/; helpers @keywords internal (no new @export).
  P10 Atomic bisectable commits: only R/redcap_api.R + man/ + NAMESPACE + lock-test COMMENTS touched; no commit >60 logic lines; git bisect run devtools::test() viable.
- **probe:** Rscript -e 'devtools::document(); devtools::document(); devtools::test(filter="redcap-behavior-lock"); devtools::test(filter="eligible-participants"); devtools::test(filter="faq-verbatim")' && grep -F the 3 unpinned strings && git diff man/ shows no \usage{} line changes.
- **negative:** param rename with lock green (P2); unpinned string altered; warning→message swap; %||% deletion; shadowing left with stale comment; row-order flip; &→&& NA guard reorder; factor coercion; error-frame column rename; rewrite-not-refactor (>60 lines/commit); new @export on helper; non-idempotent roxygen.
- **verification:** code. ui: block NOT applicable.
- **fixture status:** existing lock suite; only test-comment edits.
- **rubric anchor:** §5, §2, §4.

## Design Intent
§1 9 export signatures frozen. §2 extraction pushes pure transforms into named helpers; I/O confined to call_redcap_api. §3 helpers within R/redcap_api.R (module ownership single). §4 header retained; helpers "internal, used by <export>". §5 loop bodies → named helpers; for-loop retained when accumulation is intent (novice-readable).

## Technical Context
- Files: R/redcap_api.R, man/ (regen), NAMESPACE (regen metadata), test-redcap-behavior-lock.R (comments only).
- Extraction map: call_redcap_api → build_request_body + parse_response (shadow fix); get_survey_completions → list_to_df, ensure_fields (L243), reshape_long (L257), activity_guard; eligibility → report_to_df, filter_recent_dates, eligibility_mask — keep suppressWarnings(as.numeric( + USE.NAMES verbatim (FAQ scans source L205-206); enrollment → detect_guid_field (L667), group_enrolled, monthly_breakdown — preserve debug fields available_fields/date_range/valid_dates_count (lock pins).
- Rename map (1:1): x → metadata_entry/survey_record/report_record (context); content (local) → parsed_response; result_list → instrument_rows; final_result → combined_survey_data.
- Commit cadence (8 steps, test green after each): helpers-extract request/parse → shadow fix → L243 → L257 → eligibility → L667 → renames → roxygen regen + idempotence check.
- Loop strategy: extract body into named helper; one-level lapply preferred; for OK for accumulation.

## Dependencies
- depends_on: AC-3.2. Blocks: AC-3.6.
- conflict set: R/redcap_api.R, man/, NAMESPACE, test-redcap-behavior-lock.R.
- risk: HIGH.

### Progress
- [ ] refactor — pending
### Decision Log
- spec-resolved — \usage{} freeze mandatory (named-arg callers); shadowing resolved (not "locked as-is"); %||% kept with honest comment.
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: per-commit lock-green; bisect via git bisect run devtools::test().
- Rollback: git revert; lock suite catches drift.
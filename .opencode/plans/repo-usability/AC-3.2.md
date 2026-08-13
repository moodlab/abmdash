---
ac: 3.2
depends_on: AC-3.1
risk: medium
status: complete
---

# AC-3.2: Behavior-lock redcap_api.R (9 exports, "" empty-string cases)

## Executable Spec
- **predicate (10 conjuncts):**
  1. EXPORT COVERAGE (9, value-level): call_redcap_api, get_redcap_records, get_redcap_metadata, get_survey_completions, get_redcap_logs, get_redcap_report, get_eligible_participants, get_weekly_screening_stats, get_enrollment_stats. (AC-listed check_recent_responses/check_participant_issues = gsheet_api.R; get_late_participants = compliance_tracking.R; get_behind_participants = compliance_summary.R — OUT of scope.) Shape-only snapshots = vacuous = fail.
  2. "" EMPTY-STRING FIDELITY at exact fields: phq8score="" (parse-once guard L446/L575); ≥1 sibling raw-guard case r01es_commute="" (L440-450/L568-580 style, not counted); interview_date=""; r01es_name="" → "Unknown" in output; r01es_hispanic="" (not counted L587-588); guid="" (not enrolled L690). Each "" case documents LOCKING-CURRENT (fix deferred to AC-3.9).
  3. FIXTURES COMMITTED + TOKEN-REDACTED: grep -rlE '[a-f0-9]{32}' fixtures/ → empty; set_redactor() configured.
  4. OFFLINE-GREEN with DUMMY token: get_redcap_token() (L7-13) stops BEFORE httptest2 intercepts (env gate upstream of req_perform) → setup uses withr::local_envvar(REDCAP_API_TOKEN="DUMMY") + one DIRECT test asserting the verbatim stop("REDCAP_API_TOKEN environment variable is not set or is empty") (string also referenced in test-faq-verbatim.R:91 — must stay stable).
  5. NO R/ EDITS: git diff --name-only of branch → zero files under R/.
  6. DETERMINISM: Sys.Date() frozen (usages L415/L545/L763 + format(Sys.Date(),"%B %Y") L628/L763); fixture dates relative to frozen date.
  7. ROW-ORDER PINNED exactly (no sort/normalization — aggregate() L687/L714 + do.call(rbind,...) L284 order changes must fail).
  8. REPORT-ID BRANCH COVERAGE: fixtures for 14081 (eligible + weekly) AND 13387 (enrollment, L612), including empty-result per report.
  9. MULTI-REQUEST: get_survey_completions covered by 2 fixtures (metadata call L173 + records call L222); %||% (L502) locked indirectly.
  10. ERROR-RETURN SHAPES locked: HTTP-error fixture per stats fn asserting tryCatch error frames (L491-498/L599-607/L831-843).
- **probe:** cd repo && Rscript -e 'devtools::test(filter="redcap-behavior-lock")' (NOT bare test_dir). Full-suite gate: make test.
- **negative:** unlocked export or shape-only snapshot; "" case missing at any of phq8score/r01es_commute-sibling/interview_date/r01es_name/r01es_hispanic/guid; live-API dependence claimed offline without dummy token; 32-hex token in committed fixtures; date-dependent failure under frozen vs unfrozen Sys.Date; missing 13387 path; get_survey_completions with only 1 fixture; row order normalized; error shape unlocked; anything under R/ modified.
- **verification:** code. ui: block NOT applicable.
- **fixture status:** NEW — test-redcap-behavior-lock.R + httptest2 fixtures. Coexists ADDITIVELY with existing test-eligible-participants.R (2 of 9 covered via local_mocked_bindings — do NOT supersede). httptest2 added by AC-3.1.
- **rubric anchor:** §2, §5.

## Design Intent
§1 "" vs NA distinction representable-invariant, locked at value level. §2 HTTP layer isolated via httptest2 mocks; everything else deterministic. §3 scope = redcap_api.R only. §4 lock documents current contract incl. quirks (content param shadowing L65). §5 testable without live network; internal helpers (get_redcap_token, %||%) locked via direct/indirect tests.

## Technical Context
- Files: tests/testthat/test-redcap-behavior-lock.R (NEW), httptest2 fixtures under tests/testthat/ (NEW). renv.lock via AC-3.1 only.
- Per-export plan: call_redcap_api (generic record call); get_redcap_records (record fixture + field ""); get_redcap_metadata; get_survey_completions (2 fixtures); get_redcap_logs; get_redcap_report (14081+13387+empty); get_eligible_participants (14081; phq8score="" L446, r01es_commute="" L440-style, r01es_hispanic="" L587-588); get_weekly_screening_stats (14081; phq8score="" L575, sibling raw-guard); get_enrollment_stats (13387 + error fixture; guid="" L690).
- Token redaction: set_redactor + CI grep guard. Order: exact, no sorting helper. Error shapes: one HTTP-error fixture per stats fn.
- Quirks locked as-is: content param shadowing (L65); %||% coalescing (L502).

## Dependencies
- depends_on: AC-3.1 (httptest2 harness + renv.lock). Blocks: AC-3.9 sweep (lock before sweep changes guards).
- conflict set: test-redcap-behavior-lock.R, httptest2 fixtures, R/redcap_api.R (read-only).
- risk: medium (fixture recording + determinism freezing; zero production-code risk).

### Progress
- [x] redcap locked — 2026-08-13 (test-redcap-behavior-lock.R, 64 tests; full suite 202 green; tautology spot-check + determinism verified)
### Decision Log
- spec-resolved — "WITHOUT token" in AC text is misleading: tests require DUMMY token via local_envvar (env gate fires upstream of httptest2).
- variant mock roots — same request body needs populated/empty/error responses; committed under fixtures/redcap-errors/ + fixtures/redcap-empty/ and prepended per-test via .mockPaths() (find_mock_file first-match).
- passthrough lock style — list-returning exports (records/metadata/logs/report) locked against the committed fixture content (parse_fixture_body) = reviewed canonical value; derived outputs locked against explicit literals.
### Surprises & Discoveries
- get_enrollment_stats() available_fields is "record_id, parsed_interview_date, month_year" — L786 mutates enrolled_df (adds month_year) BEFORE L826 snapshots names(enrolled_df); the two-column expectation was wrong.
- httr2::resp_body_json() returns list-of-lists (jsonlite simplifyVector=FALSE behavior) even for uniform record arrays — not data.frames; "" survives the round-trip as "" (never NA), "[]" → list() (length 0) which is what triggers the empty-report branches.
- get_redcap_report() sends returnFormat TWICE (call_redcap_api L50 + get_redcap_report L351); the form body is "…&returnFormat=json&returnFormat=json" — fixture hashes were captured from the real requests, never hand-computed.
- httptest2 replays a 400 .R fixture as a response and httr2's OWN error machinery then throws ("HTTP 400 Bad Request.") — so the stats fns' tryCatch error frames are reachable through fixtures.
- monthly_breakdown row names are c(2,1) after the L800 descending sort — value ORDER is the lock; row names are a cosmetic sort artifact (stripped before comparison).
### Idempotence & Recovery
- Safe retry: re-record fixtures with creds-holder workflow; tests idempotent offline.
- Rollback: git revert.

### Carryover Log
- PR #54 (AC-3.2): merged 1b5241e. Builder self-reviewed (no task tool) — independent post-hoc review dispatched. Known nit: 14081 error fixture shared by get_eligible_participants + get_weekly_screening_stats (same request body forces one file; both error frames asserted). CI devtools flake fixed via extra-packages (PR #54). Status: resolved (post-hoc review pending result).
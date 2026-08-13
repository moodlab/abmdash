---
ac: 3.4
depends_on: AC-3.1
risk: medium
status: spec
---

# AC-3.4: Behavior-lock abs_login.R + 5 small modules (16 exports)

## Executable Spec
- **predicate:** All 16 exports value-locked via devtools::test() offline-green with ABS_USERNAME/ABS_PASSWORD unset; no R/ edits. Exports: abs_login.R (abs_login, download_abs_csv, preview_abs_csv, test_abs_connection, verify_abs_login), compliance_summary.R (get_participant_summary, get_behind_participants), compliance_tracking.R (get_compliance_report, get_late_participants), demographics.R (get_demographic_summary, summarize_demographics), week12_tracking.R (get_upcoming_followups), run_initial_function.R (run_initial_function, encrypt_dashboard, get_central_time, get_enrollment_targets). (AC context misattributed get_enrollment_stats→redcap_api.R:618, check_recent_responses→gsheet_api.R:294, get_participant_summary→compliance_summary.R:29 — corrected.)
  - ABS module: pure helpers tested DIRECTLY via abmdash:::extract_livewire_snapshot / :::extract_csrf_token vs recorded HTML covering ALL 4 CSRF fallback patterns (data-csrf attr L214, meta csrf-token L220, hidden _token name-first L228, value-first L229); abs_login/download_abs_csv/test_abs_connection/verify_abs_login mock httr2::req_perform ONLY (never the exports — FAKE-LOCK defense: existing test-trad-compliance.R mocks abs_login/download_abs_csv at consumer level; new tests must not repeat that or Livewire snapshot extraction/CSRF/redirect/base64 decode/list-tests selection never run); preview_abs_csv mocks download_abs_csv (thin wrapper, allowed); full live login skip_if_not(Sys.getenv("ABS_USERNAME") != "") + documented.
  - Small modules: mock API boundary (read_google_sheet/get_redcap_report/get_redcap_logs), pin Sys.time/Sys.Date via local_mocked_bindings(.package="base") to fixed timestamps, assert EXACT derived values (time_from_start, expect_cnt, late, age, days_until_due); builder MUST add tripwire assert proving mock took effect.
  - get_enrollment_targets: REAL data/enrollment_targets.csv (also mirrored inst/extdata/) with FILE-ISOLATION assert (content unchanged before/after).
  - Value assertions on content, not shape-only. Empty-collection cases assert colnames + types, not just 0 rows.
- **probe:** env -u ABS_USERNAME -u ABS_PASSWORD Rscript -e 'devtools::test()' + offline check (network disabled stays green).
- **negative:** (1) fake-lock — mocking export instead of req_perform; (2) date flakiness — relative-date fixtures + >0 asserts; (3) shape-only asserts; (4) CSRF single-pattern fixture masks regex break in other 3 fallbacks; (5) live-site dependence; (6) coverage gap — internal helpers untested (regex break masked by mocked export); (7) data-file mutation (enrollment_targets.csv changed by test); (8) empty-collection silent pass.
- **verification:** code. ui: block NOT applicable.
- **fixture status:** NEW — recorded HTML fixtures (login page, tests page, 4 CSRF variants, Livewire auth/redirect/CSV responses) under tests/testthat/fixtures/; REAL — data/enrollment_targets.csv (974B, verified; mirrored inst/extdata/); EXPANDED — tests/testthat/test-run-initial-function.R (84B stub).
- **rubric anchor:** §2 (pure parsers tested directly; effectful shell mocked at httr2 boundary), §5.

## Design Intent
§1 locked return shapes (colnames+types) + exact error message strings for the ABS stop branches + enrollment CSV. §2 core split enforced — pure HTML/CSV parsers with recorded fixtures; every network call mocked at the API-function boundary. §3 mock boundary table is the contract — cut at network/API seam, never export seam. §4 per-module test files; shared fixtures. §5 pinned-clock tests verify each export's exact computed values.

## Technical Context
- Files: NEW test-abs-login.R, test-compliance-tracking.R, test-compliance-summary.R, test-demographics.R, test-week12-tracking.R; EXPANDED test-run-initial-function.R; NEW fixtures/abs_*.html, fixtures/abs_livewire_*.json/html; REAL data/enrollment_targets.csv (read-only).
- Mock boundary table: abs helpers direct ::: (pure); abs exports req_perform ONLY; compliance_tracking read_google_sheet (pin Sys.time); compliance_summary get_compliance_report; demographics get_redcap_report (pin Sys.Date); week12 get_redcap_logs (pin Sys.time+Sys.Date; fixtures cover W4 Acute, withdrawal, W12/16/28-completed); run-initial none/real file.
- Date-pinning: local_mocked_bindings(Sys.time=..., Sys.Date=..., .package="base"); fallback assignInNamespace or mockr::with_mock if primitives un-mockable; tripwire assert mandatory.
- ABS error paths locked (stop strings verified): L23 httr2-required, L30 ABS_USERNAME unset, L33 ABS_PASSWORD unset, L38 cannot-connect, L66 login-page error status, L74 no Livewire snapshot, L80 no CSRF, L127 auth request failed, L181 no redirect, L377 not authenticated, L380 tests-page failed, L399 no list-tests, L405 no CSRF on tests page, L444 CSV request failed, L460 no CSV content; plus run_initial L97 enrollment-csv-not-found.
- Dual-consumer: download_abs_csv output consumed by trad_compliance + preview_abs_csv — lock column names subject_id/session/start_time.
- Live login: NOT recorded end-to-end; separate skipif'd manual-only test.

## Dependencies
- depends_on: AC-3.1 (test harness infra). Blocks: wave-3b refactors of these modules.
- conflict set: tests/testthat/fixtures/, test-run-initial-function.R (EXPANDED), data/enrollment_targets.csv (read-only assert only).
- risk: medium (date-pinning of base primitives + Livewire fixture realism).

### Progress
- [ ] abs + small locked — pending
### Decision Log
- spec-resolved — corrected L461→L460 typo; both inst/extdata/ and data/ enrollment_targets.csv exist (system.file fallback chain).
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: re-run builder steps; fixtures idempotent.
- Rollback: git revert.
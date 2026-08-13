---
ac: 3.3
depends_on: AC-3.1
risk: medium
status: spec
---

# AC-3.3: Behavior-lock gsheet_api.R + gcal_api.R (7 exports)

## Executable Spec
- **predicate:**
  (a) ALL 7 exports exercised offline vs recorded fixtures, VALUE-level asserts (exact cells/event ids/list structure, not nrow/%in% alone): read_google_sheet, print_sheet_head, check_recent_responses, check_participant_issues, get_calendar_events, list_calendars, get_combined_calendar_events. (get_upcoming_followups is in week12_tracking.R:14 — OUT of scope, AC-3.4.)
  (b) CORRECT token-fn mocked per module: gsheet tests mock get_google_sheets_access_token; gcal tests mock get_google_access_token (mocking the other = real fn runs = fail).
  (c) get_combined_calendar_events returns EXACT ORDERED sequence c("A1","A2","B1") (calendar_ids iteration order then fixture order — no sort, no dedup — current behavior) AND dedup-collision: same event id in two calendars → length(items)==2.
  (d) ERROR-SWALLOW locked: one failing calendar → expect_message + surviving calendars' events still returned (gcal_api.R:256-258 message+continue).
  (e) FIXTURE-SCAN test over fixtures/gsheet/, fixtures/gcal/, _snaps/: zero matches for private_key|BEGIN PRIVATE KEY|BEGIN RSA PRIVATE|GOOGLE_SERVICE_ACCOUNT_JSON, PLUS POSITIVE COMPANION: scan pattern demonstrably matches a "private_key" string (not vacuously green).
  (f) Whole suite green with GOOGLE_SERVICE_ACCOUNT_JSON unset/throwaway + httptest2 without_internet (or equivalent).
  (g) Deterministic clock frozen for check_recent_responses (mocked Sys.time), asserting exact recent_count/recent_data/has_recent.
  (h) read_google_sheet ragged-row NA-padding (L97-103), no-data warning (L115), print_sheet_head invisible-return (L250) all locked. JWT signing exercised with throwaway openssl::rsa_keygen() (never committed; httptest2 matches URL not body so key non-determinism harmless) proving jose path end-to-end.
- **probe:** GOOGLE_SERVICE_ACCOUNT_JSON='' Rscript -e 'devtools::test(filter="gsheet|gcal|combined-calendar|fixture-scan-google|google-token-signing")' + without_internet wrapper in test files.
- **negative:** (i) real private-key/service-account material in any fixture (scan fails); (ii) export unexercised; (iii) gcal/gsheet tests pass only with live network; (iv) removing one of 7 exports → suite red; (v) added sort()/unique()/dedup in get_combined_calendar_events → order/dedup asserts fail; (vi) error-swallow removed → message assert fails; (vii) wrong token-fn mocked → real fn invoked → fail; (viii) clock-freeze removed + system-recent fixture timestamps → flaky; (ix) shape-only asserts replacing value asserts.
- **verification:** code. ui: block NOT applicable.
- **fixture status:** test-combined-calendar.R:30 EXISTING — %in%-only, MUST EXTEND (replace with exact-order/value asserts + expect_message). fixtures/gsheet/, fixtures/gcal/ NEW. test-gsheet-api.R, test-gcal-api.R, test-fixture-scan-google.R, test-google-token-signing.R NEW.
- **rubric anchor:** §1.5.1 (fixture secret-scan + positive companion).

## Design Intent
§1 fixture shapes ARE the locked interface. §2 network+env at module seam; token fns are the mock seam. §3 mocks at token fn + HTTP layer, not deeper; error-swallow message is observable behavior — lock it. §4 gsheet=Sheets reads; gcal=Calendar reads+merge. §5 one behavior per test section; throwaway RSA key in setup.

## Technical Context
- Files: test-gsheet-api.R (NEW), test-gcal-api.R (NEW), test-combined-calendar.R (EXTEND — %in% → exact-order/value + expect_message for error-swallow), test-fixture-scan-google.R (NEW + positive companion), test-google-token-signing.R (NEW), fixtures/gsheet/* (NEW), fixtures/gcal/* (NEW).
- Architecture: gcal parses GOOGLE_SERVICE_ACCOUNT_JSON env BEFORE token call (L40-56) → dummy JSON suffices when token fn mocked. check_participant_issues has hardcoded public sheet URL (L397) — mock check_recent_responses; sheet ID is a public URL not a secret. Calendar merge = calendar_ids order, no sort/dedup (L243-261) — lock as-is, do NOT improve. JWT: private key used LOCALLY only (openssl::read_key → jose::jwt_encode_sig); signed JWT sent to token endpoint; httptest2 records RESPONSES not requests → key mechanically absent from fixtures; scan is defense-in-depth.
- Harness: devtools::test(), NOT bare test_dir (pkgload requirement).

## Dependencies
- depends_on: AC-3.1 (httptest2/harness). Blocks: AC-3.4 (adjacent week12), refactor waves.
- conflict set: test-combined-calendar.R, fixtures/{gsheet,gcal}/, R/gsheet_api.R + R/gcal_api.R (read-only).
- risk: medium (hardcoded-URL mock seam + order lock could surface latent nondeterminism).

### Progress
- [ ] google locked — pending
### Decision Log
- spec-resolved — EXTEND test-combined-calendar.R (B wins over A's as-is; %in%-only leaves merge un-locked).
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: re-record fixtures; tests idempotent offline.
- Rollback: git revert.
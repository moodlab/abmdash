---
ac: 3.3
depends_on: AC-3.1
risk: medium
status: complete
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
- [x] google locked — 2026-08-13 (commits bd0e74d test(red), f80b4d5 test(fixtures); 187 pass offline, env unset; determinism 2/2 runs)
### Decision Log
- spec-resolved — EXTEND test-combined-calendar.R (B wins over A's as-is; %in%-only leaves merge un-locked).
- impl — HTTP layer mocked via httr2::with_mocked_responses() + URL-dispatch helper mock_google_fixture() serving synthetic fixtures from fixtures/{gsheet,gcal}/. Chosen over httptest2's with_mock_api file convention because build_mock_url hashes POST bodies (token POST body = signed JWT, non-deterministic per throwaway key) — URL-only matching makes key non-determinism harmless, matching the spec's stated intent.
### Surprises & Discoveries
- httptest2 1.2.2 does NOT export with_mocked_responses (httr2 does) — tests must call httr2::with_mocked_responses(); httptest2's with_mock_api relies on file naming that hashes the POST body, so the spec's "httptest2 matches URL not body" only holds for URL-dispatch mocks. Resolved: URL-dispatch mock helper (helper-harness.R mock_google_fixture()).
- check_recent_responses timestamp-format fallback is DEAD CODE: as.POSIXct() with a mismatched format returns all-NA with a WARNING, not an error, so the tryCatch never falls through to the ISO/%Y-%m-%d formats — the first format (%m/%d/%Y) always wins and ISO timestamps yield recent_count 0. Locked current behavior via MDY-format fixture + frozen clock; flag for a real refactor AC (not a behavior-lock fix).
- utils::URLencode(..., reserved = TRUE) encodes "!" as %21 and ":" as %3A, so the sheets range URL is /values/Sheet1%21A1%3AC3 — the mock pattern must match the encoded form.
- openssl 2.4.2 has no write_key export — use openssl::write_pem(key) to get the PEM string for the throwaway service-account JSON.
- CI flake: workflow's devtools install step (install.packages from cloud.r-project.org, source-only) flaked — ragg/systemfonts/textshaping fail to build without font system libs, cascading to pkgdown/devtools → "no package called devtools" at make test. PR #52 had the same flake (one of its runs failed on the same step). Branch attempt (install from RSPM binaries, run time 9m→4m37s) was DROPPED on rebase — main already merged the same fix via PR #54 using `extra-packages: devtools` on setup-r-dependencies@v2; rebased branch's test.yml matches main exactly.
- Negative control: temporarily patched get_combined_calendar_events with sort()+unique() and removed the error message → combined-calendar tests failed 3/12, proving order/dedup/message locks bind. Patch reverted; zero R/ edits.
### Idempotence & Recovery
- Safe retry: re-record fixtures; tests idempotent offline.
- Rollback: git revert.

### Carryover Log
- PR #53 (AC-3.3): merged after rebase + strengtheners. Cycle-1: fix-now process (CI-fix duplication with #54 → rebased, kept main's extra-packages, 651d793 dropped) + consider folds (env-stop verbatim test; cal_a fixture made UNSORTED so exact-order asserts genuinely lock no-sort). Pre-existing finding: gsheet_api.R:328-342 dead tryCatch timestamp fallback — QUEUED for wave-3b AC-3.6. Status: resolved.
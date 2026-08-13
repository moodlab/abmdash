# RECORDING.md — Fixture Recording & Redaction Workflow

This is the operational guide for recording, redacting, and committing test
fixtures in the abmdash behavior-lock harness. It exists so that credentials
never leak into the repo and so that every fixture is deterministic,
offline-replayable, and namespaced.

## 1. Fixture namespacing

Fixtures live under `tests/testthat/fixtures/`, one subdirectory per module:

| Directory                | Module / purpose                                |
|--------------------------|-------------------------------------------------|
| `fixtures/trad/`         | Traditional ABM (ABS CSV download) fixtures      |
| `fixtures/redcap/`       | REDCap API fixtures                             |
| `fixtures/gsheet/`       | Google Sheets API fixtures                      |
| `fixtures/gcal/`         | Google Calendar API fixtures                    |
| `fixtures/abs/`          | ABS portal HTML fixtures                        |
| `fixtures/example.test/` | httptest2 mock files (host-named, see §4)       |

Load fixtures only through `load_fixture(module, file)` from
`tests/testthat/helper-harness.R` — never by hand-writing `test_path()` paths
in tests. Namespacing prevents parallel ACs from colliding on fixture paths.

httptest2 mock files use the httptest2 convention `fixtures/<host>/<path>.<ext>`
(e.g. `fixtures/redcap.prc.utexas.edu/redcap/api-POST...json`) because httptest2
derives the path from the request URL. Module dirs (`trad/`, `redcap/`, ...) hold
hand-crafted inputs; host dirs hold httptest2-recorded request/response pairs.

## 2. Local record workflow (creds-holders only)

Recording real API traffic requires valid credentials. Only run recording on a
machine that legitimately holds the creds; never record from CI.

1. Write the test as a replay test first: `with_mock_api({ ... })` around the
   httr2 request chain.
2. Run with the creds env vars set and wrap the call in
   `capture_requests({ ... })` (or `start_capturing()` / `stop_capturing()`)
   to write the mock file under `fixtures/`.
3. Inspect the recorded file: the redactor MUST have stripped all credential
   material (see §3). If not, fix the redactor and re-record.
4. Re-run the replay test with creds UNSET (`local_isolated_env()` in the
   test) — it must pass offline.
5. Assert a no-token guard in the test (pattern match over the committed mock
   file) so a re-record that leaks a secret fails CI instead of merging.

REDCap specifics: `redcap_api.R` POSTs one URL with `content` as a form param.
AC-3.2 must register a redactor via `set_redactor()` that scrubs the request
body (`within_body_text()`, pattern `token=[A-Za-z0-9]{16,}`) and add custom
request naming so POST bodies do not collide on hash-only names.

## 3. Token redaction

`tests/testthat/helper-harness.R` ships a `token_scrub_redactor` pattern
(used by the httptest2 dry-run) that:

- redacts request/response headers via `redact_headers()` (`Authorization`,
  `X-REDCap-Token`);
- scrubs `token=<16+ alnum>` from response bodies via `within_body_text()`.

The redactor is applied at RECORD time by httptest2: `start_capturing()` runs
`save_response(redactor(resp), ...)`. Always register the redactor before
recording, and keep the test-side no-token guard as a second layer so a missed
redaction fails CI rather than shipping a secret.

## 4. Commit-no-creds rule

- NEVER commit a fixture containing real tokens, passwords, or keys.
- Every committed httptest2 mock file must pass the no-token pattern guard.
- The `example.test` dry-run fixture is the minimum bar: a hand-authored
  `{"ok": true, "value": 42}` JSON with a test asserting no token-shaped
  strings.
- `.Renviron` and any local `.env`-style files are gitignored and are never
  a source of fixtures.

## 5. "" vs NA convention

REDCap returns the literal `""` for unset fields. The fixture loader must NOT
coerce `""` to NA:

- `load_fixture()` defaults to `na.strings = "NA"` — only the literal token
  `NA` becomes NA, `""` stays `""`.
- NEVER pass `na.strings = c("NA", "")` in a fixture loader.
- Reason: `as.numeric("")` yields NA in code under test, exactly as it does
  against the live API. Coercing `""` to NA in the fixture silently hides the
  real field shape (see issue #38 regression tests in
  `test-eligible-participants.R`).

## 6. Row-order policy

- Behavior-lock snapshots pin row order exactly (`expect_snapshot_locked()`
  compares the full object; no normalization).
- If a module's output order is legitimately unstable, the TEST must sort
  explicitly before locking (`sort_by` argument is the test's choice) — the
  snapshot itself NEVER normalizes, so a silent reorder of output fails the
  lock.
- Recording HTTP fixtures: httptest2 derives mock file names from the request
  URL/method/body hash — no manual ordering concerns.

## 7. Snapshots

Behavior-lock snapshots are committed under `tests/testthat/_snaps/snapshot-lock/`
as `<module>.rds` (e.g. `trad.rds`). `expect_snapshot_locked("trad", value)`
creates the snapshot on first local run (skip + rerun), fails if it is missing
in CI, and compares with waldo on every subsequent run. Commit generated
snapshots with the tests that produce them.

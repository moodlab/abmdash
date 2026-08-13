---
ac: 3.1
depends_on: wave-2 complete
risk: medium
status: spec
---

# AC-3.1: Deterministic test harness + trad_compliance pilot

## Executable Spec
- **predicate (9 conjuncts, ALL must hold):**
  1. DETERMINISM: two consecutive `make test` runs → `diff -r` of `_snaps/` + fixture outputs byte-identical; harness pins Sys.time() AND Sys.Date() (local_mocked_bindings(.package="base") both + TZ via withr::local_envvar) AND unsets API creds (local_isolated_env); pilot asserts EXACT value (e.g. weeks_from_start == 4.43) that fails if clock mock did not propagate.
  2. NON-TAUTOLOGICAL LOCK: snapshot covers full 8-column output of process_trad_compliance_data; mutating sessions_per_week*4→*3 or round(...,2)→round(...,1) FAILS the lock.
  3. PILOT SPECIFICS: frozen clock 2026-03-20 → nrow==2 (P001 ~2.7wk, P002 ~0.7wk active), P003 excluded (old). Existing test-trad-compliance.R passes VACUOUSLY (stale 2026-03-01 fixture dates vs real clock → all filtered by active_window_weeks=5 → empty result) — pilot must REPLACE vacuity.
  4. OFFLINE-GREEN: full suite green with all API creds unset (REDCAP_API_TOKEN, GOOGLE_SERVICE_ACCOUNT_JSON, ABS_USERNAME, ABS_PASSWORD, STATICRYPT_PASSWORD).
  5. CI EXERCISES LOCK: NEW .github/workflows/test.yml runs `make test` with NO API-cred secrets. (NO test job exists today — verified; build-dashboard.yml UNTOUCHED.)
  6. HTTPTEST2 STACK PROVEN: test-harness-httptest2-dryrun.R record→replay fake httr2::request |> req_perform via with_mock_api + redactor proof passes (proves stack before 3.2/3.3 depend on it).
  7. RECORDING.md documents: local record workflow for creds-holders, token redaction, commit-no-creds rule, "" vs NA convention (REDCap returns literal "" — fixture loader must NOT na.strings=""), row-order policy (snapshot pins order; optional sort_by arg; default no normalization).
  8. NAMESPACING: fixtures/{trad,redcap,gsheet,gcal,abs}/ + _snaps/snapshot-lock/<module>.rds. Parallel AC-3.2/3.3/3.4 never collide on fixture paths.
  9. SINGLE-OWNER DEPS: 3.1 owns DESCRIPTION Suggests (httptest2, withr, waldo) + renv.lock additions, INCLUDING closing latent gap: testthat in Suggests but ABSENT from renv.lock Packages (verified — CI test job would fail on renv::restore without it). AC-3.2/3.3/3.4 add ONLY fixture files + test files, never DESCRIPTION/renv.lock.
- **probe:**
  make test && cp -r tests/testthat/_snaps /tmp/snaps-run1 && make test && diff -r tests/testthat/_snaps /tmp/snaps-run1 && env -u REDCAP_API_TOKEN -u REDCAP_API_URL make test && rg -q 'Rscript.*devtools::test|make test' .github/workflows/test.yml && Rscript -e 'testthat::test_dir("tests/testthat", filter="harness-httptest2-dryrun")' && test -f RECORDING.md
- **negative:** N1 snapshot non-deterministic across runs (clock/TZ leak); N2 tautological lock (inequality assert or mock-propagation failure passes silently); N3 suite requires network/creds; N4 lock never exercised in CI (no test job); N5 httptest2 added but no dry-run proves record/replay/redaction; N6 recording undocumented; N7 flat fixture dir (collision across modules); N8 3.2/3.3/3.4 touching DESCRIPTION/renv.lock; N9 "" vs NA fixture drift; N10 row-order normalization hiding reorders; N11 TZ-only mock (withr::local_time sets tz NOT the clock — insufficient).
- **verification:** code · devtools::test (make test) + diff -r + Rscript dry-run. ui: block NOT applicable.
- **fixture status:** NEW — tests/testthat/_snaps/ exists but EMPTY; fixtures/ has only trad_csv_sample.csv; pilot target process_trad_compliance_data at R/trad_compliance.R:83, Sys.time() impurity at :132.
- **rubric anchor:** §2, §5.

## Design Intent
§1 load_fixture(module, name) + with_fixed_clock(fn, time) helpers in helper-harness.R — namespaced interface prevents cross-AC collision by construction. §2 clock/env boundary mocked AT function boundary; core computation snapshot-locked unmodified. §3 per-module mechanism matrix: trad=manual CSV+snapshot; redcap=httptest2 record (httr2 stack verified redcap_api.R:58-61; REDCap POSTs one URL with content param → 3.2 must use set_redactor + custom request naming); gsheet/gcal=httptest2; abs=manual crafted HTML (httptest2 can't freeze dynamic server tokens); small pure=none. §4 RECORDING.md owns record/redact/commit workflow. §5 new layers additive; existing local_mocked_bindings style preserved.

## Technical Context
- Files: tests/testthat/helper-harness.R (NEW: load_fixture + with_fixed_clock + local_isolated_env), test-harness-snapshot-lock-trad.R (NEW pilot), test-harness-httptest2-dryrun.R (NEW), fixtures/{trad,redcap,gsheet,gcal,abs}/ (NEW subdirs; migrate trad_csv_sample.csv → fixtures/trad/), _snaps/snapshot-lock/ (NEW), DESCRIPTION (Suggests += httptest2, withr, waldo), renv.lock (httptest2/waldo + latent testthat entry — close it), RECORDING.md (NEW), .github/workflows/test.yml (NEW), existing test-trad-compliance.R (vacuity noted; pilot may supersede).
- Architecture: withr::local_time sets TZ only — mock BOTH clock fns AND pin TZ. local_mocked_bindings(.package="base") is testthat-version-sensitive → exact-value assert doubles as mock-propagation tripwire. expect_snapshot_value is testthat-native (no extra dep). Snapshot pins row order — no silent normalization.
- Test command: make test → devtools::test() (Makefile:7); make test-trad (Makefile:11). NOT bare test_dir (needs pkgload).

## Dependencies
- depends_on: wave 2 complete. Blocks: AC-3.2/3.3/3.4 + all wave-3b refactors.
- conflict set: DESCRIPTION, renv.lock, _snaps/**, fixtures/**, .github/workflows/test.yml — SOLE OWNER until 3.1 merges.
- risk: medium (new CI workflow; renv.lock edits; base-package mocking version sensitivity — tripwire assert mitigates).

### Progress
- [ ] harness built — pending
### Decision Log
- spec-resolved — CI test job in-scope (lock worthless if CI never runs it); httptest2 dry-run in pilot (prove stack before 3.2).
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: re-run builder steps; harness additive.
- Rollback: git revert.
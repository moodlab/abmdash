---
feature: repo-usability
ac: 4.2
status: complete
title: "[chore] Add debug workflows to abmdash-guide skill"
---

# AC-4.2 — Debug workflows in abmdash-guide skill

Slice from `repo-usability`. Issue #67. Spec block in issue body (13 conjuncts).

## Progress

- [x] 2026-08-13 — Read issue #67 + spec (13 conjuncts); plan file absent → spec carried in issue body (same as AC-4.1 precedent).
- [x] 2026-08-13 — Ground-truth verification: Makefile `test` target = bare `devtools::test()` (no filter support); `test-trad` = `devtools::test(filter = "trad-compliance")` only filtered target. 18 test files under tests/testthat/. redcap_api.R lines 471/615/657 exact. gsheet env stop at :208 (not :163 — post-refactor drift), openssl::read_key at gsheet:267 / gcal:160 (not 188-189/118-119). test-abs-login.R:519 skip_if_not. FAQ 9 sections confirmed. CI jobs build-image → build-dashboard, secrets list, git pull --rebase.
- [x] 2026-08-13 — Fixed two #68-review nits in SKILL.md: bare-directory link `(vignettes/)` → `(vignettes/faq.Rmd)`; `_snaps/snapshot-lock/` dir link → plain-code text; REDCap expanded at first use in non-engineer section.
- [x] 2026-08-13 — Extended frontmatter description with debug triggers (debug/diagnose/probe/broken/error/symptom) WITHOUT removing learn-repo triggers.
- [x] 2026-08-13 — Appended `## 9. Debug Workflows`: probing rules (make test filter= BAN, Rscript form, full filter strings, filter="compliance" = 3 files), canonical lock-suite-as-diagnostic text, creds+network WARNING block (docker-test-auth/docker-render/docker-build fenced), 18-row symptom→probe→likely-file table (16 spec entries + 2 closing FAQ §5/§6 gaps), CI job distinction, abs-login 1-skip disclosure.
- [x] 2026-08-13 — Verification: filter-validity loop (every devtools::test filter matches a tests/testthat/ file), no make-test-filter as probe, no live R call probes, link-existence loop (30 links, 0 bad), path:line existence checks, PEM rg-false-zero confirmed, ran abs-login suite offline → 38 test_that / 1 skipped / 37 pass / 0 failures.
- [x] 2026-08-13 — Committed, pushed, PR created with probe results in body.

## Decision Log

- **Post-refactor path:line over issue's stale lines**: issue Technical Context said gsheet:163 (env stop) and gsheet:188-189/gcal:118-119 (PEM) — those are PRE-refactor. Ground truth: gsheet:208, gsheet:267, gcal:160. Used actual lines (AC-7 "No stale names/lines" + design intent §4).
- **18 rows not 16**: 16 spec entries verbatim + 2 supplementary rows (missing R package; enrollment_targets.csv) to close the FAQ §5/§6 gap in 9-cluster coverage (AC-11). Both probes verified real (`rg "package is required" R/`, `rg "enrollment_targets" R/run_initial_function.R`).
- **PEM probe**: "Failed to parse private key PEM" is NOT a stop() string (openssl runtime) — probe = test(filter="google-token-signing"), explicitly noting rg returns zero. Confirmed zero matches in source.
- **abs-login disclosure**: "green with 1 skip" — the single skip is the live-login test at :519; other 37 tests run offline (verified by executing the suite: 38 test_that, 1 skip, 37 pass).
- **Snapshot-lock link**: made plain-code text rather than a file link (AC-4.1 already links the test file; avoids bare-directory link class).

## Surprises & Discoveries

- Plan file `.opencode/plans/repo-usability/AC-4.2.md` did not exist at branch creation — spec lives entirely in the issue body (same as AC-4.1). Not a blocker.
- Issue's gsheet/gcal path:line claims (163/188-189/118-119) were stale post-refactor — actual env stop at gsheet:208, read_key at gsheet:267 / gcal:160. Grounded against source rather than trusting spec numbers.
- abs-login suite has 38 test_that blocks (not "16 behavior-lock tests"); only 1 skips offline (the live-login manual test). Ran the suite to verify the claim before writing it.
- `make test filter=X` doesn't "report filter unused" — make never passes it to devtools::test(); it silently runs the full suite. Wording corrected to match mechanics.

## Verification Notes

- Filter-validity loop: all 16 table filters match a tests/testthat/ file (redcap-behavior-lock, google-token-signing, gsheet-api, gcal-api, eligible-participants, snapshot-lock, faq-verbatim, trad-compliance, abs-login, fixture-scan-google, combined-calendar + 2 supplementary).
- `make test filter=` appears only in ban text, never as a probe.
- No live R calls as probes (abs_login(/download_abs_csv(/read_google_sheet( absent from probe column).
- docker-test-auth/docker-render/docker-build only in WARNING block + pre-existing §5 table (AC-4.1 content).
- Link-existence loop: 30 links, 0 broken.
- Path:line existence: 471/615/657/267/160/208/90/104/519 all resolve.
- abs-login offline run: 37 pass, 1 skip, 0 fail.
- Probe outputs pasted into PR body.

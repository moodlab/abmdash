---
ac: 2.2
depends_on: AC-2.1 (vignettes/ + DESCRIPTION mechanism), AC-1.1 (OKF links)
risk: low
status: complete
---

# AC-2.2: FAQ vignette — verbatim errors → cause → repo-specific fix

## Executable Spec
- **predicate:** (1) vignettes/faq.Rmd exists, non-stub (>20 lines), SAME VignetteBuilder mechanism + eval=FALSE chunk policy as AC-2.1. (2) Every error string quoted in FAQ is rg -F-findable verbatim in repo (R/, Makefile, Dockerfile, build-dashboard.sh, .github/workflows/) — paraphrased/invented fails. (3) Every fix step names a command that exists in THIS repo (renv::restore() per Dockerfile L38, make docker-build|docker-render|docker-test-auth|test|test-trad|lint per Makefile, staticrypt per workflow) — generic internet advice fails. (4) REDCap "" vs NA entry complete: quotes "row names contain missing values" verbatim, cause chain (as.numeric("")→NA→logical row index→data.frame() raises), fix pattern (parse once suppressWarnings(as.numeric()), guard PARSED value, USE.NAMES = FALSE), names BOTH manifestation sites (get_eligible_participants, get_weekly_screening_stats), references PR #39, points to docs/okf/modules/redcap_api.md. (5) No fix recommends destructive command; rm -rf/docker prune only in WARNING context (build-dashboard.sh L53 rm -rf docs clobber). (6) Every internal link/path target resolves; README.md contains FAQ pointer to vignettes/faq.Rmd. (7) No `scripts/` prefix anywhere (scripts/ does not exist; build-dashboard.sh, debug-docker.sh, load-env.sh at ROOT). (8) FAQ troubleshooting-only: ≥1 cross-link to docs/okf/ per module-level error entry. (9) renv guidance correct-direction: renv::restore() to recover; renv::snapshot() only to UPDATE lockfile, never as recovery.

- **probe:**
  ```
  test -s vignettes/faq.Rmd && wc -l vignettes/faq.Rmd   # >20
  rg -F 'VignetteBuilder:' DESCRIPTION && rg -n 'eval=FALSE' vignettes/faq.Rmd
  rg -n 'scripts/' vignettes/faq.Rmd                       # must be EMPTY (P7)
  rg -F 'row names contain missing values' vignettes/faq.Rmd
  rg -F 'REDCAP_API_TOKEN environment variable is not set or is empty' R/redcap_api.R
  rg -F 'GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty' R/
  rg -F 'Login failed: no redirect in response.' R/abs_login.R
  rg -F 'Not authenticated. Session may have expired. Please login again.' R/abs_login.R
  rg -F 'Could not find enrollment_targets.csv file' R/run_initial_function.R
  rg -F "httr2 package is required. Please install it with: install.packages" R/
  rg -F 'renv::restore()' Dockerfile
  rg -n 'docker-build|docker-render|docker-test-auth|^test:|^lint:' Makefile
  rg -F 'suppressWarnings(as.numeric(' vignettes/faq.Rmd && rg -F 'USE.NAMES' vignettes/faq.Rmd \
    && rg -F 'get_eligible_participants' vignettes/faq.Rmd \
    && rg -F 'get_weekly_screening_stats' vignettes/faq.Rmd && rg -F '#39' vignettes/faq.Rmd
  rg -i -n 'rm -rf|docker (system )?prune|docker rm' vignettes/faq.Rmd   # each hit = WARNING context
  rg -i -n 'faq' README.md && test -f docs/okf/modules/redcap_api.md
  uv run Rscript -e 'testthat::test_dir("tests/testthat", filter = "faq-verbatim")'
  ```
- **negative:** (a) fabricated/paraphrased error text; (b) wrong-direction fix (renv::snapshot() to recover; docker build . instead of make docker-build); (c) destructive command as FIX not WARNING; (d) REDCap entry missing #39, a manifestation site, fix pattern, or exact error string; (e) generic internet advice with no repo-specific step; (f) broken link or README pointer; (g) scripts/ path prefix anywhere; (h) mechanism mismatch vs AC-2.1 (no VignetteBuilder, executable chunks); (i) scope drift — re-documenting module interfaces instead of linking docs/okf/; (j) eval=FALSE sneaky-pass — vignette builds green despite fabricated errors (R CMD check never executes prose); test-faq-verbatim.R content sweep is the real gate.
- **verification:** code (P1–P9 greppable + test-faq-verbatim.R under make test) + manual (plain-language, numbered steps, if-you-see-X-do-Y framing).
- **fixture status:** NEW — vignettes/faq.Rmd; NEW — tests/testthat/test-faq-verbatim.R (durable-izes verbatim + fix-command + no-scripts-prefix invariants).
- **rubric anchor:** §4 (FAQ header names scope + what NOT), §3 (FAQ↔vignettes↔OKF joint).
- **ui: block:** NOT applicable.

## Design Intent
§1 verbatim-error invariant = doc analog of encode-invariant-precisely; durable-ized in testthat. §2 pure documentation; correctness verified by grep probes, NOT execution. §3 FAQ at troubleshooting joint; cross-links OKF, never duplicates. §4 FAQ header names scope + negative space. §5 N/A.

## Technical Context
- Files: vignettes/faq.Rmd (NEW), tests/testthat/test-faq-verbatim.R (NEW), README.md (FAQ pointer near codegraph section L109-124), DESCRIPTION (verify-only — 2.1 adds VignetteBuilder/Suggests), .Rbuildignore (conditional append; hot-conflict).
- Entry table (9 clusters): (1) REDCap "" vs NA → "row names contain missing values" (redcap_api.R parse-once L438/L470-477/L567; OKF redcap_api.md; PR #39; FAQ explains cause + points to fix — code fix is AC-3.9's job); (2) REDCap token env (L10; fix .Renviron/GH secret); (3) Google svc acct (gsheet L163/gcal L42; malformed-JSON gsheet L173/gcal L52); (4) ABS login (L30/L33 env; L181 creds rejected; L377 re-run abs_login()); (5) httr2 missing (4 files; fix renv::restore()); (6) enrollment csv (run_initial_function L97); (7) Docker/CI deploy (no single stop() string — point at GH Actions tab, make docker-build/docker-render/docker-test-auth, ghcr.io auth workflow L26-31); (8) staticrypt (STATICRYPT_PASSWORD empty = no encryption, NOT an error — Dockerfile L93); (9) renv restore failure (re-run renv::restore(); renv::snapshot() only to UPDATE lockfile).
- Architecture: FAQ = non-engineer entry point; OKF bundle = machine/agent mirror; FAQ links, not duplicates. eval=FALSE sneaky-pass → R CMD check CANNOT validate this AC — content-grep test is load-bearing.

## Dependencies
- depends_on: AC-2.1, AC-1.1. Blocks: AC-4.1.
- conflict set: vignettes/ (2.1), DESCRIPTION (2.1/3.1 serialized), README.md (2.1 appends pointer same section), .Rbuildignore (hot-conflict).
- risk: low (docs-only). HARM surface HIGH.

### Progress
- [x] FAQ written — 2026-08-13, commit 1c6e6c9 (vignettes/faq.Rmd, 322 lines, 9 entries)
- [x] test-faq-verbatim.R — 40 assertions, all green (also under devtools::test(): 85 pass)
- [x] README FAQ pointer added (Troubleshooting section, first bullet)
- [x] All AC-2.2 probes pass (rg -F sweeps, WARNING-context check, no scripts/ prefix)
- [x] rmarkdown::render sanity check — RENDER OK (vignette mechanism matches AC-2.1)

### Decision Log
- spec-resolved — durable content-grep test (test-faq-verbatim.R) included in AC; root-relative script paths only (no scripts/ prefix).
- impl — quoted error strings limited to repo-findable substrings: the full base-R prefix `Error in data.frame(..., check.rows = FALSE):` is NOT in repo source, so the FAQ headlines only `row names contain missing values` (findable at R/redcap_api.R:470). Matches P2's verbatim rule strictly.

### Surprises & Discoveries
- The Write tool soft-wraps long single-line paragraphs in .Rmd files (~80-100 cols), silently splitting source lines. Two test failures traced to this: the `rm -rf` line lost its `WARNING` token to the previous line, and `renv::snapshot()` + `never` landed on different lines. Fix: keep sentence-level line breaks explicit (the verbatim-invariant test catches exactly this kind of drift — the content gate works as designed).
- testthat::test_dir("tests/testthat") fails on the existing suite ("No packages loaded with pkgload", could not find function) — the repo's real harness is devtools::test() (Makefile `make test`). test_dir(filter="faq-verbatim") works standalone because the content-grep test loads no package functions.
- rmarkdown::render of the vignette succeeds — mechanism consistent with AC-2.1 (VignetteEngine knitr::rmarkdown, 6 eval=FALSE chunks).

### Idempotence & Recovery
- Safe retry: re-run builder steps.
- Rollback: git revert.

### Carryover Log
- Cycle 1 (PR #47): 2 fix-now (test allowlist asymmetry — corpus-side only for 8/9 entries, paraphrase/fabrication passes CI; eval=FALSE policy not test-enforced) + 3 consider folds (full httr2 string, README pointer grepl, OKF file.exists). Resolved in 28a0618 (single-file test strengthening; suite 85→107; fail-on-break verified both directions). Status: resolved.
- Cycle 2 (PR #47): clean pass. Reviewer B noted 2 consider-level test-robustness items: eval=FALSE fixed=TRUE match brittle against legit spaced form `eval = FALSE` (all vignettes currently use no-space convention — theoretical); floor guard >=8 tight vs future FAQ dedup. Deferred — noted for future docs-maintenance. Status: deferred-with-note.
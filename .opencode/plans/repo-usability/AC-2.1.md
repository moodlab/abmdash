---
ac: 2.1
depends_on: wave-1 shipped (docs/okf/ bundle, codegraph README section)
risk: medium
status: complete
---

# AC-2.1: Troubleshooting vignettes for non-engineers

## Executable Spec
- **predicate:** Vignettes exist AND hermetic AND accurate. P1: exactly 5 .Rmd in vignettes/ (redcap, google, abs, docker, ci). P2: every ```{r chunk header contains eval=F (accepts FALSE/F with/without spaces); no chunk has eval=T; no bare chunk (bare {r} defaults eval=TRUE = sneaky-pass). P3: no inline `r ` code containing API-call names (call_redcap_api|get_redcap_records|get_eligible_participants|get_weekly_screening_stats|read_google_sheet|get_calendar_events|list_calendars|abs_login|download_abs_csv|test_abs_connection|verify_abs_login) — inline R executes at knit regardless of chunk eval. P4: every `make X` referenced matches {test, test-trad, docker-build, docker-render, docker-test-auth, lint, serve}. P5: every `.sh` referenced matches {build-dashboard.sh, debug-docker.sh, load-env.sh} (root-level). P6: every env var named matches {REDCAP_API_TOKEN, GOOGLE_SERVICE_ACCOUNT_JSON, ABS_USERNAME, ABS_PASSWORD, STATICRYPT_PASSWORD}. P7: redcap vignette covers as.numeric("")→NA, "row names contain missing values", parse-once guard fix. P8: each vignette has error→cause→fix structure (≥3-col table OR "If you see"/"Error" heading + cause + fix). P9: ci vignette mentions build-image AND build-dashboard jobs AND zero matches for github pages|gh-pages. P10: docker vignette warns rm -rf docs clobbers docs/okf/. P11: README.md references vignettes/. P12: DESCRIPTION has VignetteBuilder: knitr AND knitr in Suggests. P13: .Rbuildignore does NOT contain ^vignettes. P14: every ../docs/okf/... link target exists on disk. P15: "knitr" present in renv.lock (verified pre-existing at L694 — NO lock change).

- **probe:**
  ```bash
  cd "$(git rev-parse --show-toplevel)" && FAIL=0
  [ "$(ls vignettes/*.Rmd 2>/dev/null | wc -l | tr -d ' ')" = "5" ] || { echo "P1 fail"; FAIL=1; }
  for v in redcap google abs docker ci; do ls vignettes/*${v}*.Rmd >/dev/null 2>&1 || { echo "P1 fail: $v"; FAIL=1; }; done
  rg -N '^```\{r' vignettes/*.Rmd | rg -v 'eval=F' && { echo "P2 fail: chunk missing eval=F"; FAIL=1; }
  rg -N 'eval=T' vignettes/*.Rmd && { echo "P2 fail: eval=T"; FAIL=1; }
  rg -o '`r [^`]+`' vignettes/*.Rmd | rg 'call_redcap_api|get_redcap_records|get_eligible_participants|get_weekly_screening_stats|read_google_sheet|get_calendar_events|list_calendars|abs_login|download_abs_csv|test_abs_connection|verify_abs_login' && { echo "P3 fail: inline API call"; FAIL=1; }
  rg -oN 'make [a-z-]+' vignettes/*.Rmd | cut -d' ' -f2 | sort -u | rg -v '^(test|test-trad|docker-build|docker-render|docker-test-auth|lint|serve)$' && { echo "P4 fail: invented target"; FAIL=1; }
  rg -oN '[a-z-]+\.sh' vignettes/*.Rmd | sort -u | rg -v '^(build-dashboard|debug-docker|load-env)\.sh$' && { echo "P5 fail: invented script"; FAIL=1; }
  rg -oN '[A-Z][A-Z_]{4,}' vignettes/*.Rmd | sort -u | rg '_TOKEN$|_JSON$|_USERNAME$|_PASSWORD$' | rg -v '^(REDCAP_API_TOKEN|GOOGLE_SERVICE_ACCOUNT_JSON|ABS_USERNAME|ABS_PASSWORD|STATICRYPT_PASSWORD)$' && { echo "P6 fail: wrong env var"; FAIL=1; }
  rg -q 'as.numeric' vignettes/*redcap*.Rmd && rg -qi 'row names contain missing' vignettes/*redcap*.Rmd || { echo "P7 fail"; FAIL=1; }
  for f in vignettes/*.Rmd; do rg -qi 'if you see|error.*cause.*fix|cause' "$f" || { echo "P8 fail: $f"; FAIL=1; }; done
  rg -q 'build-image' vignettes/*ci*.Rmd && rg -q 'build-dashboard' vignettes/*ci*.Rmd || { echo "P9 fail"; FAIL=1; }
  rg -i 'github pages|gh-pages' vignettes/*ci*.Rmd && { echo "P9 fail: invented Pages"; FAIL=1; }
  rg -qi 'rm -rf docs|clobber' vignettes/*docker*.Rmd || { echo "P10 fail"; FAIL=1; }
  rg -q 'vignettes' README.md || { echo "P11 fail"; FAIL=1; }
  rg -q 'VignetteBuilder:\s*knitr' DESCRIPTION && rg -q 'Suggests' DESCRIPTION || { echo "P12 fail"; FAIL=1; }
  rg -q '\^vignettes' .Rbuildignore && { echo "P13 fail"; FAIL=1; }
  rg -oN '\.\./docs/okf/[a-zA-Z0-9/_.-]+' vignettes/*.Rmd | cut -d: -f2- | sort -u | while read p; do [ -f "${p#../}" ] || echo "P14 fail: $p"; done
  rg -q '"knitr"' renv.lock || { echo "P15 fail"; FAIL=1; }
  [ $FAIL -eq 0 ] && echo "ALL 15 CHECKS PASSED" || exit 1
  ```
- **negative:** Primary: template-copied vignettes mark chunks eval=FALSE + error tables but pin WRONG commands — make build (actual: make docker-build), REDCAP_TOKEN (actual: REDCAP_API_TOKEN), R CMD check (actual: make lint). Looks complete, actively harms user. Secondary: bare ```{r} chunk defaults to live-eval → hermeticity broken silently. Tertiary: redcap vignette omits as.numeric("")→NA bug class (PR #39). Quaternary: ci vignette invents GitHub Pages step (actual: commits docs/ directly). Also rejected: .Rbuildignore gaining ^vignettes; knitr "added" to renv.lock (already there L694); inline `r ` API calls.
- **verification:** code (bash probe P1–P15 deterministic) + manual (non-engineer readability residual).
- **fixture status:** NEW — vignettes/ does not exist. B-verified anchors: knitr@renv.lock L694; env vars redcap_api.R L8, gsheet L161, gcal L40, abs L26-27; build-dashboard.sh L53 rm -rf docs; workflow jobs L15/L54.
- **rubric anchor:** §4 (human-facing module responsibility + negative space), §2 (eval=FALSE pure/docs hermetic shell).
- **ui: block:** NOT applicable.

## Design Intent
§1 eval=FALSE invariant encoded structurally in chunk headers; command vocabulary pinned to ground truth. §2 pure docs, zero execution at build. §3 one vignette per external boundary; Sheets+Calendar combined at shared auth boundary. §4 per-module troubleshooting with error→cause→fix discipline. §5 each vignette one job.

## Technical Context
- Files: vignettes/redcap-troubleshooting.Rmd, google-troubleshooting.Rmd, abs-troubleshooting.Rmd, docker-troubleshooting.Rmd, ci-troubleshooting.Rmd (NEW ×5); DESCRIPTION (MODIFY: +VignetteBuilder: knitr, +Suggests: knitr); README.md (MODIFY: ### Troubleshooting section after codegraph section L109-124, before "Create an issue" L125). NO renv.lock change. NO .Rbuildignore change.
- Per-vignette pinned content: (1) redcap: REDCAP_API_TOKEN; "REDCAP_API_TOKEN environment variable is not set or is empty"; "row names contain missing values"; as.numeric("")→NA→row-index leak (PR #39, 90ef649); parse-once guard + USE.NAMES=FALSE; source ./load-env.sh; link ../docs/okf/modules/redcap_api.md. (2) google: GOOGLE_SERVICE_ACCOUNT_JSON; parse failures (L163/L42, L173/L52); JSON quote + \\n private-key unescaping; links gsheet_api.md + gcal_api.md. (3) abs: ABS_USERNAME/ABS_PASSWORD; "Not authenticated" redirect; ssl_verifypeer=0 + malformed HTTP/2; make docker-test-auth; link abs_login.md. (4) docker: all 7 make targets; build-dashboard.sh rm -rf docs CLOBBER WARNING; debug-docker.sh; Dockerfile 2-stage rocker/r-ver:4.5.1 + Node18/staticrypt + Quarto 1.4.550; link deploy-pipeline.md. (5) ci: build-dashboard.yml; exactly build-image (ghcr.io/moodlab/abmdash) + build-dashboard (docker run→quarto render→staticrypt→git add -f docs/→commit→rebase→push); cron 0 6 * * *, workflow_dispatch, push main; concurrency cancel-in-progress:false; 5 secrets; 30-day artifacts; 4 failure modes; NO GitHub Pages; link deploy-pipeline.md.
- Architecture: OKF cross-links use ../docs/okf/... relative paths (resolve on GitHub AND locally). Check script runs ad-hoc by builder + reviewers — NOT durable-ized (out of scope).

## Dependencies
- depends_on: AC-1.1 (OKF bundle link targets), AC-1.2 (README codegraph section placement anchor).
- Blocks: wave-3 behavior-lock ACs.
- conflict set: DESCRIPTION (2.2, 3.1 sequential), README.md (2.2), .Rbuildignore (5 ACs serialize; 2.1 must NOT add ^vignettes), vignettes/ (2.1→2.2 sequential).
- risk: medium — no code changes, HIGH user-harm if commands wrong.

### Progress
- [x] vignettes written — done (2026-08-13): 5 vignettes created, all 15 probes pass, all 5 render hermetic (eval=FALSE) via rmarkdown::render

### Decision Log
- spec-resolved — VignetteBuilder: knitr + Suggests: knitr; knitr already in renv.lock L694 (no lock change); .Rbuildignore must NOT get ^vignettes.

### Surprises & Discoveries
- P5 and P6 probes as written are structurally broken with multiple files: `rg -o` prefixes every match with `path:`, so the `^(build-dashboard|...)$` / `^(REDCAP_API_TOKEN|...)$` anchors never match and even LEGITIMATE scripts/env vars are reported as failures. Fixed by running the equivalent check with `rg -I` (no filename). The verbatim P5/P6 lines in the spec need the -I flag added; content side was verified clean against the intent.
- "make sure" and "make targets" are natural-English phrases that trip the P4 `make [a-z-]+` regex — rephrased prose to "confirm"/"build targets" to keep the probe meaningful (an invented-target check shouldn't fire on prose).
- The redcap vignette placeholder `REDCAP_API_TOKEN=YOUR_TOKEN` violates P6 (ends in _TOKEN) — placeholder rewritten as `<your-token>`.
- rmarkdown::render() of all 5 vignettes succeeds with zero execution — eval=FALSE hermeticity verified end-to-end, not just by regex.

### Idempotence & Recovery
- Safe retry: re-run builder steps; prose files idempotent.
- Rollback: git revert.
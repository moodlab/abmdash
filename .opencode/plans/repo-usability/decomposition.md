# Decomposition: repo-usability

## Feature Goal
Make abmdash approachable for non-engineers (R learners, troubleshooters without a coding background) via four waves: (1) machine/agent knowledge surfaces (OKF bundle + codegraph index), (2) human-facing docs (troubleshooting vignettes + FAQ), (3) a behavior-lock-first, zero-behavior-change readability refactor of the R code (kills nested loops, extracts helpers, renames — with snapshot tests guaranteeing the live daily-deployed dashboard is untouched functionally), and (4) a repo-level opencode skill that teaches the repo and guides dynamic debugging. The refactor wave is the most conservative: lock first, refactor second, verify unchanged.

Context snapshot (accurate as of today): pkg v0.0.11, 34 exports, 10 R/ files (redcap_api.R 832L largest, abs_login.R 15.4K, gsheet_api.R 12.9K), 14 `for`-loops across 8 files, 3 existing testthat files, dashboard at inst/dashboard/index.qmd (26.8K) rendered via docker+quarto in .github/workflows/build-dashboard.yml, docs/ gitignored but force-added by the workflow, .opencode/ and .codegraph/ NOT currently gitignored, .Rbuildignore 130B.

Memory anchor baked in: REDCap returns `""` (not NA) for unset fields; the `as.numeric("")` → NA-leak → "row names contain missing values" class of bug (fixed in PR #39) must be covered by (a) the redcap behavior-lock tests and (b) the FAQ.

## AC Table

| AC | Description | Dependencies | Conflict Set | Risk |
|----|-------------|--------------|--------------|------|
| **WAVE 1 — knowledge surfaces** | | | | |
| 1.1 | Generate OKF bundle at docs/okf/ (index + concept docs for redcap_api, gsheet_api, gcal_api, abs_login, compliance modules, dashboard render/deploy), force-add despite docs/ gitignore, verify build-dashboard.yml doc-commit step does not clobber docs/okf/, add Rbuildignore entry | none | docs/okf/*, .gitignore, .Rbuildignore, .github/workflows/build-dashboard.yml (read-only verification) | low |
| 1.2 | Initialize codegraph index at repo root (.codegraph/), document regen procedure in README, decide gitignore-vs-commit per open question | none | .codegraph/, .gitignore, .Rbuildignore, README.md | low |
| **WAVE 2 — human docs** | | | | |
| 2.1 | Add troubleshooting-workflow vignettes (vignettes/): REDCap connectivity/token, Google Sheets/Calendar service-account, ABS login/download, Docker build + local make targets, CI deploy failure triage; chunks eval=FALSE to keep R CMD build hermetic | none (wave-2 gate: wave 1 shipped) | vignettes/*, DESCRIPTION (VignetteBuilder + Suggests), .Rbuildignore | medium |
| 2.2 | Add FAQ vignette/page covering the REDCap `""` NA-leak bug class, top runtime errors with exact error text → cause → fix, plus README pointer to vignettes | AC-2.1 (shares vignettes dir + DESCRIPTION) | vignettes/faq.*, README.md, DESCRIPTION | low |
| **WAVE 3a — behavior-lock tests** | | | | |
| 3.1 | Build snapshot/fixture harness (recorded API response fixtures, snapshot helper, deterministic clocks/IDs) + pilot lock on trad_compliance (already has tests + stable CSV contract — proves harness on safest module) | AC-2.2 (docs exist first, per approved wave order) | tests/testthat/*, tests/testthat/fixtures/, DESCRIPTION (httptest2/withr Suggests) | medium |
| 3.2 | Behavior-lock redcap_api.R: snapshots per exported fn against recorded fixtures, INCLUDING explicit cases for `""` empty-string fields proving current parse/guard behavior (REDcap NA-leak class) | AC-3.1 | tests/testthat/test-redcap_*, fixtures/, R/redcap_api.R (NO edits — lock only) | medium |
| 3.3 | Behavior-lock gsheet_api.R + gcal_api.R against recorded Google responses | AC-3.1 | tests/testthat/test-gsheet_*, test-gcal_*, fixtures/ | medium |
| 3.4 | Behavior-lock abs_login.R, compliance_summary, compliance_tracking, demographics, week12_tracking, run_initial_function | AC-3.1 | tests/testthat/test-{abs,compliance,demographics,week12,run-initial}*, fixtures/ | medium |
| **WAVE 3b — readability refactor (ZERO behavior change)** | | | | |
| 3.5 | Refactor redcap_api.R for readability: extract helpers, eliminate nested loops (LL 243, 257, 655), rename for intent; NAMESPACE/man exports byte-identical; all AC-3.2 snapshots green | AC-3.2 | R/redcap_api.R, man/ (roxygen regen) | high |
| 3.6 | Refactor gsheet_api.R + gcal_api.R (loops at gsheet L359, gcal L245); AC-3.3 snapshots green | AC-3.3 | R/gsheet_api.R, R/gcal_api.R, man/ | medium |
| 3.7 | Refactor abs_login.R (deepest nesting: LL 136/153/231/389/452); AC-3.4 lock green | AC-3.4 | R/abs_login.R, man/ | high |
| 3.8 | Refactor compliance_summary, compliance_tracking, demographics, week12_tracking, run_initial_function (remaining loops: compliance_summary L43, compliance_tracking L96, week12 L164, run_initial L89); AC-3.4 lock green | AC-3.4 | R/compliance_*.R, R/demographics.R, R/week12_tracking.R, R/run_initial_function.R, man/ | medium |
| **WAVE 3f — conditional bug-fix (PENDING clarification Q3)** | | | | |
| 3.9 | [CONDITIONAL] Sweep redcap_api.R for the `as.numeric("")` NA-leak pattern and apply parse-once/guard-on-parsed/USE.NAMES=FALSE fix, with failing-first regression test | AC-3.2, AC-3.5, user approval (Q3) | R/redcap_api.R, tests/testthat/test-redcap_* | high |
| **WAVE 4 — repo skill** | | | | |
| 4.1 | Create repo-level skill .opencode/skills/<name>/SKILL.md with learn-the-repo walkthrough grounded in docs/okf/ + vignettes; committed, travels with repo; .Rbuildignore entry for .opencode/ | AC-1.1, AC-2.1, AC-2.2 | .opencode/skills/*, .Rbuildignore | low |
| 4.2 | Add dynamic-debug workflows to the skill (symptom → probe → likely file, referencing the behavior-lock test suite as executable docs) + validate skill discovery | AC-4.1, wave 3 complete (references final module shapes) | .opencode/skills/* | low |

## Dependency DAG
```
1.1 ─┐
     ├─→ 2.1 → 2.2 ─→ 3.1 ─→ 3.2 ─→ 3.5 ─→ (3.9 conditional)
     │             ╰→ 3.1 ─→ 3.3 ─→ 3.6
1.2 ─┘             ╰→ 3.1 ─→ 3.4 ─→ 3.7
                                  ╰→ 3.8
1.1 + 2.1 + 2.2 ─→ 4.1 ─→ 4.2 (4.2 also waits on wave 3 complete)
```
Per-wave (wave gates, not per-AC): W1∥ → W2: 2.1→2.2 → W3a: 3.1→{3.2,3.3,3.4} → W3b: 3.2→3.5, 3.3→3.6, 3.4→{3.7,3.8} → [3.9?] → W4: 4.1→4.2.

Recommended lock order inside W3a (safest-first): trad_compliance pilot (AC-3.1) → redcap (AC-3.2, highest blast radius + NA-bug anchor) → google APIs (AC-3.3) → abs_login + small modules (AC-3.4). Rationale: pilot starts with existing coverage and a stable CSV contract (no API mocking risk); redcap locked before any fix discussion so current behavior is on record.

## Hot Conflict Files
- `.gitignore`: AC-1.1 (docs/okf exception), AC-1.2 (.codegraph decision) — serialize within W1.
- `.Rbuildignore`: AC-1.1, AC-1.2, AC-2.1, AC-2.2, AC-4.1 — each wave appends; serialize within a wave, no cross-wave parallelism on this file.
- `DESCRIPTION`: AC-2.1 (VignetteBuilder+Suggests), AC-2.2, AC-3.1 (test Suggests: httptest2/withr) — 2.1→2.2 already sequential; 3.1 is a later wave, fine.
- `man/`: regenerated by roxygen on every refactor AC (3.5–3.8) — run W3b refactors on separate branches per AC; rebase + `devtools::document()` re-run serializes the man/ conflict. NAMESPACE must remain byte-identical (guard in every refactor AC's acceptance criteria).
- `tests/testthat/fixtures/`: AC-3.2/3.3/3.4 all write fixtures — run in parallel OK only if fixture filenames namespaced per module (acceptance criterion on AC-3.1 harness).
- `README.md`: AC-1.2, AC-2.2 — different waves, low risk.
- `R/redcap_api.R`: AC-3.5 (refactor) then AC-3.9 (conditional fix) — strictly sequential.
- `.github/workflows/build-dashboard.yml`: NOT modified by any AC; AC-1.1 only verifies docs/-commit behavior. Do not let any AC edit this file.

## Suggested Batch Schedule
- W1-Batch 1 (parallel, serialize .gitignore/.Rbuildignore edits): AC-1.1, AC-1.2
- W2-Batch 1: AC-2.1 → W2-Batch 2: AC-2.2
- W3a-Batch 1: AC-3.1 → W3a-Batch 2 (parallel): AC-3.2, AC-3.3, AC-3.4
- W3b-Batch 1 (parallel branches, serialized man/ regen on merge): AC-3.5, AC-3.6, AC-3.7, AC-3.8
- W3f (only if Q3 approved): AC-3.9
- W4-Batch 1: AC-4.1 → W4-Batch 2: AC-4.2

## Wave Plan
- Wave 1 (phase-1): AC-1.1, AC-1.2 — agents get OKF + codegraph knowledge surfaces; no user-visible change.
- Wave 2 (phase-2): AC-2.1, AC-2.2 — non-engineers get troubleshooting workflows + FAQ.
- Wave 3a (phase-3a): AC-3.1, 3.2, 3.3, 3.4 — current behavior snapshotted per module; dashboard behavior provably pinned. Ships as "tests only, zero code change".
- Wave 3b (phase-3b): AC-3.5, 3.6, 3.7, 3.8 — readability refactor of all R/ files with snapshots green; individually revertable per module (3.5 redcap, 3.6 google, 3.7 abs, 3.8 small-modules).
- Wave 3f (phase-3f, conditional): AC-3.9 — REDCap NA-leak sweep, only if Q3 approved.
- Wave 4 (phase-4): AC-4.1, AC-4.2 — repo skill for learn + debug.

## Open Questions
- [needs-clarification] Q1 — `.codegraph/` commit or gitignore? Index sqlite is regenerable via `codegraph init`; committing gives agents instant access on fresh clone but bloats repo. Recommend gitignore + README regen note.
- [needs-clarification] Q2 — Vignette engine: knitr/rmarkdown adds build deps and R CMD check time; propose all chunks `eval=FALSE` (docs, not computation). Acceptable?
- [needs-clarification] Q3 — REDCap `""` NA-leak sweep: refactor wave is zero-behavior-change per your constraint, but memory anchor says the sweep should happen. Approve AC-3.9 as a separate, flagged behavior-FIX AC after lock+refactor of redcap_api.R? Behavior-lock (AC-3.2) records current behavior either way.
- [needs-clarification] Q4 — Skill name/trigger (e.g. `abmdash-guide`)? And should debug workflows include credential-free offline probes only (recommended), or live API calls requiring .Renviron secrets?
- [needs-clarification] Q5 — docs/okf/ inside the gitignored docs/ dir: force-add exception in .gitignore (`!docs/okf/`) + confirm the deploy workflow's commit step preserves other files under docs/ (AC-1.1 includes verification, but if the workflow wipes docs/, OKF must move to a non-gitignored path like inst/okf/ or top-level okf/ — user preference?).

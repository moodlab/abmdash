---
feature: repo-usability
ac: 4.1
status: complete
title: "[chore] Create repo-level abmdash-guide skill (learn-the-repo)"
---

# AC-4.1 — Repo-level abmdash-guide skill

Slice from `repo-usability`. Issue #66. Spec block in issue body (12 conjuncts).

## Progress

- [x] 2026-08-13 — Read issue #66 + spec (12 conjuncts); plan file absent → spec carried in issue body. Synced branch 66-abmdash-guide with origin/main (was behind 5).
- [x] 2026-08-13 — Ground-truth gathering: counted redcap_api.R top-level functions (25, 9 exported → 16 internal), NAMESPACE exports (33), R/ files (10), CI jobs (build-image, build-dashboard), cron `0 6 * * *`, Dockerfile stages (2× rocker/r-ver:4.5.1, node18 + staticrypt), Makefile targets (7), RECORDING.md at root, env var names via Sys.getenv greps.
- [x] 2026-08-13 — Wrote `.opencode/skills/abmdash-guide/SKILL.md` (frontmatter name+description, 8 top-level sections: purpose, architecture, module map, data sources, local run, behavior-lock, verify-understanding, non-engineer guide). No debug section (AC-4.2 scope guard).
- [x] 2026-08-13 — Appended `^.opencode$` to `.Rbuildignore` after `^.codegraph$`.
- [x] 2026-08-13 — Verification: file tracked, frontmatter, accuracy clauses, pipeline keywords, link-existence loop. Committed + pushed + PR created.

## Decision Log

- **Grounded count over OKF**: redcap_api internal helpers stated as 16 (25 top-level, 9 exported) + explicit stale-OKF note pointing to R/redcap_api.R as source of truth. Issue required this (AC-4.1.4).
- **R file count**: stated 10, explicitly calling out OKF index omission of week12_tracking.R (boundary-rule skip, 1 export) (AC-4.1.5).
- **Exports**: stated 33, counted from NAMESPACE (AC-4.1.6).
- **Link style**: relative-style repo-root links (`docs/okf/...`, `R/...`, no leading `/`) per okf-bundle validator false-positive avoidance (AC-4.1.10).
- **Behavior-lock framing**: "tests ARE the spec" + RECORDING.md at repo ROOT, referencing test-redcap-behavior-lock.R, test-snapshot-lock.R, helper-harness.R (≥2 of 3) (AC-4.1.8).
- **Section structure**: 8 top-level sections; verify-understanding (§7) + non-engineer (§8) satisfy dual audience (AC-4.1.9); no debug-workflow top-level section (AC-4.1.11).

## Surprises & Discoveries

- Plan file `.opencode/plans/repo-usability/AC-4.1.md` did not exist at branch creation — spec lives entirely in the issue body's Executable Spec block. Not a blocker; issue body is self-contained.
- redcap_api.R top-level function count (25) is exact via `<- function` grep; 16 internal = 25 − 9 exported (NAMESPACE). OKF's "2 internal helpers" undercounts by 14 — consistent with wave-3b refactor drift the issue warned about.
- gsheet_api.R auths via `GOOGLE_SERVICE_ACCOUNT_JSON` (service account), not OAuth2 flow as its roxygen title says — env-var grep settled the local-run section.
- docs/okf/log.md already records the 33-export cross-reference and the week12_tracking skip — useful cross-check that the stale bits are index/interface prose, not the export count.

## Verification Notes

- `git ls-files --error-unmatch .opencode/skills/abmdash-guide/SKILL.md` ✓
- Accuracy greps: no `2 internal`, no unqualified `9 R files` ✓
- Pipeline keywords: `0 6 * * *`, build-image, build-dashboard, staticrypt, RECORDING.md ✓
- Link-existence loop over all `](...)` targets: all resolve on disk ✓
- Probe outputs pasted into PR body.

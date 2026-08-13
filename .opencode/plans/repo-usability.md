# Master Plan: repo-usability (COMPLETE — 2026-08-13)

## Feature Goal
Make abmdash approachable for non-engineers (R learners, troubleshooters without a coding background) via four waves: (1) agent knowledge surfaces (OKF bundle + codegraph), (2) human-facing docs (troubleshooting vignettes + FAQ), (3) behavior-lock-first zero-behavior-change readability refactor, (4) repo-level opencode skill (abmdash-guide) for learn + debug.

## Wave Plan (all done)
| Wave | ACs | Issues | PRs |
|------|-----|--------|-----|
| W1 knowledge surfaces | 1.1 OKF bundle, 1.2 codegraph | #40, #41 | #42, #43 |
| W2 human docs | 2.1 vignettes, 2.2 FAQ | #44, #45 | #46, #47 |
| W3a behavior-locks | 3.1 harness, 3.2 redcap, 3.3 google, 3.4 abs+small | #48-#51 | #52, #54, #53, #55 |
| W3b refactors | 3.5 redcap, 3.6 google, 3.7 abs, 3.8 small | #56-#59 | #63, #61, #62, #60 |
| W3f sweep | 3.9 NA-leak verify | #65 | #69 |
| W4 skill | 4.1 learn-repo, 4.2 debug | #66, #67 | #68, #70 |

## Dependency Recap
1.1∥1.2 → 2.1 → 2.2 → 3.1 → {3.2,3.3,3.4} → {3.5,3.6,3.7,3.8} → 3.9; 1.1+2.1+2.2 → 4.1 → 4.2

## Resolved Clarifications
Q1 .codegraph/ gitignored + README regen note. Q2 vignettes eval=FALSE. Q3 AC-3.9 approved. Q4 skill = abmdash-guide, offline probes. Q5 docs/okf/ + gitignore exception.

## AC-3.9 Reframe
Approved as behavior-FIX; verified the fix already landed (PR #39, preserved through AC-3.5 refactor; phq8score parse-once in eligibility_mask; siblings benign character-equality). Reframed to formalize-verify: tripwire + failing-first evidence + lock-comment surgery + OKF update. Zero production-code diff.

## Post-Feature Follow-ups
- issue #64: cap-at-16 lock gap in compute_participant_summary + encrypt_dashboard docs_path parametrization
- OKF bundle stale post-refactor (redcap_api "2 internal helpers" vs 16 actual; "9 R files" vs 10) — needs docs/okf/ regen
- gcal env-stop (L42) still no direct lock test (logged in AC-3.6 carryover)
- okf-bundle skill Step-7 link-validator bug (rg -no path prefix) — skill fix candidate
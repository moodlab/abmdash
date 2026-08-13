---
ac: 4.1
depends_on: AC-1.1, AC-2.1, AC-2.2, AC-3.1
risk: low
status: spec
---

# AC-4.1: Repo-level abmdash-guide skill (learn-the-repo)

## Executable Spec
- **predicate (12 conjuncts):**
  1. .opencode/skills/abmdash-guide/SKILL.md exists + git-tracked (git ls-files --error-unmatch).
  2. Frontmatter parses: name: abmdash-guide + non-empty description.
  3. Walkthrough covers ALL: repo purpose; architecture (CI jobs build-image + build-dashboard, cron 0 6 * * *, staticrypt); module map; data sources; local run; behavior-lock location; verify-understanding step.
  4. ACCURACY: redcap_api — NOT "2 internal helpers" (source has ~16); state grounded count OR note OKF lag + direct to R/redcap_api.R.
  5. ACCURACY: NOT unqualified "9 R files" (10 files incl. week12_tracking.R).
  6. ACCURACY: export count 33 (matches NAMESPACE) or defer to NAMESPACE.
  7. Makefile: ≥3 of the 7 real targets {test, test-trad, docker-build, docker-test-auth, docker-render, lint, serve}; no invented.
  8. Behavior-lock-as-spec: references ≥2 of {test-redcap-behavior-lock.R, test-snapshot-lock.R, helper-harness.R} AND RECORDING.md at repo ROOT (not tests/testthat/RECORDING.md); frames tests as executable specs.
  9. Dual audience: agent-facing section (file map/symbols/test-suite) + non-engineer plain-language section (data origins, how to run).
  10. All repo-relative links resolve on disk (ground truth = filesystem, NOT the okf-bundle Step-7 validator which falsely flags absolute-style).
  11. NO debug-workflow top-level section (AC-4.2 scope guard).
  12. .Rbuildignore contains ^\.opencode$ placed after ^\.codegraph$.
- **probe:** test -f + git ls-files + grep frontmatter + grep accuracy clauses (2 internal, 9 R files) + grep 0 6 * * * / build-image / build-dashboard / staticrypt + grep RECORDING.md + link-existence loop.
- **negative:** TOC-only skill (headings but no prose); stale skill (inherits OKF "2 internal helpers"/"9 R files"); broken frontmatter; audience miss (agent-only or human-only); scope creep (debug top-level section).
- **verification:** code + manual (teaching value). ui: block NOT applicable.
- **fixture status:** NEW — SKILL.md + .Rbuildignore entry.
- **rubric anchor:** §4 (module responsibility; what/where/what-NOT).

## Design Intent
§1 frontmatter = the type contract for discoverability. §2 pure documentation. §3 learn-repo cut from debug (4.2). §4 the skill IS the repo-level responsibility document; accuracy clauses ground it in source of truth not stale OKF. §5 one topic per section.

## Technical Context
- Files: .opencode/skills/abmdash-guide/SKILL.md (NEW — repo-LEVEL, user decision Q4), .Rbuildignore (append ^\.opencode$ after ^\.codegraph$).
- Accuracy table (source-verified): redcap helpers 16-17 vs OKF "2"; R files 10 vs "9"; exports 33; CI jobs 2; cron 0 6 * * *; staticrypt; Makefile 7; RECORDING.md at root.
- OKF bundle is STALE post-wave-3b — skill must teach source-of-truth habit, not inherit staleness.
- Link-style: repo-relative paths resolving on disk.
- Skill structure: SKILL.md frontmatter (name, description) + body. Reference convention: ~/.config/opencode/skills/okf-bundle/SKILL.md.

## Dependencies
- depends_on: AC-1.1, AC-2.1, AC-2.2, AC-3.1. Blocks: AC-4.2.
- conflict set: .opencode/skills/*, .Rbuildignore.
- risk: low.

### Progress
- [ ] skill written — pending
### Decision Log
- spec-resolved — accuracy spot-checks vs current source are load-bearing (OKF bundle stale).
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: rewrite skill file.
- Rollback: git revert.
---
ac: 4.2
depends_on: AC-4.1, wave-3 complete
risk: low
status: spec
---

# AC-4.2: Debug workflows in abmdash-guide skill

## Executable Spec
- **predicate (12 conjuncts):**
  1. SKILL.md (REPO-LEVEL: .opencode/skills/abmdash-guide/SKILL.md — NOT user config) exists + debug section (heading debug|troubleshoot|diagnos or symptom→probe→likely-file structure).
  2. Probe existence: every probe = real Makefile target / Rscript devtools::test(filter=<real test file>) / rg pattern / existing path. NO imaginary targets (make test-redcap doesn't exist).
  3. **`make test filter=` sneaky-pass BAN (load-bearing): skill uses `Rscript -e 'devtools::test(filter="...")'`, NEVER `make test filter=X` (Makefile test target = bare devtools::test(), no $(filter) — silently runs FULL suite). Teach FULL filter strings (filter="compliance" matches 3 files — over-match).**
  4. **Offline guarantee: make docker-test-auth + make docker-render (creds+network) ONLY in a "requires credentials" warning, never as probes. No live R calls as probes (abs_login(, download_abs_csv(, read_google_sheet(, etc). make docker-build (network, no creds) in warning section only.**
  5. Lock-suite-as-diagnostic taught with Rscript form: "green = behavior LOCKED, bug upstream (creds/data/env); red = behavior CHANGED in code" per module filter.
  6. abs-login skip disclosure: 1 test skips offline (test-abs-login.R:519 skip_if_not) — "green with 1 skip" is expected, not "all pass".
  7. Post-refactor mapping: "row names contain missing values" → get_eligible_participants (R/redcap_api.R:471) + eligibility_mask (:615) + get_weekly_screening_stats (:657). No stale names/lines.
  8. CI job distinction: build-image → renv::restore/base image/network; build-dashboard → secrets/render/rebase — NOT conflated. CI cache = GHA, not local /tmp/docker-cache.
  9. Discovery: frontmatter description contains debug trigger (debug|troubleshoot|error|broken|diagnose|probe|symptom) — EXTENDS AC-4.1's description, doesn't replace.
  10. AC-4.1 preservation: learn-repo content still present (append-only edit).
  11. 9-cluster coverage: all FAQ verbatim error classes + CI reds + renv + staticrypt have entries.
  12. No generic-only advice: every "check logs" has a repo-specific discriminator.
  - A5: "Failed to parse private key PEM" is NOT a stop() string (openssl runtime) — probe = test(filter="google-token-signing"), NOT rg (would return zero).
- **probe:** structural rg checks per conjunct + filter-validity loop (each devtools::test filter matches a tests/testthat/ file) + no make-test-filter + offline-only probes.
- **negative:** make test filter= present; live-call probes; skip reported as failure; AC-4.1 content removed; imaginary filters/targets; private-key rg false-zero.
- **verification:** code + manual. ui: block NOT applicable.
- **fixture status:** NEW — SKILL.md debug section.
- **rubric anchor:** §4, §3 (creds boundary cut).

## Design Intent
§1 table columns typed: symptom (verbatim) → probe (closed offline set) → likely-file (path:line). §2 pure docs; effectful confined to fenced warning. §3 cut at creds boundary + CI job boundary. §4 header declares debug coverage + NOT-credential-required. §5 one symptom→probe row = one diagnosable unit.

## Technical Context
- Files: .opencode/skills/abmdash-guide/SKILL.md (append debug section + extend frontmatter description with debug triggers).
- Canonical lock-suite-as-diagnostic text: "Behavior-lock suites are executable docs: Rscript -e 'devtools::test(filter=\"<name>\")'. Green = behavior LOCKED — bug is upstream (credentials, data shape, environment). Red = behavior CHANGED in code. Never use make test filter=X — Makefile test ignores filter, silently runs the full suite; make test-trad is the only filtered make target. Filters are substrings of test-file names — use full strings."
- Valid Makefile targets: test, test-trad, lint, serve, docker-build (network), docker-test-auth (creds+network), docker-render (creds+network).
- Valid test filters (17 files): abs-login, combined-calendar, compliance-summary, compliance-tracking, demographics, eligible-participants, faq-verbatim, fixture-scan-google, gcal-api, google-token-signing, gsheet-api, harness-httptest2-dryrun, redcap-behavior-lock, run-initial-function, snapshot-lock, trad-compliance, week12-tracking.
- 16-entry symptom→probe→likely-file table: (1) row-names → redcap-behavior-lock → redcap_api.R:471/615/657; (2) Failed to parse private key → google-token-signing (NOT rg) → gsheet:188-189/gcal:118-119; (3) Google env missing → gsheet-api → gsheet:163; (4) GCal event errors → gcal-api → gcal_api.R; (5) REDCap fetch/parse → redcap-behavior-lock → redcap_api.R; (6) eligible regressions → eligible-participants → :471; (7) snapshot drift → snapshot-lock → _snaps/; (8) FAQ verbatim → faq-verbatim → test-faq-verbatim.R; (9) trad regression → make test-trad; (10) ABS offline → abs-login (green + 1 skip) → test-abs-login.R:519; (11) CI build-image red → renv.lock/Dockerfile (GHA cache); (12) CI build-dashboard red → secrets/render/rebase → build-dashboard.sh/workflow; (13) renv restore fail → rg renv.lock; (14) staticrypt → build-dashboard.sh/workflow; (15) fixture-scan → fixture-scan-google; (16) calendar order → combined-calendar → gcal_api.R.
- Offline guarantee mechanism: probes are rgrep/bash only; cred/network targets fenced in warning paragraph.

## Dependencies
- depends_on: AC-4.1 (skill scaffold — MUST land first; append-only), wave-3 complete.
- conflict set: .opencode/skills/abmdash-guide/SKILL.md (shared with AC-4.1 — strictly append after 4.1 merges).
- risk: low.

### Progress
- [ ] debug section — pending
### Decision Log
- spec-resolved — make test filter= ban is the highest-leverage catch; repo-LEVEL skill path (4.2 resolver's user-config path was wrong).
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: rewrite section.
- Rollback: git revert.
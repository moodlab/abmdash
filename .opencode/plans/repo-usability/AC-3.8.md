---
ac: 3.8
depends_on: AC-3.4
risk: medium
status: complete
---

# AC-3.8: Refactor 5 small modules (ZERO behavior change)

## Executable Spec
- **predicate (8 conjuncts):**
  1. make test green (all 17 test files incl. 5 lock files + test-faq-verbatim OKF link).
  2. git diff NAMESPACE empty (11 exports byte-identical).
  3. git diff DESCRIPTION empty — zero new deps (no dplyr/rlang/lubridate/tidyr/fs).
  4. rg 'for\s*\(' R/compliance_summary.R R/compliance_tracking.R R/week12_tracking.R R/run_initial_function.R → 0 matches (loops killed or in named helpers).
  5. devtools::document() → git diff man/ empty (idempotent, RoxygenNote 7.3.2).
  6. **rg '`%||%` <- ' R/ → exactly 1 (redcap_api.R:502) — no duplication.**
  7. Per-module commits ≥5, each bisectable (each passes make test).
  8. Pinned derived values intact (drift traps below are tripwires).
- **probe:** make test && git diff --exit-code NAMESPACE DESCRIPTION man/ && rg -c 'for\s*\(' on the 4 files (expect 4 zeros).
- **negative (drift-trap enumeration — cheapest broken impls that must NOT pass):**
  (a) round(x,1)→sprintf("%.1f") yields "50.0%" vs locked "50%" (test-demographics.R:106 — AC narrative misdescribes as 50.0%);
  (b) round(x,1)→round(x) "16.7%"→"17%";
  (c) difftime(units="weeks")→manual division → float drift vs locked 2.73;
  (d) window >0&<5 → >=0&<=5 boundary flip;
  (e) cap-at-16 never exercised (fixture max 3.73wk) — known lock-coverage gap: LOG as follow-up, do NOT block;
  (f) +56/+84/+168 → lubridate::weeks (DST drift + new dep);
  (g) **"Due in 1 days" pluralization "fix" breaks locked string (test-week12-tracking.R:74) — most tempting wrong fix;**
  (h) empty-frame week integer→numeric (seq vs 1:4);
  (i) format() tz specifiers (CDT string);
  (j) stop-string rewording (run_initial L97 verbatim);
  (k) empty-compliance error path removed;
  (l) **fs::file_exists replacing base file.exists bypasses the base mock (sneaky-pass).**
- **verification:** code. ui: block NOT applicable.
- **fixture status:** existing — no new fixtures.
- **rubric anchor:** §2, §5.

## Design Intent
§1 11 exports byte-identical; helper signatures internal-only. §2 extracted helpers pure (frames in/out); google/redcap/file.exists seams stay effectful + un-moved. §3 per-module commits — independently revertable. §4 helper names state what (compute_participant_summary, build_week_frame, build_followup_rows, resolve_first_existing). §5 loops → lapply/split+do.call(rbind).

## Technical Context
- Files: R/compliance_summary.R, R/compliance_tracking.R, R/demographics.R, R/week12_tracking.R, R/run_initial_function.R, man/.
- Per-module plan: compliance_summary L43 → compute_participant_summary + lapply/do.call(rbind); empty input still stops (locked). compliance_tracking L96 → build_week_frame + lapply(1:4, ...) (integer, NOT seq); empty-frame contract: id char/week int/start POSIXct/time_from_start double/late logical; L97 stop verbatim. demographics: minimal (optional compute_age); round(x,1) NOT sprintf. week12 L164 → build_followup_rows + split/lapply; 4 empty variants; %||% L39-43 kept (single def in redcap_api.R:502); difftime weeks, +56/+84/+168, "Due in 1 days" verbatim. run_initial L89 → possible_paths[file.exists(possible_paths)][1] or resolve_first_existing; **filename MUST NOT change (OKF link test-faq-verbatim.R:250)**; base file.exists only.
- encrypt_dashboard: STRICTLY refactor, NO parametrization (docs_path param = signature change = .Rd usage change = violates man/ idempotence + zero-behavior). /app/docs branch preserved; testability = follow-up issue.
- Mock seams preserved (sneaky-pass guards): get_participant_summary→get_compliance_report; get_compliance_report→read_google_sheet; get_demographic_summary→get_redcap_report; get_upcoming_followups→get_redcap_logs; get_enrollment_targets→base file.exists.

## Dependencies
- depends_on: AC-3.4. Blocks: none (terminal wave-3 cleanup).
- conflict set: 5 R/ files + man/.
- risk: medium.

### Progress
- [x] refactor — complete 2026-08-13 (5 commits: 8f756df compliance_summary, 4e147b5 compliance_tracking, bdc1ae4 demographics, a901eb8 week12_tracking, 7640e03 run_initial_function; each passed make test; all probes green)
### Decision Log
- spec-resolved — encrypt_dashboard strictly refactor (no signature change); cap-at-16 logged as lock gap (non-blocking).
### Surprises & Discoveries
- unlist(list-of-lists, recursive = FALSE) does NOT drop NULL elements nested inside inner lists (only empty inner lists vanish) — explicit NULL-filter needed after flattening in build_followup_rows wiring (verified in R before committing).
- rg/grep -c exits 1 on zero matches, breaking && chains — check per-file separately or use `;`.
- Rscript -e run without an explicit workdir executes in the session default cwd — a relative-path write silently hit the primary checkout (abmdash) instead of the worktree. Always pass workdir or absolute paths.
### Idempotence & Recovery
- Safe retry: per-commit lock-green.
- Rollback: git revert; lock suite catches drift.
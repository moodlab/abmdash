---
ac: 1.1
depends_on: none
risk: low
status: complete
---

# AC-1.1: OKF knowledge bundle at docs/okf/

## Executable Spec

- **predicate:** ALL hold simultaneously:
  **P1** — `docs/okf/index.md` exists, `git ls-files docs/okf/` returns ≥1 line, `docs/okf/log.md` exists with a conformance PASS entry.
  **P2** — Exactly 9 module concept docs (redcap_api, abs_login, gsheet_api, gcal_api, run_initial_function, compliance_summary, compliance_tracking, trad_compliance, demographics — each ≥2 public functions per okf-bundle hard rule) + ≥1 deploy-pipeline Service concept doc (Dockerfile + build-dashboard.yml + build-dashboard.sh). `week12_tracking.R` (1 fn) mentioned as dependency, NOT own doc. Each concept doc >200 bytes.
  **P3** — Every `.md` under `docs/okf/` except `index.md`/`log.md` has YAML frontmatter `---`-delimited, non-empty `type:`, all 5 body sections (`# Responsibility`, `# Interface`, `# Dependencies`, `# Invariants`, `# Examples`). okf-bundle skill Step-6 conformance script → zero output.
  **P4** — okf-bundle skill Step-7 link-validation script → zero broken links.
  **P5** — `docs/okf/modules/redcap_api.md` `# Invariants` matches `/as\.numeric.*""|NA.*leak|parse.*guard|suppressWarnings/i` AND notes remaining un-fixed instances at `redcap_api.R` L441/L563 (phq8score guard-pattern persists post-#39).
  **P6** — `git check-ignore docs/okf/modules/redcap_api.md` exits NON-zero. Requires `.gitignore` `docs/*` (NOT `docs/`) + `!docs/okf/`. Probe file: new file at `docs/okf/modules/_probe.md` appears in `git status` without `-f`.
  **P7** — `.Rbuildignore` UNCHANGED: `^docs/` (line 6) already excludes `docs/okf/` — TRE regex, no lookahead; `^docs/okf/` carve-out is a no-op. AC's "add .Rbuildignore entry" is ALREADY SATISFIED — verification-only (do NOT remove `^docs/`; removal breaks tarball). `tar tf` of built tarball shows no `docs/okf/`.
  **P8** — `git diff --exit-code .github/workflows/build-dashboard.yml` exits 0 (file UNMODIFIED). No `rm`/delete targeting `docs/okf/`. Docker run (L76–135) writes only `index.html`/`site_libs/` (L108–111); staticrypt (L114–134) touches only `*.html` in docs/ root; commit step `git add -f docs/` (L142) stages okf, unchanged → no diff.
  **P9** — All 34 NAMESPACE exports referenced in ≥1 concept doc `# Interface`; no invented exports.

- **probe:**
  ```bash
  # P1
  test -f docs/okf/index.md && git ls-files docs/okf/ | wc -l && test -f docs/okf/log.md  # ≥1
  # P2
  find docs/okf -name '*.md' ! -name 'index.md' ! -name 'log.md' | wc -l   # ≥10
  find docs/okf -name '*.md' ! -name 'index.md' ! -name 'log.md' -size -200c -print  # empty
  # P3
  for f in $(find docs/okf -name '*.md' ! -name 'index.md' ! -name 'log.md'); do
    head -1 "$f" | grep -q '^---' || echo "MISSING frontmatter: $f"
    grep -q '^type:\s*\S' "$f" || echo "MISSING type: $f"
    for h in Responsibility Interface Dependencies Invariants Examples; do
      grep -q "^# $h" "$f" || echo "MISSING '# $h': $f"
    done
  done  # zero output
  # P4: okf-bundle skill Step-7 script → zero broken links
  # P5
  grep -iP 'as\.numeric.*""|NA.*leak|parse.*guard|suppressWarnings' docs/okf/modules/redcap_api.md \
    && grep -in 'L44[13]|L56[35]|phq8score' docs/okf/modules/redcap_api.md
  # P6 (critical)
  git check-ignore docs/okf/modules/redcap_api.md; echo "exit=$?"   # exit=1
  touch docs/okf/modules/_probe.md && git status --short docs/okf/modules/_probe.md && rm docs/okf/modules/_probe.md  # shows ??
  # P7
  grep -q '^\^docs/' .Rbuildignore && R CMD build . 2>/dev/null && tar tf abmdash_*.tar.gz | grep '^docs/okf'   # grep EMPTY
  # P8
  git diff --exit-code .github/workflows/build-dashboard.yml   # exit 0
  grep -n 'rm\|delete' .github/workflows/build-dashboard.yml   # no destructive cmd on docs/okf
  # P9
  grep -oP '^export\(\K[^)]+' NAMESPACE | while read fn; do grep -rq "$fn" docs/okf/ || echo "UNDOCUMENTED: $fn"; done  # zero output
  ```

- **negative:** 1. Silent gitignore failure — `docs/` + `!docs/okf/` silently ignored by git (parent-dir exclusion cannot be re-included); `git add -f` masks at initial commit. Fix: `docs/*` not `docs/`. 2. Stub bundle (index.md only / empty docs <200 bytes). 3. REDCap invariant omitted or remaining L441/L563 instances unflagged. 4. Builder removes `^docs/` from .Rbuildignore → tarball bloat. 5. Workflow modified (AC says read-only). 6. NAMESPACE drift / invented exports. 7. Broken cross-links (hyphen-vs-underscore). 8. Bundle generated but never committed (blendtutor lesson). 9. Conformance failure (missing `# Invariants`/`# Examples`). 10. OUT OF SCOPE (document only): `build-dashboard.sh` L53 `rm -rf docs` destroys `docs/okf/` on local run — deploy-pipeline concept doc must record this latent clobber; no code change this AC.

- **verification:** `code` · shell + grep + git/R CMD build asserts (P1–P9) + `manual` residual: concept-doc prose quality.
- **fixture status:** NEW — probe commands ARE the fixture. No `tests/testthat/` file (docs/config AC).
- **rubric anchor:** §4.1 (primary), §1.1 (redcap `# Invariants` encodes `""`-not-`NA` parse-guard), §3 (tertiary).
- **ui: block:** NOT APPLICABLE.

## Design Intent
- **Types / interfaces (§1):** redcap_api `# Invariants` records `""`-not-`NA` invariant: parse once via `suppressWarnings(as.numeric())`, guard on PARSED value, `USE.NAMES=FALSE`. Doc notes L441/L563 still carry raw-guard pattern (feeds wave-3a lock / AC-3.9).
- **Pure / effectful (§2):** `pure:` frontmatter — demographics/compliance_summary pure; redcap/gsheet/gcal/abs effectful shells.
- **Boundary cuts (§3):** concept boundaries = R/ file boundaries. week12_tracking (1 fn) → dependency mention only.
- **Module responsibility (§4):** each `# Responsibility` names what module does AND does NOT do.
- **Function discipline (§5):** `# Interface` = exported-signatures quick reference (all 34 exports).

## Technical Context
- **Files touched:** `docs/okf/index.md`, `log.md`, `modules/index.md` + 9 module docs, `services/index.md` + `deploy-pipeline.md` (all NEW); `.gitignore` MODIFIED (`docs/` → `docs/*` + `!docs/okf/`); `.Rbuildignore` UNCHANGED (verified-only).
- **NOT touched:** `.github/workflows/build-dashboard.yml` (read-only verify), `build-dashboard.sh` (out of scope, clobber risk documented only).
- **gitignore mechanics:** git cannot re-include under excluded parent → `docs/*` + `!docs/okf/`. Workflow `git add -f docs/` masks wrong pattern initially — hence P6 probe-file test.
- **Function counts:** redcap_api 10, abs_login 7, gsheet_api 6, gcal_api 4, run_initial_function 4, compliance_summary 2, compliance_tracking 2, trad_compliance 2, demographics 2 → docs; week12_tracking 1 → dep mention.
- **okf-bundle skill:** read ~/.config/opencode/skills/okf-bundle/SKILL.md and follow its bundle structure + Step-6 conformance + Step-7 link validation.

## Dependencies
- **Depends on:** none. **Blocks:** W2 vignettes, W3 refactor baseline.
- **Conflict set:** `.gitignore`, `.Rbuildignore` — serialized with AC-1.2 (both touch same files; AC-1.1 lands FIRST, AC-1.2 appends after).
- **Risk level:** low.

### Progress
- [x] bundle generated — complete 2026-08-13 (P1-P9 all green; R CMD build ran full tarball check)

### Decision Log
- spec-resolved — `.Rbuildignore` "add entry" instruction is ALREADY SATISFIED by `^docs/`; verify-only, do not edit.
- P9 count — NAMESPACE contains **33** `export()` lines, not 34 (spec off-by-one, likely counted the roxygen comment header). P9 probe reads the real NAMESPACE; all 33 documented in `# Interface` sections, none invented. log.md records 33.
- L441/L563 flags — verified via `git show 90ef649^:R/redcap_api.R`: pre-fix raw-guard `!is.na(phq8score) & as.numeric(phq8score) >= 17` sat at exactly L441 (get_eligible_participants) and L563 (get_weekly_screening_stats). Post-#39 parse-once guards now at L446/L575. redcap_api.md flags historical positions + documents that the raw-guard STYLE persists on sibling criteria fields (L440-450 / L568-580) as the AC-3.9 sweep target.

### Surprises & Discoveries
- okf-bundle skill Step-7 link-validation script has a latent bug: `rg -no` emits `path:line:` prefixes, so `while read p` receives the whole prefix and `[ -f "docs/okf${p}" ]` can never succeed — EVERY absolute-style mdlink (`](/path.md)`) is falsely reported BROKEN. Workaround: use same-dir relative links (`name.md`) and cross-dir relative links (`../modules/index.md`) — none match the `](/` regex, verbatim script output is zero, and all links resolve (verified per-file). Any future doc that adds an absolute-style link will re-trigger the false positive.
- macOS BSD grep has no `-P` (PCRE): the spec's P5/P9 probes (`grep -iP`, `grep -oP`) fail on this machine. Equivalent `-E` regex + `sed`/`rg -P` extraction produce identical semantics; results reported from those.
- R CMD build completes in seconds in this env (no renv restore needed for tarball build), so P7 was fully verified (tarball grep empty) rather than falling back to the line-presence check.

### Idempotence & Recovery
- Safe retry: re-run okf-bundle skill generation; files idempotent.
- Rollback: git revert of the docs/okf + .gitignore commit.

### Carryover Log
- Cycle 1 (PR #42): index.md link-convention mismatch [consider] + run_initial_function.md self-link text [nit] — resolved in a40deaa (fix(docs) commit, link validation re-passed). No fix-now items. Status: resolved.
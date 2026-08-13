---
ac: 3.7
depends_on: AC-3.4
risk: high
status: spec
---

# AC-3.7: Refactor abs_login.R (ZERO behavior change)

## Executable Spec
- **predicate (10 conjuncts):**
  1. Lock suite green unchanged: test-abs-login.R (526L), test-faq-verbatim.R, test-trad-compliance.R — zero test edits.
  2. **Export signatures byte-identical: formals() of 5 exports identical pre/post (arg names + defaults, not just NAMESPACE names).**
  3. NAMESPACE byte-identical; man/ \usage+\arguments identical.
  4. No new deps.
  5. Nesting flattened: rg '^\s{16,}' strictly fewer lines; no line deeper than pre-refactor max (sites L136/153/231/389/452).
  6. 15 stop strings verbatim (rg -c 'stop\(' == 15; each greps once).
  7. Internal helper names preserved (extract_livewire_snapshot, extract_csrf_token — ::: calls break if renamed); NEW helpers @keywords internal; **`session` formal MUST NOT rename**.
  8. **REQUEST-CONSTRUCTION PRESERVED (lock-gap — req_perform mocked so cookie/SSL/UA/timeout never exercised; grep-lock instead):** req_cookie_preserve(cookie_file) + cookie_file <- file.path(tempdir(),"abs_session_cookies.txt") (L49-55); req_options(ssl_verifypeer=0, ssl_verifyhost=0, http_version=2) in BOTH abs_login AND test_abs_connection (L50-52, L263-265); req_user_agent exact strings (L54 "Mozilla/5.0...", L261 "R httr2"); req_timeout(30) L262.
  9. **REGEX SEMANTICS preserved:** extract_livewire_snapshot uses regexpr (first-match) NOT gregexpr; download_abs_csv list-tests uses gregexpr (all-match) + filter.
  10. **CSRF CASCADE ORDER preserved (data-csrf L214 → meta L220 → hidden name-first L228 → value-first L229)**; verify_abs_login redirect set c(301,302,303,307,308) preserved (L319; tests cover 302 only).
- **probe:** test_file on 3 lock files; formals snapshot comparison; git diff NAMESPACE DESCRIPTION man/; rg '^\s{16,}'; rg -c 'stop\(' == 15; rg req_cookie_preserve|ssl_verifypeer|http_version|req_timeout|Mozilla|R httr2; rg 'regexpr\(.*wire:snapshot' (must be regexpr); rg 'data-csrf|csrf-token|_token' (line order); rg 'c\(301, 302, 303, 307, 308\)'.
- **negative:** lock-green does NOT suffice — sneaky-passes the lock CANNOT catch: cookie file path/type drift; SSL/http_version dropped; regexpr→gregexpr swap; CSRF cascade reorder; session formal rename; scope creep "improving" httr2 construction. Predicate points 8/9/10 exist because req_perform-mocking blinds the lock.
- **verification:** code. ui: block NOT applicable.
- **fixture status:** existing; no new fixtures. OPTIONAL separate-commit strengthening: preview_abs_csv n-param coverage (wave-3a carryover, NOT required).
- **rubric anchor:** §2, §5.

## Design Intent
§1 5 export signatures frozen (session name part of interface). §2 pure parsers already isolated; new helpers continue pure-vs-effectful split. §3 cut at pipeline stages of login flow. §5 one thing per helper, flattened nesting.

## Technical Context
- Files: R/abs_login.R, man/ (regen). NAMESPACE/DESCRIPTION byte-identical.
- Extraction map (login): build_base_request (httr2 chain: URL/method/SSL/UA/cookies) → fetch_login_page → submit_credentials → detect_redirect (incl. 301/302/303/307/308) → build_login_diagnostics. (CSV): fetch_tests_page → find_list_tests_snapshot → call_download_action → extract_csv_content → parse_csv. All @keywords internal.
- Rename map: 1:1; no renames of existing symbols.
- Commit cadence (bisectable, safest-first): 1. pure helpers (parsers, diagnostics); 2. mid-risk stage extractions; 3. LAST build_abs_base_request (effectful chain — cookie/SSL/UA/timeout policy; grep-8 verified immediately).

## Dependencies
- depends_on: AC-3.4. Blocks: none (leaf).
- conflict set: R/abs_login.R, man/.
- risk: HIGH.

### Progress
- [ ] refactor — pending
### Decision Log
- spec-resolved — request-construction/regex/CSRF-order grep probes close real mock-blind gaps ("lock green = done" is FALSE here).
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: per-commit lock-green.
- Rollback: git revert; lock suite catches drift.
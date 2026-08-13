---
ac: 3.9
depends_on: AC-3.2, AC-3.5
risk: low
status: spec
---

# AC-3.9: Formalize-verify the REDCap NA-leak sweep (fix already landed)

## IMPORTANT PREMISE CORRECTION
The AC was approved as a behavior-FIX, but both speculators + resolver verified the fix is ALREADY in place: post-refactor eligibility_mask (R/redcap_api.R:615-631) has phq8score parse-once (L616 suppressWarnings(as.numeric()) → guard on parsed L625); sibling guards are CHARACTER-EQUALITY (benign — "" == "1" is FALSE, never NA → no leak); USE.NAMES=FALSE at L529; filter_recent_dates date-parse-once correct. PR #39 fixed it; AC-3.5 refactor preserved it. Failing-first is impossible. This AC = formalize-verify-document (tripwire + evidence + comment surgery + OKF update + classification pin). Flag the reframe in the issue + user summary.

## Executable Spec
- **predicate (10 conjuncts):**
  1. Tripwire: direct eligibility_mask() call with phq8score=="" row → !anyNA(mask), no all-NA row — BYPASSES the outer tryCatch that would mask the crash into an error frame.
  2. Benign pin: eligibility_mask() on rows with r01es_commute=="" / austin=="" → FALSE never NA — classifies == "1"/== "0" guards benign, blocks future "fix" adding as.numeric() there.
  3. Raw-layer invariance: report_to_df preserves "" for phq8score/guid/interview_date — NO upstream normalization (lock tests L113/L129/L214 already red if violated).
  4. Zero-diff guard: no as.numeric( introduced adjacent to any == "1"/== "0" expression in R/redcap_api.R (grep negative).
  5. Fix-pattern presence: suppressWarnings(as.numeric( at :616; guard operates on parsed var (phq8score_num) not raw field.
  6. USE.NAMES = FALSE present at :529.
  7. Lock-comment surgery: 5 LOCKING-CURRENT/"AC-3.9 will decide" comments in test-redcap-behavior-lock.R (L6, L111-112, L128, L214-215, L244) → LOCKED: with what's pinned; no weakened assertions, strict correctness upgrades.
  8. OKF doc update: docs/okf/ redcap concept doc — stale L441/L563 refs → helper names (eligibility_mask, parse-once); character guards classified BENIGN; sweep complete.
  9. Valid-data invariance: all existing lock tests green unchanged.
  10. Failing-first evidence: reverting eligibility_mask to raw-guard (`!is.na(phq8score) & as.numeric(phq8score) >= 17`) makes the tripwire FAIL — captured in PR body (one-off verification, not committed).
- **probe:** Rscript test_file on new test-na-leak-sweep.R + test-redcap-behavior-lock.R + test-eligible-participants.R; grep probes for 4/5/6.
- **negative:** tripwire fails to bite on raw-guard revert (vacuous — AC-3.6 lesson); raw-layer normalization forbidden (report_to_df "" → NA fails existing locks); public-API-only test asserting error frame does NOT count (tryCatch masks the crash); over-fixing char guards (zero-diff on == "1"/== "0").
- **verification:** code. ui: block NOT applicable.
- **fixture status:** existing fixtures; NEW test-na-leak-sweep.R (tripwire + benign pin).
- **rubric anchor:** §1 (invariant !anyNA(mask)), §2 (pure eligibility_mask tested directly; tryCatch shell must not mask the pure-core invariant), §5.

## Design Intent
§1 invariant: eligibility mask never contains NA regardless of raw-string input; NO type change at raw layer. §2 pure core tested directly. §3 parse-once boundary = mask helper, not report_to_df. §4 lock-test header + OKF doc state sweep complete. §5 each test one thing.

## Technical Context
- Files: tests/testthat/test-na-leak-sweep.R (NEW), test-redcap-behavior-lock.R (comment surgery only), docs/okf/ redcap concept doc. R/redcap_api.R READ-ONLY (zero diff).
- Guard classification (20 sites condensed): phq8score (c)-site ALREADY FIXED; siblings r01es_commute/austin/phone/computer/bpd/etc == "1"/== "0" BENIGN; first-name sapply USE.NAMES=FALSE FIXED; filter_recent_dates correct; week12 sapply(list,is.null) NOT (c); report_to_df "" fidelity locked.
- Sweep conclusion: exactly one real (c)-site (phq8score), fixed PR #39 + preserved through AC-3.5; remaining 19 benign-or-fixed. AC-3.9 closes the loop: prove it, pin it, document it.

## Dependencies
- depends_on: AC-3.5, AC-3.2, PR #39. Blocks: OKF redcap doc accuracy.
- conflict set: test-redcap-behavior-lock.R (comment-only), docs/okf/.
- risk: low (zero production-code diff).

### Progress
- [ ] tripwire + surgery — pending
### Decision Log
- spec-resolved — reframed from behavior-FIX to formalize-verify (fix landed pre-AC). Flag to user.
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: re-run tests.
- Rollback: git revert.
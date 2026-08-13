# Evidence — Issue #38: "row names contain missing values" in Recent Eligible Participants

**Date:** 2026-08-12 · **Branch:** `38-fix-recent-eligible-NA` · **Base:** `origin/main` (1b874dd)

## Root cause (confirmed empirically, R 4.5.1 — same version as the Docker deploy)

The issue's hypothesis pointed at the 30-day `interview_date` filter
(`R/redcap_api.R:422-426`), but that filter **already contains a `!is.na()`
guard** and was verified NOT to be the failing line. Live reproduction against
REDCap report 14081 (83 records) pinned the real chain:

1. **`R/redcap_api.R:441` (eligibility filter):** `!is.na(recent_records$phq8score) & as.numeric(recent_records$phq8score) >= 17` guards the *raw string* but not the *parsed value*. REDCap returns unset fields as `""`; `as.numeric("")` is `NA`, so the guard is `TRUE & NA = NA` and the logical row index contains NA. `[.data.frame` does not error — it inserts an all-NA row (row name coerced to the string `"NA"`).
2. **`R/redcap_api.R:462` (first name extraction):** `sapply(eligible_participants$r01es_name, ...)` with default `USE.NAMES = TRUE` names the result with the full-name *values*; the all-NA row's value is a real `NA`.
3. **`R/redcap_api.R:472-479` (result construction):** `data.frame(first_name = first_names, ...)` promotes those names to row names; base R's `data.frame()` raises **"row names contain missing values"** (`anyNA(row.names)` check, R source line 154 of `data.frame`). The function's internal `tryCatch` converts that into the summary frame `Status = "Error: row names contain missing values"`, `Total_Records = 0`, `Eligible_Count = 0` — the exact production output.

Relevant real-data facts: 21 of 61 recent records have empty `phq8score`; one
of those passes every other criterion, so one NA row leaked.

## Fix (R/redcap_api.R)

| Location | Before | After |
|---|---|---|
| `get_eligible_participants`, eligibility filter | `!is.na(recent_records$phq8score) & as.numeric(recent_records$phq8score) >= 17` | parse once + guard parsed value: `phq8score_num <- suppressWarnings(as.numeric(...))` then `!is.na(phq8score_num) & phq8score_num >= 17` |
| `get_eligible_participants`, first names | `sapply(eligible_participants$r01es_name, ...)` | `sapply(..., USE.NAMES = FALSE)` — stops the name→row-name promotion entirely |
| `get_weekly_screening_stats`, eligibility filter | same raw-string guard as above | same parsed-value guard (sibling function, same bug: silently inflated `eligible_count` by 1) |

The 30-day date filter was left untouched — it was already NA-safe.

## Evidence files

- `01-test-red-before-fix.txt` — testthat output on the un-fixed code: test 2
  (`get_eligible_participants excludes records with empty/unparseable
  phq8score`) fails; the function returns the error summary frame instead of
  participant data (`"first_name" %in% names(result)` is `FALSE`).
- `02-test-green-after-fix.txt` — same tests after the fix: 19 pass, 0 fail.
- `03-full-suite.txt` — full `testthat::test_local()`: 39 pass, 0 fail.
- `04-e2e-live-api-before-fix.txt` — live REDCap run, pre-fix package: exact
  production error frame + step trace.
- `04-e2e-live-api-after-fix.txt` — live REDCap run, post-fix package: 5 real
  eligible participants (First Name / Phone / Screen Date / Link), no error.

## Test coverage added (tests/testthat/test-eligible-participants.R)

Mocking the HTTP boundary per existing convention (`local_mocked_bindings` on
`get_redcap_report`, REDCap-shaped list-of-lists fixtures):

1. NA / unparseable `interview_date` records are excluded, not crashed on.
2. **Regression for #38:** empty `phq8score` record excluded — participant data
   returned, no error frame, no garbage all-NA row (`Unknown`/`NA` phone/`id=NA` link).
3. No eligible records → summary frame with `Eligible_Count = 0`.
4. Empty report → summary frame with `Total_Records = 0`.

## Spec conformance

- AC "no longer errors with row names contain missing values": covered by test 2 + live-API run.
- AC "table renders eligible participants": live-API run returns 5 rows with all 4 columns.
- AC "NA/unparseable interview_date handled safely": covered by test 1 (guard pre-existed; now pinned).
- AC "unit test added": tests/testthat/test-eligible-participants.R (4 tests).

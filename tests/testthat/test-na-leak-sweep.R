# NA-leak sweep tripwire + benign pin (AC-3.9)
#
# The wave-3b/AC-3.5 REDCap NA-leak sweep found exactly ONE real leak site:
# the phq8score raw-string guard (as.numeric("") == NA in a logical row
# index inserts an all-NA row, and data.frame() then raises "row names
# contain missing values"). PR #39 fixed it; the AC-3.5 refactor preserved
# it. Every sibling eligibility guard is CHARACTER-EQUALITY ("" == "1" is
# FALSE, never NA) and therefore benign.
#
# This file pins that classification so a future change FAILS loudly if it:
#   * reverts eligibility_mask to the raw-guard form (!is.na(raw) &
#     as.numeric(raw) >= N) — tripwire below, or
#   * "improves" a character-equality guard with as.numeric() — benign pin.
#
# eligibility_mask is called DIRECTLY via ::: — NOT through
# get_eligible_participants() — so the outer tryCatch error frame (which
# would degrade a crash into a Status data.frame) can never hide an NA leak.
# The pure core is tested at its own layer (design-principles rubric §2).

# Build one row of the eligibility data frame with exactly the fields
# eligibility_mask reads. Every sibling guard defaults to its eligible value
# ("1" / "0") so a test can exercise one field at a time.
eligible_mask_row <- function(record_id,
                              phq8score = "20",
                              r01es_commute = "1",
                              r01es_austin = "1") {
  data.frame(
    record_id = record_id,
    r01es_commute = r01es_commute,
    r01es_austin = r01es_austin,
    r01es_phone = "1",
    r01es_computer = "1",
    r01es_bpd = "0",
    r01es_psychotherapy = "0",
    phq8score = phq8score,
    r01es_druguse = "0",
    medchng = "0",
    r01es_medstop = "0",
    r01es_medstart = "0",
    stringsAsFactors = FALSE
  )
}

test_that("eligibility_mask never emits NA for phq8score \"\" (tripwire)", {
  records <- eligible_mask_row("P001", phq8score = "")

  # Direct helper call bypasses the outer tryCatch: a crash here RAISES
  # instead of degrading to the error data.frame the public API returns.
  mask <- abmdash:::eligibility_mask(records)

  expect_type(mask, "logical")     # a tryCatch error frame would not be
  expect_false(anyNA(mask))        # invariant: the mask has no NA entries
  expect_false(mask[[1]])          # "" is below cutoff, never NA
  # An NA mask entry would make [.data.frame insert an all-NA row; row
  # selection must return exactly sum(mask) rows.
  expect_identical(nrow(records[mask, ]), as.integer(sum(mask)))
})

test_that("eligibility_mask character-equality guards are benign: \"\" is FALSE, never NA", {
  records <- rbind(
    eligible_mask_row("P001", r01es_commute = ""),
    eligible_mask_row("P002", r01es_austin = "")
  )

  mask <- abmdash:::eligibility_mask(records)

  expect_type(mask, "logical")
  expect_false(anyNA(mask))
  # "" == "1" is FALSE with no coercion, so the guard excludes the row; it
  # does NOT produce NA. A future "fix" adding as.numeric() here would make
  # as.numeric("") == NA and leak an NA into the mask.
  expect_identical(mask, c(FALSE, FALSE))
  expect_identical(nrow(records[mask, ]), as.integer(sum(mask)))
})

test_that("eligibility_mask phq8score mixed edge: \"\", NA, below-cutoff all yield FALSE", {
  records <- rbind(
    eligible_mask_row("P001", phq8score = "20"),  # >= 17 -> eligible
    eligible_mask_row("P002", phq8score = ""),    # unparseable -> not eligible
    eligible_mask_row("P003", phq8score = NA),    # NA -> not eligible
    eligible_mask_row("P004", phq8score = "15")   # below cutoff -> not eligible
  )

  mask <- abmdash:::eligibility_mask(records)

  expect_type(mask, "logical")
  expect_false(anyNA(mask))
  expect_identical(mask, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(nrow(records[mask, ]), as.integer(sum(mask)))
})

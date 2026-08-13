# Unit tests for get_eligible_participants()
#
# Regression tests for #38: "Error: row names contain missing values" in the
# Recent Eligible Participants dashboard table. The error was raised by
# data.frame() when a record with an empty/unparseable phq8score ("") slipped
# through the eligibility filter as an NA row: as.numeric("") is NA, so the
# logical row index contained NA, [.data.frame inserted an all-NA row, and the
# sapply(USE.NAMES=TRUE) + data.frame() construction promoted the NA row name
# into "row names contain missing values".
#
# All tests mock the REDCap HTTP boundary (get_redcap_report) with
# REDCap-shaped JSON data: a list of named lists, character scalars, "" for
# unset fields, "YYYY-MM-DD" interview dates.

# Build a REDCap-shaped record list with the fields get_eligible_participants
# actually reads. All eligibility fields default to "1" (eligible) so a test can
# exercise one field at a time.
eligible_record <- function(record_id,
                            interview_date = format(Sys.Date() - 2, "%Y-%m-%d"),
                            phq8score = "20",
                            r01es_name = "Test Person") {
  list(
    record_id = record_id,
    interview_date = interview_date,
    r01es_name = r01es_name,
    r01es_phonenumber = "5120000000",
    r01es_commute = "1",
    r01es_austin = "1",
    r01es_phone = "1",
    r01es_computer = "1",
    r01es_bpd = "0",
    r01es_psychotherapy = "0",
    phq8score = phq8score,
    r01es_druguse = "0",
    medchng = "0",
    r01es_medstop = "0",
    r01es_medstart = "0"
  )
}

test_that("get_eligible_participants excludes records with missing interview_date", {
  local_mocked_bindings(
    get_redcap_report = function(report_id, ...) {
      list(
        eligible_record("1"),                       # valid, recent
        eligible_record("2", interview_date = NA),   # NA interview_date
        eligible_record("3", interview_date = "")    # unparseable interview_date
      )
    }
  )

  result <- get_eligible_participants()

  expect_s3_class(result, "data.frame")
  expect_true("first_name" %in% names(result))
  # NA/empty interview_date records are excluded, not crashed on
  expect_equal(nrow(result), 1)
  expect_equal(result$first_name, "Test")
  expect_equal(result$link_to_record_id, "https://redcap.prc.utexas.edu/redcap/redcap_v15.5.6/DataEntry/record_home.php?pid=3385&arm=1&id=1")
})

test_that("get_eligible_participants excludes records with empty/unparseable phq8score", {
  local_mocked_bindings(
    get_redcap_report = function(report_id, ...) {
      list(
        eligible_record("1", phq8score = "20", r01es_name = "Alice Alpha"),
        eligible_record("2", phq8score = "", r01es_name = "Bob Beta")
      )
    }
  )

  # Regression for #38: previously the empty phq8score leaked an NA row into
  # the eligibility filter (as.numeric("") is NA) and data.frame() then raised
  # "row names contain missing values", so the function returned the error
  # summary frame instead of participant data.
  result <- get_eligible_participants()

  # Participant data must be returned, not the error summary frame
  expect_true("first_name" %in% names(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$first_name, "Alice")
  expect_equal(result$phone_number, "5120000000")
  # No garbage all-NA row (first_name "Unknown", NA phone, id=NA link)
  expect_false(any(result$first_name == "Unknown"))
  expect_false(anyNA(result$phone_number))
  expect_false(any(grepl("id=NA$", result$link_to_record_id)))
})

test_that("get_eligible_participants returns summary frame when no records are eligible", {
  local_mocked_bindings(
    get_redcap_report = function(report_id, ...) {
      list(
        eligible_record("1", phq8score = "5")  # below the >= 17 cutoff
      )
    }
  )

  result <- get_eligible_participants()

  expect_s3_class(result, "data.frame")
  expect_true("Status" %in% names(result))
  expect_equal(result$Eligible_Count, 0)
})

test_that("get_eligible_participants returns summary frame when report has no rows", {
  local_mocked_bindings(
    get_redcap_report = function(report_id, ...) list()
  )

  result <- get_eligible_participants()

  expect_s3_class(result, "data.frame")
  expect_true("Status" %in% names(result))
  expect_equal(result$Total_Records, 0)
  expect_equal(result$Eligible_Count, 0)
})

test_that("get_weekly_screening_stats excludes records with empty phq8score", {
  # #38 corrected diagnosis: root cause was phq8score "" -> NA row leak, NOT
  # interview_date as the issue ACs originally hypothesized.
  local_mocked_bindings(
    get_redcap_report = function(report_id, ...) {
      list(
        eligible_record("1", phq8score = "20"),  # eligible (>= 17 cutoff)
        eligible_record("2", phq8score = "")     # empty phq8score
      )
    }
  )

  result <- get_weekly_screening_stats()

  # Structure consumed by inst/dashboard/index.qmd "Screening Summary
  # (Past 7 Days)" card (index.qmd:486-517): a one-row data.frame whose
  # eligible_count column feeds the "Eligible Participants" row.
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("total_screenings", "eligible_count", "hispanic_count"))
  expect_equal(nrow(result), 1)
  expect_equal(result$total_screenings, 2)
  # Empty-phq8score record must NOT count: pre-fix code leaked an all-NA row
  # (as.numeric("") is NA in the row index) that inflated eligible_count to 2.
  expect_equal(result$eligible_count, 1)
  expect_equal(result$hispanic_count, 0)
})

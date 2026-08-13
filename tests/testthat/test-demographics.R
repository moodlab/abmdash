# Behavior-lock tests for demographics.R (AC-3.4)
#
# Mock boundary: get_redcap_report (the network/API seam). Sys.Date() is pinned
# via the harness with_fixed_clock(); the exact age values are the TRIPWIRE
# that the date mock took effect — with the real date (2026-08+) the ages
# differ and the asserts fail loudly.

test_that("get_demographic_summary maps race/ethnicity/sex/age at frozen clock", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  fixture <- load_fixture("redcap", "redcap_demographics_sample.csv")

  # Environment (not list): assignment inside the mock closure must mutate the
  # outer binding, not shadow it with a local copy.
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    get_redcap_report = function(report_id, format = "json", ...) {
      seen$report_id <- report_id
      seen$format <- format
      fixture
    }
  )

  demo <- get_demographic_summary()

  # Default report id + CSV format reach the API boundary.
  expect_equal(seen$report_id, "13349")
  expect_equal(seen$format, "csv")

  expect_named(demo, c("record_id", "race", "ethnicity", "sex", "age"))
  expect_equal(nrow(demo), 6)
  expect_equal(demo$record_id, paste0("P00", 1:6))
  expect_equal(demo$race, c("White", "Black", "American Indian", "Asian",
                            "Hawaiian", "More than one race"))
  expect_equal(demo$ethnicity, c("Hispanic or Latino", "Not Hispanic or Latino",
                                 "Hispanic or Latino", "Not Hispanic or Latino",
                                 "Hispanic or Latino", "Not Hispanic or Latino"))
  expect_equal(demo$sex, c("Male", "Female", "Male", "Female", "Male", "Female"))
  # TRIPWIRE: age depends on Sys.Date() — with the real date (2026-08+) these
  # exact ages fail loudly instead of passing vacuous.
  expect_equal(demo$age, c(9, 20, 29, 40, 49, 60))
  expect_type(demo$age, "double")
})

test_that("get_demographic_summary filters to enrolled_ids", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  fixture <- load_fixture("redcap", "redcap_demographics_sample.csv")
  local_mocked_bindings(
    get_redcap_report = function(report_id, format = "json", ...) fixture
  )

  demo <- get_demographic_summary(enrolled_ids = c("P001", "P003"))
  expect_equal(demo$record_id, c("P001", "P003"))
  expect_equal(demo$race, c("White", "American Indian"))
})

test_that("get_demographic_summary errors on an empty report (locked behavior)", {
  # Lock the current behavior: a zero-row REDCap report makes the code under
  # test error (race_data$race <- "Unknown" cannot be added to a 0-row frame)
  # rather than silently returning an empty frame. The empty-case
  # columns/types contract is covered by summarize_demographics().
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  empty <- data.frame(
    record_id = character(0), redcap_event_name = character(0),
    raceid___1 = integer(0), raceid___2 = integer(0), raceid___3 = integer(0),
    raceid___4 = integer(0), raceid___5 = integer(0), raceid___6 = integer(0),
    raceid___7 = integer(0), demo_ethnicity = character(0),
    gender_identity = character(0), interview_age = character(0)
  )
  local_mocked_bindings(
    get_redcap_report = function(report_id, format = "json", ...) empty
  )
  expect_error(get_demographic_summary())
})

test_that("summarize_demographics returns exact summary strings", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  fixture <- load_fixture("redcap", "redcap_demographics_sample.csv")
  local_mocked_bindings(
    get_redcap_report = function(report_id, format = "json", ...) fixture
  )

  demo <- get_demographic_summary()
  sums <- summarize_demographics(demo)

  expect_named(sums, c("overall", "age_groups", "sex", "race", "ethnicity"))

  # Overall: 6 participants, ages 9/20/29/40/49/60.
  expect_equal(sums$overall$Metric, c("Total Participants", "Age (years)"))
  expect_equal(sums$overall$Value, c("6", "34.5 (SD=18.9), Range: 9-60"))

  # Race: one participant per category, sorted descending with alphabetical
  # tie-break (all counts equal -> level/name order preserved).
  expect_equal(sums$race$Category, c("American Indian", "Asian", "Black",
                                     "Hawaiian", "More than one race", "White"))
  expect_equal(sums$race$Count, rep(1, 6))
  expect_equal(sums$race$Percentage, rep("16.7%", 6))

  # Ethnicity: 3 Hispanic, 3 Not Hispanic (round(50.0, 1) -> "50%").
  expect_equal(sums$ethnicity$Category, c("Hispanic or Latino",
                                          "Not Hispanic or Latino"))
  expect_equal(sums$ethnicity$Count, c(3, 3))
  expect_equal(sums$ethnicity$Percentage, c("50%", "50%"))

  # Sex: 3 Female, 3 Male.
  expect_equal(sums$sex$Category, c("Female", "Male"))
  expect_equal(sums$sex$Count, c(3, 3))
  expect_equal(sums$sex$Percentage, c("50%", "50%"))

  # Age groups: one per bucket, in level order (9 -> <18, 20 -> 18-24, ...).
  expect_equal(sums$age_groups$Category, c("<18", "18-24", "25-34", "35-44",
                                           "45-54", "55+"))
  expect_equal(sums$age_groups$Count, rep(1, 6))
  expect_equal(sums$age_groups$Percentage, rep("16.7%", 6))
})

test_that("summarize_demographics handles empty input", {
  empty <- data.frame(record_id = character(0), race = character(0),
                      ethnicity = character(0), sex = character(0),
                      age = numeric(0))
  sums <- summarize_demographics(empty)
  expect_named(sums, "overall")
  expect_equal(sums$overall$Metric, "Total Participants")
  expect_equal(sums$overall$Value, "0")
})

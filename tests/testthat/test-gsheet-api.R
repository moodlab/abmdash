# Behavior-lock tests for the Google Sheets exports (gsheet_api.R) — AC-3.3
#
# Offline strategy: GOOGLE_SERVICE_ACCOUNT_JSON is UNSET (local_isolated_env())
# and the CORRECT token fn (get_google_sheets_access_token) is mocked. If the
# wrong token fn were mocked instead, the real get_google_sheets_access_token
# would run and fail fast on the missing env var (AC-3.3b). The Sheets API GET
# is served from synthetic fixtures via mock_google_fixture() inside
# httr2::with_mocked_responses() — no live network, no .Renviron.

test_that("all 7 google exports remain exported", {
  exports <- getNamespaceExports("abmdash")
  expected <- c(
    "read_google_sheet", "print_sheet_head", "check_recent_responses",
    "check_participant_issues",
    "get_calendar_events", "list_calendars", "get_combined_calendar_events"
  )
  missing <- setdiff(expected, exports)
  expect_true(
    length(missing) == 0,
    label = paste("google exports missing from NAMESPACE:", paste(missing, collapse = ", "))
  )
})

test_that("read_google_sheet returns exact cells from the recorded sheet", {
  local_isolated_env()
  local_mocked_bindings(get_google_sheets_access_token = function() "fake-token")
  url <- "https://docs.google.com/spreadsheets/d/TEST_SHEET_123/edit"

  df <- httr2::with_mocked_responses(
    mock_google_fixture(testthat::test_path("fixtures", "gsheet")),
    read_google_sheet(url)
  )

  expect_equal(colnames(df), c("Timestamp", "Participant", "Issue"))
  expect_equal(nrow(df), 4)
  expect_equal(df$Participant, c("P004", "P005", "P003", "P001"))
  expect_equal(df$Issue, c("App crash", "Sync delay", "Old issue", "Older"))
  expect_equal(df$Timestamp[1], "04/13/2026 09:15:00")
})

test_that("read_google_sheet pads ragged rows with NA", {
  local_isolated_env()
  local_mocked_bindings(get_google_sheets_access_token = function() "fake-token")
  url <- "https://docs.google.com/spreadsheets/d/TEST_SHEET_123/edit"

  df <- httr2::with_mocked_responses(
    mock_google_fixture(
      testthat::test_path("fixtures", "gsheet"),
      mapping = c("values/Sheet1%21A1%3AC3" = "sheet-ragged.json")
    ),
    read_google_sheet(url, range = "Sheet1!A1:C3")
  )

  expect_equal(nrow(df), 2)
  expect_equal(df$Participant, c("P004", "P005"))
  expect_true(is.na(df$Issue[1]))
  expect_equal(df$Issue[2], "Sync delay")
})

test_that("read_google_sheet warns and returns empty frame when sheet has no data", {
  local_isolated_env()
  local_mocked_bindings(get_google_sheets_access_token = function() "fake-token")
  url <- "https://docs.google.com/spreadsheets/d/TEST_SHEET_123/edit"

  res <- NULL
  expect_warning(
    res <- httr2::with_mocked_responses(
      mock_google_fixture(
        testthat::test_path("fixtures", "gsheet"),
        mapping = c("values/Sheet1$" = "sheet-empty.json")
      ),
      read_google_sheet(url)
    ),
    "No data found in the sheet"
  )
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 0)
})

test_that("print_sheet_head prints the head and returns the full frame invisibly", {
  local_isolated_env()
  local_mocked_bindings(get_google_sheets_access_token = function() "fake-token")
  url <- "https://docs.google.com/spreadsheets/d/TEST_SHEET_123/edit"

  out <- httr2::with_mocked_responses(
    mock_google_fixture(testthat::test_path("fixtures", "gsheet")),
    capture.output(vis <- withVisible(print_sheet_head(url, n = 2)))
  )

  expect_false(vis$visible)
  expect_equal(nrow(vis$value), 4)
  expect_true(grepl("First 2 rows of the sheet", out[1], fixed = TRUE))
  expect_true(grepl("Dimensions: 4 rows x 3 columns", out[length(out)], fixed = TRUE))
})

test_that("check_recent_responses returns exact recent counts with frozen clock", {
  local_isolated_env()
  local_mocked_bindings(get_google_sheets_access_token = function() "fake-token")
  url <- "https://docs.google.com/spreadsheets/d/TEST_SHEET_123/edit"

  with_fixed_clock("2026-04-15 00:00:00 UTC")
  result <- httr2::with_mocked_responses(
    mock_google_fixture(testthat::test_path("fixtures", "gsheet")),
    suppressMessages(check_recent_responses(url, days_back = 14))
  )

  expect_true(result$has_recent)
  expect_equal(result$recent_count, 2)
  expect_equal(result$recent_data$Participant, c("P004", "P005"))
  expect_equal(result$recent_data$Issue, c("App crash", "Sync delay"))
  expect_equal(nrow(result$all_data), 4)
  expect_equal(result$cutoff_date, as.POSIXct("2026-04-01 00:00:00", tz = "UTC"))
})

test_that("check_participant_issues forwards the hardcoded public sheet args", {
  local_isolated_env()
  captured <- NULL
  local_mocked_bindings(
    check_recent_responses = function(...) {
      captured <<- list(...)
      list(
        has_recent = TRUE,
        recent_count = 1,
        recent_data = data.frame(Participant = "P900"),
        all_data = data.frame(Participant = "P900")
      )
    }
  )

  out <- capture.output(vis <- withVisible(check_participant_issues(days_back = 7)))

  expect_false(vis$visible)
  expect_equal(vis$value$recent_count, 1)
  # The sheet ID is a PUBLIC spreadsheet URL (not a secret) — locked as-is.
  expect_true(
    grepl("/d/11FAAY4cUvqpW7QN7k-mcpOCx3WfrcA6GNeTXgBT1St8/", captured$sheet_url, fixed = TRUE)
  )
  expect_equal(captured$days_back, 7)
  expect_equal(captured$timestamp_col, 1)
  expect_equal(captured$sheet_name, "Form Responses 1")
  expect_true(grepl("Checking for participant issues", out[1], fixed = TRUE))
})

# Integration tests for get_trad_compliance_summary()

test_that("get_trad_compliance_summary returns expected columns with fixture data", {
  fixture_data <- read.csv(
    test_path("fixtures", "trad_csv_sample.csv"),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    abs_login = function(...) "mock_session",
    download_abs_csv = function(...) fixture_data
  )

  result <- get_trad_compliance_summary()

  expect_s3_class(result, "data.frame")
  expected_cols <- c(
    "id", "current_week", "weeks_from_start",
    "total_sessions", "expected_sessions",
    "sessions_behind", "start_date", "status"
  )
  expect_named(result, expected_cols)
})

test_that("test IDs are excluded from results", {
  fixture_data <- read.csv(
    test_path("fixtures", "trad_csv_sample.csv"),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    abs_login = function(...) "mock_session",
    download_abs_csv = function(...) fixture_data
  )

  result <- get_trad_compliance_summary()

  expect_false(any(grepl("test", result$id, ignore.case = TRUE)))
})

test_that("participants beyond active window are excluded", {
  fixture_data <- read.csv(
    test_path("fixtures", "trad_csv_sample.csv"),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    abs_login = function(...) "mock_session",
    download_abs_csv = function(...) fixture_data
  )

  # P003 started 2025-01-01 — well beyond 5 weeks ago
  result <- get_trad_compliance_summary()

  expect_false("P003" %in% result$id)
})

test_that("sessions_behind is correctly calculated", {
  # Create controlled data: participant started exactly 1 week ago with 2 sessions
  one_week_ago <- format(Sys.time() - (7 * 24 * 60 * 60), "%Y-%m-%d %H:%M:%S")
  five_days_ago <- format(Sys.time() - (5 * 24 * 60 * 60), "%Y-%m-%d %H:%M:%S")

  fixture_data <- data.frame(
    subject_id = c("P_CALC", "P_CALC"),
    session = c(1, 2),
    start_time = c(one_week_ago, five_days_ago),
    score = c(74.0, 80.0),
    finished = c(1, 1),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    abs_login = function(...) "mock_session",
    download_abs_csv = function(...) fixture_data
  )

  result <- get_trad_compliance_summary()

  expect_equal(nrow(result), 1)
  expect_equal(result$id, "P_CALC")
  expect_equal(result$total_sessions, 2)
  # ~1 week in, expected ~4 sessions, so behind by ~2
  expect_true(result$sessions_behind > 0)
})

test_that("returns empty data frame when no active participants", {
  # All participants started long ago
  fixture_data <- data.frame(
    subject_id = c("OLD1", "OLD1"),
    session = c(1, 2),
    start_time = c("2020-01-01 10:00:00", "2020-01-03 10:00:00"),
    score = c(74.0, 80.0),
    finished = c(1, 1),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    abs_login = function(...) "mock_session",
    download_abs_csv = function(...) fixture_data
  )

  result <- get_trad_compliance_summary()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("errors propagate with meaningful messages when login fails", {
  local_mocked_bindings(
    abs_login = function(...) stop("Login failed: Invalid credentials"),
    download_abs_csv = function(...) data.frame()
  )

  expect_error(
    get_trad_compliance_summary(),
    "login"
  )
})

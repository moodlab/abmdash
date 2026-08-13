# Behavior-lock tests for compliance_tracking.R (AC-3.4)
#
# Mock boundary: read_google_sheet (the network/API seam — never the export).
# Sys.time() is pinned via the harness with_fixed_clock(); the exact derived
# values (time_from_start, expect_cnt, late) are the TRIPWIRE that the clock
# mock took effect — with the real clock (2026-08+) these asserts fail loudly
# instead of passing vacuous.

test_that("get_compliance_report returns exact compliance values at frozen clock", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  fixture <- load_fixture("gsheet", "gsheet_gameplay_sample.csv", check.names = FALSE)
  local_mocked_bindings(
    read_google_sheet = function(sheet_url, sheet_name = NULL, range = NULL) fixture
  )

  result <- suppressMessages(get_compliance_report("mock_sheet"))
  comp <- result$compliance

  expect_named(comp, c("id", "week", "start", "end", "sess_cnt",
                       "time_from_start", "expect_cnt", "late"))
  expect_equal(nrow(comp), 5)

  p1w1 <- comp[comp$id == "P001" & comp$week == 1, ]
  p1w2 <- comp[comp$id == "P001" & comp$week == 2, ]
  p1w3 <- comp[comp$id == "P001" & comp$week == 3, ]
  p2w1 <- comp[comp$id == "P002" & comp$week == 1, ]
  p2w2 <- comp[comp$id == "P002" & comp$week == 2, ]

  # Exact derived values (tripwire: clock mock must have propagated).
  expect_equal(p1w1$sess_cnt, 4)
  expect_equal(p1w1$time_from_start, 2.73)
  expect_equal(p1w1$expect_cnt, 10.90)
  expect_true(p1w1$late)
  expect_equal(p1w2$sess_cnt, 1)
  expect_equal(p1w2$time_from_start, 1.73)
  expect_equal(p1w2$expect_cnt, 6.90)
  expect_true(p1w2$late)
  expect_equal(p1w3$sess_cnt, 0)
  expect_equal(p1w3$time_from_start, 0.73)
  expect_equal(p1w3$expect_cnt, 2.90)
  expect_true(p1w3$late)
  expect_equal(p2w1$sess_cnt, 2)
  expect_equal(p2w1$time_from_start, 1.44)
  expect_equal(p2w1$expect_cnt, 5.76)
  expect_true(p2w1$late)
  expect_equal(p2w2$sess_cnt, 1)
  expect_equal(p2w2$time_from_start, 0.44)
  expect_equal(p2w2$expect_cnt, 1.76)
  expect_false(p2w2$late)

  # Locked types: dates are POSIXct, derived values numeric/logical.
  expect_s3_class(p1w1$start, "POSIXct")
  expect_s3_class(p1w1$end, "POSIXct")
  expect_type(p1w1$time_from_start, "double")
  expect_type(p1w1$expect_cnt, "double")
  expect_type(p1w1$late, "logical")
})

test_that("get_compliance_report finds 178-179 turn short sessions", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  fixture <- load_fixture("gsheet", "gsheet_gameplay_sample.csv", check.names = FALSE)
  local_mocked_bindings(
    read_google_sheet = function(sheet_url, sheet_name = NULL, range = NULL) fixture
  )

  result <- suppressMessages(get_compliance_report("mock_sheet"))
  short <- result$short_sessions

  expect_named(short, c("id", "date", "event", "week", "session", "turns"))
  expect_equal(nrow(short), 2)
  expect_equal(short$id, c("P001", "P002"))
  expect_equal(short$turns, c(179, 178))
})

test_that("get_compliance_report excludes test IDs and excluded IDs", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  fixture <- load_fixture("gsheet", "gsheet_gameplay_sample.csv", check.names = FALSE)
  local_mocked_bindings(
    read_google_sheet = function(sheet_url, sheet_name = NULL, range = NULL) fixture
  )

  result <- suppressMessages(get_compliance_report("mock_sheet"))
  ids <- unique(result$compliance$id)
  expect_equal(sort(ids), c("P001", "P002"))
  expect_false("test_acct" %in% result$compliance$id)
  expect_false("123456789" %in% result$compliance$id)
})

test_that("get_compliance_report keeps locked columns and types when compliance is empty", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  # All sessions start ~11 weeks before the frozen clock -> outside the
  # (0, 5) week window, so compliance is 0 rows but the columns/types must be
  # locked (non-vacuous: the fixture has real gameplay rows).
  old <- data.frame(
    `Referral ID` = c("OLD1", "OLD1", "OLD2"),
    `Date of Event UTC` = c("01/05/2026 10:00:00", "01/06/2026 10:00:00",
                            "01/07/2026 10:00:00"),
    `Event Type` = c("Gameplay", "Gameplay", "Gameplay"),
    `Week #` = c("1", "1", "1"),
    `Session #` = c("1", "2", "1"),
    `Turns Completed` = c("180", "180", "180"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    read_google_sheet = function(sheet_url, sheet_name = NULL, range = NULL) old
  )

  result <- suppressMessages(get_compliance_report("mock_sheet"))
  comp <- result$compliance
  expect_equal(nrow(comp), 0)
  expect_named(comp, c("id", "week", "start", "end", "sess_cnt",
                       "time_from_start", "expect_cnt", "late"))
  expect_type(comp$id, "character")
  expect_type(comp$week, "integer")
  expect_s3_class(comp$start, "POSIXct")
  expect_type(comp$time_from_start, "double")
  expect_type(comp$late, "logical")
})

test_that("get_late_participants returns only rows flagged late", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  fixture <- load_fixture("gsheet", "gsheet_gameplay_sample.csv", check.names = FALSE)
  local_mocked_bindings(
    read_google_sheet = function(sheet_url, sheet_name = NULL, range = NULL) fixture
  )

  late <- suppressMessages(get_late_participants("mock_sheet"))

  # P001 W1-W3 + P002 W1 are late; P002 W2 is not.
  expect_equal(nrow(late), 4)
  expect_true(all(late$late))
  expect_equal(sort(unique(late$id)), c("P001", "P002"))
  expect_true(all(c("id", "week", "sess_cnt", "expect_cnt",
                    "time_from_start") %in% names(late)))
})

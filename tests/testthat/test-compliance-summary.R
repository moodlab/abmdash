# Behavior-lock tests for compliance_summary.R (AC-3.4)
#
# Mock boundary: get_compliance_report — the only dependency of
# get_participant_summary(). Exact derived values lock the summary math
# (expected_total cap at 16, current_week cap at 4, start_date back-derivation,
# sort by sessions_behind).

# Crafted compliance report (frozen-clock independent: time_from_start is
# supplied as data, matching what get_compliance_report would return).
compliance_fixture <- function() {
  start1 <- as.POSIXct(c("2026-01-11 10:00:00", "2026-01-18 10:00:00",
                         "2026-01-25 10:00:00", "2026-02-01 10:00:00"),
                       tz = "UTC")
  start2 <- as.POSIXct(c("2026-01-15 10:00:00", "2026-01-22 10:00:00"),
                       tz = "UTC")
  data.frame(
    id = c(rep("P001", 4), rep("P002", 2)),
    week = c(1:4, 1:2),
    start = c(start1, start2),
    end = c(start1, start2) + 7 * 24 * 60 * 60,
    sess_cnt = c(4, 4, 4, 4, 1, 1),
    time_from_start = c(0.73, 1.73, 2.73, 3.73, 0.50, 1.50),
    expect_cnt = c(4.16, 9.89, 15.61, 21.33, 2.86, 8.57),
    late = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("get_participant_summary returns exact derived values", {
  local_isolated_env()
  comp <- compliance_fixture()
  local_mocked_bindings(
    get_compliance_report = function(sheet_url, ...) {
      list(compliance = comp, short_sessions = comp[0, ])
    }
  )

  summary <- suppressMessages(get_participant_summary("mock_sheet"))

  expect_named(summary, c("id", "current_week", "weeks_from_start",
                          "total_sessions_completed", "expected_sessions",
                          "sessions_behind", "start_date"))
  expect_equal(nrow(summary), 2)

  p1 <- summary[summary$id == "P001", ]
  p2 <- summary[summary$id == "P002", ]

  # P001: 4 weeks in, 16 sessions, weeks_from_start 3.73 -> expected
  # round(3.73 * 4, 2) = 14.92 (below the 16 cap) -> 1.08 AHEAD.
  expect_equal(p1$current_week, 4)
  expect_equal(p1$weeks_from_start, 3.73)
  expect_equal(p1$total_sessions_completed, 16)
  expect_equal(p1$expected_sessions, 14.92)
  expect_equal(p1$sessions_behind, -1.08)
  expect_equal(p1$start_date, "2026-01-11")

  # P002: 2 weeks in, 2 sessions, weeks_from_start 1.50 -> expected 6 -> 4 behind.
  expect_equal(p2$current_week, 2)
  expect_equal(p2$weeks_from_start, 1.50)
  expect_equal(p2$total_sessions_completed, 2)
  expect_equal(p2$expected_sessions, 6.00)
  expect_equal(p2$sessions_behind, 4.00)
  expect_equal(p2$start_date, "2026-01-15")
})

test_that("get_participant_summary sorts most-behind first", {
  local_isolated_env()
  comp <- compliance_fixture()
  local_mocked_bindings(
    get_compliance_report = function(sheet_url, ...) {
      list(compliance = comp, short_sessions = comp[0, ])
    }
  )

  summary <- suppressMessages(get_participant_summary("mock_sheet"))
  expect_equal(summary$id, c("P002", "P001"))
  expect_equal(summary$sessions_behind, c(4.00, -1.08))
})

test_that("get_behind_participants returns only participants behind", {
  local_isolated_env()
  comp <- compliance_fixture()
  local_mocked_bindings(
    get_compliance_report = function(sheet_url, ...) {
      list(compliance = comp, short_sessions = comp[0, ])
    }
  )

  behind <- suppressMessages(get_behind_participants("mock_sheet"))
  expect_equal(nrow(behind), 1)
  expect_equal(behind$id, "P002")
  expect_equal(behind$sessions_behind, 4.00)
})

test_that("get_participant_summary errors on empty compliance (locked behavior)", {
  # Lock the current behavior: an empty compliance report errors rather than
  # silently returning an empty summary (which would be a vacuous pass).
  local_isolated_env()
  comp <- data.frame(
    id = character(0), week = numeric(0),
    start = as.POSIXct(character(0), tz = "UTC"),
    end = as.POSIXct(character(0), tz = "UTC"),
    sess_cnt = numeric(0), time_from_start = numeric(0),
    expect_cnt = numeric(0), late = logical(0)
  )
  local_mocked_bindings(
    get_compliance_report = function(sheet_url, ...) {
      list(compliance = comp, short_sessions = comp[0, ])
    }
  )
  expect_error(get_participant_summary("mock_sheet"))
})

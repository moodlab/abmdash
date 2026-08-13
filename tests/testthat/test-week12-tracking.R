# Behavior-lock tests for week12_tracking.R (AC-3.4)
#
# Mock boundary: get_redcap_logs (the network/API seam). Sys.time() and
# Sys.Date() are pinned via the harness with_fixed_clock(). The begin_time
# argument captured by the mock is the TRIPWIRE that the clock mock took
# effect; the exact days_until_due / status values then lock the date math.

test_that("get_upcoming_followups returns exact due dates and statuses", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")

  logs <- data.frame(
    timestamp = c(
      "2026-01-15 10:00",  # P001 W4
      "2025-10-08 10:00",  # P002 W4
      "2026-01-23 10:00",  # P003 W4 (would be Due Today — but withdrawn)
      "2026-01-24 10:00",  # P003 withdrawal log
      "2026-01-26 10:00",  # P004 W4
      "2026-01-23 10:00",  # P005 W4
      "2025-12-27 10:00",  # P006 W4
      "2026-01-24 10:00",  # P007 W4 (lowercase variant match)
      "2026-01-15 11:00"   # empty-record row -> filtered out
    ),
    username = rep("u1", 9),
    action = c(
      "Completed W4 Acute [IN-PERSON]",
      "Completed W4 Acute [IN-PERSON]",
      "Completed W4 Acute [IN-PERSON]",
      "Recorded Reason for Withdrawal",
      "Completed W4 Acute [IN-PERSON]",
      "Completed W4 Acute [IN-PERSON]",
      "Completed W4 Acute [IN-PERSON]",
      "Other action",
      "Completed W4 Acute [IN-PERSON]"
    ),
    details = c(rep("", 7), "Completed w4 acute [in-person] in lab", ""),
    record = c("P001", "P002", "P003", "P003", "P004", "P005", "P006",
               "P007", ""),
    stringsAsFactors = FALSE
  )

  # Environment (not list): assignment inside the mock closure must mutate the
  # outer binding, not shadow it with a local copy.
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    get_redcap_logs = function(begin_time = NULL, ...) {
      captured$begin_time <- begin_time
      logs
    }
  )

  result <- get_upcoming_followups()

  # TRIPWIRE: begin_time derives from the frozen Sys.time() — with the real
  # clock this differs and the assert fails loudly.
  expect_equal(captured$begin_time,
               format(Sys.time() - (200 * 24 * 60 * 60), "%Y-%m-%d %H:%M:%S"))

  expect_named(result, c("record_id", "follow_up_type", "w4_completion_date",
                         "due_date", "days_until_due", "status"))
  expect_equal(result$record_id, c("P001", "P005", "P006", "P007", "P004",
                                   "P002"))
  expect_equal(result$follow_up_type, c("Week 12", "Week 12", "Week 16",
                                        "Week 12", "Week 12", "Week 28"))
  expect_equal(result$w4_completion_date, c("2026-01-15", "2026-01-23",
                                            "2025-12-27", "2026-01-24",
                                            "2026-01-26", "2025-10-08"))
  expect_equal(result$due_date, c("2026-03-12", "2026-03-20", "2026-03-21",
                                  "2026-03-21", "2026-03-23", "2026-03-25"))
  expect_equal(result$days_until_due, c(-8, 0, 1, 1, 3, 5))
  # Note: the code hardcodes the plural form — "Due in 1 days" is the locked
  # behavior (paste("Due in", n, "days")).
  expect_equal(result$status, c("Overdue by 8 days", "Due Today",
                                "Due in 1 days", "Due in 1 days",
                                "Due in 3 days", "Due in 5 days"))

  # Withdrawal: P003's W12 would be Due Today (W4 2026-01-23) but is skipped.
  expect_false("P003" %in% result$record_id)
  # Empty-record log contributed nothing.
  expect_false("" %in% result$record_id)

  # Locked types.
  expect_type(result$days_until_due, "double")
  expect_type(result$status, "character")
  expect_type(result$w4_completion_date, "character")
})

test_that("get_upcoming_followups suppresses completed follow-ups", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  logs <- data.frame(
    timestamp = c("2026-01-09 10:00", "2026-03-10 10:00"),
    username = c("u1", "u1"),
    action = c("Completed W4 Acute [IN-PERSON]", "Completed W12 Booster REMOTE"),
    details = c("", ""),
    record = c("P008", "P008"),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(get_redcap_logs = function(begin_time = NULL, ...) logs)

  result <- get_upcoming_followups()

  # W12 due 2026-03-06 (-14, within range) is suppressed by the W12-completed
  # log; W16 due 2026-04-03 (+14, within range) still shows.
  expect_equal(nrow(result), 1)
  expect_equal(result$record_id, "P008")
  expect_equal(result$follow_up_type, "Week 16")
  expect_equal(result$days_until_due, 14)
  expect_equal(result$status, "Due in 14 days")
})

test_that("get_upcoming_followups handles list-of-list logs (the %||% branch)", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  logs <- list(
    list(timestamp = "2026-01-15 10:00", username = "u1",
         action = "Completed W4 Acute [IN-PERSON]", details = "", record = "P010"),
    list(timestamp = "2026-01-15 10:00", username = "u1",
         action = "Some other action", details = "x", record = "P011")
  )
  local_mocked_bindings(get_redcap_logs = function(begin_time = NULL, ...) logs)

  result <- get_upcoming_followups()
  expect_equal(nrow(result), 1)
  expect_equal(result$record_id, "P010")
  expect_equal(result$days_until_due, -8)
})

test_that("get_upcoming_followups returns locked empty frame when no logs", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  local_mocked_bindings(get_redcap_logs = function(begin_time = NULL, ...) NULL)

  result <- get_upcoming_followups()
  expect_equal(nrow(result), 0)
  expect_named(result, c("record_id", "follow_up_type", "w4_completion_date",
                         "due_date", "days_until_due", "status"))
  expect_type(result$record_id, "character")
  expect_type(result$days_until_due, "double")
  expect_type(result$status, "character")
})

test_that("get_upcoming_followups returns locked empty frame for 0-row logs", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  logs <- data.frame(timestamp = character(0), username = character(0),
                     action = character(0), details = character(0),
                     record = character(0))
  local_mocked_bindings(get_redcap_logs = function(begin_time = NULL, ...) logs)

  result <- get_upcoming_followups()
  expect_equal(nrow(result), 0)
  expect_named(result, c("record_id", "follow_up_type", "w4_completion_date",
                         "due_date", "days_until_due", "status"))
  expect_type(result$days_until_due, "double")
})

test_that("get_upcoming_followups returns locked empty frame when no W4 matches", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  logs <- data.frame(
    timestamp = "2026-02-01 10:00", username = "u1",
    action = "Completed something else", details = "", record = "P009",
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(get_redcap_logs = function(begin_time = NULL, ...) logs)

  result <- get_upcoming_followups()
  expect_equal(nrow(result), 0)
  expect_named(result, c("record_id", "follow_up_type", "w4_completion_date",
                         "due_date", "days_until_due", "status"))
  expect_type(result$days_until_due, "double")
})

test_that("get_upcoming_followups returns locked empty frame when every window is out of range", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  logs <- data.frame(timestamp = "2026-02-09 10:00", username = "u1",
    action = "Completed W4 Acute [IN-PERSON]", details = "", record = "P020",
    stringsAsFactors = FALSE)                    # W12 due in 17d > days_ahead 14
  local_mocked_bindings(get_redcap_logs = function(begin_time = NULL, ...) logs)
  result <- get_upcoming_followups()
  expect_equal(nrow(result), 0)
  expect_named(result, c("record_id", "follow_up_type", "w4_completion_date",
                         "due_date", "days_until_due", "status"))
  expect_type(result$days_until_due, "double")
})

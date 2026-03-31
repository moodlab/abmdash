# BDD tests for get_combined_calendar_events()

test_that("get_combined_calendar_events merges events from multiple calendars", {
  call_log <- list()

  local_mocked_bindings(
    get_calendar_events = function(calendar_id, ...) {
      call_log[[length(call_log) + 1]] <<- calendar_id
      if (calendar_id == "cal_a") {
        list(items = list(
          list(summary = "Event A1", start = list(dateTime = "2026-04-01T10:00:00Z")),
          list(summary = "Event A2", start = list(dateTime = "2026-04-01T11:00:00Z"))
        ))
      } else if (calendar_id == "cal_b") {
        list(items = list(
          list(summary = "Event B1", start = list(dateTime = "2026-04-01T14:00:00Z"))
        ))
      }
    }
  )

  result <- get_combined_calendar_events(
    calendar_ids = c("cal_a", "cal_b"),
    time_min = "2026-04-01T00:00:00Z",
    time_max = "2026-04-07T23:59:59Z"
  )

  expect_equal(length(result$items), 3)
  summaries <- vapply(result$items, function(e) e$summary, character(1))
  expect_true("Event A1" %in% summaries)
  expect_true("Event B1" %in% summaries)
})

test_that("handles one calendar being empty", {
  local_mocked_bindings(
    get_calendar_events = function(calendar_id, ...) {
      if (calendar_id == "cal_full") {
        list(items = list(
          list(summary = "Event 1", start = list(dateTime = "2026-04-01T10:00:00Z"))
        ))
      } else {
        list(items = list())
      }
    }
  )

  result <- get_combined_calendar_events(
    calendar_ids = c("cal_full", "cal_empty"),
    time_min = "2026-04-01T00:00:00Z",
    time_max = "2026-04-07T23:59:59Z"
  )

  expect_equal(length(result$items), 1)
  expect_equal(result$items[[1]]$summary, "Event 1")
})

test_that("handles one calendar erroring without losing the other", {
  local_mocked_bindings(
    get_calendar_events = function(calendar_id, ...) {
      if (calendar_id == "cal_ok") {
        list(items = list(
          list(summary = "Good Event", start = list(dateTime = "2026-04-01T10:00:00Z"))
        ))
      } else {
        stop("API error for this calendar")
      }
    }
  )

  result <- get_combined_calendar_events(
    calendar_ids = c("cal_ok", "cal_broken"),
    time_min = "2026-04-01T00:00:00Z",
    time_max = "2026-04-07T23:59:59Z"
  )

  expect_equal(length(result$items), 1)
  expect_equal(result$items[[1]]$summary, "Good Event")
})

test_that("returns empty items when all calendars are empty", {
  local_mocked_bindings(
    get_calendar_events = function(calendar_id, ...) {
      list(items = list())
    }
  )

  result <- get_combined_calendar_events(
    calendar_ids = c("cal_a", "cal_b"),
    time_min = "2026-04-01T00:00:00Z",
    time_max = "2026-04-07T23:59:59Z"
  )

  expect_equal(length(result$items), 0)
})

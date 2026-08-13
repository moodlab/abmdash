# BDD tests for get_combined_calendar_events() — AC-3.3
#
# The merge behavior is locked AS-IS: events keep calendar_ids iteration order,
# then fixture order within each calendar — NO sorting, NO deduplication
# (gcal_api.R:243-261). The error-swallow message is observable behavior and is
# locked too. Do NOT "improve" the merge here — that is a separate refactor AC.

test_that("get_combined_calendar_events merges events in calendar order (no sort)", {
  call_log <- list()

  local_mocked_bindings(
    get_calendar_events = function(calendar_id, ...) {
      call_log[[length(call_log) + 1]] <<- calendar_id
      if (calendar_id == "cal_a") {
        # Deliberately UNSORTED (A2 11:00 before A1 10:00): if the merge ever
        # starts sorting, this fixture flips the exact-order assert below.
        list(items = list(
          list(id = "evt-a2", summary = "A2", start = list(dateTime = "2026-04-01T11:00:00Z")),
          list(id = "evt-a1", summary = "A1", start = list(dateTime = "2026-04-01T10:00:00Z"))
        ))
      } else {
        list(items = list(
          list(id = "evt-b1", summary = "B1", start = list(dateTime = "2026-04-01T14:00:00Z"))
        ))
      }
    }
  )

  result <- get_combined_calendar_events(
    calendar_ids = c("cal_a", "cal_b"),
    time_min = "2026-04-01T00:00:00Z",
    time_max = "2026-04-07T23:59:59Z"
  )

  # Calendar fetch order AND event order are exact — no sort, no dedup.
  # Fixture is UNSORTED (A2 before A1): if a sort() ever lands, the merged
  # order flips to A1,A2 and this assert fails.
  expect_equal(unlist(call_log), c("cal_a", "cal_b"))
  expect_equal(length(result$items), 3)
  expect_equal(vapply(result$items, function(e) e$summary, character(1)), c("A2", "A1", "B1"))
  expect_equal(vapply(result$items, function(e) e$id, character(1)), c("evt-a2", "evt-a1", "evt-b1"))
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

test_that("reports and continues when one calendar errors", {
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

  result <- NULL
  expect_message(
    result <- get_combined_calendar_events(
      calendar_ids = c("cal_ok", "cal_broken"),
      time_min = "2026-04-01T00:00:00Z",
      time_max = "2026-04-07T23:59:59Z"
    ),
    "failed to fetch calendar cal_broken"
  )

  expect_equal(length(result$items), 1)
  expect_equal(result$items[[1]]$summary, "Good Event")
})

test_that("same event id in two calendars is NOT deduplicated", {
  local_mocked_bindings(
    get_calendar_events = function(calendar_id, ...) {
      list(items = list(
        list(id = "evt-shared", summary = "Shared", start = list(dateTime = "2026-04-01T10:00:00Z"))
      ))
    }
  )

  result <- get_combined_calendar_events(
    calendar_ids = c("cal_a", "cal_b"),
    time_min = "2026-04-01T00:00:00Z",
    time_max = "2026-04-07T23:59:59Z"
  )

  expect_equal(length(result$items), 2)
  expect_equal(vapply(result$items, function(e) e$id, character(1)), c("evt-shared", "evt-shared"))
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

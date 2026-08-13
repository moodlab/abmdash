# Behavior-lock tests for the Google Calendar exports (gcal_api.R) — AC-3.3
#
# gcal_api.R parses GOOGLE_SERVICE_ACCOUNT_JSON BEFORE calling the token fn
# (gcal_api.R:40-56), so these tests set a THROWAWAY dummy JSON (no private
# key — if the real get_google_access_token ever runs, openssl::read_key()
# fails on the missing key) and mock the CORRECT token fn
# get_google_access_token (AC-3.3b). Calendar GETs are served from synthetic
# fixtures via mock_google_fixture() inside httr2::with_mocked_responses().

dummy_sa_json <- '{"client_email":"dummy@example.com"}'

test_that("get_calendar_events returns exact event structure from fixture", {
  withr::local_envvar(GOOGLE_SERVICE_ACCOUNT_JSON = dummy_sa_json)
  local_mocked_bindings(get_google_access_token = function(service_account) "fake-token")

  result <- httr2::with_mocked_responses(
    mock_google_fixture(testthat::test_path("fixtures", "gcal")),
    get_calendar_events(
      calendar_id = "primary",
      time_min = "2026-04-01T00:00:00Z",
      time_max = "2026-04-07T23:59:59Z",
      max_results = 5
    )
  )

  expect_equal(result$kind, "calendar#events")
  expect_equal(length(result$items), 2)
  expect_equal(vapply(result$items, function(e) e$id, character(1)), c("evt-1", "evt-2"))
  expect_equal(vapply(result$items, function(e) e$summary, character(1)), c("E1", "E2"))
  expect_equal(result$items[[1]]$start$dateTime, "2026-04-01T10:00:00Z")
})

test_that("list_calendars returns exact calendar list structure", {
  withr::local_envvar(GOOGLE_SERVICE_ACCOUNT_JSON = dummy_sa_json)
  local_mocked_bindings(get_google_access_token = function(service_account) "fake-token")

  result <- httr2::with_mocked_responses(
    mock_google_fixture(testthat::test_path("fixtures", "gcal")),
    list_calendars()
  )

  expect_equal(result$kind, "calendar#calendarList")
  expect_equal(length(result$items), 2)
  expect_equal(result$items[[1]]$id, "cal-a@group.calendar.google.com")
  expect_equal(result$items[[1]]$summary, "Calendar A")
  expect_equal(result$items[[2]]$id, "cal-b@group.calendar.google.com")
})

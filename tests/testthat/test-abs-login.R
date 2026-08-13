# Behavior-lock tests for the ABS admin portal module (AC-3.4)
#
# Mock boundary (per the AC-3.4 mock-boundary contract):
#   * Pure parsers (extract_livewire_snapshot / extract_csrf_token) are tested
#     DIRECTLY via abmdash:::, against recorded HTML covering all 4 CSRF
#     fallback patterns — the FAKE-LOCK defense. test-trad-compliance.R already
#     mocks abs_login()/download_abs_csv() at the consumer level; repeating that
#     here would mean Livewire snapshot extraction, CSRF fallback, redirect
#     detection, base64 decode and list-tests selection never execute under
#     test.
#   * Exports (abs_login, download_abs_csv, test_abs_connection,
#     verify_abs_login) mock httr2::req_perform ONLY — never the exports.
#   * preview_abs_csv is a thin wrapper over download_abs_csv; it mocks
#     download_abs_csv (explicitly allowed by the AC).
#
# Fixtures are hand-crafted SYNTHETIC HTML/JSON matching the real ABS login
# page structure (wire:snapshot="..." attr with &quot;-encoded JSON,
# data-csrf / meta csrf-token / hidden _token CSRF patterns, the Livewire
# {effects:{redirect}} auth response, the list-tests page, and a
# base64-encoded CSV download response). All participant data is synthetic
# (P0xx / P1xx IDs). No live site is ever contacted.

# Local test helpers ------------------------------------------------------

# Read a fixture text file from tests/testthat/fixtures/abs/.
read_fixture_text <- function(file) {
  paste(readLines(testthat::test_path("fixtures", "abs", file), warn = FALSE),
        collapse = "\n")
}

# A minimal authenticated session: httr2::req_url_path() validates that the
# session argument is a real httr2 request, so exports cannot be called with
# plain strings.
mock_session <- function() httr2::request("https://abs.la.utexas.edu")

# Build a minimal httr2 response object for mocked req_perform calls.
http_resp <- function(status, body = "", headers = list(),
                      url = "https://abs.la.utexas.edu/") {
  httr2::response(
    status_code = as.integer(status),
    headers = headers,
    body = charToRaw(body),
    url = url
  )
}

# Mock for httr2::req_perform that replays a fixed response sequence and
# records the request objects (accessible via attr(mock, "state")$reqs) so
# tests can assert on the assembled payload.
req_capture <- function(...) {
  responses <- list(...)
  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$reqs <- list()
  fn <- function(req, ...) {
    state$n <- state$n + 1L
    if (state$n > length(responses)) {
      stop("unexpected req_perform call #", state$n)
    }
    state$reqs[[state$n]] <- req
    responses[[state$n]]
  }
  attr(fn, "state") <- state
  fn
}

# Pure helpers: extract_livewire_snapshot ---------------------------------

test_that("extract_livewire_snapshot decodes &quot;-encoded snapshot JSON", {
  html <- read_fixture_text("abs_login_page.html")
  snapshot <- abmdash:::extract_livewire_snapshot(html)

  expect_type(snapshot, "character")
  expect_false(grepl("&quot;", snapshot, fixed = TRUE))
  parsed <- jsonlite::fromJSON(snapshot, simplifyVector = FALSE)
  expect_equal(parsed$memo$name, "login")
  expect_equal(parsed$memo$id, "LW-login-1")
})

test_that("extract_livewire_snapshot returns NULL when no wire:snapshot present", {
  expect_null(abmdash:::extract_livewire_snapshot("<html><body>plain</body></html>"))
})

# Pure helpers: extract_csrf_token (all 4 fallback patterns) ---------------

test_that("extract_csrf_token reads Livewire data-csrf attribute (L214)", {
  html <- read_fixture_text("abs_login_page.html")
  expect_equal(abmdash:::extract_csrf_token(html), "data-csrf-token-abc123")
})

test_that("extract_csrf_token falls back to meta csrf-token tag (L220)", {
  html <- read_fixture_text("abs_csrf_meta.html")
  expect_equal(abmdash:::extract_csrf_token(html), "meta-csrf-token-42")
})

test_that("extract_csrf_token falls back to hidden input name-first (L228)", {
  html <- read_fixture_text("abs_csrf_hidden_name_first.html")
  expect_equal(abmdash:::extract_csrf_token(html), "hidden-name-first-77")
})

test_that("extract_csrf_token falls back to hidden input value-first (L229)", {
  html <- read_fixture_text("abs_csrf_hidden_value_first.html")
  expect_equal(abmdash:::extract_csrf_token(html), "hidden-value-first-99")
})

test_that("extract_csrf_token precedence: data-csrf wins when several present", {
  html <- paste(
    read_fixture_text("abs_csrf_meta.html"),
    read_fixture_text("abs_csrf_hidden_name_first.html"),
    read_fixture_text("abs_csrf_hidden_value_first.html"),
    '<html lang="en" data-csrf="data-csrf-precedence-1"></html>'
  )
  expect_equal(abmdash:::extract_csrf_token(html), "data-csrf-precedence-1")
})

test_that("extract_csrf_token returns NULL when no CSRF pattern present", {
  expect_null(abmdash:::extract_csrf_token(
    '<div wire:snapshot="{\\"memo\\":{\\"name\\":\\"x\\"}}"></div>'
  ))
})

# abs_login ----------------------------------------------------------------

test_that("abs_login returns an authenticated request after Livewire redirect", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com", ABS_PASSWORD = "s3cret-pass")

  mock <- req_capture(
    http_resp(200, read_fixture_text("abs_login_page.html"),
              headers = list(`content-type` = "text/html; charset=utf-8")),
    http_resp(200, read_fixture_text("abs_livewire_auth.json"),
              headers = list(`content-type` = "application/json"))
  )
  local_mocked_bindings(req_perform = mock, .package = "httr2")

  session <- suppressMessages(abs_login(check_connection = FALSE))

  expect_s3_class(session, "httr2_request")
  reqs <- attr(mock, "state")$reqs
  expect_length(reqs, 2)

  # Payload value asserts: CSRF + snapshot + credentials reached the POST
  login_post <- reqs[[2]]
  expect_equal(login_post$method, "POST")
  expect_match(login_post$url, "livewire/update")
  expect_equal(login_post$headers[["X-CSRF-TOKEN"]], "data-csrf-token-abc123")
  body <- login_post$body$data
  # The payload uses jsonlite::unbox() scalars — match the exact representation.
  expect_equal(body$`_token`, jsonlite::unbox("data-csrf-token-abc123"))
  expect_equal(body$components[[1]]$calls[[1]]$method,
               jsonlite::unbox("authenticate"))
  expect_equal(body$components[[1]]$updates$`data.email`,
               jsonlite::unbox("user@example.com"))
  expect_equal(body$components[[1]]$updates$`data.password`,
               jsonlite::unbox("s3cret-pass"))
})

test_that("abs_login stops when httr2 is not installed (L23)", {
  local_isolated_env()
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base"
  )
  expect_error(
    abs_login(check_connection = FALSE),
    "httr2 package is required"
  )
})

test_that("abs_login stops when ABS_USERNAME is unset (L30)", {
  local_isolated_env()
  expect_error(
    abs_login(check_connection = FALSE),
    "ABS_USERNAME environment variable is not set"
  )
})

test_that("abs_login stops when ABS_PASSWORD is unset (L33)", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com")
  expect_error(
    abs_login(check_connection = FALSE),
    "ABS_PASSWORD environment variable is not set"
  )
})

test_that("abs_login stops when the server is unreachable (L38)", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com", ABS_PASSWORD = "pw")
  local_mocked_bindings(
    req_perform = req_capture(http_resp(500, "<html>down</html>")),
    .package = "httr2"
  )
  expect_error(
    abs_login(check_connection = TRUE),
    "Cannot connect to https://abs.la.utexas.edu"
  )
})

test_that("abs_login stops on login page error status (L66)", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com", ABS_PASSWORD = "pw")
  local_mocked_bindings(
    req_perform = req_capture(http_resp(500, "<html>oops</html>")),
    .package = "httr2"
  )
  expect_error(
    abs_login(check_connection = FALSE),
    "Login page returned error status: 500"
  )
})

test_that("abs_login stops when no Livewire snapshot on login page (L74)", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com", ABS_PASSWORD = "pw")
  local_mocked_bindings(
    req_perform = req_capture(http_resp(200, "<html><body>no livewire</body></html>")),
    .package = "httr2"
  )
  expect_error(
    abs_login(check_connection = FALSE),
    "No Livewire snapshot found on login page"
  )
})

test_that("abs_login stops when no CSRF token on login page (L80)", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com", ABS_PASSWORD = "pw")
  page <- '<div wire:snapshot="{&quot;memo&quot;:{&quot;name&quot;:&quot;login&quot;}}"></div>'
  local_mocked_bindings(
    req_perform = req_capture(http_resp(200, page)),
    .package = "httr2"
  )
  expect_error(
    abs_login(check_connection = FALSE),
    "No CSRF token found on login page"
  )
})

test_that("abs_login stops when the Livewire auth request fails (L127)", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com", ABS_PASSWORD = "pw")
  mock <- req_capture(
    http_resp(200, read_fixture_text("abs_login_page.html")),
    http_resp(500, "{}")
  )
  local_mocked_bindings(req_perform = mock, .package = "httr2")
  expect_error(
    abs_login(check_connection = FALSE),
    "Livewire authentication request failed with status: 500"
  )
})

test_that("abs_login stops with diagnostics when no redirect (L181)", {
  local_isolated_env()
  withr::local_envvar(ABS_USERNAME = "user@example.com", ABS_PASSWORD = "pw")
  mock <- req_capture(
    http_resp(200, read_fixture_text("abs_login_page.html")),
    http_resp(200, read_fixture_text("abs_livewire_auth_noredirect.json"))
  )
  local_mocked_bindings(req_perform = mock, .package = "httr2")
  expect_error(
    suppressMessages(abs_login(check_connection = FALSE)),
    "Login failed: no redirect in response.*These credentials do not match our records"
  )
})

# test_abs_connection -------------------------------------------------------

test_that("test_abs_connection returns TRUE on 2xx status", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(200, "ok")),
    .package = "httr2"
  )
  expect_true(test_abs_connection(verbose = FALSE))
})

test_that("test_abs_connection returns FALSE on error status", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(404, "not found")),
    .package = "httr2"
  )
  expect_false(test_abs_connection(verbose = FALSE))
})

test_that("test_abs_connection returns FALSE when the request errors", {
  local_mocked_bindings(
    req_perform = function(req, ...) stop("connection refused"),
    .package = "httr2"
  )
  expect_false(test_abs_connection(verbose = FALSE))
})

# verify_abs_login ----------------------------------------------------------

test_that("verify_abs_login returns TRUE on 200 status", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(200, "protected page")),
    .package = "httr2"
  )
  expect_true(suppressMessages(verify_abs_login(mock_session())))
})

test_that("verify_abs_login warns and returns FALSE when redirected to login", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(302, "", headers = list(location = "/admin/login"))),
    .package = "httr2"
  )
  expect_warning(
    result <- verify_abs_login(mock_session()),
    "Redirected to login page"
  )
  expect_false(result)
})

test_that("verify_abs_login returns TRUE when redirected elsewhere", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(302, "", headers = list(location = "/dashboard"))),
    .package = "httr2"
  )
  expect_true(suppressMessages(verify_abs_login(mock_session())))
})

test_that("verify_abs_login warns and returns FALSE on other statuses", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(500, "err")),
    .package = "httr2"
  )
  expect_warning(
    result <- verify_abs_login(mock_session()),
    "Login verification uncertain: Status 500"
  )
  expect_false(result)
})

test_that("verify_abs_login warns and returns FALSE when the request errors", {
  local_mocked_bindings(
    req_perform = function(req, ...) stop("connection reset"),
    .package = "httr2"
  )
  expect_warning(
    result <- verify_abs_login(mock_session()),
    "Login verification failed: connection reset"
  )
  expect_false(result)
})

# download_abs_csv ----------------------------------------------------------

test_that("download_abs_csv parses the base64 CSV (dual-consumer columns)", {
  mock <- req_capture(
    http_resp(200, read_fixture_text("abs_tests_page.html")),
    http_resp(200, read_fixture_text("abs_livewire_download.json"))
  )
  local_mocked_bindings(req_perform = mock, .package = "httr2")

  result <- suppressMessages(download_abs_csv(mock_session()))

  expect_identical(result, load_fixture("abs", "abs_csv_sample.csv"))
  # Dual-consumer contract: trad_compliance and preview_abs_csv both consume
  # these column names.
  expect_named(result, c("subject_id", "session", "start_time", "end_time",
                         "score", "finished", "test_data"))
  expect_equal(result$subject_id, c("P101", "P101", "P102"))
  expect_equal(result$session, c(1L, 2L, 1L))
  expect_equal(result$score, c(74.0, 80.0, 70.0))
})

test_that("download_abs_csv writes the CSV when save_path is given", {
  mock <- req_capture(
    http_resp(200, read_fixture_text("abs_tests_page.html")),
    http_resp(200, read_fixture_text("abs_livewire_download.json"))
  )
  local_mocked_bindings(req_perform = mock, .package = "httr2")
  out <- tempfile(fileext = ".csv")

  result <- suppressMessages(download_abs_csv(mock_session(), save_path = out))

  expect_true(file.exists(out))
  # write.csv() serializes 74.0 as "74", so the round-trip reads back integer —
  # compare by value, not by integer/double representation.
  expect_equal(utils::read.csv(out, stringsAsFactors = FALSE), result)
})

test_that("download_abs_csv stops when not authenticated (L377)", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(302, "", headers = list(location = "/admin/login"))),
    .package = "httr2"
  )
  expect_error(
    download_abs_csv(mock_session()),
    "Not authenticated. Session may have expired. Please login again."
  )
})

test_that("download_abs_csv stops when the tests page fails (L380)", {
  local_mocked_bindings(
    req_perform = req_capture(http_resp(500, "<html>err</html>")),
    .package = "httr2"
  )
  expect_error(
    download_abs_csv(mock_session()),
    "Failed to load tests page. Status: 500"
  )
})

test_that("download_abs_csv stops when list-tests component is missing (L399)", {
  # Login page has a wire:snapshot but no list-tests component.
  local_mocked_bindings(
    req_perform = req_capture(http_resp(200, read_fixture_text("abs_login_page.html"))),
    .package = "httr2"
  )
  expect_error(
    download_abs_csv(mock_session()),
    "Could not find list-tests component on tests page"
  )
})

test_that("download_abs_csv stops when no CSRF on the tests page (L405)", {
  page <- gsub(' data-csrf="tests-page-csrf-555"', "",
               read_fixture_text("abs_tests_page.html"))
  local_mocked_bindings(
    req_perform = req_capture(http_resp(200, page)),
    .package = "httr2"
  )
  expect_error(
    download_abs_csv(mock_session()),
    "No CSRF token found on tests page"
  )
})

test_that("download_abs_csv stops when the CSV request fails (L444)", {
  mock <- req_capture(
    http_resp(200, read_fixture_text("abs_tests_page.html")),
    http_resp(500, "{}")
  )
  local_mocked_bindings(req_perform = mock, .package = "httr2")
  expect_error(
    download_abs_csv(mock_session()),
    "CSV download request failed with status: 500"
  )
})

test_that("download_abs_csv stops when no download content (L460)", {
  dl <- jsonlite::fromJSON(read_fixture_text("abs_livewire_download.json"),
                           simplifyVector = FALSE)
  dl$components[[1]]$effects$download <- NULL
  mock <- req_capture(
    http_resp(200, read_fixture_text("abs_tests_page.html")),
    http_resp(200, jsonlite::toJSON(dl, auto_unbox = TRUE))
  )
  local_mocked_bindings(req_perform = mock, .package = "httr2")
  expect_error(
    download_abs_csv(mock_session()),
    "No CSV download content in Livewire response"
  )
})

test_that("Livewire download fixture decodes to the committed CSV fixture", {
  dl <- jsonlite::fromJSON(read_fixture_text("abs_livewire_download.json"),
                           simplifyVector = FALSE)
  decoded <- rawToChar(jsonlite::base64_dec(dl$components[[1]]$effects$download$content))
  expect_identical(
    utils::read.csv(text = decoded, stringsAsFactors = FALSE),
    load_fixture("abs", "abs_csv_sample.csv")
  )
})

# preview_abs_csv -----------------------------------------------------------

test_that("preview_abs_csv prints a head and returns the full frame invisibly", {
  fixture_data <- load_fixture("abs", "abs_csv_sample.csv")
  local_mocked_bindings(
    download_abs_csv = function(session, ...) fixture_data
  )
  output <- capture.output(result <- preview_abs_csv("mock_session"))

  expect_identical(result, fixture_data)
  expect_match(paste(output, collapse = "\n"), "First 3 rows:")
  expect_match(paste(output, collapse = "\n"), "3 rows x 7 columns")
})

# Live login (manual only) ---------------------------------------------------

test_that("live ABS login end-to-end (manual: requires creds + VPN)", {
  # Skipped unless real ABS_USERNAME/ABS_PASSWORD are set. This test is NOT
  # recorded end-to-end (see AC-3.4 Technical Context): it exists for
  # credential-holders to sanity-check against the live portal, and is
  # exercised here only when the environment explicitly opts in.
  skip_if_not(Sys.getenv("ABS_USERNAME") != "" && Sys.getenv("ABS_PASSWORD") != "")

  session <- suppressMessages(abs_login())
  expect_s3_class(session, "httr2_request")
  expect_true(verify_abs_login(session))
  data <- download_abs_csv(session)
  expect_true(all(c("subject_id", "session", "start_time") %in% names(data)))
})

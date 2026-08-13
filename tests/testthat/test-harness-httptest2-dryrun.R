# httptest2 stack dry-run (AC-3.1)
#
# Proves the recording/redaction/replay stack BEFORE AC-3.2/3.3 (redcap,
# gsheet/gcal) depend on it. .test is a reserved TLD that never resolves, so
# the fixture is authored by hand via httptest2::save_response() — exactly the
# shape start_capturing() writes — then replayed through with_mock_api().
#
# Mock file layout (httptest2 convention): fixtures/<host>/<path>.<ext>.
# A GET https://example.test/api -> fixtures/example.test/api.json.

# Redactor that scrubs token-shaped strings from both request headers and
# response bodies. This mirrors the redactor AC-3.2 will register for REDCap
# (POST body carries token=<...>), proving the mechanism end-to-end here.
token_scrub_redactor <- function(x) {
  x <- httptest2::redact_headers(x, c("Authorization", "X-REDCap-Token"))
  httptest2::within_body_text(x, function(body) {
    gsub("token=[A-Za-z0-9]{16,}", "token=<REDACTED>", body)
  })
}

test_that("httptest2 records, redacts tokens, and replays (round-trip)", {
  local_isolated_env()

  # The redactor is process-global state (an option) — restore it afterwards
  # so it cannot leak into other test files.
  old_redactor <- getOption("httptest2.redactor")
  old_redactor_pkgs <- getOption("httptest2.redactor.packages")
  on.exit(
    options(
      httptest2.redactor = old_redactor,
      httptest2.redactor.packages = old_redactor_pkgs
    ),
    add = TRUE
  )
  httptest2::set_redactor(token_scrub_redactor)

  # Record into a temp mock root (NOT the committed fixtures dir)
  mock_root <- file.path(tempdir(), "httptest2-dryrun")
  httptest2::.mockPaths(mock_root)
  on.exit(unlink(mock_root, recursive = TRUE), add = TRUE)
  on.exit(httptest2::.mockPaths(NULL), add = TRUE)

  # Simulated recorded response containing a token-shaped secret. At record
  # time start_capturing() applies the redactor to the response before saving
  # — replicate that exact step here.
  recorded <- httr2::response(
    method = "GET",
    url = "https://example.test/api",
    status_code = 200L,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw('{"ok": true, "value": 42, "secret": "token=AB12CD34EF56GH78"}')
  )
  recorded <- httptest2::get_current_redactor()(recorded)
  httptest2::save_response(recorded, file = "example.test/api")

  # The saved artifact must be token-free
  saved_file <- file.path(mock_root, "example.test", "api.json")
  expect_true(file.exists(saved_file))
  saved <- paste(readLines(saved_file, warn = FALSE), collapse = "\n")
  expect_false(grepl("AB12CD34EF56GH78", saved))
  expect_true(grepl("REDACTED", saved))

  # Replay the recorded fixture through with_mock_api
  res <- httptest2::with_mock_api({
    httr2::request("https://example.test/api") |>
      httr2::req_method("GET") |>
      httr2::req_perform()
  })
  expect_equal(httr2::resp_status(res), 200)
  expect_equal(httr2::resp_body_json(res)$value, 42)
})

test_that("committed example.test fixture replays and carries no token-shaped strings", {
  local_isolated_env()

  httptest2::.mockPaths(testthat::test_path("fixtures"))
  on.exit(httptest2::.mockPaths(NULL), add = TRUE)

  res <- httptest2::with_mock_api({
    httr2::request("https://example.test/api") |>
      httr2::req_method("GET") |>
      httr2::req_perform()
  })
  expect_equal(httr2::resp_status(res), 200)
  expect_equal(httr2::resp_body_json(res)$ok, TRUE)
  expect_equal(httr2::resp_body_json(res)$value, 42)

  # Token-shaped guard on the committed fixture: anything that looks like a
  # credential must have been scrubbed before it was committed.
  mock_files <- list.files(
    testthat::test_path("fixtures", "example.test"),
    full.names = TRUE,
    recursive = TRUE
  )
  contents <- paste(vapply(mock_files, function(f) {
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_false(
    grepl(
      "(token|api[_-]?key|secret|password)=[A-Za-z0-9._-]{12,}",
      contents,
      ignore.case = TRUE
    ),
    label = "committed httptest2 fixture must not contain token-shaped strings"
  )
})

# Behavior-lock test harness (AC-3.1)
#
# Deterministic, offline test infrastructure for the wave-3b zero-behavior
# refactor. Every helper here is additive: existing tests keep their own
# local_mocked_bindings style; new harness tests opt in.
#
# Helpers:
#   * load_fixture(module, file, ...)   — namespaced fixture reader
#   * with_fixed_clock(time, tz)        — pins Sys.time()/Sys.Date() AND TZ
#   * local_isolated_env()              — unsets all API credentials
#   * expect_snapshot_locked(module, value) — save-or-compare RDS snapshot
#
# See RECORDING.md (repo root) for the record/redact/commit workflow.

#' Load a namespaced test fixture
#'
#' Reads a CSV fixture from \code{fixtures/<module>/<file>} under the testthat
#' fixtures directory. \code{module} namespacing (trad, redcap, gsheet, gcal,
#' abs) prevents parallel ACs from colliding on fixture paths.
#'
#' @param module Character scalar, fixture subdirectory under
#'   \code{tests/testthat/fixtures/}.
#' @param file Character scalar, CSV file name inside the module directory.
#' @param ... Extra arguments passed to \code{\link[utils]{read.csv}}.
#'
#' @return A data.frame of the fixture contents.
#'
#' @section "" vs NA convention:
#' REDCap returns the literal \code{""} for unset fields. The default
#' \code{na.strings = "NA"} keeps \code{""} as the empty string instead of
#' coercing it to NA; a downstream \code{as.numeric("")} then yields NA in the
#' code under test exactly as it does against the live API. Do NOT pass
#' \code{na.strings = c("NA", "")} — that silently hides the "" field shape.
load_fixture <- function(module, file, ...) {
  utils::read.csv(
    testthat::test_path("fixtures", module, file),
    stringsAsFactors = FALSE,
    na.strings = "NA",
    ...
  )
}

#' Pin the clock to a fixed instant
#'
#' Freezes \code{Sys.time()} and \code{Sys.Date()} at \code{time} AND pins the
#' \code{TZ} environment variable to \code{tz}, for the duration of the calling
#' test. Everything is restored via deferred expressions when the test ends.
#'
#' Note that \code{withr::local_time()} alone sets only the timezone — it does
#' NOT freeze the clock. Mocking the clock functions themselves (via
#' \code{local_mocked_bindings(.package = "base")}) is what makes time-based
#' computation deterministic; pinning TZ prevents local-timezone shifting of
#' formatted output (e.g. \code{format.POSIXct} in a non-UTC locale).
#'
#' @param time POSIXct or character parseable by \code{\link{as.POSIXct}}; the
#'   instant the frozen clock reports.
#' @param tz Character scalar timezone; default "UTC".
#'
#' @return Invisibly, the POSIXct \code{time} value.
with_fixed_clock <- function(time, tz = "UTC") {
  time <- as.POSIXct(time, tz = tz)
  # .env/.local_envir must be the TEST env, NOT this helper's frame: both
  # local_mocked_bindings() and withr::local_envvar() defer restoration to the
  # passed environment, so binding them to the helper frame would revert the
  # mock the moment the helper returns.
  caller <- rlang::caller_env()
  withr::local_envvar(TZ = tz, .local_envir = caller)
  testthat::local_mocked_bindings(
    Sys.time = function() time,
    Sys.Date = function() as.Date(time, tz = tz),
    .package = "base",
    .env = caller
  )
  invisible(time)
}

#' Isolate the test from API credentials
#'
#' Unsets every API credential environment variable for the duration of the
#' calling test, so a test cannot accidentally read real credentials and a
#' misconfigured machine cannot leak them into recorded fixtures.
#'
#' @param creds Character vector of environment variable names to unset.
#'   Defaults to all API credentials the package consumes.
#'
#' @return Invisibly, NULL.
local_isolated_env <- function(creds = c(
                                 "REDCAP_API_TOKEN",
                                 "GOOGLE_SERVICE_ACCOUNT_JSON",
                                 "ABS_USERNAME",
                                 "ABS_PASSWORD",
                                 "STATICRYPT_PASSWORD"
                               )) {
  vals <- setNames(rep(list(NULL), length(creds)), creds)
  # Same frame discipline as with_fixed_clock(): restore in the TEST env, not
  # this helper's frame.
  withr::local_envvar(vals, .local_envir = rlang::caller_env())
  invisible()
}

#' Assert a value matches its committed behavior-lock snapshot
#'
#' Snapshots the full value (all columns, row order, attributes) to
#' \code{_snaps/snapshot-lock/<module>.rds} using testthat's native snapshot
#' machinery (\code{\link[testthat]{expect_snapshot_file}}). The artifact path
#' derives from the CALLING TEST FILE name: it must be \code{test-snapshot-lock.R}
#' (this pilot) or a file whose stem is \code{snapshot-lock} (future modules add
#' one \code{test_that} block per module to \code{test-snapshot-lock.R}, so every
#' artifact lands in the shared \code{_snaps/snapshot-lock/} dir).
#'
#' First local run records the RDS and passes with a warning (commit the
#' generated file); a missing lock on CI is a hard FAILURE (expect_snapshot_file
#' alone only warns); every later run byte-compares and fails on any drift.
#'
#' Row order is pinned as-is — the lock intentionally does NOT normalize row
#' order, so a silent reordering of output fails the lock.
#'
#' @param module Character scalar snapshot module name (e.g. "trad").
#' @param value Any R object to lock (typically a data.frame).
#' @param ... Extra arguments passed to \code{\link[testthat]{expect_snapshot_file}}.
#'
#' @return Invisibly, NULL.
expect_snapshot_locked <- function(module, value, ...) {
  lock_file <- testthat::test_path("_snaps", "snapshot-lock", paste0(module, ".rds"))
  on_ci <- nzchar(Sys.getenv("GITHUB_ACTIONS")) ||
    identical(Sys.getenv("CI"), "true")
  if (on_ci && !file.exists(lock_file)) {
    testthat::fail(sprintf(
      "snapshot-lock/%s.rds missing in CI — commit the generated lock file",
      module
    ))
  }
  snap_path <- tempfile(fileext = ".rds")
  saveRDS(value, snap_path)
  testthat::expect_snapshot_file(snap_path, name = paste0(module, ".rds"), ...)
  invisible()
}

#' Build a Google API HTTP mock served from synthetic fixtures
#'
#' Returns a response-mock function for \code{\link[httr2]{with_mocked_responses}}
#' that serves hand-crafted fixture JSON from \code{fixtures/gsheet/} or
#' \code{fixtures/gcal/}. Requests are matched on URL substrings: the token
#' endpoint, Sheets values, Calendar events, and calendarList. Because matching
#' is URL-based (the request BODY is never inspected), the JWT assertion posted
#' to the token endpoint can differ every run — the throwaway RSA key in
#' \code{test-google-token-signing.R} is therefore harmless, exactly as
#' RECORDING.md describes.
#'
#' @param fixture_dir Character scalar; directory holding the fixture JSON
#'   files (typically \code{testthat::test_path("fixtures", "gsheet")} or
#'   \code{"gcal"}).
#' @param mapping Named character vector of URL-pattern = fixture-file
#'   overrides/additions. Pattern names are matched with \code{\link{grepl}}
#'   against the request URL; the LAST matching pattern wins, so entries
#'   appended here take precedence over the built-in defaults (e.g. a test
#'   that wants a different file served for the same URL).
#'
#' @return A function of one argument (\code{req}) returning an
#'   \code{\link[httr2]{response}}; pass it to
#'   \code{httr2::with_mocked_responses()}.
mock_google_fixture <- function(fixture_dir, mapping = NULL) {
  defaults <- c(
    "oauth2.googleapis.com/token" = "token.json",
    "sheets.googleapis.com/v4/spreadsheets/.*/values/Sheet1$" = "sheet-full.json",
    "users/me/calendarList" = "calendar-list.json",
    "calendar/v3/calendars/" = "events.json"
  )
  if (!is.null(mapping)) {
    defaults[names(mapping)] <- mapping
  }
  patterns <- names(defaults)
  files <- unname(defaults)

  function(req) {
    url <- req$url
    hits <- which(vapply(patterns, function(p) grepl(p, url), logical(1)))
    if (length(hits) == 0) {
      stop("mock_google_fixture: no fixture mapped for URL ", url)
    }
    fixture_file <- files[hits[[length(hits)]]]
    path <- file.path(fixture_dir, fixture_file)
    if (!file.exists(path)) {
      stop("mock_google_fixture: fixture file missing: ", path)
    }
    body <- paste(readLines(path, warn = FALSE), collapse = "\n")
    httr2::response(
      status_code = 200L,
      url = url,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(body)
    )
  }
}

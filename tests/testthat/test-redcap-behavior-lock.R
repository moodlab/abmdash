# Behavior-lock redcap_api.R (AC-3.2)
#
# Locks ALL 9 exports of R/redcap_api.R against hand-crafted httptest2
# fixtures, at VALUE level (full parsed output, row order, "" vs NA), not
# shape level. Every "" empty-string case is exercised at its exact field
# and documented as LOCKING-CURRENT — the fix is deferred to AC-3.9.
#
# Fixture layout (all SYNTHETIC: P0xx IDs, fake names — never real
# participant data, never real creds):
#   fixtures/redcap.prc.utexas.edu/redcap/   populated responses (.json)
#   fixtures/redcap-errors/...               HTTP-400 error responses (.R)
#   fixtures/redcap-empty/...                empty-result responses ([])
# The redcap-errors / redcap-empty roots are PREPENDED via .mockPaths() in
# the tests that need them; find_mock_file() resolves the first match, so
# the same request body can replay populated, empty, or error responses.
#
# Frozen clock: 2026-08-13 12:00:00 UTC (with_fixed_clock). All fixture
# dates are relative to that instant: "recent" = 2026-08-11/12 (within the
# 7- and 30-day windows), "" = unset, 2026-06-14 = 60 days old (outside).
#
# Coexists ADDITIVELY with test-eligible-participants.R (which covers 2 of
# the 9 exports via local_mocked_bindings) — this file does NOT supersede it.
#
# Token policy: tests set REDCAP_API_TOKEN=DUMMY via local_redcap_mocks();
# get_redcap_token() (R/redcap_api.R L7-13) therefore passes upstream of
# httptest2 and no live API is ever contacted. The DUMMY value is not a
# 32-hex secret, and the committed fixtures carry no token-shaped strings
# (guarded below).

# Redactor registered for recording/redaction (RECORDING.md §3). Mirrors the
# AC-3.1 dry-run redactor: strips Authorization/X-REDCap-Token headers and
# scrubs long token= values from bodies. Applied by httptest2 at record time
# and (harmlessly) to requests at replay time — DUMMY is < 16 chars so the
# body used for fixture hashing is unchanged.
redcap_redactor <- function(x) {
  x <- httptest2::redact_headers(x, c("Authorization", "X-REDCap-Token"))
  httptest2::within_body_text(x, function(body) {
    gsub("token=[A-Za-z0-9]{16,}", "token=<REDACTED>", body)
  })
}

# Per-test setup for offline REDCap replay:
#   * REDCAP_API_TOKEN=DUMMY (env gate passes upstream of httptest2)
#   * token_scrub redactor registered (restored on exit)
#   * .mockPaths() = <mock_roots> + committed fixtures dir (restored on exit)
# Call with_fixed_clock() directly from the test for the frozen date.
local_redcap_mocks <- function(mock_roots = character()) {
  caller <- rlang::caller_env()
  withr::local_envvar(REDCAP_API_TOKEN = "DUMMY", .local_envir = caller)
  old_red <- getOption("httptest2.redactor")
  old_red_pkgs <- getOption("httptest2.redactor.packages")
  withr::defer(
    options(httptest2.redactor = old_red, httptest2.redactor.packages = old_red_pkgs),
    envir = caller
  )
  httptest2::set_redactor(redcap_redactor)
  httptest2::.mockPaths(NULL)
  httptest2::.mockPaths(unique(c(mock_roots, testthat::test_path("fixtures"))))
  withr::defer(httptest2::.mockPaths(NULL), envir = caller)
  invisible()
}

# Parse a committed populated fixture exactly as httr2::resp_body_json does
# (jsonlite, simplifyVector = FALSE -> list of named lists, "" preserved).
# The fixture FILE is the reviewed canonical value: this lock asserts the
# export returns it byte-for-byte (row order, field names, "" vs NA).
parse_fixture_body <- function(rel_path) {
  path <- testthat::test_path("fixtures", "redcap.prc.utexas.edu", "redcap", rel_path)
  jsonlite::fromJSON(
    paste(readLines(path, warn = FALSE), collapse = "\n"),
    simplifyVector = FALSE
  )
}

# ---------------------------------------------------------------------------
# AC-4: env gate fires upstream of httptest2
# ---------------------------------------------------------------------------

test_that("get_redcap_token stops verbatim when REDCAP_API_TOKEN is unset", {
  local_isolated_env()
  # The verbatim string is shared with test-faq-verbatim.R:91 — it must stay
  # stable. (get_redcap_token is internal; called via :::.)
  expect_error(
    abmdash:::get_redcap_token(),
    "REDCAP_API_TOKEN environment variable is not set or is empty",
    fixed = TRUE
  )
  # With DUMMY set (as every offline test does) the gate passes through.
  withr::local_envvar(REDCAP_API_TOKEN = "DUMMY")
  expect_identical(abmdash:::get_redcap_token(), "DUMMY")
})

# ---------------------------------------------------------------------------
# AC-1/AC-2: record exports + "" fidelity
# ---------------------------------------------------------------------------

test_that("call_redcap_api and get_redcap_records lock the record response value-level", {
  local_redcap_mocks()
  expected <- parse_fixture_body("api-94a9d8-POST.json")

  # call_redcap_api("record") — the local-variable shadowing of the `content`
  # parameter (resp_body_json assigned back into `content`) was resolved in
  # AC-3.5 via the parse_response helper; output is identical.
  direct <- httptest2::with_mock_api({ call_redcap_api("record") })
  expect_equal(direct, expected)

  # get_redcap_records() builds the identical body (no filters) -> same fixture.
  wrapped <- httptest2::with_mock_api({ get_redcap_records() })
  expect_equal(wrapped, expected)

  # LOCKING-CURRENT: phq8score "" and guid "" survive the JSON round-trip as
  # "" (never coerced to NA). AC-3.9 decides whether to normalize upstream.
  expect_identical(expected[[2]]$phq8score, "")
  expect_identical(expected[[2]]$guid, "")
  # Row order pinned: P001 before P002, as the API returned them.
  expect_identical(expected[[1]]$record_id, "P001")
  expect_identical(expected[[2]]$record_id, "P002")
})

test_that("get_redcap_records(fields=...) locks field selection and empty interview_date", {
  local_redcap_mocks()
  expected <- parse_fixture_body("api-f4e7c8-POST.json")

  result <- httptest2::with_mock_api({
    get_redcap_records(fields = c("phq8score", "interview_date"))
  })
  expect_equal(result, expected)
  # LOCKING-CURRENT: "" interview_date is preserved as "" (not NA).
  expect_identical(result[[2]]$interview_date, "")
  expect_identical(result[[2]]$phq8score, "")
})

test_that("get_redcap_metadata locks the data-dictionary response value-level", {
  local_redcap_mocks()
  expected <- parse_fixture_body("api-7ae338-POST.json")

  result <- httptest2::with_mock_api({ get_redcap_metadata() })
  expect_equal(result, expected)
})

# ---------------------------------------------------------------------------
# AC-9: multi-request + %||% (indirect)
# ---------------------------------------------------------------------------

test_that("get_survey_completions locks the long-format reshape (2 fixtures, exact row order)", {
  local_redcap_mocks()
  # Two HTTP requests under the hood: metadata (api-7ae338) then records
  # (api-1f57a9) — both fixtures must replay for this test to pass.
  result <- httptest2::with_mock_api({ get_survey_completions() })

  expected <- data.frame(
    record_id = c("P201", "P202"),
    survey_instrument = c("baseline", "followup"),
    survey_timestamp = c("2026-08-11 10:00:00", "2026-08-12 09:30:00"),
    survey_complete = c("2", "2"),
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected)
  expect_named(result, c("record_id", "survey_instrument", "survey_timestamp", "survey_complete"))

  # Row order pinned as-is: instrument order follows metadata field order
  # (unique() over grepl'd fields), and reshape_long() keeps that order (rbind
  # of per-instrument rows). P203 (all "" timestamps/completes) is filtered by
  # the activity_guard() helper. %||% is exercised by the metadata list->data.frame
  # conversion (metadata_to_df) — honest note: the fixture metadata has zero
  # NULL fields, so %||%'s right branch is untested by this lock; the guard is
  # retained for real metadata where REDCap can omit fields.
  expect_identical(result$record_id, c("P201", "P202"))
  expect_identical(result$survey_instrument, c("baseline", "followup"))
})

test_that("get_redcap_logs locks log entries (record filter branch)", {
  local_redcap_mocks()
  expected <- parse_fixture_body("api-81553d-POST.json")

  result <- httptest2::with_mock_api({ get_redcap_logs(records = "P301") })
  expect_equal(result, expected)
  expect_length(result, 2)
})

# ---------------------------------------------------------------------------
# AC-8: report-ID branches — 14081 (eligible + weekly)
# ---------------------------------------------------------------------------

test_that("get_redcap_report(14081) locks the screening report passthrough", {
  local_redcap_mocks()
  expected <- parse_fixture_body("api-f25e23-POST.json")

  result <- httptest2::with_mock_api({ get_redcap_report(14081) })
  expect_equal(result, expected)
  expect_length(result, 8)
})

test_that("get_eligible_participants locks eligibility semantics incl. all \"\" cases", {
  local_redcap_mocks()
  with_fixed_clock("2026-08-13 12:00:00", tz = "UTC")

  result <- httptest2::with_mock_api({ get_eligible_participants() })

  expected <- data.frame(
    first_name = c("Alice", "David", "Unknown"),
    phone_number = c("5125550101", "5125550104", "5125550105"),
    interview_date = c("2026-08-11", "2026-08-11", "2026-08-11"),
    link_to_record_id = c(
      "https://redcap.prc.utexas.edu/redcap/redcap_v15.5.6/DataEntry/record_home.php?pid=3385&arm=1&id=P301",
      "https://redcap.prc.utexas.edu/redcap/redcap_v15.5.6/DataEntry/record_home.php?pid=3385&arm=1&id=P304",
      "https://redcap.prc.utexas.edu/redcap/redcap_v15.5.6/DataEntry/record_home.php?pid=3385&arm=1&id=P305"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected)
  expect_named(result, c("first_name", "phone_number", "interview_date", "link_to_record_id"))

  # "" empty-string fidelity at the exact fields (all LOCKING-CURRENT — the
  # AC-3.9 sweep will decide whether to change any of these):
  #   phq8score "" (eligibility_mask parse-once guard)  -> P302 excluded, no NA row
  #   r01es_commute "" (eligibility_mask raw-guard)      -> P303 excluded
  #   interview_date "" (filter_recent_dates)            -> P306 excluded
  #   r01es_name "" -> "Unknown" (first_name_of)         -> P305 first_name "Unknown"
  expect_identical(nrow(result), 3L)
  expect_identical(result$first_name[3], "Unknown")          # r01es_name ""
  expect_false("Bob" %in% result$first_name)                 # phq8score ""
  expect_false("Carol" %in% result$first_name)               # r01es_commute ""
  expect_false("Erin" %in% result$first_name)                # interview_date ""
  expect_false("Frank" %in% result$first_name)               # 60 days old
  expect_false("Grace" %in% result$first_name)               # phq8score below cutoff
})

test_that("get_weekly_screening_stats locks 7-day counts incl. \"\" guards", {
  local_redcap_mocks()
  with_fixed_clock("2026-08-13 12:00:00", tz = "UTC")

  result <- httptest2::with_mock_api({ get_weekly_screening_stats() })

  expected <- data.frame(
    total_screenings = 6,
    eligible_count = 3,
    hispanic_count = 1,
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected)
  expect_named(result, c("total_screenings", "eligible_count", "hispanic_count"))

  # LOCKING-CURRENT:
  #   phq8score "" (eligibility_mask parse-once guard)  -> P302 not eligible
  #   r01es_commute "" (eligibility_mask raw-guard)     -> P303 not eligible
  #   r01es_hispanic "" (not counted)                   -> P304 eligible but hispanic
  #                                                       not tallied (count stays 1)
  #   interview_date "" (filter_recent_dates)           -> P306 not in the 7-day window
})

# ---------------------------------------------------------------------------
# AC-8: report-ID branches — 13387 (enrollment)
# ---------------------------------------------------------------------------

test_that("get_redcap_report(13387) locks the enrollment report passthrough", {
  local_redcap_mocks()
  expected <- parse_fixture_body("api-89e542-POST.json")

  result <- httptest2::with_mock_api({ get_redcap_report(13387) })
  expect_equal(result, expected)
  expect_length(result, 6)
})

test_that("get_enrollment_stats locks enrollment semantics incl. guid \"\" not enrolled", {
  local_redcap_mocks()
  with_fixed_clock("2026-08-13 12:00:00", tz = "UTC")

  result <- httptest2::with_mock_api({ get_enrollment_stats() })

  # P401 (guid, recent) and P402 (guid, old) enrolled; P403 (guid "" on every
  # row) NOT enrolled (record_has_guid any-GUID guard); P404 enrolled via
  # ANY-row guid across its two longitudinal rows; P405 enrolled but has no
  # valid interview_date.
  expect_identical(result$total_enrolled, 4L)
  expect_identical(result$weekly_enrolled, 2L)    # P401 (2026-08-11), P404 (2026-08-10)
  expect_identical(result$current_month, "August 2026")  # format(Sys.Date(), "%B %Y")
  expect_identical(result$guid_field, "guid")
  expect_identical(result$date_field, "interview_date")
  # get_enrollment_stats appends month_year to enrolled_df before building
  # available_fields — locked as-is.
  expect_identical(result$available_fields, "record_id, parsed_interview_date, month_year")
  expect_identical(result$valid_dates_count, 3L)
  expect_identical(result$date_range, "2026-06-01 to 2026-08-11")
  # Row order pinned (monthly_breakdown_counts sorts month descending); the
  # row names are a sort artifact — strip them, the value ORDER is what the
  # lock pins.
  actual_breakdown <- result$monthly_breakdown
  rownames(actual_breakdown) <- NULL
  expected_breakdown <- data.frame(
    month = c("2026-08", "2026-06"),
    count = c(2, 1),
    stringsAsFactors = FALSE
  )
  expect_equal(actual_breakdown, expected_breakdown)
})

# ---------------------------------------------------------------------------
# AC-8: empty-result branch per report (14081 + 13387)
# ---------------------------------------------------------------------------

test_that("empty-result report branches lock per report (14081 + 13387)", {
  local_redcap_mocks(mock_roots = testthat::test_path("fixtures", "redcap-empty"))
  with_fixed_clock("2026-08-13 12:00:00", tz = "UTC")

  eligible <- httptest2::with_mock_api({ get_eligible_participants() })
  expect_equal(
    eligible,
    data.frame(
      Status = "No data from report 14081",
      Total_Records = 0,
      Eligible_Count = 0,
      stringsAsFactors = FALSE
    )
  )

  weekly <- httptest2::with_mock_api({ get_weekly_screening_stats() })
  expect_equal(
    weekly,
    data.frame(total_screenings = 0, eligible_count = 0, hispanic_count = 0, stringsAsFactors = FALSE)
  )

  enrollment <- httptest2::with_mock_api({ get_enrollment_stats() })
  expect_identical(enrollment$total_enrolled, 0)
  expect_identical(enrollment$monthly_enrolled, 0)
  expect_identical(enrollment$current_month, "August 2026")
  expect_identical(enrollment$error, "No data from report 13387")
  expect_equal(
    enrollment$monthly_breakdown,
    data.frame(month = character(0), count = numeric(0), stringsAsFactors = FALSE)
  )
})

# ---------------------------------------------------------------------------
# AC-10: HTTP-error fixture per stats fn — tryCatch error frames
# ---------------------------------------------------------------------------

test_that("HTTP-error fixtures lock the stats fns' tryCatch error frames", {
  local_redcap_mocks(mock_roots = testthat::test_path("fixtures", "redcap-errors"))
  with_fixed_clock("2026-08-13 12:00:00", tz = "UTC")

  # get_eligible_participants error frame (tryCatch error handler)
  eligible <- httptest2::with_mock_api({ get_eligible_participants() })
  expect_named(eligible, c("Status", "Total_Records", "Eligible_Count"))
  expect_identical(eligible$Total_Records, 0)
  expect_identical(eligible$Eligible_Count, 0)
  expect_identical(eligible$Status, "Error: REDCap API call failed: HTTP 400 Bad Request.")

  # get_weekly_screening_stats error frame
  weekly <- httptest2::with_mock_api({ get_weekly_screening_stats() })
  expect_named(weekly, c("total_screenings", "eligible_count", "hispanic_count", "error_message"))
  expect_identical(weekly$total_screenings, 0)
  expect_identical(weekly$eligible_count, 0)
  expect_identical(weekly$hispanic_count, 0)
  expect_identical(weekly$error_message, "REDCap API call failed: HTTP 400 Bad Request.")

  # get_enrollment_stats error frame
  enrollment <- httptest2::with_mock_api({ get_enrollment_stats() })
  expect_identical(enrollment$total_enrolled, 0)
  expect_identical(enrollment$weekly_enrolled, 0)
  expect_identical(enrollment$current_month, "August 2026")
  expect_identical(enrollment$error, "REDCap API call failed: HTTP 400 Bad Request.")
})

# ---------------------------------------------------------------------------
# AC-3: committed fixtures carry no token-shaped strings
# ---------------------------------------------------------------------------

test_that("committed httptest2 fixtures carry no 32-hex or long token= strings", {
  local_isolated_env()
  mock_files <- list.files(
    testthat::test_path("fixtures"),
    full.names = TRUE,
    recursive = TRUE
  )
  contents <- paste(vapply(mock_files, function(f) {
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  # REDCap API tokens are 32-hex; anything that long must never be committed.
  expect_false(
    grepl("[a-f0-9]{32}", contents),
    label = "no 32-hex token-shaped strings in fixtures/"
  )
  # Belt and suspenders: no long token= values either (DUMMY is 5 chars).
  expect_false(
    grepl("token=[A-Za-z0-9]{16,}", contents),
    label = "no long token= values in fixtures/"
  )
})

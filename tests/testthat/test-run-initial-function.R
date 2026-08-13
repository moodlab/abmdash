# Behavior-lock tests for run_initial_function.R (AC-3.4)
#
# run_initial_function: value asserts (kept the original stub's expectation).
# encrypt_dashboard: STATICRYPT_PASSWORD / docs-dir decision branches.
# get_central_time: pinned-clock exact string (tripwire — real clock fails).
# get_enrollment_targets: REAL committed CSV + file-isolation assert.

test_that("initial function works", {
  expect_equal(run_initial_function(2), 4)
})

test_that("run_initial_function returns n + n", {
  expect_equal(run_initial_function(-3), -6)
  expect_equal(run_initial_function(0), 0)
  expect_equal(run_initial_function(1.5), 3)
})

test_that("encrypt_dashboard skips when STATICRYPT_PASSWORD is unset", {
  local_isolated_env()
  output <- capture.output(result <- encrypt_dashboard())
  expect_null(result)
  expect_match(paste(output, collapse = "\n"),
               "No STATICRYPT_PASSWORD set, skipping encryption")
})

test_that("encrypt_dashboard encrypts when password set and docs dir found", {
  withr::local_envvar(STATICRYPT_PASSWORD = "test-password")
  # Unique subdir: tempdir() is process-constant, so sibling tests would see a
  # leftover docs/ directory.
  work_dir <- file.path(tempdir(), "encrypt_docs_found")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  withr::local_dir(work_dir)
  dir.create("docs", showWarnings = FALSE)

  output <- capture.output(result <- encrypt_dashboard())
  expect_null(result)
  expect_match(paste(output, collapse = "\n"), "Encrypting files in: docs")
})

test_that("encrypt_dashboard reports when no docs directory is found", {
  withr::local_envvar(STATICRYPT_PASSWORD = "test-password")
  work_dir <- file.path(tempdir(), "encrypt_docs_missing")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  withr::local_dir(work_dir)

  output <- capture.output(result <- encrypt_dashboard())
  expect_null(result)
  expect_match(paste(output, collapse = "\n"),
               "Could not find docs directory, skipping encryption")
})

test_that("get_central_time formats in Central Time at frozen clock", {
  # 2026-03-20 12:00 UTC == 07:00 CDT (US DST active since 2026-03-08).
  # TRIPWIRE: with the real clock the formatted string differs and fails.
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")
  expect_equal(get_central_time(), "2026-03-20 07:00 AM CDT")
})

test_that("get_enrollment_targets reads the committed CSV", {
  targets <- get_enrollment_targets()
  expect_s3_class(targets, "data.frame")
  expect_equal(nrow(targets), 43)
  expect_named(targets, c("Date", "Target", "Total", "Minority", "Hispanic"))
  expect_type(targets$Date, "character")
  expect_type(targets$Target, "integer")
  expect_type(targets$Total, "integer")
  expect_type(targets$Minority, "integer")
  expect_type(targets$Hispanic, "integer")
  # Exact first and last rows.
  expect_equal(targets$Date[1], "1-May-24")
  expect_equal(targets$Target[1], 0)
  expect_equal(targets$Total[1], 0)
  expect_equal(targets$Date[43], "1-Nov-27")
  expect_equal(targets$Target[43], 18)
  expect_equal(targets$Total[43], 600)
  expect_equal(targets$Minority[43], 144)
  expect_equal(targets$Hispanic[43], 108)
})

test_that("get_enrollment_targets does not mutate the data files", {
  # FILE-ISOLATION: both committed copies (data/ and inst/extdata/) must be
  # byte-identical before and after the call.
  pkg_root <- dirname(system.file(package = "abmdash"))
  data_csv <- file.path(pkg_root, "data", "enrollment_targets.csv")
  ext_csv <- file.path(pkg_root, "inst", "extdata", "enrollment_targets.csv")
  expect_true(file.exists(data_csv))
  expect_true(file.exists(ext_csv))

  before_data <- tools::md5sum(data_csv)
  before_ext <- tools::md5sum(ext_csv)
  invisible(get_enrollment_targets())
  expect_identical(tools::md5sum(data_csv), before_data)
  expect_identical(tools::md5sum(ext_csv), before_ext)
})

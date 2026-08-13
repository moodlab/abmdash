# Fixture secret-scan for the Google modules — AC-3.3(e)
#
# Defense-in-depth on top of the httptest2 redactor (RECORDING.md §3): scans
# the committed gsheet/gcal fixture files AND the snapshot directory for
# private-key / service-account material. The positive companion proves the
# pattern actually fires on a "private_key" string — the scan is not
# vacuously green.

google_secret_pattern <- paste(
  "private_key",
  "BEGIN PRIVATE KEY",
  "BEGIN RSA PRIVATE",
  "GOOGLE_SERVICE_ACCOUNT_JSON",
  sep = "|"
)

scan_google_fixtures <- function() {
  scan_dirs <- c("fixtures/gsheet", "fixtures/gcal", "_snaps")
  files <- unlist(lapply(scan_dirs, function(d) {
    dir <- testthat::test_path(d)
    if (!dir.exists(dir)) {
      return(character(0))
    }
    list.files(dir, full.names = TRUE, recursive = TRUE)
  }), use.names = FALSE)
  files[!vapply(files, function(f) dir.exists(f), logical(1))]
}

test_that("google fixtures and snapshots contain no private-key material", {
  files <- scan_google_fixtures()
  expect_true(
    length(files) >= 6,
    label = sprintf("expected gsheet + gcal + snapshot fixtures to be scanned, got %d", length(files))
  )

  has_hit <- vapply(files, function(f) {
    tryCatch({
      raw <- readBin(f, what = "raw", n = file.info(f)$size)
      grepl(google_secret_pattern, rawToChar(raw), useBytes = TRUE, ignore.case = TRUE)
    }, error = function(e) FALSE)
  }, logical(1))

  expect_false(
    any(has_hit),
    label = sprintf(
      "secret-looking material found in: %s",
      paste(files[has_hit], collapse = ", ")
    )
  )
})

test_that("positive companion: scan pattern matches a private_key string", {
  synthetic <- paste0(
    '{"type":"service_account","client_email":"svc@example.com",',
    '"private_key":"-----BEGIN PRIVATE KEY-----\\nMIIEvQ...\\n-----END PRIVATE KEY-----"}'
  )
  expect_true(
    grepl(google_secret_pattern, synthetic, useBytes = TRUE, ignore.case = TRUE),
    label = "scan pattern must fire on a private_key-bearing service account JSON"
  )
})

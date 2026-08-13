#' Initial function
#'
#' @param n A number
#'
#' @return Sum of n + n
#' @export
#'
#' @examples
#' run_initial_function(5)
#' run_initial_function(10)
#' run_initial_function(-3)
run_initial_function <- function(n) {
  return(n + n)
}

#' Encrypting dashboard
#'
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' encrypt_dashboard()
encrypt_dashboard <- function() {
  password <- Sys.getenv("STATICRYPT_PASSWORD")

  # Check if password is set
  if (password == "" || is.na(password)) {
    cat("No STATICRYPT_PASSWORD set, skipping encryption\n")
    return(invisible(NULL))
  }

  # Determine the correct docs path - use absolute path for Docker
  docs_path <- resolve_docs_path()
  if (is.na(docs_path)) {
    cat("Could not find docs directory, skipping encryption\n")
    return(invisible(NULL))
  }

  cat("Encrypting files in:", docs_path, "\n")

  return(invisible(NULL))
}

# Internal helper: resolve the docs directory, preferring the absolute Docker
# path (/app/docs), then the relative fallbacks. Returns NA when no docs
# directory exists anywhere (caller handles the skip message).
resolve_docs_path <- function() {
  # Use absolute path for Docker
  if (file.exists("/app/docs")) {
    return("/app/docs")
  }

  # Fallback to relative paths if not in Docker
  if (file.exists("../../docs")) {
    return("../../docs")
  }
  if (file.exists("docs")) {
    return("docs")
  }

  NA_character_
}

#' Get current time in Central Time Zone
#'
#' Returns the current date and time formatted in Central Time (US/Central)
#' with AM/PM format
#'
#' @return A character string with the formatted date and time
#' @export
#'
#' @examples
#' get_central_time()
get_central_time <- function() {
  # Get current time in Central Time with AM/PM format
  ct_time <- format(Sys.time(), tz = "America/Chicago", format = "%Y-%m-%d %I:%M %p %Z")
  return(ct_time)
}

#' Get enrollment targets data
#'
#' Reads the enrollment targets CSV file and returns it as a data frame
#'
#' @return A data frame with enrollment targets by date
#' @export
#'
#' @examples
#' get_enrollment_targets()
get_enrollment_targets <- function() {
  # Find the CSV file - try multiple possible locations
  possible_paths <- c(
    system.file("extdata", "enrollment_targets.csv", package = "abmdash"),
    "inst/extdata/enrollment_targets.csv",
    "../inst/extdata/enrollment_targets.csv",
    "data/enrollment_targets.csv",
    "../data/enrollment_targets.csv",
    "../../inst/extdata/enrollment_targets.csv"
  )

  csv_path <- resolve_first_existing(possible_paths)

  if (is.na(csv_path)) {
    stop("Could not find enrollment_targets.csv file")
  }

  # Read the CSV file
  enrollment_data <- read.csv(csv_path, stringsAsFactors = FALSE)

  return(enrollment_data)
}

# Internal helper: the first path in `paths` that exists, or NA if none do.
# Uses base file.exists (NOT fs::file_exists) so the mock in
# test-run-initial-function.R keeps the verbatim stop firing. Called once on the
# whole vector; the "" that system.file() returns when extdata is absent is
# skipped because file.exists("") is FALSE (the old `path != ""` guard).
resolve_first_existing <- function(paths) {
  paths[file.exists(paths)][1]
}

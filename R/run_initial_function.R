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
  docs_path <- "/app/docs"
  if (!file.exists(docs_path)) {
    # Fallback to relative paths if not in Docker
    if (file.exists("../../docs")) {
      docs_path <- "../../docs"
    } else if (file.exists("docs")) {
      docs_path <- "docs"
    } else {
      cat("Could not find docs directory, skipping encryption\n")
      return(invisible(NULL))
    }
  }

  cat("Encrypting files in:", docs_path, "\n")

  return(invisible(NULL))
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

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

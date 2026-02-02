#' Login to ABS Admin Portal
#'
#' Authenticates to the ABS (Attention Bias Study) admin portal and returns
#' an authenticated httr2 request object that can be used for subsequent requests.
#'
#' @param base_url Character string with the base URL. Default is "https://abs.la.utexas.edu"
#' @param login_path Character string with the login path. Default is "/admin/login"
#' @param check_connection Logical, whether to check connection first. Default is TRUE
#'
#' @return An httr2 request object with authenticated session cookies
#' @export
#'
#' @examples
#' \dontrun{
#' # Login and get authenticated session
#' session <- abs_login()
#'
#' # Use the session for subsequent requests
#' response <- session |>
#'   httr2::req_url_path_append("/admin/dashboard") |>
#'   httr2::req_perform()
#' }
abs_login <- function(base_url = "https://abs.la.utexas.edu",
                      login_path = "/admin/login",
                      check_connection = TRUE) {

  # Check if required packages are available
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 package is required. Please install it with: install.packages('httr2')")
  }

  # Get credentials from environment
  username <- Sys.getenv("ABS_USERNAME")
  password <- Sys.getenv("ABS_PASSWORD")

  if (username == "" || is.na(username)) {
    stop("ABS_USERNAME environment variable is not set")
  }

  if (password == "" || is.na(password)) {
    stop("ABS_PASSWORD environment variable is not set")
  }

  # Check connection first
  if (check_connection) {
    if (!test_abs_connection(base_url, verbose = FALSE)) {
      stop("Cannot connect to ", base_url,
           "\nPlease check:\n",
           "  1. You are connected to UT VPN (if required)\n",
           "  2. The URL is correct\n",
           "  3. You have network connectivity\n",
           "Run test_abs_connection() for more details.")
    }
  }

  login_url <- paste0(base_url, login_path)

  # Step 1: Create a session with cookie handling
  cookie_file <- tempfile()
  session <- httr2::request(base_url) |>
    httr2::req_options(
      ssl_verifypeer = 0,
      ssl_verifyhost = 0,
      http_version = 2  # Force HTTP/1.1
    ) |>
    httr2::req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36")

  # Step 2: Get the login page to retrieve CSRF token and establish session
  message("Fetching login page...")
  tryCatch({
    login_page <- session |>
      httr2::req_url_path(login_path) |>
      httr2::req_cookie_preserve(cookie_file) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(login_page)
    message("Login page status: ", status)

    if (status >= 400) {
      stop("Login page returned error status: ", status)
    }

    # Get the page content to look for CSRF token
    page_content <- httr2::resp_body_string(login_page)

    # Try to extract CSRF token from meta tag and hidden inputs
    csrf_token <- extract_csrf_token(page_content)

    if (!is.null(csrf_token)) {
      message("Found CSRF token: ", substr(csrf_token, 1, 20), "...")
    } else {
      stop("No CSRF token found in page")
    }

  }, error = function(e) {
    stop("Failed to fetch login page: ", e$message, "\nURL: ", login_url)
  })

  # Step 3: Submit login credentials
  message("Submitting credentials...")
  tryCatch({
    # Build form data
    form_data <- list(
      email = username,
      password = password,
      `_token` = csrf_token
    )

    # Submit login form with the same session (preserving cookies)
    login_response <- session |>
      httr2::req_url_path(login_path) |>
      httr2::req_method("POST") |>
      httr2::req_cookie_preserve(cookie_file) |>
      httr2::req_body_form(!!!form_data) |>
      httr2::req_options(followlocation = FALSE) |>  # Don't follow redirects initially
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    # Check response
    status_code <- httr2::resp_status(login_response)
    message("Login response status: ", status_code)

    # Success indicators:
    # - 302/301 redirect (common for successful login)
    # - 200 with no error message
    # - Set-Cookie headers indicating session

    if (status_code %in% c(301, 302, 303, 307, 308)) {
      redirect_location <- httr2::resp_header(login_response, "location")
      message("Login successful! Redirected to: ", redirect_location)

      # Return the authenticated session object
      authenticated_session <- session |>
        httr2::req_cookie_preserve(cookie_file)

      return(authenticated_session)

    } else if (status_code == 200) {
      # Check if login was successful by looking for error indicators
      response_content <- httr2::resp_body_string(login_response)

      # Common error indicators
      has_errors <- grepl("invalid|incorrect|failed|error", response_content, ignore.case = TRUE)

      if (has_errors) {
        stop("Login failed: Invalid credentials or login error detected in response")
      } else {
        message("Login appears successful (status 200)")

        # Return the authenticated session
        authenticated_session <- session |>
          httr2::req_cookie_preserve(cookie_file)

        return(authenticated_session)
      }
    } else {
      # Get more details about the error
      response_content <- httr2::resp_body_string(login_response)
      stop("Unexpected response status: ", status_code,
           "\nResponse preview: ", substr(response_content, 1, 200))
    }

  }, error = function(e) {
    stop("Login failed: ", e$message)
  })
}


#' Extract CSRF Token from HTML
#'
#' Internal function to extract CSRF token from HTML content.
#' Looks in both meta tags and hidden form inputs.
#'
#' @param html Character string containing HTML content
#' @return Character string with CSRF token, or NULL if not found
#' @keywords internal
extract_csrf_token <- function(html) {
  # Try meta tag first (Laravel standard)
  meta_pattern <- '<meta name=["\']csrf-token["\']\\s+content=["\']([^"\']+)["\']'
  match <- regmatches(html, regexec(meta_pattern, html))
  if (length(match[[1]]) >= 2) {
    return(match[[1]][2])
  }

  # Try hidden input patterns
  patterns <- c(
    'name=["\']_token["\']\\s+value=["\']([^"\']+)["\']',
    'value=["\']([^"\']+)["\']\\s+name=["\']_token["\']',
    'name=["\']csrf_token["\']\\s+value=["\']([^"\']+)["\']',
    'value=["\']([^"\']+)["\']\\s+name=["\']csrf_token["\']'
  )

  for (pattern in patterns) {
    match <- regmatches(html, regexec(pattern, html))
    if (length(match[[1]]) >= 2) {
      return(match[[1]][2])
    }
  }

  return(NULL)
}


#' Test ABS Connection
#'
#' Simple function to test if the ABS server is reachable.
#'
#' @param base_url Character string with the base URL. Default is "https://abs.la.utexas.edu"
#'
#' @return Logical TRUE if server is reachable, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' test_abs_connection()
#' }
test_abs_connection <- function(base_url = "https://abs.la.utexas.edu", verbose = TRUE) {

  if (verbose) cat("Testing connection to:", base_url, "\n")

  tryCatch({
    # Force HTTP/1.1 to avoid HTTP/2 header parsing issues
    response <- httr2::request(base_url) |>
      httr2::req_user_agent("R httr2") |>
      httr2::req_timeout(30) |>
      httr2::req_options(
        ssl_verifypeer = 0,
        ssl_verifyhost = 0,
        http_version = 2  # Force HTTP/1.1 (0=default, 2=HTTP/1.1, 3=HTTP/2)
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (verbose) cat("Status:", status, "\n")

    if (status < 400) {
      if (verbose) cat("✓ Server is reachable\n")
      return(TRUE)
    } else {
      if (verbose) cat("✗ Server returned error:", status, "\n")
      return(FALSE)
    }

  }, error = function(e) {
    if (verbose) {
      cat("✗ Connection failed:", e$message, "\n")
      cat("\nError details:", conditionMessage(e), "\n")
      cat("\nPossible issues:\n")
      cat("- Network connectivity\n")
      cat("- VPN required?\n")
      cat("- Firewall blocking access\n")
      cat("- Invalid URL\n")
      cat("- SSL certificate issue\n")
      cat("\nTry:\n")
      cat("1. Connect to UT VPN if required\n")
      cat("2. Check if URL is accessible in browser\n")
      cat("3. Run: curl -v", base_url, "\n")
    }
    return(FALSE)
  })
}


#' Verify ABS Login
#'
#' Verifies that login was successful by attempting to access a protected page.
#'
#' @param session An httr2 request object returned from abs_login()
#' @param test_path Character string with a path to test. Default is "/admin/dashboard"
#'
#' @return Logical TRUE if login verification successful, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' session <- abs_login()
#' if (verify_abs_login(session)) {
#'   message("Successfully authenticated!")
#' }
#' }
verify_abs_login <- function(session, test_path = "/admin/dashboard") {

  tryCatch({
    response <- session |>
      httr2::req_url_path(test_path) |>
      httr2::req_options(http_version = 2) |>  # Force HTTP/1.1
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)

    # If we get 200, we're logged in
    # If we get redirected to login, we're not
    if (status == 200) {
      message("Login verification successful!")
      return(TRUE)
    } else if (status %in% c(301, 302, 303, 307, 308)) {
      redirect <- httr2::resp_header(response, "location")
      if (grepl("login", redirect, ignore.case = TRUE)) {
        warning("Login verification failed: Redirected to login page")
        return(FALSE)
      } else {
        message("Login verification successful (redirected to: ", redirect, ")")
        return(TRUE)
      }
    } else {
      warning("Login verification uncertain: Status ", status)
      return(FALSE)
    }

  }, error = function(e) {
    warning("Login verification failed: ", e$message)
    return(FALSE)
  })
}


#' Download CSV from ABS Admin
#'
#' Downloads a CSV file from the ABS admin portal using an authenticated session.
#'
#' @param session An httr2 request object returned from abs_login()
#' @param csv_path Character string with the path to the CSV endpoint.
#'   Default is "/admin/test/download-csv-all"
#' @param save_path Optional character string with file path to save the CSV.
#'   If NULL, returns the data frame without saving. Default is NULL.
#'
#' @return Data frame containing the CSV data
#' @export
#'
#' @examples
#' \dontrun{
#' session <- abs_login()
#' data <- download_abs_csv(session)
#' head(data)
#'
#' # Or save to file
#' data <- download_abs_csv(session, save_path = "output.csv")
#' }
download_abs_csv <- function(session,
                              csv_path = "/admin/test/download-csv-all",
                              save_path = NULL) {

  message("Downloading CSV from: ", csv_path)

  tryCatch({
    # Download the CSV file
    response <- session |>
      httr2::req_url_path(csv_path) |>
      httr2::req_options(http_version = 2) |>  # Force HTTP/1.1
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)

    if (status != 200) {
      if (status %in% c(301, 302, 303, 307, 308)) {
        redirect <- httr2::resp_header(response, "location")
        if (grepl("login", redirect, ignore.case = TRUE)) {
          stop("Not authenticated. Session may have expired. Please login again.")
        }
      }
      stop("Failed to download CSV. Status: ", status)
    }

    # Get the CSV content
    csv_content <- httr2::resp_body_string(response)

    # Parse CSV
    data <- utils::read.csv(text = csv_content, stringsAsFactors = FALSE)

    message("Successfully downloaded CSV: ", nrow(data), " rows, ", ncol(data), " columns")

    # Save to file if requested
    if (!is.null(save_path)) {
      utils::write.csv(data, save_path, row.names = FALSE)
      message("Saved to: ", save_path)
    }

    return(data)

  }, error = function(e) {
    stop("Failed to download CSV: ", e$message)
  })
}


#' Download and Preview ABS CSV
#'
#' Convenience function to download CSV and print the first few rows.
#'
#' @param session An httr2 request object returned from abs_login()
#' @param csv_path Character string with the path to the CSV endpoint.
#'   Default is "/admin/test/download-csv-all"
#' @param n Integer number of rows to display. Default is 6.
#'
#' @return Invisibly returns the full data frame
#' @export
#'
#' @examples
#' \dontrun{
#' session <- abs_login()
#' preview_abs_csv(session)
#' }
preview_abs_csv <- function(session,
                            csv_path = "/admin/test/download-csv-all",
                            n = 6) {

  data <- download_abs_csv(session, csv_path = csv_path)

  cat("\nFirst", min(n, nrow(data)), "rows:\n")
  print(head(data, n))
  cat("\nDimensions:", nrow(data), "rows x", ncol(data), "columns\n")

  invisible(data)
}

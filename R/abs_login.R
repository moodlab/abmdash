#' Login to ABS Admin Portal
#'
#' Authenticates to the ABS (Attention Bias Study) admin portal via Livewire
#' and returns an authenticated httr2 request object with session cookies.
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
#' session <- abs_login()
#' data <- download_abs_csv(session)
#' }
abs_login <- function(base_url = "https://abs.la.utexas.edu",
                      login_path = "/admin/login",
                      check_connection = TRUE) {

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 package is required. Please install it with: install.packages('httr2')")
  }

  username <- Sys.getenv("ABS_USERNAME")
  password <- Sys.getenv("ABS_PASSWORD")

  if (username == "" || is.na(username)) {
    stop("ABS_USERNAME environment variable is not set")
  }
  if (password == "" || is.na(password)) {
    stop("ABS_PASSWORD environment variable is not set")
  }

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

  cookie_file <- file.path(tempdir(), "abs_session_cookies.txt")
  base_req <- httr2::request(base_url) |>
    httr2::req_options(
      ssl_verifypeer = 0,
      ssl_verifyhost = 0,
      http_version = 2  # HTTP/1.1 — ABS server sends malformed HTTP/2 headers
    ) |>
    httr2::req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36") |>
    httr2::req_cookie_preserve(cookie_file)

  # Step 1: Fetch login page to get Livewire snapshot and CSRF token
  message("Fetching login page...")
  login_page <- base_req |>
    httr2::req_url_path(login_path) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(login_page)
  if (status >= 400) {
    stop("Login page returned error status: ", status)
  }

  page_content <- httr2::resp_body_string(login_page)

  # Extract Livewire snapshot (contains component state)
  snapshot_json <- extract_livewire_snapshot(page_content)
  if (is.null(snapshot_json)) {
    stop("No Livewire snapshot found on login page")
  }

  # Extract CSRF token from Livewire script tag
  csrf_token <- extract_csrf_token(page_content)
  if (is.null(csrf_token)) {
    stop("No CSRF token found on login page")
  }
  message("Found CSRF token: ", substr(csrf_token, 1, 20), "...")

  # Step 2: Authenticate via Livewire update endpoint
  message("Submitting credentials via Livewire...")
  payload <- list(
    `_token` = jsonlite::unbox(csrf_token),
    components = list(
      list(
        snapshot = jsonlite::unbox(snapshot_json),
        updates = list(
          `data.email` = jsonlite::unbox(username),
          `data.password` = jsonlite::unbox(password)
        ),
        calls = list(
          list(
            path = jsonlite::unbox(""),
            method = jsonlite::unbox("authenticate"),
            params = list()
          )
        )
      )
    )
  )

  auth_response <- base_req |>
    httr2::req_url_path("/livewire/update") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `X-CSRF-TOKEN` = csrf_token,
      `X-Livewire` = "true"
    ) |>
    httr2::req_body_json(payload, auto_unbox = FALSE) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  auth_status <- httr2::resp_status(auth_response)
  if (auth_status != 200) {
    stop("Livewire authentication request failed with status: ", auth_status)
  }

  # Parse Livewire response — successful login has redirect effect
  auth_body <- httr2::resp_body_string(auth_response)
  auth_result <- jsonlite::fromJSON(auth_body, simplifyVector = FALSE)

  has_redirect <- FALSE
  if (!is.null(auth_result$components)) {
    for (comp in auth_result$components) {
      if (!is.null(comp$effects$redirect)) {
        has_redirect <- TRUE
        message("Login successful! Redirected to: ", comp$effects$redirect)
        break
      }
    }
  }

  if (!has_redirect) {
    stop("Login failed: no redirect in response (likely invalid credentials)")
  }

  return(base_req)
}


#' Extract Livewire Snapshot from HTML
#'
#' Extracts the first wire:snapshot attribute from the login page HTML.
#'
#' @param html Character string containing HTML content
#' @return Decoded JSON string of the snapshot, or NULL
#' @keywords internal
extract_livewire_snapshot <- function(html) {
  match <- regmatches(html, regexpr('wire:snapshot="([^"]+)"', html))
  if (length(match) == 0 || match == "") return(NULL)

  encoded <- sub('wire:snapshot="', "", sub('"$', "", match))
  gsub("&quot;", '"', encoded)
}


#' Extract CSRF Token from HTML
#'
#' Extracts CSRF token from Livewire data-csrf attribute, meta tags,
#' or hidden form inputs.
#'
#' @param html Character string containing HTML content
#' @return Character string with CSRF token, or NULL if not found
#' @keywords internal
extract_csrf_token <- function(html) {
  # Try Livewire data-csrf attribute first (Livewire v3)
  lw_match <- regmatches(html, regexec('data-csrf="([^"]+)"', html))
  if (length(lw_match[[1]]) >= 2) {
    return(lw_match[[1]][2])
  }

  # Try meta tag (Laravel standard)
  meta_pattern <- '<meta name=["\']csrf-token["\']\\s+content=["\']([^"\']+)["\']'
  match <- regmatches(html, regexec(meta_pattern, html))
  if (length(match[[1]]) >= 2) {
    return(match[[1]][2])
  }

  # Try hidden input patterns
  patterns <- c(
    'name=["\']_token["\']\\s+value=["\']([^"\']+)["\']',
    'value=["\']([^"\']+)["\']\\s+name=["\']_token["\']'
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
#' @param verbose Logical, whether to print diagnostic messages. Default is TRUE
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
    response <- httr2::request(base_url) |>
      httr2::req_user_agent("R httr2") |>
      httr2::req_timeout(30) |>
      httr2::req_options(
        ssl_verifypeer = 0,
        ssl_verifyhost = 0,
        http_version = 2  # HTTP/1.1 — ABS server sends malformed HTTP/2 headers
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (verbose) cat("Status:", status, "\n")

    if (status < 400) {
      if (verbose) cat("Server is reachable\n")
      return(TRUE)
    } else {
      if (verbose) cat("Server returned error:", status, "\n")
      return(FALSE)
    }
  }, error = function(e) {
    if (verbose) {
      cat("Connection failed:", e$message, "\n")
    }
    return(FALSE)
  })
}


#' Verify ABS Login
#'
#' Verifies that login was successful by attempting to access a protected page.
#'
#' @param session An httr2 request object returned from abs_login()
#' @param test_path Character string with a path to test. Default is "/admin"
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
verify_abs_login <- function(session, test_path = "/admin") {
  tryCatch({
    response <- session |>
      httr2::req_url_path(test_path) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)

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
#' Downloads the complete test data CSV from the ABS admin portal using
#' the Livewire downloadCompleteCsv action.
#'
#' @param session An httr2 request object returned from abs_login()
#' @param tests_path Character string with the tests page path.
#'   Default is "/admin/tests"
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
#' }
download_abs_csv <- function(session,
                              tests_path = "/admin/tests",
                              save_path = NULL,
                              ...) {

  message("Fetching tests page...")

  # Get the tests page to obtain the Livewire component snapshot
  tests_resp <- session |>
    httr2::req_url_path(tests_path) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  tests_status <- httr2::resp_status(tests_resp)
  if (tests_status != 200) {
    if (tests_status %in% c(301, 302, 303, 307, 308)) {
      redirect <- httr2::resp_header(tests_resp, "location")
      if (grepl("login", redirect, ignore.case = TRUE)) {
        stop("Not authenticated. Session may have expired. Please login again.")
      }
    }
    stop("Failed to load tests page. Status: ", tests_status)
  }

  tests_body <- httr2::resp_body_string(tests_resp)

  # Find the list-tests Livewire component snapshot
  all_snapshots <- regmatches(tests_body, gregexpr('wire:snapshot="([^"]+)"', tests_body))
  list_tests_snapshot <- NULL

  for (snapshot_attr in all_snapshots[[1]]) {
    encoded <- sub('wire:snapshot="', "", sub('"$', "", snapshot_attr))
    decoded <- gsub("&quot;", '"', encoded)
    if (grepl("list-tests", decoded, ignore.case = TRUE)) {
      list_tests_snapshot <- decoded
      break
    }
  }

  if (is.null(list_tests_snapshot)) {
    stop("Could not find list-tests component on tests page")
  }

  # Extract CSRF token from the tests page
  csrf_token <- extract_csrf_token(tests_body)
  if (is.null(csrf_token)) {
    stop("No CSRF token found on tests page")
  }

  # Call the downloadCompleteCsv Livewire action
  message("Downloading CSV via Livewire action...")
  payload <- list(
    `_token` = jsonlite::unbox(csrf_token),
    components = list(
      list(
        snapshot = jsonlite::unbox(list_tests_snapshot),
        updates = list(),
        calls = list(
          list(
            path = jsonlite::unbox(""),
            method = jsonlite::unbox("mountAction"),
            params = list(
              jsonlite::unbox("downloadCompleteCsv"),
              list(),
              list(table = jsonlite::unbox(TRUE))
            )
          )
        )
      )
    )
  )

  download_resp <- session |>
    httr2::req_url_path("/livewire/update") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `X-CSRF-TOKEN` = csrf_token,
      `X-Livewire` = "true"
    ) |>
    httr2::req_body_json(payload, auto_unbox = FALSE) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(download_resp) != 200) {
    stop("CSV download request failed with status: ", httr2::resp_status(download_resp))
  }

  # Parse Livewire response and extract base64-encoded CSV from download effect
  download_body <- httr2::resp_body_string(download_resp)
  download_result <- jsonlite::fromJSON(download_body, simplifyVector = FALSE)

  csv_content <- NULL
  for (comp in download_result$components) {
    if (!is.null(comp$effects$download$content)) {
      csv_content <- rawToChar(jsonlite::base64_dec(comp$effects$download$content))
      break
    }
  }

  if (is.null(csv_content)) {
    stop("No CSV download content in Livewire response")
  }

  # Parse CSV
  data <- utils::read.csv(text = csv_content, stringsAsFactors = FALSE)
  message("Successfully downloaded CSV: ", nrow(data), " rows, ", ncol(data), " columns")

  if (!is.null(save_path)) {
    utils::write.csv(data, save_path, row.names = FALSE)
    message("Saved to: ", save_path)
  }

  return(data)
}


#' Download and Preview ABS CSV
#'
#' Convenience function to download CSV and print the first few rows.
#'
#' @param session An httr2 request object returned from abs_login()
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
preview_abs_csv <- function(session, n = 6) {
  data <- download_abs_csv(session)

  cat("\nFirst", min(n, nrow(data)), "rows:\n")
  print(utils::head(data, n))
  cat("\nDimensions:", nrow(data), "rows x", ncol(data), "columns\n")

  invisible(data)
}

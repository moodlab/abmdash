#' Read Google Sheet
#'
#' Reads data from a Google Sheet using OAuth2 authentication.
#'
#' @param sheet_url Character string with the Google Sheet URL
#' @param sheet_name Character string with the name of the sheet/tab to read.
#'   If NULL, reads the first sheet. Default is NULL.
#' @param range Character string specifying the range to read (e.g., "A1:D10").
#'   If NULL, reads all data. Default is NULL.
#'
#' @return Data frame containing the sheet data
#' @export
#'
#' @examples
#' \dontrun{
#' # Read entire first sheet
#' data <- read_google_sheet(
#'   "https://docs.google.com/spreadsheets/d/SHEET_ID/edit"
#' )
#'
#' # Read specific sheet by name
#' data <- read_google_sheet(
#'   "https://docs.google.com/spreadsheets/d/SHEET_ID/edit",
#'   sheet_name = "Sheet1"
#' )
#'
#' # Read specific range
#' data <- read_google_sheet(
#'   "https://docs.google.com/spreadsheets/d/SHEET_ID/edit",
#'   range = "A1:D10"
#' )
#' }
read_google_sheet <- function(sheet_url, sheet_name = NULL, range = NULL) {

  # Check if required packages are available
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 package is required. Please install it with: install.packages('httr2')")
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required. Please install it with: install.packages('jsonlite')")
  }

  # Extract spreadsheet ID from URL
  sheet_id <- extract_sheet_id(sheet_url)

  # Get access token
  access_token <- get_google_sheets_access_token()

  # Build the range parameter
  range_param <- if (!is.null(sheet_name) && !is.null(range)) {
    paste0(sheet_name, "!", range)
  } else if (!is.null(sheet_name)) {
    sheet_name
  } else if (!is.null(range)) {
    range
  } else {
    NULL
  }

  # Build API URL
  if (!is.null(range_param)) {
    api_url <- sprintf(
      "https://sheets.googleapis.com/v4/spreadsheets/%s/values/%s",
      sheet_id,
      utils::URLencode(range_param, reserved = TRUE)
    )
  } else {
    # Get the first sheet if no range specified
    api_url <- sprintf(
      "https://sheets.googleapis.com/v4/spreadsheets/%s/values/Sheet1",
      sheet_id
    )
  }

  # Make the API request
  tryCatch({
    response <- httr2::request(api_url) |>
      httr2::req_headers(Authorization = paste("Bearer", access_token)) |>
      httr2::req_perform()

    # Parse the JSON response
    sheet_data <- httr2::resp_body_json(response)

    # Convert to data frame
    if (!is.null(sheet_data$values) && length(sheet_data$values) > 0) {
      # First row as column names
      col_names <- unlist(sheet_data$values[[1]])

      # Remaining rows as data
      if (length(sheet_data$values) > 1) {
        data_rows <- sheet_data$values[2:length(sheet_data$values)]

        # Convert to data frame
        # Pad shorter rows with NA
        max_cols <- length(col_names)
        data_rows <- lapply(data_rows, function(row) {
          row_vec <- unlist(row)
          if (length(row_vec) < max_cols) {
            row_vec <- c(row_vec, rep(NA, max_cols - length(row_vec)))
          }
          row_vec
        })

        df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
        colnames(df) <- col_names
        return(df)
      } else {
        # Only header row
        df <- as.data.frame(matrix(ncol = length(col_names), nrow = 0))
        colnames(df) <- col_names
        return(df)
      }
    } else {
      warning("No data found in the sheet")
      return(data.frame())
    }

  }, error = function(e) {
    stop("Google Sheets API call failed: ", e$message)
  })
}


#' Extract Sheet ID from URL
#'
#' Internal function to extract the spreadsheet ID from a Google Sheets URL.
#'
#' @param url Character string with the Google Sheets URL
#' @return Character string containing the spreadsheet ID
#' @keywords internal
extract_sheet_id <- function(url) {
  # Pattern: /d/{sheet_id}/
  # Note: hyphen must be at end of character class to avoid being interpreted as range
  pattern <- "/d/([a-zA-Z0-9_-]+)"
  match <- regmatches(url, regexec(pattern, url))

  if (length(match[[1]]) < 2) {
    stop("Could not extract spreadsheet ID from URL: ", url)
  }

  return(match[[1]][2])
}


#' Get Google Sheets Access Token
#'
#' Internal function to get an access token using service account credentials.
#' Uses the same service account as the Google Calendar API.
#'
#' @return Character string containing the access token
#' @keywords internal
get_google_sheets_access_token <- function() {

  # Check if jose package is available
  if (!requireNamespace("jose", quietly = TRUE)) {
    stop("jose package is required for JWT signing. Please install it with: install.packages('jose')")
  }

  # Get service account JSON from environment
  service_account_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
  if (service_account_json == "" || is.na(service_account_json)) {
    stop("GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty")
  }

  # Parse the service account JSON
  tryCatch({
    # Remove outer quotes and unescape inner quotes (from .Renviron formatting)
    clean_json <- gsub('^"|"$', '', service_account_json)
    clean_json <- gsub('\\\\"', '"', clean_json)
    service_account <- jsonlite::fromJSON(clean_json)
  }, error = function(e) {
    stop("Failed to parse GOOGLE_SERVICE_ACCOUNT_JSON: ", e$message)
  })

  # Create JWT payload for service account authentication
  scope <- "https://www.googleapis.com/auth/spreadsheets.readonly"

  now <- as.numeric(Sys.time())
  claim <- jose::jwt_claim(
    iss = service_account$client_email,
    scope = scope,
    aud = "https://oauth2.googleapis.com/token",
    exp = now + 3600,  # 1 hour
    iat = now
  )

  # Create and sign JWT using the private key
  # Fix escaped newlines in the private key
  private_key <- gsub("\\\\n", "\n", service_account$private_key)
  private_key_obj <- openssl::read_key(private_key)
  jwt_token <- jose::jwt_encode_sig(claim, key = private_key_obj)

  # Request access token from Google
  token_response <- httr2::request("https://oauth2.googleapis.com/token") |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion = jwt_token
    ) |>
    httr2::req_perform()

  # Extract access token from response
  token_data <- tryCatch({
    httr2::resp_body_json(token_response)
  }, error = function(e) {
    # If JSON parsing fails, get the raw response
    raw_response <- httr2::resp_body_string(token_response)
    stop("Failed to parse token response as JSON. Raw response: ", substr(raw_response, 1, 200))
  })

  if (!is.list(token_data)) {
    stop("Token response is not a list. Type: ", class(token_data), " Content: ", substr(as.character(token_data), 1, 100))
  }

  if (!"access_token" %in% names(token_data)) {
    available_fields <- paste(names(token_data), collapse = ", ")
    stop("No access_token in response. Available fields: ", available_fields)
  }

  return(token_data$access_token)
}


#' Print Head of Google Sheet
#'
#' Convenience function to read and print the first few rows of a Google Sheet.
#'
#' @param sheet_url Character string with the Google Sheet URL
#' @param n Integer number of rows to display. Default is 6.
#' @param sheet_name Character string with the name of the sheet/tab to read.
#'   If NULL, reads the first sheet. Default is NULL.
#'
#' @return Invisibly returns the full data frame
#' @export
#'
#' @examples
#' \dontrun{
#' print_sheet_head(
#'   "https://docs.google.com/spreadsheets/d/SHEET_ID/edit"
#' )
#' }
print_sheet_head <- function(sheet_url, n = 6, sheet_name = NULL) {
  data <- read_google_sheet(sheet_url, sheet_name = sheet_name)

  cat("First", min(n, nrow(data)), "rows of the sheet:\n")
  print(head(data, n))
  cat("\nDimensions:", nrow(data), "rows x", ncol(data), "columns\n")

  invisible(data)
}


#' Check for Recent Google Sheet Responses
#'
#' Reads a Google Sheet and checks for responses within a specified time period.
#' Useful for monitoring issue reports or feedback forms.
#'
#' @param sheet_url Character string with the Google Sheet URL
#' @param days_back Integer number of days to look back. Default is 14.
#' @param timestamp_col Character string or integer specifying the timestamp column.
#'   Can be column name or column number. Default is 1 (first column).
#' @param sheet_name Character string with the name of the sheet/tab to read.
#'   If NULL, reads the first sheet. Default is NULL.
#'
#' @return List containing:
#'   \itemize{
#'     \item has_recent: Logical, TRUE if there are recent responses
#'     \item recent_count: Integer, number of recent responses
#'     \item recent_data: Data frame of recent responses
#'     \item all_data: Data frame of all responses
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Check for responses in past 14 days
#' result <- check_recent_responses(
#'   "https://docs.google.com/spreadsheets/d/SHEET_ID/edit"
#' )
#'
#' if (result$has_recent) {
#'   cat("Found", result$recent_count, "recent responses\n")
#'   print(result$recent_data)
#' }
#'
#' # Check past 7 days with specific timestamp column
#' result <- check_recent_responses(
#'   "https://docs.google.com/spreadsheets/d/SHEET_ID/edit",
#'   days_back = 7,
#'   timestamp_col = "Timestamp"
#' )
#' }
check_recent_responses <- function(sheet_url,
                                   days_back = 14,
                                   timestamp_col = 1,
                                   sheet_name = NULL) {

  # Read the sheet
  message("Reading Google Sheet...")
  data <- read_google_sheet(sheet_url, sheet_name = sheet_name)

  if (nrow(data) == 0) {
    message("No data found in sheet")
    return(list(
      has_recent = FALSE,
      recent_count = 0,
      recent_data = data.frame(),
      all_data = data
    ))
  }

  # Get timestamp column
  if (is.character(timestamp_col)) {
    if (!timestamp_col %in% colnames(data)) {
      stop("Timestamp column '", timestamp_col, "' not found. Available columns: ",
           paste(colnames(data), collapse = ", "))
    }
    timestamps <- data[[timestamp_col]]
  } else {
    if (timestamp_col > ncol(data) || timestamp_col < 1) {
      stop("Timestamp column index ", timestamp_col, " out of range (1-", ncol(data), ")")
    }
    timestamps <- data[[timestamp_col]]
  }

  # Parse timestamps - try multiple formats
  parsed_timestamps <- tryCatch({
    # Try standard formats
    as.POSIXct(timestamps, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
  }, error = function(e) {
    tryCatch({
      as.POSIXct(timestamps, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    }, error = function(e2) {
      tryCatch({
        as.POSIXct(timestamps, tz = "UTC")
      }, error = function(e3) {
        stop("Could not parse timestamps. First timestamp value: ", timestamps[1],
             "\nPlease specify the correct timestamp column.")
      })
    })
  })

  # Calculate cutoff date
  cutoff_date <- Sys.time() - (days_back * 24 * 60 * 60)

  # Filter recent responses
  recent_mask <- !is.na(parsed_timestamps) & parsed_timestamps >= cutoff_date
  recent_data <- data[recent_mask, , drop = FALSE]
  recent_count <- nrow(recent_data)

  # Report findings
  message("Total responses: ", nrow(data))
  message("Recent responses (past ", days_back, " days): ", recent_count)

  if (recent_count > 0) {
    message("\nRecent response dates:")
    recent_dates <- parsed_timestamps[recent_mask]
    for (i in seq_along(recent_dates)) {
      message("  ", format(recent_dates[i], "%Y-%m-%d %H:%M:%S"))
    }
  }

  return(list(
    has_recent = recent_count > 0,
    recent_count = recent_count,
    recent_data = recent_data,
    all_data = data,
    cutoff_date = cutoff_date
  ))
}


#' Check Participant Issues Sheet
#'
#' Convenience function specifically for checking the participant issues Google Sheet.
#'
#' @param days_back Integer number of days to look back. Default is 14.
#'
#' @return List containing recent issues information
#' @export
#'
#' @examples
#' \dontrun{
#' # Check for new issues
#' issues <- check_participant_issues()
#'
#' if (issues$has_recent) {
#'   cat("⚠️ Found", issues$recent_count, "new issues!\n")
#'   print(issues$recent_data)
#' } else {
#'   cat("✓ No new issues in the past", days_back, "days\n")
#' }
#' }
check_participant_issues <- function(days_back = 14, verbose = TRUE) {

  sheet_url <- "https://docs.google.com/spreadsheets/d/11FAAY4cUvqpW7QN7k-mcpOCx3WfrcA6GNeTXgBT1St8/edit?gid=2031348125#gid=2031348125"

  if (verbose) {
    cat("Checking for participant issues in the past", days_back, "days...\n\n")
  }

  result <- suppressMessages({
    check_recent_responses(
      sheet_url = sheet_url,
      days_back = days_back,
      timestamp_col = 1,  # Assuming timestamp is in first column
      sheet_name = "Form Responses 1"  # Specify the correct sheet tab name
    )
  })

  if (verbose) {
    cat("\n")
    if (result$has_recent) {
      cat("Found", result$recent_count, "recent issue(s)!\n\n")
      cat("Recent issues:\n")
      print(result$recent_data)
    } else {
      cat("No new issues reported in the past", days_back, "days\n")
    }
  }

  invisible(result)
}

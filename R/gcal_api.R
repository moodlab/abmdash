#' Get Google Calendar Events
#'
#' Retrieves events from Google Calendar using service account authentication.
#'
#' @param calendar_id Character string with the calendar ID (email address). 
#'   Defaults to "primary" for the service account's primary calendar.
#' @param time_min Character string for start time filter (RFC3339 format, e.g., "2024-01-01T00:00:00Z").
#' @param time_max Character string for end time filter (RFC3339 format, e.g., "2024-12-31T23:59:59Z").
#' @param max_results Integer for maximum number of events to return. Default is 10.
#'
#' @return List containing calendar events
#' @export
#'
#' @examples
#' \dontrun{
#' # Get today's events
#' today_events <- get_calendar_events(
#'   time_min = paste0(Sys.Date(), "T00:00:00Z"),
#'   time_max = paste0(Sys.Date(), "T23:59:59Z")
#' )
#' 
#' # Get events from a specific calendar
#' events <- get_calendar_events(
#'   calendar_id = "shared-calendar@example.com",
#'   max_results = 50
#' )
#' }
get_calendar_events <- function(calendar_id = "primary", time_min = NULL, time_max = NULL, max_results = 10) {
  
  # Check if required packages are available
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 package is required. Please install it with: install.packages('httr2')")
  }
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required. Please install it with: install.packages('jsonlite')")
  }
  
  # Get service account JSON from environment
  service_account_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
  if (service_account_json == "" || is.na(service_account_json)) {
    stop("GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty")
  }
  
  # Parse the service account JSON
  service_account <- parse_service_account_json(service_account_json)
  
  # Get access token using service account
  access_token <- get_google_access_token(service_account)
  
  # Build query parameters
  params <- list(
    maxResults = max_results
  )
  
  if (!is.null(time_min)) {
    params$timeMin <- time_min
  }
  
  if (!is.null(time_max)) {
    params$timeMax <- time_max
  }
  
  # Calendar API endpoint
  api_url <- paste0("https://www.googleapis.com/calendar/v3/calendars/", 
                   utils::URLencode(calendar_id, reserved = TRUE), "/events")
  
  # Make the API request
  tryCatch({
    response <- httr2::request(api_url) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_headers(Authorization = paste("Bearer", access_token)) |>
      httr2::req_perform()
    
    # Parse the JSON response
    events_data <- httr2::resp_body_json(response)
    
    return(events_data)
    
  }, error = function(e) {
    stop("Google Calendar API call failed: ", e$message)
  })
}

#' Parse Service Account JSON
#'
#' Internal function to parse the GOOGLE_SERVICE_ACCOUNT_JSON environment
#' variable value into a service account list. Removes outer quotes and
#' unescapes inner quotes (from .Renviron formatting) before parsing.
#'
#' This is the gcal module's own copy — gsheet_api.R keeps a separate copy so
#' the modules stay independent (merging them breaks the wrong-mock tripwire in
#' the behavior-lock tests).
#'
#' @param service_account_json Character scalar raw value from the environment.
#' @return List containing the parsed service account.
#' @keywords internal
parse_service_account_json <- function(service_account_json) {
  tryCatch({
    # Remove outer quotes and unescape inner quotes (from .Renviron formatting)
    clean_json <- gsub('^"|"$', '', service_account_json)
    clean_json <- gsub('\\\\"', '"', clean_json)
    jsonlite::fromJSON(clean_json)
  }, error = function(e) {
    stop("Failed to parse GOOGLE_SERVICE_ACCOUNT_JSON: ", e$message)
  })
}

#' Get Google Access Token
#'
#' Internal function to get an access token using service account credentials.
#'
#' @param service_account List containing parsed service account JSON
#' @return Character string containing the access token
#' @keywords internal
get_google_access_token <- function(service_account) {
  
  # Check if jose package is available
  if (!requireNamespace("jose", quietly = TRUE)) {
    stop("jose package is required for JWT signing. Please install it with: install.packages('jose')")
  }
  
  # Create JWT payload for service account authentication
  scope <- "https://www.googleapis.com/auth/calendar.readonly"
  
  sign_and_exchange_jwt(service_account, scope)
}

#' Sign and Exchange JWT
#'
#' Internal function to build the JWT claim for a service account, sign it with
#' the private key, and exchange it for an access token at the OAuth2 token
#' endpoint.
#'
#' This is the gcal module's own copy — gsheet_api.R keeps a separate copy so
#' the modules stay independent (merging them breaks the wrong-mock tripwire in
#' the behavior-lock tests).
#'
#' @param service_account List containing parsed service account JSON.
#' @param scope Character scalar OAuth2 scope to request.
#' @return Character string containing the access token.
#' @keywords internal
sign_and_exchange_jwt <- function(service_account, scope) {
  
  # Create JWT payload for service account authentication
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
  
  token_data$access_token
}

#' List Google Calendars
#'
#' Lists all calendars the service account has access to.
#'
#' @return List containing calendar information
#' @export
#'
#' @examples
#' \dontrun{
#' calendars <- list_calendars()
#' }
list_calendars <- function() {
  
  # Check if required packages are available
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 package is required. Please install it with: install.packages('httr2')")
  }
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required. Please install it with: install.packages('jsonlite')")
  }
  
  # Get service account JSON from environment
  service_account_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
  if (service_account_json == "" || is.na(service_account_json)) {
    stop("GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty")
  }
  
  # Parse the service account JSON
  service_account <- parse_service_account_json(service_account_json)
  
  # Get access token using service account
  access_token <- get_google_access_token(service_account)
  
  # Calendar List API endpoint
  api_url <- "https://www.googleapis.com/calendar/v3/users/me/calendarList"
  
  # Make the API request
  tryCatch({
    response <- httr2::request(api_url) |>
      httr2::req_headers(Authorization = paste("Bearer", access_token)) |>
      httr2::req_perform()
    
    # Parse the JSON response
    calendars_data <- httr2::resp_body_json(response)
    
    return(calendars_data)

  }, error = function(e) {
    stop("Google Calendar List API call failed: ", e$message)
  })
}


#' Get Combined Calendar Events from Multiple Calendars
#'
#' Fetches events from multiple Google Calendar IDs and merges them
#' into a single result. If one calendar errors, events from the
#' other calendars are still returned.
#'
#' @param calendar_ids Character vector of calendar IDs
#' @param time_min RFC3339 format start time filter
#' @param time_max RFC3339 format end time filter
#' @param max_results Integer maximum events per calendar. Default is 100
#'
#' @return List with \code{$items} containing all events from all calendars
#' @export
#'
#' @examples
#' \dontrun{
#' events <- get_combined_calendar_events(
#'   calendar_ids = c("calendar1@group.calendar.google.com",
#'                    "calendar2@group.calendar.google.com"),
#'   time_min = paste0(Sys.Date(), "T00:00:00Z"),
#'   time_max = paste0(Sys.Date() + 7, "T23:59:59Z")
#' )
#' }
get_combined_calendar_events <- function(calendar_ids,
                                          time_min = NULL,
                                          time_max = NULL,
                                          max_results = 100) {

  # Fetch one calendar's items, swallowing per-calendar errors: a failing
  # calendar drops its events but does not abort the merge.
  fetch_calendar_items <- function(cal_id) {
    tryCatch({
      result <- get_calendar_events(
        calendar_id = cal_id,
        time_min = time_min,
        time_max = time_max,
        max_results = max_results
      )
      if (is.null(result$items)) NULL else result$items
    }, error = function(e) {
      message("Warning: failed to fetch calendar ", cal_id, ": ", e$message)
      NULL
    })
  }

  # Preserve calendar_ids iteration order, fixture order within each calendar,
  # and duplicates — NO sorting, NO deduplication (locked by
  # test-combined-calendar.R). unlist(recursive = FALSE) flattens exactly one
  # level: NULL (error/empty calendars) drops out and event lists concatenate
  # in order; unname() keeps the merged items unnamed as before.
  list(items = unname(unlist(lapply(calendar_ids, fetch_calendar_items), recursive = FALSE)))
}

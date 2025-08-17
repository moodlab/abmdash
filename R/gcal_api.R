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
  tryCatch({
    # Remove outer quotes and unescape inner quotes (from .Renviron formatting)
    clean_json <- gsub('^"|"$', '', service_account_json)
    clean_json <- gsub('\\\\"', '"', clean_json)
    service_account <- jsonlite::fromJSON(clean_json)
  }, error = function(e) {
    stop("Failed to parse GOOGLE_SERVICE_ACCOUNT_JSON: ", e$message)
  })
  
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
  tryCatch({
    # Remove outer quotes and unescape inner quotes (from .Renviron formatting)
    clean_json <- gsub('^"|"$', '', service_account_json)
    clean_json <- gsub('\\\\"', '"', clean_json)
    service_account <- jsonlite::fromJSON(clean_json)
  }, error = function(e) {
    stop("Failed to parse GOOGLE_SERVICE_ACCOUNT_JSON: ", e$message)
  })
  
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
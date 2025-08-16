#' Get REDCap API Token from Environment
#'
#' Retrieves the REDCap API token from the REDCAP_API_TOKEN environment variable.
#'
#' @return Character string containing the API token
#' @keywords internal
get_redcap_token <- function() {
  token <- Sys.getenv("REDCAP_API_TOKEN")
  if (token == "" || is.na(token)) {
    stop("REDCAP_API_TOKEN environment variable is not set or is empty")
  }
  token
}

#' Call REDCap API
#'
#' Makes a request to the REDCap API using the configured token.
#'
#' @param content Character string specifying what to export (e.g., "record", "metadata", "instrument")
#' @param format Character string specifying the format ("json", "csv", "xml"). Default is "json"
#' @param ... Additional parameters to pass to the REDCap API
#'
#' @return Parsed response from REDCap API
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all records
#' records <- call_redcap_api("record")
#' 
#' # Get metadata
#' metadata <- call_redcap_api("metadata")
#' 
#' # Get specific instrument
#' instrument_data <- call_redcap_api("record", forms = "baseline_form")
#' }
call_redcap_api <- function(content = "record", format = "json", ...) {
  # Check if httr2 is available
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 package is required. Please install it with: install.packages('httr2')")
  }
  
  token <- get_redcap_token()
  
  # Build the request body
  body_params <- list(
    token = token,
    content = content,
    format = format,
    returnFormat = format,
    ...
  )
  
  # Make the API request
  api_url <- "https://redcap.prc.utexas.edu/redcap/api/"
  
  tryCatch({
    response <- httr2::request(api_url) |>
      httr2::req_method("POST") |>
      httr2::req_body_form(!!!body_params) |>
      httr2::req_perform()
    
    # Parse response based on format
    if (format == "json") {
      content <- httr2::resp_body_json(response)
    } else if (format == "csv") {
      content <- httr2::resp_body_string(response)
      content <- utils::read.csv(text = content, stringsAsFactors = FALSE)
    } else {
      content <- httr2::resp_body_string(response)
    }
    
    return(content)
    
  }, error = function(e) {
    stop("REDCap API call failed: ", e$message)
  })
}

#' Get REDCap Records
#'
#' Convenience function to retrieve records from REDCap.
#'
#' @param fields Character vector of field names to retrieve. If NULL, all fields are returned.
#' @param forms Character vector of form names to retrieve. If NULL, all forms are returned.
#' @param records Character vector of record IDs to retrieve. If NULL, all records are returned.
#' @param events Character vector of event names to retrieve (for longitudinal projects).
#' @param format Character string specifying the format ("json", "csv"). Default is "json"
#'
#' @return Data frame or list containing the requested records
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all records
#' all_records <- get_redcap_records()
#' 
#' # Get specific fields
#' subset_records <- get_redcap_records(fields = c("record_id", "age", "gender"))
#' 
#' # Get specific forms
#' baseline_data <- get_redcap_records(forms = c("baseline_form"))
#' }
get_redcap_records <- function(fields = NULL, forms = NULL, records = NULL, 
                               events = NULL, format = "json") {
  
  # Build parameters
  params <- list()
  
  if (!is.null(fields)) {
    params$fields <- paste(fields, collapse = ",")
  }
  
  if (!is.null(forms)) {
    params$forms <- paste(forms, collapse = ",")
  }
  
  if (!is.null(records)) {
    params$records <- paste(records, collapse = ",")
  }
  
  if (!is.null(events)) {
    params$events <- paste(events, collapse = ",")
  }
  
  # Call the API
  do.call(call_redcap_api, c(list(content = "record", format = format), params))
}

#' Get REDCap Metadata
#'
#' Convenience function to retrieve metadata (data dictionary) from REDCap.
#'
#' @param format Character string specifying the format ("json", "csv"). Default is "json"
#'
#' @return Data frame or list containing the project metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # Get metadata
#' metadata <- get_redcap_metadata()
#' }
get_redcap_metadata <- function(format = "json") {
  call_redcap_api(content = "metadata", format = format)
}
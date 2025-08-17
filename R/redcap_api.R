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

#' Get Survey Completion Status
#'
#' Retrieves survey completion timestamps and status for all participants.
#'
#' @param surveys Character vector of survey instrument names to check. If NULL, all surveys are included.
#' @param records Character vector of record IDs to check. If NULL, all records are included.
#' @param format Character string specifying the format ("json", "csv"). Default is "json"
#'
#' @return Data frame with columns: record_id, survey_instrument, survey_timestamp, survey_complete
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all survey completions
#' completions <- get_survey_completions()
#' 
#' # Get specific surveys
#' baseline_completions <- get_survey_completions(surveys = c("baseline_survey", "followup_survey"))
#' 
#' # Get completions for specific participants
#' participant_completions <- get_survey_completions(records = c("001", "002", "003"))
#' }
get_survey_completions <- function(surveys = NULL, records = NULL, format = "json") {
  
  # Get the metadata to identify survey instruments and timestamp fields
  metadata <- get_redcap_metadata(format = "json")
  
  if (is.null(metadata) || length(metadata) == 0) {
    stop("Could not retrieve metadata from REDCap")
  }
  
  # Convert to data frame if it's a list
  if (is.list(metadata) && !is.data.frame(metadata)) {
    metadata_df <- do.call(rbind, lapply(metadata, function(x) {
      data.frame(
        field_name = x$field_name %||% "",
        form_name = x$form_name %||% "",
        field_type = x$field_type %||% "",
        stringsAsFactors = FALSE
      )
    }))
  } else {
    metadata_df <- metadata
  }
  
  # Find survey timestamp and completion fields
  # REDCap automatically creates fields like: [instrument]_timestamp and [instrument]_complete
  survey_fields <- metadata_df[grepl("_(timestamp|complete)$", metadata_df$field_name), ]
  
  # Extract unique instrument names
  instrument_names <- unique(gsub("_(timestamp|complete)$", "", survey_fields$field_name))
  
  # Filter to requested surveys if specified
  if (!is.null(surveys)) {
    instrument_names <- intersect(instrument_names, surveys)
  }
  
  if (length(instrument_names) == 0) {
    warning("No survey instruments found")
    return(data.frame(
      record_id = character(0),
      survey_instrument = character(0),
      survey_timestamp = character(0),
      survey_complete = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Build field list for timestamp and complete fields
  timestamp_fields <- paste0(instrument_names, "_timestamp")
  complete_fields <- paste0(instrument_names, "_complete")
  all_fields <- c("record_id", timestamp_fields, complete_fields)
  
  # Get the data
  survey_data <- get_redcap_records(
    fields = all_fields,
    records = records,
    format = format
  )
  
  if (is.null(survey_data) || length(survey_data) == 0) {
    warning("No survey data retrieved")
    return(data.frame(
      record_id = character(0),
      survey_instrument = character(0),
      survey_timestamp = character(0),
      survey_complete = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Convert to data frame if needed
  if (is.list(survey_data) && !is.data.frame(survey_data)) {
    survey_df <- do.call(rbind, lapply(survey_data, function(x) {
      # Ensure all expected fields exist
      for (field in all_fields) {
        if (!field %in% names(x)) {
          x[[field]] <- ""
        }
      }
      data.frame(x, stringsAsFactors = FALSE)
    }))
  } else {
    survey_df <- survey_data
  }
  
  # Reshape from wide to long format
  result_list <- list()
  
  for (instrument in instrument_names) {
    timestamp_field <- paste0(instrument, "_timestamp")
    complete_field <- paste0(instrument, "_complete")
    
    if (timestamp_field %in% names(survey_df) && complete_field %in% names(survey_df)) {
      instrument_data <- data.frame(
        record_id = survey_df$record_id,
        survey_instrument = instrument,
        survey_timestamp = survey_df[[timestamp_field]],
        survey_complete = survey_df[[complete_field]],
        stringsAsFactors = FALSE
      )
      
      # Only include rows where there's some survey activity
      instrument_data <- instrument_data[
        !is.na(instrument_data$survey_timestamp) & 
        instrument_data$survey_timestamp != "" |
        !is.na(instrument_data$survey_complete) & 
        instrument_data$survey_complete != "", 
      ]
      
      result_list[[instrument]] <- instrument_data
    }
  }
  
  # Combine all instruments
  if (length(result_list) > 0) {
    final_result <- do.call(rbind, result_list)
    rownames(final_result) <- NULL
    return(final_result)
  } else {
    return(data.frame(
      record_id = character(0),
      survey_instrument = character(0),
      survey_timestamp = character(0),
      survey_complete = character(0),
      stringsAsFactors = FALSE
    ))
  }
}

#' Get REDCap Logs
#'
#' Retrieves logging data from REDCap.
#'
#' @param records Character vector of record IDs. If NULL, gets all records.
#' @param begin_time Character string for start time filter (format: "YYYY-MM-DD HH:MM:SS").
#' @param end_time Character string for end time filter (format: "YYYY-MM-DD HH:MM:SS").
#'
#' @return List or data frame with log entries
#' @export
get_redcap_logs <- function(records = NULL, begin_time = NULL, end_time = NULL) {
  
  # Build parameters
  params <- list(content = "log", format = "json")
  
  if (!is.null(records)) {
    if (length(records) == 1) {
      params$record <- records
    } else {
      params$records <- paste(records, collapse = ",")
    }
  }
  
  if (!is.null(begin_time)) {
    params$beginTime <- begin_time
  }
  
  if (!is.null(end_time)) {
    params$endTime <- end_time
  }
  
  # Call the API
  do.call(call_redcap_api, params)
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
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

#' Get REDCap Report
#'
#' Retrieves data from a specific REDCap report using the report ID.
#'
#' @param report_id Character or numeric report ID
#' @param format Character string specifying the format ("json", "csv"). Default is "json"
#' @param date_begin Optional start date for filtering (YYYY-MM-DD format)
#' @param date_end Optional end date for filtering (YYYY-MM-DD format)
#'
#' @return Data frame or list containing the report data
#' @export
get_redcap_report <- function(report_id, format = "json", date_begin = NULL, date_end = NULL) {
  
  # Build parameters for report API call
  params <- list(
    content = "report",
    report_id = as.character(report_id),
    format = format,
    returnFormat = format
  )
  
  # Add date filtering if provided
  if (!is.null(date_begin)) {
    params$dateRangeBegin <- date_begin
  }
  
  if (!is.null(date_end)) {
    params$dateRangeEnd <- date_end
  }
  
  # Call the API
  do.call(call_redcap_api, params)
}

#' Get Eligible Participants from Report 14081
#'
#' Filters report 14081 data to find participants who meet eligibility criteria.
#' Eligibility requires: r01es_commute == "1" && r01es_austin == "1" && 
#' r01es_phone == "1" && r01es_computer == "1" && r01es_bpd == "0" && 
#' r01es_psychotherapy == "0" && phq8score >= 17 && r01es_druguse == "0" && 
#' medchng == "0" && r01es_medstop == "0" && r01es_medstart == "0"
#'
#' @return Data frame with de-identified summary of eligible participants
#' @export
get_eligible_participants <- function() {
  
  tryCatch({
    # Get data from report 14081 (records from last 30 days)
    raw_data <- get_redcap_report(14081)
    
    if (is.null(raw_data) || length(raw_data) == 0) {
      return(data.frame(
        Status = "No data from report 14081",
        Total_Records = 0,
        Eligible_Count = 0,
        stringsAsFactors = FALSE
      ))
    }
    
    # Convert list to data frame
    if (is.list(raw_data) && !is.data.frame(raw_data)) {
      report_df <- do.call(rbind, lapply(raw_data, function(x) {
        data.frame(x, stringsAsFactors = FALSE)
      }))
    } else {
      report_df <- raw_data
    }
    
    if (is.null(report_df) || nrow(report_df) == 0) {
      return(data.frame(
        Status = "No participants in report",
        Total_Records = 0,
        Eligible_Count = 0,
        stringsAsFactors = FALSE
      ))
    }
    
    total_records <- nrow(report_df)
    
    # Filter to past 30 days using interview_date
    one_month_ago <- Sys.Date() - 30
    today <- Sys.Date()
    
    # First filter by date if interview_date is available
    if ("interview_date" %in% names(report_df)) {
      # Parse interview_date and filter to past month
      report_df$interview_date_parsed <- as.Date(report_df$interview_date)
      recent_records <- report_df[
        !is.na(report_df$interview_date_parsed) &
        report_df$interview_date_parsed >= one_month_ago &
        report_df$interview_date_parsed <= today,
      ]
    } else {
      recent_records <- report_df
    }
    
    recent_count <- nrow(recent_records)
    
    # Apply eligibility criteria to recent records
    eligible_participants <- recent_records[
      !is.na(recent_records$r01es_commute) & recent_records$r01es_commute == "1" &
      !is.na(recent_records$r01es_austin) & recent_records$r01es_austin == "1" &
      !is.na(recent_records$r01es_phone) & recent_records$r01es_phone == "1" &
      !is.na(recent_records$r01es_computer) & recent_records$r01es_computer == "1" &
      !is.na(recent_records$r01es_bpd) & recent_records$r01es_bpd == "0" &
      !is.na(recent_records$r01es_psychotherapy) & recent_records$r01es_psychotherapy == "0" &
      !is.na(recent_records$phq8score) & as.numeric(recent_records$phq8score) >= 17 &
      !is.na(recent_records$r01es_druguse) & recent_records$r01es_druguse == "0" &
      !is.na(recent_records$medchng) & recent_records$medchng == "0" &
      !is.na(recent_records$r01es_medstop) & recent_records$r01es_medstop == "0" &
      !is.na(recent_records$r01es_medstart) & recent_records$r01es_medstart == "0",
    ]
    
    eligible_count <- nrow(eligible_participants)
    
    # If no eligible participants, return summary
    if (eligible_count == 0) {
      return(data.frame(
        Status = "No eligible participants in past 30 days",
        Total_Records = total_records,
        Recent_Records = recent_count,
        Eligible_Count = 0,
        stringsAsFactors = FALSE
      ))
    }
    
    # Return the specific columns for eligible participants
    result <- data.frame(
      record_id = eligible_participants$record_id,
      interview_date = eligible_participants$interview_date,
      link_to_record_id = paste0("https://redcap.prc.utexas.edu/redcap/redcap_v15.5.6/DataEntry/record_home.php?pid=3385&arm=1&id=", 
                                eligible_participants$record_id),
      stringsAsFactors = FALSE
    )
    
    return(result)
    
  }, error = function(e) {
    return(data.frame(
      Status = paste("Error:", substr(e$message, 1, 50)),
      Total_Records = 0,
      Eligible_Count = 0,
      stringsAsFactors = FALSE
    ))
  })
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
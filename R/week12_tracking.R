#' Get upcoming follow-up appointments
#'
#' Finds participants who completed W4 Acute [IN-PERSON] and calculates
#' upcoming Week 12, 16, and 28 follow-up dates.
#'
#' @param days_ahead Number of days ahead to show upcoming follow-ups. Default is 14.
#' @return Data frame with upcoming follow-ups
#' @export
#'
#' @examples
#' \dontrun{
#' upcoming_followups <- get_upcoming_followups()
#' }
get_upcoming_followups <- function(days_ahead = 14) {
  
  # Get logs for all records looking for W4 Acute completions
  # We'll look back up to 120 days to catch anyone who might be due
  begin_time <- format(Sys.time() - (200 * 24 * 60 * 60), "%Y-%m-%d %H:%M:%S")
  
  # Get all logs
  logs <- get_redcap_logs(begin_time = begin_time)
  
  if (is.null(logs) || length(logs) == 0) {
    return(data.frame(
      record_id = character(0),
      follow_up_type = character(0),
      w4_completion_date = character(0),
      due_date = character(0),
      days_until_due = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Convert to data frame if needed
  if (is.list(logs) && !is.data.frame(logs)) {
    logs_df <- do.call(rbind, lapply(logs, function(x) {
      data.frame(
        timestamp = x$timestamp %||% "",
        username = x$username %||% "",
        action = x$action %||% "",
        details = x$details %||% "",
        record = x$record %||% "",
        stringsAsFactors = FALSE
      )
    }))
  } else {
    logs_df <- logs
  }
  
  if (nrow(logs_df) == 0) {
    return(data.frame(
      record_id = character(0),
      follow_up_type = character(0),
      w4_completion_date = character(0),
      due_date = character(0),
      days_until_due = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Look for W4 Acute [IN-PERSON] completions
  w4_logs <- logs_df[grepl("W4 Acute.*IN-PERSON", logs_df$action, ignore.case = TRUE) |
                     grepl("W4 Acute.*IN-PERSON", logs_df$details, ignore.case = TRUE) |
                     grepl("w4.*acute.*person", logs_df$action, ignore.case = TRUE) |
                     grepl("w4.*acute.*person", logs_df$details, ignore.case = TRUE), ]
  
  
  if (nrow(w4_logs) == 0) {
    return(data.frame(
      record_id = character(0),
      follow_up_type = character(0),
      w4_completion_date = character(0),
      due_date = character(0),
      days_until_due = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Parse timestamps and filter valid records
  w4_logs$timestamp_parsed <- as.POSIXct(w4_logs$timestamp, format = "%Y-%m-%d %H:%M")
  w4_logs_with_records <- w4_logs[w4_logs$record != "" & !is.na(w4_logs$record), ]
  
  if (nrow(w4_logs_with_records) == 0) {
    return(data.frame(
      record_id = character(0),
      follow_up_type = character(0),
      w4_completion_date = character(0),
      due_date = character(0),
      days_until_due = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get most recent W4 completion per record
  split_records <- split(w4_logs_with_records, w4_logs_with_records$record)
  recent_completions_list <- lapply(split_records, function(record_logs) {
    if (nrow(record_logs) == 0 || sum(!is.na(record_logs$timestamp_parsed)) == 0) {
      return(NULL)
    }
    
    max_index <- which.max(record_logs$timestamp_parsed)
    return(record_logs[max_index, ])
  })
  
  # Remove NULL entries and combine
  recent_completions_list <- recent_completions_list[!sapply(recent_completions_list, is.null)]
  
  if (length(recent_completions_list) == 0) {
    return(data.frame(
      record_id = character(0),
      follow_up_type = character(0),
      w4_completion_date = character(0),
      due_date = character(0),
      days_until_due = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  recent_completions <- do.call(rbind, recent_completions_list)
  
  # Find withdrawn participants (completed "Reason for Withdrawal")
  withdrawal_logs <- logs_df[grepl("Reason.*for.*Withdrawal", logs_df$action, ignore.case = TRUE) |
                            grepl("Reason.*for.*Withdrawal", logs_df$details, ignore.case = TRUE), ]
  withdrawn_records <- unique(withdrawal_logs$record[withdrawal_logs$record != ""])
  
  # Get completed follow-ups for each type  
  w12_completed_logs <- logs_df[grepl("W12.*Booster.*REMOTE", logs_df$action, ignore.case = TRUE) |
                               grepl("W12.*Booster.*PERSON", logs_df$action, ignore.case = TRUE), ]
  w16_completed_logs <- logs_df[grepl("W16.*Follow.*Up.*REMOTE", logs_df$action, ignore.case = TRUE) |
                               grepl("W16.*Follow.*Up.*PERSON", logs_df$action, ignore.case = TRUE), ]
  w28_completed_logs <- logs_df[grepl("W28.*Follow.*Up.*REMOTE", logs_df$action, ignore.case = TRUE) |
                               grepl("W28.*Follow.*Up.*PERSON", logs_df$action, ignore.case = TRUE), ]
  
  # Get unique records that have completed each follow-up type
  w12_completed_records <- unique(w12_completed_logs$record[w12_completed_logs$record != ""])
  w16_completed_records <- unique(w16_completed_logs$record[w16_completed_logs$record != ""])
  w28_completed_records <- unique(w28_completed_logs$record[w28_completed_logs$record != ""])
  
  
  # Calculate follow-up dates and status
  today <- Sys.Date()
  recent_completions$completion_date <- as.Date(recent_completions$timestamp_parsed)
  recent_completions$days_since <- as.numeric(today - recent_completions$completion_date)
  
  # Calculate target dates for each follow-up
  recent_completions$week12_due_date <- recent_completions$completion_date + 56  # 8 weeks
  recent_completions$week16_due_date <- recent_completions$completion_date + 84  # 12 weeks  
  recent_completions$week28_due_date <- recent_completions$completion_date + 168 # 24 weeks
  
  # Calculate days until each follow-up (negative = overdue, positive = future)
  recent_completions$days_to_week12 <- as.numeric(recent_completions$week12_due_date - today)
  recent_completions$days_to_week16 <- as.numeric(recent_completions$week16_due_date - today)
  recent_completions$days_to_week28 <- as.numeric(recent_completions$week28_due_date - today)
  
  # Create a list to collect all upcoming follow-ups
  all_followups <- list()
  
  # Check each participant for incomplete follow-ups
  for (i in 1:nrow(recent_completions)) {
    row <- recent_completions[i, ]
    record_id <- row$record
    
    # Skip withdrawn participants entirely
    if (record_id %in% withdrawn_records) {
      next
    }
    
    # Week 12 - only show if not completed and within timeframe
    if (abs(row$days_to_week12) <= days_ahead && !record_id %in% w12_completed_records) {
      all_followups[[length(all_followups) + 1]] <- data.frame(
        record_id = record_id,
        follow_up_type = "Week 12",
        w4_completion_date = as.character(row$completion_date),
        due_date = as.character(row$week12_due_date),
        days_until_due = row$days_to_week12,
        status = ifelse(row$days_to_week12 < 0, 
                       paste("Overdue by", abs(row$days_to_week12), "days"),
                       ifelse(row$days_to_week12 == 0, "Due Today",
                             paste("Due in", row$days_to_week12, "days"))),
        stringsAsFactors = FALSE
      )
    }
    
    # Week 16 - only show if not completed and within timeframe
    if (abs(row$days_to_week16) <= days_ahead && !record_id %in% w16_completed_records) {
      all_followups[[length(all_followups) + 1]] <- data.frame(
        record_id = record_id,
        follow_up_type = "Week 16",
        w4_completion_date = as.character(row$completion_date),
        due_date = as.character(row$week16_due_date),
        days_until_due = row$days_to_week16,
        status = ifelse(row$days_to_week16 < 0, 
                       paste("Overdue by", abs(row$days_to_week16), "days"),
                       ifelse(row$days_to_week16 == 0, "Due Today",
                             paste("Due in", row$days_to_week16, "days"))),
        stringsAsFactors = FALSE
      )
    }
    
    # Week 28 - only show if not completed and within timeframe
    if (abs(row$days_to_week28) <= days_ahead && !record_id %in% w28_completed_records) {
      all_followups[[length(all_followups) + 1]] <- data.frame(
        record_id = record_id,
        follow_up_type = "Week 28",
        w4_completion_date = as.character(row$completion_date),
        due_date = as.character(row$week28_due_date),
        days_until_due = row$days_to_week28,
        status = ifelse(row$days_to_week28 < 0, 
                       paste("Overdue by", abs(row$days_to_week28), "days"),
                       ifelse(row$days_to_week28 == 0, "Due Today",
                             paste("Due in", row$days_to_week28, "days"))),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all follow-ups
  if (length(all_followups) > 0) {
    result <- do.call(rbind, all_followups)
    # Sort by days until due (overdue items first, then soonest)
    result <- result[order(result$days_until_due), ]
    rownames(result) <- NULL
    return(result)
  } else {
    return(data.frame(
      record_id = character(0),
      follow_up_type = character(0),
      w4_completion_date = character(0),
      due_date = character(0),
      days_until_due = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }
}
#' Get Participant Compliance Summary
#'
#' Provides a simplified view of participant progress showing each participant once
#' with their current week, total sessions completed, and sessions behind.
#'
#' @param sheet_url Character string with the Google Sheet URL
#' @param sheet_name Character string with the sheet tab name. Default is NULL
#' @param exclude_ids Character vector of IDs to exclude. Default is c("123456789", "12345678")
#' @param sessions_per_week Numeric expected sessions per week. Default is 4
#'
#' @return Data frame with one row per participant showing:
#'   \itemize{
#'     \item id: Participant ID
#'     \item current_week: Which week they should be in based on start date
#'     \item weeks_from_start: Decimal weeks since they started
#'     \item total_sessions_completed: Total sessions with 180 turns
#'     \item expected_sessions: Total sessions expected so far
#'     \item sessions_behind: How many sessions behind (negative = ahead)
#'     \item start_date: When they started week 1
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' sheet_url <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit"
#' summary <- get_participant_summary(sheet_url)
#' print(summary)
#' }
get_participant_summary <- function(sheet_url,
                                   sheet_name = NULL,
                                   exclude_ids = c("123456789", "12345678"),
                                   sessions_per_week = 4) {

  # Get the full compliance report
  result <- get_compliance_report(sheet_url, sheet_name, exclude_ids, sessions_per_week)
  compliance <- result$compliance

  # Get unique participants
  unique_ids <- unique(compliance$id)

  summary_list <- list()

  for (i in seq_along(unique_ids)) {
    participant_id <- unique_ids[i]
    participant_data <- compliance[compliance$id == participant_id, ]

    # Get the most recent week's data
    latest_week <- participant_data[which.max(participant_data$time_from_start), ]

    # Calculate totals across all weeks
    total_completed <- sum(participant_data$sess_cnt, na.rm = TRUE)
    weeks_from_start <- latest_week$time_from_start
    expected_total <- round(weeks_from_start * sessions_per_week, 2)

    # Cap expected sessions at 16 (4 weeks * 4 sessions/week)
    expected_total <- min(expected_total, 16)

    behind <- expected_total - total_completed

    # Determine current week (1-4)
    current_week <- min(ceiling(weeks_from_start), 4)

    summary_list[[i]] <- data.frame(
      id = participant_id,
      current_week = current_week,
      weeks_from_start = round(weeks_from_start, 2),
      total_sessions_completed = total_completed,
      expected_sessions = expected_total,
      sessions_behind = round(behind, 2),
      start_date = format(latest_week$start - ((current_week - 1) * 7 * 24 * 60 * 60), "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
  }

  summary <- do.call(rbind, summary_list)

  # Sort by sessions behind (most behind first)
  summary <- summary[order(-summary$sessions_behind), ]
  rownames(summary) <- NULL

  # Print summary statistics
  message("\nParticipant Summary:")
  message("  - Total active participants: ", nrow(summary))
  message("  - Participants behind schedule: ", sum(summary$sessions_behind > 0))
  message("  - Participants on/ahead of schedule: ", sum(summary$sessions_behind <= 0))

  return(summary)
}


#' Get Behind Participants Only
#'
#' Convenience function to show only participants who are behind schedule.
#'
#' @param sheet_url Character string with the Google Sheet URL
#' @param sheet_name Character string with the sheet tab name. Default is NULL
#' @param min_sessions_behind Numeric minimum sessions behind to include. Default is 1
#'
#' @return Data frame with participants behind schedule
#' @export
#'
#' @examples
#' \dontrun{
#' behind <- get_behind_participants(sheet_url)
#' print(behind)
#' }
get_behind_participants <- function(sheet_url,
                                   sheet_name = NULL,
                                   min_sessions_behind = 1) {

  summary <- get_participant_summary(sheet_url, sheet_name)

  behind <- summary[summary$sessions_behind >= min_sessions_behind, ]

  if (nrow(behind) > 0) {
    message("\nParticipants behind schedule:")
    print(behind)
  } else {
    message("\nNo participants behind schedule!")
  }

  invisible(behind)
}

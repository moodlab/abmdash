#' Get Compliance Report from Google Sheets
#'
#' Calculates participant compliance based on gameplay session data from Google Sheets.
#' Tracks expected vs actual sessions completed per week.
#'
#' @param sheet_url Character string with the Google Sheet URL
#' @param sheet_name Character string with the sheet tab name. Default is NULL (first sheet)
#' @param exclude_ids Character vector of IDs to exclude (test accounts).
#'   Default is c("123456789", "12345678")
#' @param sessions_per_week Numeric expected sessions per week. Default is 4
#'
#' @return List containing:
#'   \itemize{
#'     \item compliance: Data frame with compliance information for active participants
#'     \item short_sessions: Data frame with sessions that had 178-179 turns
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' sheet_url <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit"
#' result <- get_compliance_report(sheet_url)
#' head(result$compliance)
#' }
get_compliance_report <- function(sheet_url,
                                  sheet_name = NULL,
                                  exclude_ids = c("123456789", "12345678"),
                                  sessions_per_week = 4) {

  # Read the Google Sheet
  message("Reading Google Sheet...")
  data <- read_google_sheet(sheet_url, sheet_name = sheet_name)

  # Rename columns and select needed ones
  data <- data.frame(
    id = data$`Referral ID`,
    date = data$`Date of Event UTC`,
    event = data$`Event Type`,
    week = data$`Week #`,
    session = data$`Session #`,
    turns = data$`Turns Completed`,
    stringsAsFactors = FALSE
  )

  # Parse dates
  data$date <- as.POSIXct(data$date, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")

  # Convert "N/A" to NA
  data$week[data$week == "N/A"] <- NA
  data$session[data$session == "N/A"] <- NA
  data$turns[data$turns == "N/A"] <- NA

  # Convert to numeric
  data$week <- as.numeric(data$week)
  data$session <- as.numeric(data$session)
  data$turns <- as.numeric(data$turns)

  # Filter out excluded IDs, test IDs, and get only Gameplay events
  # Remove any ID containing "test" (case-insensitive)
  is_test_id <- grepl("test", data$id, ignore.case = TRUE)

  gameplay_data <- data[!data$id %in% exclude_ids &
                        !is_test_id &
                        data$event == "Gameplay" &
                        !is.na(data$event), ]

  # Count number of sessions per week (only completed sessions with 180 turns)
  completed_sessions <- gameplay_data[gameplay_data$turns == 180 & !is.na(gameplay_data$turns), ]

  # Group by id and week, get max session number
  sessions <- aggregate(session ~ id + week,
                       data = completed_sessions,
                       FUN = max,
                       na.rm = TRUE)
  names(sessions)[3] <- "sess_cnt"

  # Compute start date (first session of week 1)
  start_data <- gameplay_data[gameplay_data$session == 1 &
                              gameplay_data$week == 1 &
                              !is.na(gameplay_data$session) &
                              !is.na(gameplay_data$week), ]

  start_dates <- data.frame(
    id = start_data$id,
    start_trial = start_data$date,
    end_trial = start_data$date + (7 * 24 * 60 * 60),  # Add 1 week in seconds
    stringsAsFactors = FALSE
  )

  # Remove duplicates (keep first occurrence)
  start_dates <- start_dates[!duplicated(start_dates$id), ]

  # Compute weekly dates (weeks 1-4)
  study_dates_list <- list()

  for (week_num in 1:4) {
    week_data <- data.frame(
      id = start_dates$id,
      week = week_num,
      start = start_dates$start_trial + ((week_num - 1) * 7 * 24 * 60 * 60),
      end = start_dates$end_trial + ((week_num - 1) * 7 * 24 * 60 * 60),
      stringsAsFactors = FALSE
    )
    study_dates_list[[week_num]] <- week_data
  }

  # Combine all weeks
  study_dates <- do.call(rbind, study_dates_list)

  # Merge with session counts
  study_dates <- merge(study_dates, sessions,
                      by = c("id", "week"),
                      all.x = TRUE)

  # Replace NA session counts with 0
  study_dates$sess_cnt[is.na(study_dates$sess_cnt)] <- 0

  # Calculate time from start
  current_time <- Sys.time()
  study_dates$time_from_start <- as.numeric(
    difftime(current_time, study_dates$start, units = "weeks")
  )
  study_dates$time_from_start <- round(study_dates$time_from_start, 2)

  # Calculate compliance for recently active participants
  # Expected sessions: 0.5714 sessions/day = 4 per week
  compliance <- study_dates[study_dates$time_from_start < 5 &
                           study_dates$time_from_start > 0, ]

  days_from_start <- as.numeric(difftime(current_time, compliance$start, units = "days"))
  compliance$expect_cnt <- round(days_from_start * (sessions_per_week / 7), 2)

  # Flag as late if sess_cnt + 1 < expect_cnt
  compliance$late <- (compliance$sess_cnt + 1) < compliance$expect_cnt

  # Find sessions with 178 or 179 turns
  short_sessions <- gameplay_data[gameplay_data$turns >= 178 &
                                 gameplay_data$turns <= 179 &
                                 !is.na(gameplay_data$turns), ]

  message("Compliance report complete:")
  message("  - ", nrow(compliance), " active participant-weeks")
  message("  - ", sum(compliance$late), " flagged as late")
  message("  - ", nrow(short_sessions), " sessions with 178-179 turns")

  return(list(
    compliance = compliance,
    short_sessions = short_sessions
  ))
}


#' Get Late Participants
#'
#' Convenience function to get only participants who are flagged as late.
#'
#' @param sheet_url Character string with the Google Sheet URL
#' @param sheet_name Character string with the sheet tab name. Default is NULL
#'
#' @return Data frame with late participants
#' @export
#'
#' @examples
#' \dontrun{
#' late <- get_late_participants(sheet_url)
#' print(late)
#' }
get_late_participants <- function(sheet_url, sheet_name = NULL) {
  result <- get_compliance_report(sheet_url, sheet_name = sheet_name)

  late <- result$compliance[result$compliance$late == TRUE, ]

  if (nrow(late) > 0) {
    message("\nLate participants:")
    print(late[, c("id", "week", "sess_cnt", "expect_cnt", "time_from_start")])
  } else {
    message("No late participants!")
  }

  invisible(late)
}

#' Get Traditional ABM Compliance Summary
#'
#' Downloads session data from the ABS portal and calculates compliance
#' for active traditional ABM participants. Mirrors the output format of
#' \code{\link{get_participant_summary}} for GABM data.
#'
#' @param base_url Character string with the ABS base URL.
#'   Default is "https://abs.la.utexas.edu"
#' @param csv_path Character string with the CSV download endpoint.
#'   Default is "/admin/test/download-csv-all"
#' @param exclude_pattern Regex pattern for test IDs to exclude.
#'   Default is "test"
#' @param sessions_per_week Numeric expected sessions per week. Default is 4
#' @param active_window_weeks Numeric weeks from start to consider active.
#'   Default is 5
#' @param verbose Logical, whether to emit progress messages. Default is FALSE
#'
#' @return Data frame with one row per active participant:
#'   \itemize{
#'     \item id: Participant ID
#'     \item current_week: Which week (1-4) based on start date
#'     \item weeks_from_start: Decimal weeks since first session
#'     \item total_sessions: Number of completed sessions
#'     \item expected_sessions: Sessions expected by now (max 16)
#'     \item sessions_behind: Expected minus actual (negative = ahead)
#'     \item start_date: Date of first session
#'     \item status: "On Track" or "Behind by N"
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- get_trad_compliance_summary()
#' print(summary)
#' }
get_trad_compliance_summary <- function(
    base_url = "https://abs.la.utexas.edu",
    csv_path = "/admin/test/download-csv-all",
    exclude_pattern = "test",
    sessions_per_week = 4,
    active_window_weeks = 5,
    verbose = FALSE) {

  # Login to ABS portal
  if (verbose) message("[trad_compliance] logging in to ABS portal...")
  session <- tryCatch(
    abs_login(base_url = base_url, check_connection = FALSE),
    error = function(e) stop("ABS login failed: ", e$message, call. = FALSE)
  )

  # Download CSV data
  if (verbose) message("[trad_compliance] downloading CSV data...")
  trad_data <- tryCatch(
    download_abs_csv(session, csv_path = csv_path),
    error = function(e) stop("CSV download failed: ", e$message, call. = FALSE)
  )

  if (verbose) message("[trad_compliance] downloaded ", nrow(trad_data), " rows")

  # Process the data
  process_trad_compliance_data(
    trad_data,
    exclude_pattern = exclude_pattern,
    sessions_per_week = sessions_per_week,
    active_window_weeks = active_window_weeks,
    verbose = verbose
  )
}


#' Process Traditional ABM Compliance Data
#'
#' Internal function that processes raw ABS CSV data into a compliance summary.
#'
#' @param trad_data Data frame from ABS CSV download
#' @param exclude_pattern Regex pattern for test IDs to exclude
#' @param sessions_per_week Expected sessions per week
#' @param active_window_weeks Weeks from start to consider active
#' @param verbose Whether to emit progress messages
#'
#' @return Data frame with compliance summary
#' @keywords internal
process_trad_compliance_data <- function(
    trad_data,
    exclude_pattern = "test",
    sessions_per_week = 4,
    active_window_weeks = 5,
    verbose = FALSE) {

  # Define empty result for early returns

empty_result <- data.frame(
    id = character(),
    current_week = numeric(),
    weeks_from_start = numeric(),
    total_sessions = numeric(),
    expected_sessions = numeric(),
    sessions_behind = numeric(),
    start_date = character(),
    status = character(),
    stringsAsFactors = FALSE
  )

  if (nrow(trad_data) == 0) return(empty_result)

  # Validate expected columns
  required_cols <- c("subject_id", "session", "start_time")
  missing <- setdiff(required_cols, names(trad_data))
  if (length(missing) > 0) {
    stop("CSV missing expected columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Parse timestamps
  trad_data$start_time <- as.POSIXct(trad_data$start_time, tz = "UTC")

  # Filter out test IDs
  is_test <- grepl(exclude_pattern, trad_data$subject_id, ignore.case = TRUE)
  trad_data <- trad_data[!is_test, ]
  if (verbose) message("[trad_compliance] ", sum(!is_test), " rows after filtering test IDs")

  if (nrow(trad_data) == 0) return(empty_result)

  # Get first session date per participant
  first_sessions <- aggregate(
    start_time ~ subject_id,
    data = trad_data,
    FUN = min
  )
  names(first_sessions) <- c("id", "start_date")

  # Calculate weeks from start
  current_time <- Sys.time()
  first_sessions$weeks_from_start <- as.numeric(
    difftime(current_time, first_sessions$start_date, units = "weeks")
  )

  # Filter to active participants
  active <- first_sessions[first_sessions$weeks_from_start <= active_window_weeks, ]
  if (verbose) message("[trad_compliance] ", nrow(active), " active participants")

  if (nrow(active) == 0) return(empty_result)

  # Count sessions per participant
  session_counts <- aggregate(
    session ~ subject_id,
    data = trad_data,
    FUN = length
  )
  names(session_counts) <- c("id", "total_sessions")

  # Merge
  active <- merge(active, session_counts, by = "id", all.x = TRUE)
  active$total_sessions[is.na(active$total_sessions)] <- 0

  # Calculate expected sessions and status
  active$current_week <- pmin(ceiling(active$weeks_from_start), 4)
  active$expected_sessions <- pmin(
    round(active$weeks_from_start * sessions_per_week),
    sessions_per_week * 4  # max 16
  )
  active$sessions_behind <- active$expected_sessions - active$total_sessions

  active$status <- ifelse(
    active$sessions_behind > 0,
    paste("Behind by", round(active$sessions_behind)),
    "On Track"
  )

  # Format start_date as string
  active$start_date <- format(active$start_date, "%Y-%m-%d")

  # Round weeks_from_start
  active$weeks_from_start <- round(active$weeks_from_start, 2)

  # Sort by sessions behind (most behind first)
  active <- active[order(-active$sessions_behind), ]
  rownames(active) <- NULL

  # Select and order columns
  active[, c("id", "current_week", "weeks_from_start", "total_sessions",
             "expected_sessions", "sessions_behind", "start_date", "status")]
}

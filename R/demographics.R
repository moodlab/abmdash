#' Get Demographic Summary
#'
#' Retrieves and summarizes demographic data from REDCap for enrolled participants.
#'
#' @param report_id Character string with REDCap report ID. Default is "13349"
#' @param enrolled_ids Optional vector of enrolled participant IDs to filter to
#'
#' @return Data frame with demographic summaries
#' @export
#'
#' @examples
#' \dontrun{
#' demographics <- get_demographic_summary()
#' }
get_demographic_summary <- function(report_id = "13349", enrolled_ids = NULL) {

  # Get demographic data from REDCap report
  demo_raw <- get_redcap_report(report_id, format = "csv")

  # Filter to enrolled IDs if provided
  if (!is.null(enrolled_ids)) {
    demo_raw <- demo_raw[demo_raw$record_id %in% enrolled_ids, ]
  }

  # Process Race
  race_data <- demo_raw[demo_raw$redcap_event_name == "eligibility_screen_arm_1", ]
  race_data$race <- "Unknown"
  race_data$race[race_data$raceid___1 == "1"] <- "American Indian"
  race_data$race[race_data$raceid___2 == "1"] <- "Asian"
  race_data$race[race_data$raceid___3 == "1"] <- "Black"
  race_data$race[race_data$raceid___4 == "1"] <- "Hawaiian"
  race_data$race[race_data$raceid___5 == "1"] <- "White"
  race_data$race[race_data$raceid___6 == "1"] <- "More than one race"
  race_data$race[race_data$raceid___7 == "1"] <- "Unknown"

  # Process Ethnicity
  ethnicity_data <- demo_raw[demo_raw$redcap_event_name == "week_0_eligibility_arm_1", ]
  ethnicity_data$ethnicity <- "Unknown"
  ethnicity_data$ethnicity[ethnicity_data$demo_ethnicity == "1"] <- "Hispanic or Latino"
  ethnicity_data$ethnicity[ethnicity_data$demo_ethnicity == "2"] <- "Not Hispanic or Latino"

  # Process Sex
  sex_data <- demo_raw[demo_raw$redcap_event_name == "eligibility_screen_arm_1", ]
  sex_data$sex <- "Unknown"
  sex_data$sex[sex_data$gender_identity == "1"] <- "Male"
  sex_data$sex[sex_data$gender_identity == "2"] <- "Female"

  # Process Age
  age_data <- demo_raw[demo_raw$redcap_event_name == "eligibility_screen_arm_1", ]
  age_data$age <- NA
  if ("interview_age" %in% names(age_data)) {
    birthdays <- as.Date(age_data$interview_age)
    today <- Sys.Date()
    age_data$age <- floor(as.numeric(difftime(today, birthdays, units = "days")) / 365.25)
  }

  # Merge demographics
  demo_merged <- data.frame(
    record_id = race_data$record_id,
    race = race_data$race,
    stringsAsFactors = FALSE
  )

  # Add ethnicity
  ethnicity_match <- match(demo_merged$record_id, ethnicity_data$record_id)
  demo_merged$ethnicity <- ethnicity_data$ethnicity[ethnicity_match]
  demo_merged$ethnicity[is.na(demo_merged$ethnicity)] <- "Unknown"

  # Add sex
  sex_match <- match(demo_merged$record_id, sex_data$record_id)
  demo_merged$sex <- sex_data$sex[sex_match]
  demo_merged$sex[is.na(demo_merged$sex)] <- "Unknown"

  # Add age
  age_match <- match(demo_merged$record_id, age_data$record_id)
  demo_merged$age <- age_data$age[age_match]

  return(demo_merged)
}


#' Summarize Demographics for Display
#'
#' Creates summary statistics tables for demographic data.
#'
#' @param demo_data Data frame from get_demographic_summary()
#'
#' @return List with summary data frames for different demographics
#' @export
#'
#' @examples
#' \dontrun{
#' demo <- get_demographic_summary()
#' summaries <- summarize_demographics(demo)
#' }
summarize_demographics <- function(demo_data) {

  if (nrow(demo_data) == 0) {
    return(list(
      overall = data.frame(
        Metric = "Total Participants",
        Value = "0",
        stringsAsFactors = FALSE
      )
    ))
  }

  # Overall summary
  n_total <- nrow(demo_data)

  # Age summary
  age_valid <- demo_data$age[!is.na(demo_data$age)]
  if (length(age_valid) > 0) {
    age_summary <- sprintf(
      "%.1f (SD=%.1f), Range: %d-%d",
      mean(age_valid),
      sd(age_valid),
      min(age_valid),
      max(age_valid)
    )
  } else {
    age_summary <- "No data"
  }

  # Helper function for categorical summaries
  summarize_category <- function(data, var_name) {
    var_data <- data[[var_name]]
    var_data <- var_data[!is.na(var_data) & var_data != "Unknown"]

    if (length(var_data) == 0) {
      return(data.frame(
        Category = "No data",
        Count = 0,
        Percentage = "",
        stringsAsFactors = FALSE
      ))
    }

    counts <- table(var_data)
    counts_sorted <- sort(counts, decreasing = TRUE)

    data.frame(
      Category = names(counts_sorted),
      Count = as.numeric(counts_sorted),
      Percentage = paste0(round(100 * as.numeric(counts_sorted) / n_total, 1), "%"),
      stringsAsFactors = FALSE
    )
  }

  # Create summaries
  race_summary <- summarize_category(demo_data, "race")
  ethnicity_summary <- summarize_category(demo_data, "ethnicity")
  sex_summary <- summarize_category(demo_data, "sex")

  # Create age groups for display
  age_groups <- cut(
    demo_data$age,
    breaks = c(0, 18, 25, 35, 45, 55, 100),
    labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55+"),
    right = FALSE
  )
  age_group_summary <- summarize_category(
    data.frame(age_group = age_groups, stringsAsFactors = FALSE),
    "age_group"
  )

  return(list(
    overall = data.frame(
      Metric = c("Total Participants", "Age (years)"),
      Value = c(n_total, age_summary),
      stringsAsFactors = FALSE
    ),
    age_groups = age_group_summary,
    sex = sex_summary,
    race = race_summary,
    ethnicity = ethnicity_summary
  ))
}

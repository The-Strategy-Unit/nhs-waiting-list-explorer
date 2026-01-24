#' Normalize Multiple Reports to a Common Reference Date
#'
#' This function takes waiting list data from multiple report dates and normalizes
#' all arrival/removal dates to a common reference date. This allows combining
#' data across multiple time periods into a single histogram for percentile calculations.
#'
#' @param wl_data A data frame containing waiting list data with columns:
#'   Provider_Code, Treatment_Function_Code (or their variations),
#'   arrived_since/arrived_before (or removed_since/removed_before),
#'   report_date, and n (patient count).
#' @param reference_date The date to normalize all data to. If NULL (default),
#'   uses the maximum report_date in the data.
#' @param date_prefix Either "arrived" or "removed" to specify which date columns to use.
#'   Default is "arrived".
#'
#' @return A data frame with normalized dates, grouped and summed by:
#'   Provider_Code, Treatment_Function_Code, and the normalized date bins.
#'   The report_date column will be set to the reference_date.
#'
#' @examples
#' \dontrun{
#' # Normalize incomplete pathway data
#' normalized <- normalize_reports(incomplete_data, date_prefix = "arrived")
#' 
#' # Normalize admitted data with specific reference date
#' normalized <- normalize_reports(admitted_data, 
#'                                  reference_date = as.Date("2024-12-31"),
#'                                  date_prefix = "removed")
#' }
#'
#' @importFrom dplyr %>% mutate group_by summarise ungroup
#' @export

normalize_reports <- function(wl_data, reference_date = NULL, date_prefix = "arrived") {
  library(dplyr)
  
  # Determine column names based on prefix
  col_since <- paste0(date_prefix, "_since")
  col_before <- paste0(date_prefix, "_before")
  
  # Check that required columns exist
  required_cols <- c(col_since, col_before, "report_date", "n")
  missing_cols <- setdiff(required_cols, colnames(wl_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Ensure date columns are Date type
  wl_data <- wl_data %>%
    mutate(
      across(c(all_of(col_since), all_of(col_before), report_date), as.Date)
    )
  
  # Set reference date to max report_date if not specified
  if (is.null(reference_date)) {
    reference_date <- max(wl_data$report_date, na.rm = TRUE)
    message("Using reference date: ", reference_date)
  } else {
    reference_date <- as.Date(reference_date)
  }
  
  # Calculate the age of each bin boundary relative to its report date
  # Then normalize to the reference date
  wl_data_normalized <- wl_data %>%
    mutate(
      days_since_since = as.numeric(difftime(report_date, .data[[col_since]], units = "days")),
      days_since_before = as.numeric(difftime(report_date, .data[[col_before]], units = "days")),
      normalized_since = reference_date - days_since_since,
      normalized_before = reference_date - days_since_before
    ) %>%
    select(-days_since_since, -days_since_before, -all_of(col_since), -all_of(col_before), -report_date)
  
  # Group by provider, treatment function, and normalized date bins, then sum counts
  grouping_cols <- c("Provider_Code", "Provider_Name", "Treatment_Function_Code", "Treatment_Function", "normalized_since", "normalized_before")
  
  # Only include grouping columns that exist in the data
  existing_grouping_cols <- intersect(grouping_cols, colnames(wl_data_normalized))
  
  wl_data_combined <- wl_data_normalized %>%
    group_by(across(all_of(existing_grouping_cols))) %>%
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
    mutate(report_date = reference_date)
  
  # Rename normalized columns back to original names
  wl_data_combined <- wl_data_combined %>%
    rename(
      !!col_since := normalized_since,
      !!col_before := normalized_before
    )
  
  return(wl_data_combined)
}

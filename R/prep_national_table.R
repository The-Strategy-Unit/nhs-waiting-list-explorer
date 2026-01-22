# Quarto table to display the National Waiting list table

# This script generates a national-level summary table of Waiting List Data

# The script assumes that the following files are present:
# - data/all_national_new_periods.rds
# - data/all_national_incomplete_stats.rds
# These can be generated using the scripts:
# - inst/scripts/download_national_data.R
# - inst/scripts/process_national_data.R

# Below we :
# 1. Load the preprocessed data
# 2a. Extract the latest report dates for incompletes
# 2b. And we extract the same report from a year ago
# 3. We compute mean arrivals per provider
# 4. We then combine and process rows to generate a final table
# 5. Finally we render the table using reactable adding filtering and sorting


########################
# 0a. Required libraries
########################

library(readxl)
library(dplyr)
library(tidyr)
library(reactable)
library(htmlwidgets)
library(htmltools) 
source("R/download_national_data.R")
source("R/process_national_data.R")

########################
# 0b. Load and process data 
########################
reload = TRUE
if(reload ==TRUE) {
  download_national_data()
}

reload = FALSE
if(reload==FALSE) {
  process_national_data(reload=FALSE)
}

#################################
# 0c. Define the latest report date
#################################
# max_report_date <- max(all_national_incomplete$report_date, na.rm = TRUE)
max_report_date <- as.Date("2025-11-01") # Override for testing with fixed date

################################
# 1. Load the preprocessed data
################################ 

all_national_incomplete <- readRDS("data/all_national_incomplete_stats.rds")
all_national_new_periods <- readRDS("data/all_national_new_periods.rds")

#########################################################
# 2. We extract the report for incompletes
#########################################################
# TODO: Correct so that mean is over the last years data only
# TODO: Don't name things 2024 just say a year ago

# There are some old labels (when Area Code and 95th percentile was used)
# Remove Area_Team_Code if present to avoid carrying it through joins
all_national_incomplete <-
  all_national_incomplete |>
  dplyr::select(-any_of("Area_Team_Code"))

# Drop 95th percentile column(s) if present (handle both common name variants)
cols_95 <- grep("(?i)95th", names(all_national_incomplete),
  value = TRUE, perl = TRUE
)

if (length(cols_95) > 0) {
  all_national_incomplete <- all_national_incomplete |> dplyr::select(-any_of(cols_95))
}

# Relabel some columns from national format
# Rename as Queue_Size for clarity
all_national_incomplete <- all_national_incomplete |>
  dplyr::rename(Queue_Size = Total_number_of_incomplete_pathways)
all_national_incomplete <- all_national_incomplete |>
  dplyr::rename(percentile_92 = "92nd_percentile_waiting_time_(in_weeks)")
all_national_incomplete$Queue_Size <- as.numeric(all_national_incomplete$Queue_Size)

#########################################################
# 2a. The latest report date
#########################################################


incomplete_data <- dplyr::filter(
  all_national_incomplete,
  report_date == max_report_date
)


##############################################################
# 2b. The same report date from a year ago (approx 52 weeks)
##############################################################

# compute the date 12 months before the latest report_date (handles Date/POSIX)
one_year_ago <- seq(as.Date(max_report_date), length = 2, by = "-12 months")[2]
# extract rows for that report date (year-ago snapshot)
incomplete_data_year_ago <- dplyr::filter(all_national_incomplete, report_date == one_year_ago)


# We don't need all columns so we select and rename what we need
a_year_ago <- incomplete_data_year_ago[, c(
  "Provider_Code",
  "Treatment_Function_Code",
  "Queue_Size",
  "Total_within_18_weeks",
  "%_within_18_weeks",
  "percentile_92",
  "Average_(median)_waiting_time_(in_weeks)"
)]

# Rename the median waiting time column
colnames(a_year_ago)[colnames(a_year_ago) == "Average_(median)_waiting_time_(in_weeks)"] <- "median_wait_year_ago"

# Don't multiply by 100 here; will be done consistently later with current year data
a_year_ago$`%_within_18_weeks` <- as.numeric(a_year_ago$`%_within_18_weeks`)


colnames(a_year_ago)[colnames(a_year_ago) == "Queue_Size"] <- "Queue_size_year_ago"
colnames(a_year_ago)[colnames(a_year_ago) == "Total_within_18_weeks"] <- "Total_within_18_weeks_year_ago"
colnames(a_year_ago)[colnames(a_year_ago) == "%_within_18_weeks"] <- "%_within_18_weeks_year_ago"
colnames(a_year_ago)[colnames(a_year_ago) == "percentile_92"] <- "92nd_percentile_year_ago"
colnames(a_year_ago)[colnames(a_year_ago) == "Total_within_18_weeks"] <- "Total_within_18_weeks_year_ago"
colnames(a_year_ago)[colnames(a_year_ago) == "%_within_18_weeks"] <- "%_within_18_weeks_year_ago"


##########################################
# 3. We compute mean arrivals per provider
##########################################

mean_rows_df <- all_national_new_periods |>
  dplyr::filter(
    as.Date(report_date) >= as.Date(one_year_ago),
    as.Date(report_date) <= as.Date(max_report_date)
  ) |>
  dplyr::group_by(
    Provider_Code,
    Treatment_Function_Code,
    Provider_Name,
    Treatment_Function,
    Region_Code
  ) |>
  dplyr::summarise(
    Mean_Arrival = mean(n, na.rm = TRUE),
    .groups = "drop"
  )

###############################################################
# 4. We then combine and process to generate a final table
###############################################################

# Join the incompletes data with the arrival rate data
joined_data <- incomplete_data |>
  dplyr::left_join(mean_rows_df, by = c("Provider_Code", "Treatment_Function_Code"))

# remove duplicated columns and zero entries
joined_data <- joined_data[, !grepl("\\.y$", names(joined_data)), drop = FALSE]
names(joined_data) <- sub("\\.x$", "", names(joined_data))
#
joined_data <- subset(
  joined_data,
  Mean_Arrival != 0 & Queue_Size != 0
)

# Now we merge with date from a year-ago
joined_data <- merge(joined_data, a_year_ago, by = c("Provider_Code", "Treatment_Function_Code"), all.x = TRUE)

View(head(joined_data))
# Call it final_table
final_table <- joined_data

View(head(final_table))

final_table <- final_table |>
  dplyr::rename(median_wait = "Average_(median)_waiting_time_(in_weeks)")
# Remove unwanted columns
final_table$Total_52_plus_weeks <- NULL
final_table$Total_78_plus_weeks <- NULL
final_table$Total_65_plus_weeks <- NULL
final_table$`%_52_plus_weeks` <- NULL
final_table$Region_Code <- NULL
final_table$`Average_(median)_waiting_time_(in_weeks)` <- NULL
final_table$report_date <- NULL
final_table$source_file <- NULL


# Make sure the types of columns are correct
final_table[, 1:4] <- lapply(final_table[, 1:4], function(x) as.character(x))
final_table[, 5:ncol(final_table)] <- lapply(final_table[, 5:ncol(final_table)], function(x) as.numeric(as.character(x)))

# Calculate Improvement within 18 weeks
final_table$Relative_Improvement_in_18_weeks <-
  (final_table$Total_within_18_weeks_year_ago - final_table$Total_within_18_weeks) / final_table$Total_within_18_weeks_year_ago


final_table$Change_in_18_weeks <-
  (final_table$Total_within_18_weeks_year_ago - final_table$Total_within_18_weeks) 


  final_table$`%_within_18_weeks` <- final_table$`%_within_18_weeks` * 100
  final_table$`%_within_18_weeks_year_ago` <- final_table$`%_within_18_weeks_year_ago` * 100


# Calculate percentage point change in %_within_18_weeks
final_table$`%_within_18_weeks_Change` <-
  final_table$`%_within_18_weeks` - final_table$`%_within_18_weeks_year_ago`
#View(head(final_table))

final_table$median_change <- final_table$median_wait - final_table$median_wait_year_ago

# Calculate Improvement in 92nd percentile
final_table$percentile_improvement <- (final_table$percentile_92 - final_table$`92nd_percentile_year_ago`)

# Calculate Relative Improvement in 92nd percentile
final_table$percentile_relative_improvement <- (final_table$percentile_92 - final_table$`92nd_percentile_year_ago`) / final_table$`92nd_percentile_year_ago`

# Calculate Changes in Queue Size
final_table$Queue_Size_Change <- final_table$Queue_Size - final_table$Queue_size_year_ago
final_table$`%_Queue_Size_Change` <- 100 * (final_table$Queue_Size - final_table$Queue_size_year_ago) / final_table$Queue_size_year_ago


# Calculate Target Queue Size
final_table$Target_Q_Size <- round(as.numeric(final_table$Mean_Arrival) * (18 / (2.52 * 4.345)))

# Calculate Queue Ratio
final_table$Queue_Ratio <- as.numeric(final_table$Queue_Size) / final_table$Target_Q_Size

# Calculated departure rate and Load
final_table$Mean_Departure <- final_table$Mean_Arrival - (final_table$Queue_Size - final_table$Queue_size_year_ago) / 12
final_table$Load <- final_table$Mean_Arrival / final_table$Mean_Departure


# Round numbers a bit for presentation purposes
final_table$Percentile_Pressure <- round(as.numeric(final_table$percentile_92) / 18, 1)
final_table$`%_within_18_weeks` <- as.numeric(final_table$`%_within_18_weeks`)
final_table$Mean_Arrival <- round(as.numeric(final_table$Mean_Arrival), 1)
final_table$`%_within_18_weeks` <- round(as.numeric(final_table$`%_within_18_weeks`), 1)
final_table$Load <- round(as.numeric(final_table$Load), 2)
final_table$Queue_Ratio <- round(final_table$Queue_Ratio, 2)
final_table$percentile_92 <- round(as.numeric(final_table$percentile_92), 1)

# reset rows
rownames(final_table) <- NULL


# Drop unwanted columns
cols_to_remove <- c(
  "95th_percentile_waiting_time_(in_weeks)",
  "Area_Team_Code",
  "Mean_Arrival_Rank"
)
cols_present <- intersect(names(final_table), cols_to_remove)
if (length(cols_present) > 0) {
  final_table[cols_present] <- NULL
}

# ONLY INCLUDE FULL ROWS WITH FINITE VALUES
final_table_finite <- final_table[complete.cases(final_table), ]
numeric_cols <- sapply(final_table, is.numeric)
final_table_finite <- final_table[apply(final_table[, numeric_cols], 1, function(row) all(is.finite(row))), ]
final_table <- final_table_finite[final_table_finite$Queue_Size != 0, ]

View(head(final_table))

# Reorder columns to put Treatment_Function_Code last and include percentile_relative_improvement
finalized_table <- final_table[, intersect(c(
  "Provider_Code",
  "Provider_Name",
  "Treatment_Function_Code",
  "Treatment_Function",
  "Queue_Size",
  "Target_Q_Size",
  "Queue_Ratio",
  "median_wait",
  "percentile_92",
  "%_within_18_weeks",
  "Percentile_Pressure",
  "median_change",
  "%_within_18_weeks_Change",
  "Queue_Size_Change",
  "Relative_Improvement_in_18_weeks",
  "%_Queue_Size_Change",
  "percentile_improvement",
  "Load"
), names(final_table))]

saveRDS(finalized_table, "data/finalized_table.rds")

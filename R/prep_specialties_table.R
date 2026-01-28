all_national_incomplete <- readRDS("data/all_national_incomplete_stats.rds")
incomplete_pathway_data <- readRDS("data/all_national_incomplete.rds")

library(purrr)
library(lubridate)
library(tidyr)

View(head(all_national_incomplete))
View(head(incomplete_pathway_data))

latest_report_date <- max(incomplete_pathway_data$report_date)

latest_report_date <- as.Date(latest_report_date)
report_date_year_ago <- latest_report_date - years(1)

###### GET PERCENTILES FOR INCOMPLETE PATHWAYS ######


# Filter incomplete_pathway_data by the latest report date
filtered_incomplete_pathway_data <- incomplete_pathway_data[incomplete_pathway_data$report_date == latest_report_date, ]

# Group by Treatment_Function_Code, Treatment_Function, report_date, arrived_since, arrived_before and sum n
grouped_data <- filtered_incomplete_pathway_data |>
  dplyr::group_by(Treatment_Function_Code, Treatment_Function, report_date, arrived_since, arrived_before) |>
  dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = 'drop')



# Source the wl_percentile_hist function
source("R/utils_percentile_hist.R")

View(head(grouped_data))

# Create a table with median and 92nd percentile for each Treatment_Function_Code
percentile_table <- grouped_data %>%
  rename(arrival_since = arrived_since, arrival_before = arrived_before) %>%
  nest(data = -c(Treatment_Function_Code, Treatment_Function)) %>%
  mutate(
    median_weeks = map_dbl(data, ~wl_percentile_hist(.x, percentage = 50)$weeks),
    percentile_92_weeks = map_dbl(data, ~wl_percentile_hist(.x, percentage = 92)$weeks)
  ) %>%
  select(Treatment_Function_Code, Treatment_Function, median_weeks, percentile_92_weeks)

View(percentile_table)


########## GET QUEUE SIZES NOW AND A YEAR AGO ##########


# Group by just Treatment_Function_Code and sum n at the latest report date
grouped_data_by_code <- filtered_incomplete_pathway_data |>
  dplyr::group_by(Treatment_Function_Code) |>
  dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = 'drop')

# Rename column n to Queue_Size
grouped_data_by_code <- grouped_data_by_code %>%
    rename(Queue_Size = n)

View(grouped_data_by_code)

# Filter incomplete_pathway_data by the report date one year ago
filtered_incomplete_pathway_data_year_ago <- incomplete_pathway_data[incomplete_pathway_data$report_date == report_date_year_ago, ]

# Group by just Treatment_Function_Code and sum n one year ago
grouped_data_by_code_year_ago <- filtered_incomplete_pathway_data_year_ago |>
  dplyr::group_by(Treatment_Function_Code) |>
  dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = 'drop')

# Rename column n to Queue_Size_year_ago
grouped_data_by_code_year_ago <- grouped_data_by_code_year_ago %>%
    rename(Queue_Size_year_ago = n)
View(grouped_data_by_code_year_ago)

###### GET ARRIVAL RATES #######


all_national_new_periods <- readRDS("data/all_national_new_periods.rds")
View(head(all_national_new_periods))

# Filter all_national_new_periods for the latest report date and group by Treatment_Function_Code and Treatment_Function
new_periods_grouped <- all_national_new_periods %>%
    filter(report_date <= latest_report_date & report_date > report_date_year_ago) %>%
    dplyr::group_by(Treatment_Function_Code, Treatment_Function) %>%
    dplyr::summarise(n = sum(n, na.rm = TRUE) / 12, .groups = "drop") %>%
    rename(arrival_rate = n)

View((new_periods_grouped))

specialties_table <- percentile_table %>%
    left_join(grouped_data_by_code, by = "Treatment_Function_Code") %>%
    left_join(grouped_data_by_code_year_ago, by = "Treatment_Function_Code") %>%
    left_join(new_periods_grouped, by = c("Treatment_Function_Code", "Treatment_Function"))

# Calculate Target Queue Size
specialties_table$Target_Q_Size <- round(as.numeric(specialties_table$arrival_rate) * (18 / (2.52 * 4.345)))

# Calculate Queue Ratio
specialties_table$Queue_Ratio <- as.numeric(specialties_table$Queue_Size) / specialties_table$Target_Q_Size

# Calculate Percentile Pressure (92nd percentile weeks relative to 18 weeks)
specialties_table$Percentile_Pressure <- round(as.numeric(specialties_table$percentile_92_weeks) / 18, 2)

# Calculate Mean Departure and Load
specialties_table$Mean_Departure <- as.numeric(specialties_table$arrival_rate) - (as.numeric(specialties_table$Queue_Size) - as.numeric(specialties_table$Queue_Size_year_ago)) / 12
specialties_table$Load <- as.numeric(specialties_table$arrival_rate) / specialties_table$Mean_Departure

## Summary table: key pressure metrics by treatment function
specialties_summary <- specialties_table %>%
  dplyr::select(
    Treatment_Function_Code,
    Treatment_Function,
    Queue_Ratio,
    Percentile_Pressure,
    Load
  )



View(specialties_summary)


# Z-score, composite score, and ranking for treatment functions
queue_ratio_mean <- mean(specialties_summary$Queue_Ratio, na.rm = TRUE)
queue_ratio_sd   <- sd(specialties_summary$Queue_Ratio, na.rm = TRUE)
percentile_pressure_mean <- mean(specialties_summary$Percentile_Pressure, na.rm = TRUE)
percentile_pressure_sd   <- sd(specialties_summary$Percentile_Pressure, na.rm = TRUE)
load_mean <- mean(specialties_summary$Load, na.rm = TRUE)
load_sd   <- sd(specialties_summary$Load, na.rm = TRUE)

specialties_ranked <- specialties_summary |>
  dplyr::mutate(
    Queue_Ratio_Z = (Queue_Ratio - queue_ratio_mean) / queue_ratio_sd,
    Percentile_Pressure_Z = (Percentile_Pressure - percentile_pressure_mean) / percentile_pressure_sd,
    Load_Z = (Load - load_mean) / load_sd,
    Score = (Queue_Ratio_Z + Percentile_Pressure_Z + Load_Z) / 3,
    Ranking = rank(Score, ties.method = "first")
  ) |>
  dplyr::arrange(Ranking) |>
  dplyr::select(
    Treatment_Function_Code,
    Treatment_Function,
    Queue_Ratio,
    Percentile_Pressure,
    Load,
    Score,
    Ranking
  )

View(specialties_ranked)

# Save ranked specialties table to data folder
saveRDS(specialties_ranked, "data/specialties_ranked.rds")









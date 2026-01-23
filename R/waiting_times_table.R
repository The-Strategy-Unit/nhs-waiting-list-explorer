# Load and view the RDS file
library(lubridate)

admitted_data <- readRDS("data/all_national_admitted.rds")
non_admitted_data <- readRDS("data/all_national_non_admitted.rds")


View(head(non_admitted_data))
latest_date <- max(non_admitted_data$report_date) %>% as.Date()
year_ago_date <- latest_date - lubridate::years(1)

admitted_filtered <- admitted_data %>%
    filter(report_date <= latest_date & report_date > year_ago_date)

non_admitted_filtered <- non_admitted_data %>%
    filter(report_date <= latest_date & report_date > year_ago_date)



# ---------------------
# Inline normalization test (does not alter above code)
# ---------------------

library(dplyr)
source("R/normalize_reports.R")

cat("\nUsing already loaded non_admitted_data for test...\n")

cat("Filtering data for Provider RBL, Treatment Function 101/C_101 (Nov 2024 & Nov 2025)...\n")
test_data <- non_admitted_data %>%
    dplyr::mutate(report_date = as.Date(report_date)) %>%
    dplyr::filter(
        Provider_Code == "RBL",
        Treatment_Function_Code %in% c("101", "C_101"),
        report_date %in% c(as.Date("2024-11-30"), as.Date("2025-11-30"))
    )

cat("Rows:", nrow(test_data), " Unique report dates:", paste(unique(test_data$report_date), collapse = ", "), "\n")

View(test_data)

cat("Normalizing to reference date 2050-12-31...\n")
normalized <- normalize_reports(
    test_data,
    reference_date = as.Date("2050-12-31"),
    # Columns are still named arrived_* in non_admitted_data
    date_prefix = "arrived"
)

cat("\nNormalized data (head):\n")
print(utils::head(normalized))

View(normalized)

cat("\nSummary: total rows =", nrow(normalized), ", total patients =", sum(normalized$n, na.rm = TRUE), "\n")


# Percentile calculations (median and 92nd percentile) on normalized data
source("R/wl_percentile_hist.R")

cat("\nRenaming columns: arrived_* to arrival_*...\n")
normalized <- normalized %>%
    dplyr::rename(
        arrival_before = arrived_before,
        arrival_since = arrived_since
    )

colnames(normalized)

p50 <- wl_percentile_hist(normalized, percentage = 50)
p92 <- wl_percentile_hist(normalized, percentage = 92)

cat("\nPercentiles (reference date:", as.character(unique(normalized$report_date)), ")\n")
cat("Median (50th): date =", as.character(p50$date), ", weeks =", round(p50$weeks, 2), "\n")
cat("92nd percentile: date =", as.character(p92$date), ", weeks =", round(p92$weeks, 2), "\n")



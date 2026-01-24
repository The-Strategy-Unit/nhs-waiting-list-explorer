# Load and view the RDS file
library(lubridate)
library(dplyr)

admitted_data <- readRDS("data/all_national_admitted.rds")
non_admitted_data <- readRDS("data/all_national_non_admitted.rds")

View(head(admitted_data))

# Remove rows where n is zero
admitted_data <- admitted_data %>% filter(n > 0)
non_admitted_data <- non_admitted_data %>% filter(n > 0)

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



# -----------------------------------------------------------------------------
# Batch percentile table for all providers/treatments (admitted & non-admitted)
# -----------------------------------------------------------------------------

source("R/normalize_reports.R")
source("R/wl_percentile_hist.R")

reference_date <- as.Date("2050-01-01")

compute_percentiles <- function(df, label) {

    if (nrow(df) == 0) return(dplyr::tibble())

    norm <- normalize_reports(
        df,
        reference_date = reference_date,
        date_prefix = "arrived"
    )

    # wl_percentile_hist expects arrival_* column names
    if (all(c("arrived_since", "arrived_before") %in% names(norm))) {
        norm <- norm %>%
            dplyr::rename(
                arrival_since = arrived_since,
                arrival_before = arrived_before
            )
    }

    norm %>%
        dplyr::group_by(Provider_Code, Provider_Name, Treatment_Function_Code, Treatment_Function) %>%
        {
            groups <- dplyr::group_split(., .keep = TRUE)
            total_groups <- length(groups)
            if (total_groups == 0) return(dplyr::tibble())
            start_time <- Sys.time()
            cat("  Groups to process:", total_groups, "\n")

            res_list <- vector("list", total_groups)
            for (i in seq_along(groups)) {
                g <- groups[[i]]
                p50 <- wl_percentile_hist(g, percentage = 50)
                p92 <- wl_percentile_hist(g, percentage = 92)
                res_list[[i]] <- dplyr::tibble(
                    Provider_Code = g$Provider_Code[1],
                    Provider_Name = g$Provider_Name[1],
                    Treatment_Function_Code = g$Treatment_Function_Code[1],
                    Treatment_Function = g$Treatment_Function[1],
                    median_weeks = as.numeric(p50$weeks),
                    p92_weeks = as.numeric(p92$weeks),
                    average_list_size = sum(g$n, na.rm = TRUE) / 12
                )

                if (i == 1 || i %% 10 == 0 || i == total_groups) {
                    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                    rate <- if (elapsed > 0) i / elapsed else NA_real_
                    remaining <- if (!is.na(rate) && rate > 0) (total_groups - i) / rate else NA_real_
                    cat(sprintf("  Progress [%d/%d] (%.1f%%) elapsed: %.1fs ETA: %s\n",
                                i, total_groups, 100 * i / total_groups, elapsed,
                                ifelse(is.na(remaining), "n/a", sprintf("%.1fs", remaining))))
                }
            }

            dplyr::bind_rows(res_list)
        } %>%
        dplyr::ungroup() %>%
        dplyr::mutate(patient_type = label) %>%
        dplyr::relocate(patient_type, .after = Treatment_Function_Code)
}

cat("\nComputing percentiles for all providers/treatments (admitted & non-admitted)...\n")

admitted_percentiles <- compute_percentiles(admitted_filtered, "admitted")
non_admitted_percentiles <- compute_percentiles(non_admitted_filtered, "non-admitted")

all_percentiles <- dplyr::bind_rows(admitted_percentiles, non_admitted_percentiles)

# Save to RDS
saveRDS(all_percentiles, file = "data/waiting_times_table.rds")

cat("\nWaiting times table saved to data/waiting_times_table.rds\n")







all_national_incomplete <- readRDS("data/all_national_incomplete.rds")
all_national_new_periods <- readRDS("data/all_national_new_periods.rds")

# Find maximum report date in all_national_incomplete
max_report_date <- max(all_national_incomplete$Report_Date, na.rm = TRUE)
print(max_report_date)

View(head(all_national_incomplete))

# Find maximum report date in all_national_incomplete
max_report_date <- max(all_national_incomplete$report_date, na.rm = TRUE)
print(max_report_date)




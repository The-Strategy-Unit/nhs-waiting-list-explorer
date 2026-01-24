create_waiting_times_table <- function(waiting_times_table) {
  # Color scale function similar to specialties/national tables but with adjustable threshold
  color_scale <- function(value, min_val, max_val, threshold) {
    if (is.na(value)) return("transparent")

    if (value >= threshold) {
      # Red scale for values at/above threshold
      if (max_val > threshold) {
        intensity <- (value - threshold) / (max_val - threshold)
      } else {
        intensity <- 0
      }
      # Light red near threshold, darker red at max
      red <- 255
      green <- as.integer(255 - (intensity * 140))
      blue <- as.integer(255 - (intensity * 140))
      sprintf("rgb(%d, %d, %d)", red, green, blue)
    } else {
      # Green scale for values below threshold
      if (min_val < threshold) {
        intensity <- (threshold - value) / (threshold - min_val)
      } else {
        intensity <- 0
      }
      # Light green near threshold, darker green at min
      red <- as.integer(255 - (intensity * 140))
      green <- 255
      blue <- as.integer(255 - (intensity * 140))
      sprintf("rgb(%d, %d, %d)", red, green, blue)
    }
  }

  reactable::reactable(
    waiting_times_table,
    defaultPageSize = 15,
    filterable = TRUE,
    searchable = TRUE,
    columns = list(
      Provider_Name = reactable::colDef(name = "Provider", filterable = TRUE),
      Treatment_Function = reactable::colDef(name = "Treatment Function", filterable = TRUE),
      patient_type = reactable::colDef(name = "Patient Type", filterable = TRUE, width = 120),
      median_weeks = reactable::colDef(
        name = "Median Weeks",
        format = reactable::colFormat(digits = 1),
        width = 120,
        style = function(value) {
          list(background = color_scale(value, min(waiting_times_table$median_weeks, na.rm = TRUE), max(waiting_times_table$median_weeks, na.rm = TRUE), threshold = 8))
        }
      ),
      p92_weeks = reactable::colDef(
        name = "92nd Percentile Weeks",
        format = reactable::colFormat(digits = 1),
        width = 150,
        style = function(value) {
          list(background = color_scale(value, min(waiting_times_table$p92_weeks, na.rm = TRUE), max(waiting_times_table$p92_weeks, na.rm = TRUE), threshold = 18))
        }
      ),
      average_list_size = reactable::colDef(name = "Average List Size", format = reactable::colFormat(digits = 1), width = 140)
    )
  )
}

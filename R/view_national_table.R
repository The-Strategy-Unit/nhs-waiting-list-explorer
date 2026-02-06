create_national_view_table <- function(finalized_table) {
  national_view <- finalized_table |>
    dplyr::filter(Treatment_Function_Code == "C_999") |>
    dplyr::select(Provider_Name, Queue_Ratio, Percentile_Pressure, Load)
  
  # Calculate means and standard deviations
  queue_ratio_mean <- mean(national_view$Queue_Ratio, na.rm = TRUE)
  queue_ratio_sd <- sd(national_view$Queue_Ratio, na.rm = TRUE)
  percentile_pressure_mean <- mean(national_view$Percentile_Pressure, na.rm = TRUE)
  percentile_pressure_sd <- sd(national_view$Percentile_Pressure, na.rm = TRUE)
  load_mean <- mean(national_view$Load, na.rm = TRUE)
  load_sd <- sd(national_view$Load, na.rm = TRUE)
  
  # Calculate Z-scores and composite score
  national_view <- national_view |>
    dplyr::mutate(
      Queue_Ratio_Z = (Queue_Ratio - queue_ratio_mean) / queue_ratio_sd,
      Percentile_Pressure_Z = (Percentile_Pressure - percentile_pressure_mean) / percentile_pressure_sd,
      Load_Z = (Load - load_mean) / load_sd,
      Score = (Queue_Ratio_Z + Percentile_Pressure_Z + Load_Z) / 3,
      Ranking = rank(Score, ties.method = "first")
    ) |>
    dplyr::arrange(Ranking) |>
    dplyr::select(Provider_Name, Queue_Ratio, Percentile_Pressure, Load, Score, Ranking)
  
  # Color scale function: red above 1, green below 1, lighter as approaching 1
  color_scale <- function(value, min_val, max_val) {
    if (is.na(value)) return("transparent")
    
    if (value >= 1) {
      # Red scale for values >= 1
      # Normalize between 1 and max_val
      if (max_val > 1) {
        intensity <- (value - 1) / (max_val - 1)
      } else {
        intensity <- 0
      }
      # Light red near 1, darker red at max
      red <- 255
      green <- as.integer(255 - (intensity * 140))
      blue <- as.integer(255 - (intensity * 140))
      sprintf("rgb(%d, %d, %d)", red, green, blue)
    } else {
      # Green scale for values < 1
      # Normalize between min_val and 1
      if (min_val < 1) {
        intensity <- (1 - value) / (1 - min_val)
      } else {
        intensity <- 0
      }
      # Light green near 1, darker green at min
      red <- as.integer(255 - (intensity * 140))
      green <- 255
      blue <- as.integer(255 - (intensity * 140))
      sprintf("rgb(%d, %d, %d)", red, green, blue)
    }
  }
  
  reactable::reactable(
    national_view,
    defaultPageSize = 10,
    filterable = TRUE,
    columns = list(
      Provider_Name = reactable::colDef(name = "Provider", filterable = TRUE),
      Queue_Ratio = reactable::colDef(
        name = "Queue Size Ratio", 
        filterable = FALSE,
        style = function(value) {
          list(background = color_scale(value, min(national_view$Queue_Ratio, na.rm = TRUE), max(national_view$Queue_Ratio, na.rm = TRUE)))
        }
      ),
      Percentile_Pressure = reactable::colDef(
        name = "Pressure", 
        filterable = FALSE,
        style = function(value) {
          list(background = color_scale(value, min(national_view$Percentile_Pressure, na.rm = TRUE), max(national_view$Percentile_Pressure, na.rm = TRUE)))
        }
      ),
      Load = reactable::colDef(
        filterable = FALSE,
        style = function(value) {
          list(background = color_scale(value, min(national_view$Load, na.rm = TRUE), max(national_view$Load, na.rm = TRUE)))
        }
      ),
      Ranking = reactable::colDef(width = 100, filterable = FALSE),
      Score = reactable::colDef(name = "Overall Score", format = reactable::colFormat(digits = 2), filterable = FALSE)
    )
  )
}

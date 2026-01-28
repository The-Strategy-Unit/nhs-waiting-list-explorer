create_specialties_view_table <- function(specialties_ranked) {
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
    specialties_ranked,
    defaultPageSize = 10,
    filterable = TRUE,
    columns = list(
      Treatment_Function_Code = reactable::colDef(name = "Specialty Code", filterable = TRUE),
      Treatment_Function = reactable::colDef(name = "Specialty", filterable = TRUE),
      Queue_Ratio = reactable::colDef(
        name = "Queue Size Ratio",
        format = reactable::colFormat(digits = 2),
        style = function(value) {
          list(background = color_scale(value, min(specialties_ranked$Queue_Ratio, na.rm = TRUE), max(specialties_ranked$Queue_Ratio, na.rm = TRUE)))
        }
      ),
      Percentile_Pressure = reactable::colDef(
        name = "Pressure",
        format = reactable::colFormat(digits = 2),
        style = function(value) {
          list(background = color_scale(value, min(specialties_ranked$Percentile_Pressure, na.rm = TRUE), max(specialties_ranked$Percentile_Pressure, na.rm = TRUE)))
        }
      ),
      Load = reactable::colDef(
        name = "Load",
        format = reactable::colFormat(digits = 2),
        style = function(value) {
          list(background = color_scale(value, min(specialties_ranked$Load, na.rm = TRUE), max(specialties_ranked$Load, na.rm = TRUE)))
        }
      ),
      Score = reactable::colDef(name = "Overall Score", format = reactable::colFormat(digits = 2)),
      Ranking = reactable::colDef(name = "Rank", width = 100)
    )
  )
}

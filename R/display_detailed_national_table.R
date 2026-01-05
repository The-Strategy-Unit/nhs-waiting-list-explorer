# Custom numeric filter: show rows with value >= filter input
numeric_filter_method <- reactable::JS(
  "function(rows, id, filterValue) {\n",
  "  if (filterValue === undefined || filterValue === null || filterValue === '' || isNaN(Number(filterValue))) { return rows; }\n",
  "  var num = Number(filterValue);\n",
  "  return rows.filter(function(row) {\n",
  "    var value = row.values[id];\n",
  "    var valNum = Number(value);\n",
  "    if (value === null || value === undefined || value === '' || isNaN(valNum) || !isFinite(valNum)) { return false; }\n",
  "    return valNum >= num;\n",
  "  });\n",
  "}"
)

# Dynamic Rank numbering: remove any existing Row/Rank then add placeholder as first column
existing_rank_cols <- intersect(names(finalized_table), c("Row", "Rank"))
if (length(existing_rank_cols)) finalized_table[existing_rank_cols] <- NULL
finalized_table <- cbind(Rank = NA_integer_, finalized_table)
## Remove any automatic row names so reactable doesn't render an extra index column
rownames(finalized_table) <- NULL



# Custom color function for gradients (light blue to light red) from tom_report_3
gradient_color <- function(value, min, max) {
  if (is.na(value)) {
    return(NA)
  }
  # Interpolate between light blue (#add8e6) and light red (#ffb3b3)
  pal <- colorRampPalette(c("#add8e6", "#ffb3b3"))
  n <- 100
  colors <- pal(n)
  idx <- as.integer((value - min) / (max - min) * (n - 1)) + 1
  colors[pmax(pmin(idx, n), 1)]
}

# Reverse gradient (light red to light blue) so higher values are blue
gradient_color_rev <- function(value, min, max) {
  if (is.na(value)) {
    return(NA)
  }
  pal <- colorRampPalette(c("#ffb3b3", "#add8e6"))
  n <- 100
  colors <- pal(n)
  idx <- as.integer((value - min) / (max - min) * (n - 1)) + 1
  colors[pmax(pmin(idx, n), 1)]
}


# Gradient color by rank: 1st is red, last is blue (same palette as above)
gradient_color_by_rank <- function(values) {
  pal <- colorRampPalette(c("#ffb3b3", "#add8e6")) # Red to blue
  n <- length(values)
  colors <- rep(NA_character_, n)
  valid <- which(is.finite(values) & !is.na(values))
  if (length(valid) > 0) {
    # Rank so that highest value is rank 1 (red), lowest is last (blue)
    ranks <- rank(-values[valid], ties.method = "first")
    pal_colors <- pal(length(valid))
    colors[valid] <- pal_colors[ranks]
  }
  colors
}

# Calculate min/max for relevant columns
min_queue_ratio <- min(finalized_table$Queue_Ratio[is.finite(finalized_table$Queue_Ratio)], na.rm = TRUE)
max_queue_ratio <- max(finalized_table$Queue_Ratio[is.finite(finalized_table$Queue_Ratio)], na.rm = TRUE)
min_percentile_pressure <- min(finalized_table$Percentile_Pressure[is.finite(finalized_table$Percentile_Pressure)], na.rm = TRUE)
max_percentile_pressure <- max(finalized_table$Percentile_Pressure[is.finite(finalized_table$Percentile_Pressure)], na.rm = TRUE)
min_queue_size <- min(finalized_table$Queue_Size[is.finite(finalized_table$Queue_Size)], na.rm = TRUE)
max_queue_size <- max(finalized_table$Queue_Size[is.finite(finalized_table$Queue_Size)], na.rm = TRUE)
min_queue_size_change_pct <- min(finalized_table$`%_Queue_Size_Change`[is.finite(finalized_table$`%_Queue_Size_Change`)], na.rm = TRUE)
max_queue_size_change_pct <- max(finalized_table$`%_Queue_Size_Change`[is.finite(finalized_table$`%_Queue_Size_Change`)], na.rm = TRUE)

# Define columns that should use default string filter
# String filter columns, split to multiple lines for readability
string_filter_cols <- c(
  "Provider_Code",
  "Provider_Name",
  "Treatment_Function_Code",
  "Treatment_Function"
)

# Auto-insert Rank column if missing (supports running bottom block alone)
if (!"Rank" %in% names(finalized_table)) {
  finalized_table <- cbind(Rank = NA_integer_, finalized_table)
}

###############################################
## Build columns list with appropriate filterMethod
columns_list <- lapply(seq_along(names(finalized_table)), function(i) {
  col <- names(finalized_table)[i]
  if (col == "Rank") {
    # Per-page numbering (simpler & reliable); will restart each page
    reactable::colDef(
      name = "Rank",
      # Use CSS counter for numbering so that numbers always re-sequence
      # after filtering/sorting (no gaps, restart at 1 for visible rows on each page)
      # The cell content itself is left blank; numbers injected via ::before.
      cell = reactable::JS("function(cellInfo){ return ''; }"),
      sortable = FALSE,
      filterable = FALSE,
      width = 65,
      align = "center",
      sticky = "left",
      style = function(value) list(fontWeight = "bold"),
    )
  } else if (col %in% c("Provider_Code", "Provider_Name", "Treatment_Function_Code", "Treatment_Function")) {
    # Key string filter columns
    reactable::colDef(
      filterable = TRUE,
    )
  } else if (col == "Queue_Ratio") {
    queue_ratio_colours <- gradient_color_by_rank(finalized_table$Queue_Ratio)
    reactable::colDef(
      style = function(value, index) {
        list(background = queue_ratio_colours[index], fontWeight = "bold")
      },
      format = reactable::colFormat(digits = 2),
      name = "Queue Ratio",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "Queue_Size") {
    reactable::colDef(
      style = function(value) {
        list(fontWeight = "bold")
      },
      name = "Queue Size",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "Target_Q_Size") {
    reactable::colDef(
      style = function(value) {
        list(fontWeight = "bold")
      },
      name = "Target Q Size",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "Percentile_Pressure") {
    reactable::colDef(
      style = function(value) {
        list(
          background = gradient_color(
            value,
            min_percentile_pressure,
            max_percentile_pressure
          ),
          fontWeight = "bold"
        )
      },
      format = reactable::colFormat(digits = 2),
      name = "Percentile Pressure",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "Queue_Size_Change") {
    reactable::colDef(
      format = reactable::colFormat(digits = 0),
      name = "Queue Size Change",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "%_Queue_Size_Change") {
    reactable::colDef(
      format = reactable::colFormat(digits = 2),
      name = "% Queue Size Change",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "Load") {
    reactable::colDef(
      style = function(value) {
        if (is.na(value)) {
          return(NULL)
        }
        if (value > 1) {
          list(background = "#ffb3b3", fontWeight = "bold")
        } else {
          list(background = "#b3ffb3", fontWeight = "bold")
        }
      },
      format = reactable::colFormat(digits = 2),
      name = "Load",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "%_within_18_weeks") {
    reactable::colDef(
      style = function(value) {
        rng <- range(finalized_table$`%_within_18_weeks`, na.rm = TRUE)
        list(
          background = gradient_color_rev(value, rng[1], rng[2]),
          fontWeight = "bold"
        )
      },
      name = "% within 18 Weeks",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "Relative_Improvement_in_18_weeks") {
    reactable::colDef(
      format = reactable::colFormat(digits = 2),
      name = "Relative Improvement in 18 Weeks",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else if (col == "percentile_improvement") {
    reactable::colDef(
      format = reactable::colFormat(digits = 2),
      name = "Percentile Change",
      filterable = TRUE,
      filterMethod = numeric_filter_method
    )
  } else {
    reactable::colDef(
      filterable = TRUE
    )
  }
})
names(columns_list) <- names(finalized_table)

# Create the reactable widget with filters enabled
tbl_widget <- reactable::reactable(
  finalized_table,
  sortable = TRUE,
  filterable = TRUE,
  defaultSorted = "Provider_Code",
  elementId = "national_table",
  highlight = TRUE,
  bordered = TRUE,
  striped = TRUE,
  resizable = TRUE,
  defaultPageSize = 250,
  pageSizeOptions = c(25, 50, 100, 250, 500),
  defaultColDef = reactable::colDef(
    align = "center",
    minWidth = 100,
    filterable = TRUE
  ),
  columns = columns_list,
  theme = reactable::reactableTheme(
    cellPadding = "8px 12px",
    style = list(
      fontFamily = "Segoe UI, Arial, sans-serif",
      fontSize = "1em"
    ),
    inputStyle = list(color = "black", background = "white")
  ),
    columnGroups = list(
    reactable::colGroup(name = "Size", columns = c("Queue_Size", "Target_Q_Size", "Queue_Ratio")),
    reactable::colGroup(name = "Shape", columns = c("percentile_92", "%_within_18_weeks", "Percentile_Pressure")),
    reactable::colGroup(name = "Improvement", columns = c("Queue_Size_Change", "Relative_Improvement_in_18_weeks",
    "%_Queue_Size_Change", "percentile_improvement", "Load"))
  )
)


# Bits and pieces to improve appearance
# Add CSS for row numbering using counter (works with filtering/sorting)
# Also hide the filter box for the Rank column
row_number_css <- htmltools::tags$style(htmltools::HTML(
  "#national_table .rt-tbody { counter-reset: rowNumber; }\n#national_table .rt-tbody .rt-tr-group { counter-increment: rowNumber; }\n#national_table .rt-tbody .rt-tr-group .rt-td:nth-child(1)::before {\n  content: counter(rowNumber);\n  font-weight: bold;\n  display: inline-block;\n  width: 100%;\n  color: inherit;\n}\n"
))
hide_row_filter_css <- htmltools::tags$style(htmltools::HTML(
  "#national_table .rt-th:nth-child(1) input { display: none !important; }\n"
))


tbl_widget <- htmlwidgets::prependContent(tbl_widget, list(row_number_css, hide_row_filter_css))

tbl_widget
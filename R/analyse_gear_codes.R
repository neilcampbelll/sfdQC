#' Analyse Gear Codes by Country and Year
#'
#' @description
#' This function analyses the distribution of fishing gear codes across different countries
#' and years in a fisheries VMS dataset, providing both a tabular summary and a visualisation.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', and
#' 'gear_code' columns.
#'
#' @return A tibble summarising the number of records by country, year, and gear code,
#' arranged by country, year, and gear code. Additionally, prints a ggplot visualisation
#' showing the distribution of records across gear codes for each country and year.
#'
#' @details
#' The function aggregates VMS data by country, year, and gear code to produce record counts.
#' The resulting summary is visualised using a faceted bar chart with countries as facets, gear
#' codes on the x-axis, and years differentiated by colour. This analysis helps identify fishing
#' method patterns and potential data quality issues in gear code reporting.
#'
#' In fisheries management, understanding the distribution of fishing gears is important for
#' estimating fishing impact on different habitats and stocks. Common gear codes include OTB
#' (otter trawl bottom), PTM (pelagic pair trawl), DRB (dredge), and others defined in the
#' Data Collection Framework.
#'
#' @note
#' The function assumes the presence of 'country', 'year', and 'gear_code' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#'
#' @examples
#' # Create sample VMS data with various gear codes
#' vms_data <- data.frame(
#'   ID = paste0("ID", 1:12),
#'   country = rep(c("FR", "DK", "GB"), each = 4),
#'   year = rep(c(2009, 2010), 6),
#'   gear_code = rep(c("OTB", "PTM", "DRB", "OTM"), 3),
#'   fishing_hours = runif(12, 0.5, 12)
#' )
#'
#' # Analyse gear code distribution
#' gear_summary <- analyse_gear_codes(vms_data)
#' head(gear_summary)
#'
#' @export
analyse_gear_codes <- function(data) {
  gear_summary <- data %>%
    group_by(country, year, gear_code) %>%
    summarise(records = n(), .groups = "drop") %>%
    arrange(country, year, gear_code)
  
  # Create a visualization
  p <- ggplot(gear_summary, aes(x = gear_code, y = records, fill = factor(year))) +
    geom_col(position = "dodge") +
    facet_wrap(~country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Number of Records by Gear Code",
         x = "Gear Code", y = "Number of Records",
         fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  return(gear_summary)
}
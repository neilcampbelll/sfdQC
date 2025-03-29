#' Analyse Vessel Counts by Country and Year
#'
#' @description
#' This function analyses the number of unique vessels in a fisheries VMS dataset across
#' different countries and years, providing insights into fleet dynamics and data completeness.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', 'UniqueVessels',
#' and 'AnonVessels' columns.
#'
#' @return A tibble summarising vessel counts by country and year, including total records,
#' sum of unique vessels, and number of distinct anonymised vessel identifiers. Additionally,
#' a line plot visualisation of unique vessel counts over time is displayed.
#'
#' @details
#' The function calculates three key metrics for each country and year:
#' 1. Total number of VMS records
#' 2. Sum of 'UniqueVessels' values (typically 1 for each unique vessel)
#' 3. Count of distinct anonymised vessel identifiers
#'
#' A line plot is created to visualise trends in the number of unique vessels over time
#' for each country, which can help identify potential fleet changes, data gaps, or 
#' reporting issues.
#'
#' This analysis is valuable for understanding fishing fleet dynamics, monitoring changes
#' in fleet size over time, and ensuring completeness in vessel reporting across the dataset.
#'
#' @note
#' The function assumes the presence of 'country', 'year', 'UniqueVessels', and 'AnonVessels'
#' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#' The 'UniqueVessels' column typically contains a count of 1 for each unique vessel in a record,
#' while 'AnonVessels' contains anonymised vessel identifiers.
#'
#' @examples
#' # Create sample VMS data with vessel identifiers
#' vms_data <- data.frame(
#'   country = rep(c("FR", "DK", "GB"), each = 12),
#'   year = rep(rep(2018:2021, each = 3), 3),
#'   UniqueVessels = rep(1, 36),  # Each record represents one unique vessel
#'   AnonVessels = c(
#'     paste0("FR", 1:9, pad = "00"), paste0("FR", 7:9, pad = "00"),
#'     paste0("DK", 1:9, pad = "00"), paste0("DK", 4:6, pad = "00"),
#'     paste0("GB", 1:9, pad = "00"), paste0("GB", 7:9, pad = "00")
#'   )
#' )
#'
#' # Analyse vessel counts
#' vessel_counts <- analyse_vessel_counts(vms_data)
#' print(vessel_counts)
#'
#' @export
analyse_vessel_counts <- function(data) {
  # Count unique vessels by country and year
  vessel_counts <- data %>%
    group_by(country, year) %>%
    summarise(
      total_records = n(),
      unique_vessels = sum(UniqueVessels, na.rm = TRUE),
      anon_vessels = n_distinct(AnonVessels, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(country, year)
  
  # Create visualization
  p <- ggplot(vessel_counts, aes(x = year, y = unique_vessels, color = country, group = country)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Number of Unique Vessels by Country and Year",
         x = "Year", y = "Unique Vessels") +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1))
  
  print(p)
  
  return(vessel_counts)
}
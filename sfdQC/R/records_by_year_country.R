#' Analyse Records by Year and Country in VMS Data
#'
#' @description
#' This function analyses fisheries VMS data to summarise the number of records by year and country,
#' providing both a tabular summary and a time series visualisation.
#'
#' @param data A data frame or tibble containing VMS data with 'country' and 'year' columns.
#'
#' @return A wide-format tibble showing the count of records for each country-year combination,
#' with countries as rows and years as columns. Additionally, prints a ggplot visualisation
#' showing the trend of record counts over time for each country.
#'
#' @details
#' The function aggregates VMS data by country and year to produce record counts. It returns
#' this information in a wide format table and also creates a line plot showing temporal trends
#' for each country. This visualisation is useful for identifying potential data gaps or
#' unusual patterns in data submission across years.
#'
#' Temporal analysis of records is essential for assessing the consistency and completeness
#' of VMS data reporting across different countries and time periods.
#'
#' @note
#' The function assumes the presence of 'country' and 'year' columns in the input dataset.
#' The function requires the dplyr, tidyr, and ggplot2 packages.
#'
#' @examples
#' # Create sample VMS data spanning multiple years and countries
#' vms_data <- data.frame(
#'   ID = paste0("ID", 1:12),
#'   country = rep(c("FR", "DK", "GB", "ES"), each = 3),
#'   year = rep(c(2009, 2010, 2011), 4),
#'   c_square = paste0("Square", 1:12),
#'   fishing_hours = runif(12, 0.5, 12)
#' )
#'
#' # Analyse records by year and country
#' yearly_summary <- records_by_year_country(vms_data)
#' print(yearly_summary)
#'
#' @export
records_by_year_country <- function(data) {
  yearly_summary <- data %>%
    group_by(country, year) %>%
    summarise(records = n(), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = records)
  
  # Create a visualization
  plot_data <- data %>%
    group_by(country, year) %>%
    summarise(records = n(), .groups = "drop")
  
  p <- ggplot(plot_data, aes(x = year, y = records, group = country, color = country)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Number of Records by Year and Country",
         x = "Year", y = "Number of Records") +
    scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), by = 1))
  
  print(p)
  return(yearly_summary)
}
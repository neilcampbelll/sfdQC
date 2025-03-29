#' Analyse Import Dates by Country for VMS Data
#'
#' @description
#' This function analyses the distribution of import dates across different countries in a fisheries VMS dataset.
#' It generates both a summary table and a visualisation showing the number of records imported per date for each country.
#'
#' @param data A data frame or tibble containing VMS data with 'importDate' and 'country' columns.
#'
#' @return A tibble summarising the number of records by country and import date, arranged by country
#' and most recent import date first. Additionally, prints a ggplot visualisation showing the distribution
#' of records across import dates for each country.
#'
#' @details
#' The function first converts 'importDate' to Date format (if not already), then aggregates the data
#' by country and date to count the number of records. The resulting summary is visualised using ggplot2
#' with a faceted bar chart. This visualisation helps identify patterns in data submission timing
#' across different countries.
#'
#' Tracking import dates is useful for monitoring data submissions and identifying potential delays
#' in data reporting from specific countries.
#'
#' @note
#' The function assumes the presence of 'importDate' and 'country' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#'
#' @examples
#' # Create sample VMS data with various import dates and countries
#' vms_data <- data.frame(
#'   ID = c(
#'     "1E9FE878-43B6-4BA6-BF15-EC91DE86F57F",
#'     "5530B0B1-4F41-4782-955A-7F1C58648366",
#'     "CD10D3E1-A2FE-4D66-A281-51A9C4E0D77C",
#'     "F97BF5E5-A9CD-4B9F-A04B-CB684623CFDF",
#'     "D7E961E0-876C-49B6-8888-247C9C3D33DF",
#'     "BC9F02CB-4D10-4A43-AC98-F3900FFA4AC4"
#'   ),
#'   importDate = c(
#'     "2024-10-03", "2024-10-03", "2024-10-03", 
#'     "2023-10-24", "2025-01-21", "2024-10-03"
#'   ),
#'   country = c("FR", "DK", "FR", "ES", "GB", "DK"),
#'   year = c(2012, 2009, 2018, 2013, 2009, 2009)
#' )
#'
#' # Analyse import dates
#' import_summary <- analyse_import_dates(vms_data)
#' head(import_summary)
#'
#' @export
analyse_import_dates <- function(data) {
  import_summary <- data %>%
    mutate(importDate = as.Date(importDate)) %>%
    group_by(country, importDate) %>%
    summarise(records = n(), .groups = "drop") %>%
    arrange(country, desc(importDate))
  
  # Create a visualization
  p <- ggplot(import_summary, aes(x = importDate, y = records, fill = country)) +
    geom_col() +
    facet_wrap(~country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Number of Records by Import Date and Country",
         x = "Import Date", y = "Number of Records") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  return(import_summary)
}
#' Analyse Métier Level 5 by Country and Year
#'
#' @description
#' This function analyses the distribution of métier level 5 classifications across different countries
#' and years in a fisheries VMS dataset, providing both a tabular summary and a visualisation.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', and
#' 'LE_MET_level5' columns.
#'
#' @return A tibble summarising the number of records by country, year, and métier level 5,
#' arranged by country, year, and métier level 5. Additionally, prints a ggplot visualisation
#' showing the distribution of records across métier level 5 categories for each country and year.
#'
#' @details
#' The function aggregates VMS data by country, year, and métier level 5 to produce record counts.
#' The resulting summary is visualised using a faceted bar chart with countries as facets, métier
#' level 5 categories on the x-axis, and years differentiated by colour. This analysis helps identify
#' fishing activity patterns and potential data quality issues in métier reporting.
#'
#' In fisheries management, métier classifications (such as DEF for demersal fish, CRU for crustaceans,
#' MOL for molluscs) provide important information about target species groups. Level 5 métiers combine
#' gear type and target assemblage information and are essential for fisheries effort characterisation.
#'
#' @note
#' The function assumes the presence of 'country', 'year', and 'LE_MET_level5' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#'
#' @examples
#' # Create sample VMS data with various métier level 5 classifications
#' vms_data <- data.frame(
#'   ID = paste0("ID", 1:12),
#'   country = rep(c("FR", "DK", "GB"), each = 4),
#'   year = rep(c(2009, 2010), 6),
#'   LE_MET_level5 = rep(c("DEF", "CRU", "MOL", "SPF"), 3),
#'   fishing_hours = runif(12, 0.5, 12)
#' )
#'
#' # Analyse métier level 5 distribution
#' metier_summary <- analyse_metier_level5(vms_data)
#' head(metier_summary)
#'
#' @export
analyse_metier_level5 <- function(data) {
  metier_summary <- data %>%
    group_by(country, year, LE_MET_level5) %>%
    summarise(records = n(), .groups = "drop") %>%
    arrange(country, year, LE_MET_level5)
  
  # Create a visualization
  p <- ggplot(metier_summary, aes(x = LE_MET_level5, y = records, fill = factor(year))) +
    geom_col(position = "dodge") +
    facet_wrap(~country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Number of Records by Metier Level 5",
         x = "Metier Level 5", y = "Number of Records",
         fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  return(metier_summary)
}
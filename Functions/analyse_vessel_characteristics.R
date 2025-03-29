#' Analyse Vessel Power and Length Characteristics
#'
#' @description
#' This function analyses vessel power (kW) and overall length (OAL) characteristics in a
#' fisheries VMS dataset, identifying potential data quality issues and visualising
#' distributions across countries and years.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', 'avg_kw',
#' and 'avg_oal' columns.
#'
#' @return A tibble summarising vessel power and length statistics by country and year, including
#' flagged percentages for potentially problematic values. Additionally, histogram visualisations
#' of power and length distributions are displayed.
#'
#' @details
#' The function first categorises vessel power (kW) and length (OAL) values into status groups:
#' NA, Zero, Negative, Suspicious (unusually high values), or Normal. It then calculates the
#' percentage of records in each category by country and year.
#'
#' Two histograms are created to visualise the distribution of "normal" values for both vessel power
#' and length, faceted by country and year. These visualisations help identify patterns, outliers,
#' or inconsistencies in vessel characteristics data.
#'
#' This analysis is important for quality control of VMS data, as vessel characteristics affect
#' fishing capacity estimates and are used in effort calculations and fleet segmentation.
#'
#' @note
#' The function assumes the presence of 'country', 'year', 'avg_kw', and 'avg_oal' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#' Values above 5000 kW for power or 120 m for length are flagged as potentially suspicious.
#'
#' @examples
#' # Create sample VMS data with vessel characteristics
#' vms_data <- data.frame(
#'   country = rep(c("FR", "DK", "GB"), each = 3),
#'   year = rep(c(2019, 2020, 2021), 3),
#'   avg_kw = c(250, 300, 400, 350, 450, 550, 500, 600, 700),
#'   avg_oal = c(15, 18, 20, 22, 25, 28, 30, 35, 40)
#' )
#'
#' # Analyse vessel characteristics
#' vessel_summary <- analyse_vessel_characteristics(vms_data)
#' print(vessel_summary)
#'
#' @export
analyse_vessel_characteristics <- function(data) {
  # Check for NA, 0, or placeholder values
  vessel_data <- data %>%
    mutate(
      kw_status = case_when(
        is.na(avg_kw) ~ "NA",
        avg_kw == 0 ~ "Zero",
        avg_kw < 0 ~ "Negative",
        avg_kw > 5000 ~ "Suspicious (>5000)",
        TRUE ~ "Normal"
      ),
      oal_status = case_when(
        is.na(avg_oal) ~ "NA",
        avg_oal == 0 ~ "Zero",
        avg_oal < 0 ~ "Negative",
        avg_oal > 120 ~ "Suspicious (>120)",
        TRUE ~ "Normal"
      )
    )
  
  # Calculate summaries by country and year
  vessel_summary <- vessel_data %>%
    group_by(country, year) %>%
    summarise(
      records = n(),
      # KW stats
      kw_na_pct = 100 * sum(kw_status == "NA") / n(),
      kw_zero_pct = 100 * sum(kw_status == "Zero") / n(),
      kw_neg_pct = 100 * sum(kw_status == "Negative") / n(),
      kw_sus_pct = 100 * sum(kw_status == "Suspicious (>5000)") / n(),
      avg_kw = mean(avg_kw, na.rm = TRUE),
      # OAL stats
      oal_na_pct = 100 * sum(oal_status == "NA") / n(),
      oal_zero_pct = 100 * sum(oal_status == "Zero") / n(),
      oal_neg_pct = 100 * sum(oal_status == "Negative") / n(),
      oal_sus_pct = 100 * sum(oal_status == "Suspicious (>120)") / n(),
      avg_oal = mean(avg_oal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(country, year)
  
  # Create visualizations for KW
  p1 <- ggplot(vessel_data %>% filter(kw_status == "Normal"), 
               aes(x = avg_kw)) +
    geom_histogram(bins = 30) +
    facet_grid(year ~ country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Distribution of Normal Vessel Power (KW) by Country and Year",
         x = "Average KW", y = "Count")
  
  # Create visualizations for OAL
  p2 <- ggplot(vessel_data %>% filter(oal_status == "Normal"), 
               aes(x = avg_oal)) +
    geom_histogram(bins = 30) +
    facet_grid(year ~ country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Distribution of Normal Vessel Length (OAL) by Country and Year",
         x = "Average OAL", y = "Count")
  
  print(p1)
  print(p2)
  
  return(vessel_summary)
}
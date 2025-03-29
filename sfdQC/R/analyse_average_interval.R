#' Analyse Average Interval Between VMS Pings
#'
#' @description
#' This function analyses the distribution of average intervals between VMS pings
#' across different countries, helping to identify potential unit inconsistencies
#' and data quality issues.
#'
#' @param data A data frame or tibble containing VMS data with 'country' and 'AverageInterval' columns.
#'
#' @return A tibble summarising average interval statistics by country, including minimum, maximum,
#' mean, and median values, as well as a classification of the likely time unit used. Additionally,
#' a histogram visualisation of interval distributions is displayed.
#'
#' @details
#' The function calculates summary statistics for the average interval between VMS pings by country.
#' It then attempts to classify the likely time unit (hours vs. minutes) based on the magnitude of
#' the average interval values.
#'
#' A histogram is created to visualise the distribution of interval values across different countries,
#' which can help identify inconsistencies in reporting or potential data quality issues.
#'
#' This analysis is important because VMS ping frequency affects the accuracy of fishing activity
#' estimates and can vary between countries due to different regulatory requirements or reporting
#' conventions. Some countries may report in minutes while others use hours, leading to potential
#' misinterpretations if not properly accounted for.
#'
#' @note
#' The function assumes the presence of 'country' and 'AverageInterval' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#' The function uses basic heuristics to classify time units: values < 5 are likely hours,
#' values between 5 and 300 are likely minutes, and values > 300 are flagged as suspicious.
#'
#' @examples
#' # Create sample VMS data with different interval reporting conventions
#' vms_data <- data.frame(
#'   country = c(rep("FR", 50), rep("DK", 50), rep("GB", 50)),
#'   AverageInterval = c(
#'     rnorm(50, 120, 20),   # FR: reporting in minutes (around 2 hours)
#'     rnorm(50, 2, 0.5),    # DK: reporting in hours (around 2 hours)
#'     rnorm(50, 60, 10)     # GB: reporting in minutes (around 1 hour)
#'   )
#' )
#'
#' # Analyse average intervals
#' interval_summary <- analyse_average_interval(vms_data)
#' print(interval_summary)
#'
#' @export
analyse_average_interval <- function(data) {
  # Calculate summary statistics
  interval_summary <- data %>%
    filter(!is.na(AverageInterval)) %>%
    group_by(country) %>%
    summarise(
      count = n(),
      min_interval = min(AverageInterval, na.rm = TRUE),
      max_interval = max(AverageInterval, na.rm = TRUE),
      avg_interval = mean(AverageInterval, na.rm = TRUE),
      median_interval = median(AverageInterval, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(country)
  
  # Identify potential units (minutes vs hours)
  interval_summary <- interval_summary %>%
    mutate(
      likely_unit = case_when(
        avg_interval < 5 ~ "Likely hours",
        avg_interval >= 5 & avg_interval < 300 ~ "Likely minutes",
        avg_interval >= 300 ~ "Suspicious (very large)",
        TRUE ~ "Unknown"
      )
    )
  
  # Create visualization
  p <- ggplot(data %>% filter(!is.na(AverageInterval)), 
              aes(x = AverageInterval)) +
    geom_histogram(bins = 30) +
    facet_wrap(~country, scales = "free") +
    theme_minimal() +
    labs(title = "Distribution of Average Interval by Country",
         x = "Average Interval", y = "Count")
  
  print(p)
  
  return(interval_summary)
}
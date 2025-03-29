#' Create Comprehensive Anomaly Scores by Country and Year
#'
#' @description
#' This function calculates composite anomaly scores for VMS data quality across different
#' countries and years, based on multiple quality metrics related to missing data and
#' suspicious values.
#'
#' @param data A data frame or tibble containing VMS data with various columns required
#' for calculating quality metrics.
#'
#' @return A tibble containing various data quality metrics and composite anomaly scores
#' for each country and year combination. Additionally, a line plot visualisation of the
#' overall anomaly scores over time is displayed.
#'
#' @details
#' The function calculates two categories of data quality metrics:
#' 
#' 1. Missing data percentages for key variables:
#'    - Fishing speed
#'    - Gear width
#'    - Vessel power (kW)
#'    - Vessel length (OAL)
#'    - Habitat type
#'    - Depth range
#'    - Swept area
#' 
#' 2. Suspicious values percentages:
#'    - Zero fishing hours
#'    - Unusually large fishing hours (>24 hours)
#'    - Suspicious gear width (very large or negative)
#'    - Suspicious vessel length (very large or negative)
#'    - Suspicious vessel power (very large or negative)
#' 
#' These metrics are then combined into composite scores:
#'    - Missing data score (weighted average of missing data percentages)
#'    - Suspicious value score (weighted average of suspicious value percentages)
#'    - Overall anomaly score (weighted combination of the above two scores)
#' 
#' A line plot visualises the overall anomaly scores over time for each country, allowing
#' for easy identification of data quality trends and problematic country-year combinations.
#'
#' This comprehensive quality scoring is valuable for prioritizing data cleaning efforts,
#' identifying systematic issues in data reporting, and providing context for interpreting
#' analysis results.
#'
#' @note
#' The function assumes the presence of numerous columns in the input dataset, including
#' 'country', 'year', 'avg_fishing_speed', 'avg_gearWidth', 'avg_kw', 'avg_oal', 'HabitatType',
#' 'DepthRange', 'SweptArea', and 'fishing_hours'.
#' The function requires the dplyr and ggplot2 packages.
#' The overall anomaly score is calculated with a 70% weight for missing data and 30% for
#' suspicious values, reflecting the relative importance of data completeness.
#'
#' @examples
#' # This function works best with a comprehensive VMS dataset
#' # containing all the required columns for quality assessment
#' # anomaly_metrics <- create_anomaly_scores(vms_data)
#'
#' @export
create_anomaly_scores <- function(data) {
  # Calculate various metrics that might indicate data quality issues
  anomaly_metrics <- data %>%
    group_by(country, year) %>%
    summarise(
      records = n(),
      
      # Missing data percentages
      missing_fishing_speed = 100 * sum(is.na(avg_fishing_speed)) / n(),
      missing_gear_width = 100 * sum(is.na(avg_gearWidth)) / n(),
      missing_kw = 100 * sum(is.na(avg_kw)) / n(),
      missing_oal = 100 * sum(is.na(avg_oal)) / n(),
      missing_habitat = 100 * sum(is.na(HabitatType)) / n(),
      missing_depth = 100 * sum(is.na(DepthRange)) / n(),
      missing_swept_area = 100 * sum(is.na(SweptArea)) / n(),
      
      # Suspicious values
      zero_fishing_hours = 100 * sum(fishing_hours == 0, na.rm = TRUE) / sum(!is.na(fishing_hours)),
      large_fishing_hours = 100 * sum(fishing_hours > 24, na.rm = TRUE) / sum(!is.na(fishing_hours)),
      suspicious_gear_width = 100 * sum(avg_gearWidth > 1000 | avg_gearWidth < 0, na.rm = TRUE) / sum(!is.na(avg_gearWidth)),
      suspicious_oal = 100 * sum(avg_oal > 120 | avg_oal < 0, na.rm = TRUE) / sum(!is.na(avg_oal)),
      suspicious_kw = 100 * sum(avg_kw > 5000 | avg_kw < 0, na.rm = TRUE) / sum(!is.na(avg_kw)),
      
      .groups = "drop"
    )
  
  # Calculate an overall anomaly score (0-100, higher means more potential issues)
  anomaly_metrics <- anomaly_metrics %>%
    rowwise() %>%
    mutate(
      missing_data_score = mean(c(missing_fishing_speed, missing_gear_width, missing_kw, 
                                  missing_oal, missing_habitat, missing_depth, missing_swept_area), 
                                na.rm = TRUE),
      suspicious_value_score = mean(c(zero_fishing_hours, large_fishing_hours, 
                                      suspicious_gear_width, suspicious_oal, suspicious_kw), 
                                    na.rm = TRUE),
      overall_anomaly_score = (missing_data_score * 0.7 + suspicious_value_score * 0.3)
    ) %>%
    ungroup() %>%
    arrange(desc(overall_anomaly_score))
  
  # Create visualization
  p <- ggplot(anomaly_metrics, aes(x = year, y = overall_anomaly_score, color = country, group = country)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Overall Data Anomaly Score by Country and Year",
         subtitle = "Higher score indicates more potential data quality issues",
         x = "Year", y = "Anomaly Score (0-100)") +
    scale_x_continuous(breaks = seq(min(anomaly_metrics$year), max(anomaly_metrics$year), by = 1))
  
  print(p)
  
  return(anomaly_metrics)
}
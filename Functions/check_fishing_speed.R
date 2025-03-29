#' Check ICES Average Fishing Speed in VMS Data
#'
#' @description
#' This function examines the availability and distribution of ICES average fishing speed values
#' in a fisheries VMS dataset, providing both summary statistics and visualisations.
#'
#' @param data A data frame or tibble containing VMS data with an 'ICES_avg_fishing_speed' column.
#'
#' @return A summary of ICES average fishing speed by country is printed to the console. If non-NA
#' values are found, the function returns these records. Additionally, if sufficient non-NA values
#' exist, the function generates a histogram showing the distribution of values by country.
#'
#' @details
#' The function first summarises the presence/absence of ICES average fishing speed data by country,
#' calculating the percentage of NA values. It then examines any non-NA values in more detail,
#' providing a sample of these records and, if sufficient data exists, a visualisation of their
#' distribution. This information can be useful for quality control and for understanding the
#' availability of standardised fishing speed information.
#'
#' @note
#' The function assumes the presence of an 'ICES_avg_fishing_speed' column in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#'
#' @examples
#' # Create sample VMS data with some ICES fishing speed values
#' vms_data <- data.frame(
#'   ID = paste0("ID", 1:6),
#'   country = c("FR", "DK", "FR", "ES", "GB", "DK"),
#'   year = c(2012, 2009, 2018, 2013, 2009, 2009),
#'   ICES_avg_fishing_speed = c(NA, 3.2, NA, 3.5, NA, 2.8)
#' )
#'
#' # Check ICES fishing speed information
#' speed_info <- check_fishing_speed(vms_data)
#' print(speed_info)
#'
#' @export
check_fishing_speed <- function(data) {
  speed_summary <- data %>%
    group_by(country) %>%
    summarise(
      total_records = n(),
      speed_na = sum(is.na(ICES_avg_fishing_speed)),
      speed_na_percent = 100 * speed_na / total_records,
      non_na_count = total_records - speed_na
    ) %>%
    arrange(desc(non_na_count))
  
  cat("ICES average fishing speed summary by country:\n")
  print(speed_summary)
  
  # If there are some non-NA values, examine them
  if (sum(speed_summary$non_na_count) > 0) {
    non_na_speed <- data %>%
      filter(!is.na(ICES_avg_fishing_speed)) %>%
      select(country, year, ICES_avg_fishing_speed)
    
    cat("\nSample of non-NA ICES fishing speed records:\n")
    print(head(non_na_speed, 20))
    
    # Histogram of values if available
    if (nrow(non_na_speed) > 0) {
      p <- ggplot(non_na_speed, aes(x = ICES_avg_fishing_speed)) +
        geom_histogram(bins = 30) +
        facet_wrap(~country, scales = "free_y") +
        theme_minimal() +
        labs(title = "Distribution of ICES Average Fishing Speed",
             x = "ICES Average Fishing Speed", y = "Count")
      
      print(p)
    }
    
    return(non_na_speed)
  } else {
    return("All ICES average fishing speed values are NA.")
  }
}
#' Cross-check Vessel Characteristics Against Expected Ranges
#'
#' @description
#' This function cross-checks vessel length and power characteristics against expected ranges
#' based on vessel length categories, identifying potential mismatches or data quality issues.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', 'vessel_length_category',
#' 'avg_oal', and 'avg_kw' columns.
#'
#' @return If mismatches are found, returns a list containing a summary of mismatches by country
#' and vessel category, and a sample of mismatched records. Also displays a bar chart visualisation
#' of mismatches. If no mismatches are found, returns a character string indicating this.
#'
#' @details
#' The function first defines expected ranges for vessel length and power based on standard
#' vessel length categories (e.g., VL0010, VL1012, etc.). It then joins these expected ranges
#' with the VMS data and identifies records where the actual length or power falls outside
#' the expected range for the assigned category.
#'
#' A bar chart is created to visualise the distribution of mismatches across different vessel
#' length categories and countries, helping to identify systematic issues in vessel characteristic
#' reporting.
#'
#' This analysis is important for ensuring consistency between vessel length categories and the
#' actual reported vessel characteristics, which is crucial for accurate fleet segmentation and
#' fishing capacity assessments.
#'
#' @note
#' The function assumes the presence of 'country', 'year', 'vessel_length_category', 'avg_oal',
#' and 'avg_kw' columns in the input dataset.
#' The function requires the dplyr, tibble, and ggplot2 packages.
#' The function uses predefined expected ranges based on standard EU vessel length categories.
#'
#' @examples
#' # Create sample VMS data with some vessel characteristic mismatches
#' vms_data <- data.frame(
#'   country = rep(c("FR", "DK", "GB"), each = 3),
#'   year = rep(2020, 9),
#'   vessel_length_category = rep(c("VL0010", "VL1218", "VL2440"), 3),
#'   avg_oal = c(8, 15, 30, 9, 20, 42, 12, 16, 22),  # GB VL2440 is too small
#'   avg_kw = c(100, 300, 700, 120, 450, 900, 150, 350, 500)  # GB VL2440 has low power
#' )
#'
#' # Check vessel characteristics
#' result <- check_vessel_characteristics(vms_data)
#' print(result$summary)
#'
#' @export
check_vessel_characteristics <- function(data) {
  # Define expected ranges for different vessel length categories
  expected_ranges <- tribble(
    ~vessel_length_category, ~min_length, ~max_length, ~min_power, ~max_power,
    "VL0010", 0, 10, 0, 200,
    "VL1012", 10, 12, 50, 300,
    "VL1218", 12, 18, 100, 500,
    "VL1824", 18, 24, 200, 800,
    "VL2440", 24, 40, 300, 1200,
    "VL40XX", 40, 120, 800, 5000
  )
  
  # Join with the data and check for discrepancies
  vessel_check <- data %>%
    left_join(expected_ranges, by = "vessel_length_category") %>%
    filter(!is.na(avg_oal) & !is.na(avg_kw)) %>%
    mutate(
      length_match = avg_oal >= min_length & avg_oal <= max_length,
      power_match = avg_kw >= min_power & avg_kw <= max_power
    ) %>%
    filter(!length_match | !power_match) %>%
    select(country, year, vessel_length_category, avg_oal, min_length, max_length, 
           avg_kw, min_power, max_power, length_match, power_match)
  
  if (nrow(vessel_check) > 0) {
    cat("Found", nrow(vessel_check), "records with mismatched vessel characteristics:\n")
    
    # Summarize by country and vessel category
    vessel_mismatches <- vessel_check %>%
      group_by(country, vessel_length_category) %>%
      summarise(
        records = n(),
        length_mismatches = sum(!length_match),
        power_mismatches = sum(!power_match),
        .groups = "drop"
      ) %>%
      arrange(desc(records))
    
    # Create a visualization of mismatches
    p <- ggplot(vessel_mismatches, aes(x = vessel_length_category, y = records, fill = country)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(title = "Vessel Characteristic Mismatches by Country and Length Category",
           x = "Vessel Length Category", y = "Number of Mismatched Records") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
    
    return(list(
      summary = vessel_mismatches,
      sample_records = head(vessel_check, 20)
    ))
  } else {
    return("No vessel characteristic mismatches found.")
  }
}
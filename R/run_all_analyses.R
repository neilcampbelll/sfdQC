#' Run All VMS Data Quality Analyses
#'
#' @description
#' This function executes all available quality control and analysis functions on a fisheries
#' VMS dataset, providing a comprehensive assessment of data quality and characteristics.
#'
#' @param data A data frame or tibble containing VMS data with all required columns for
#' the individual analysis functions.
#'
#' @return A list containing the results of all individual analysis functions, organized
#' by analysis type. Progress messages are printed to the console as each analysis is completed.
#'
#' @details
#' The function sequentially runs all available analysis functions on the provided VMS dataset:
#' 
#' 1. `check_unique_ids`: Checks for duplicate IDs
#' 2. `analyse_import_dates`: Analyses import dates by country
#' 3. `check_record_types`: Checks record types
#' 4. `records_by_year_country`: Analyses records by year and country
#' 5. `analyse_vessel_length`: Analyses vessel length categories
#' 6. `analyse_gear_codes`: Analyses gear codes
#' 7. `analyse_metier_level5`: Analyses métier level 5 categories
#' 8. `check_mesh_size`: Checks mesh size information
#' 9. `check_fishing_speed`: Checks ICES average fishing speed
#' 10. `analyse_gear_width`: Analyses gear width
#' 11. `analyse_fishing_speed_by_metier`: Analyses fishing speed by métier
#' 12. `analyse_fishing_hours`: Analyses fishing hours distribution
#' 13. `analyse_vessel_characteristics`: Analyses vessel power and length
#' 14. `analyse_average_interval`: Analyses average interval between pings
#' 15. `analyse_habitat_depth`: Analyses habitat and depth information
#' 16. `analyse_swept_area`: Analyses swept area information
#' 
#' This comprehensive analysis provides a holistic view of the quality and characteristics
#' of the VMS dataset, identifying potential issues and inconsistencies that should be
#' addressed before using the data for scientific assessments.
#'
#' @note
#' The function requires all the individual analysis functions to be available in the environment.
#' The function requires all columns needed by the individual analysis functions to be present
#' in the input dataset.
#'
#' @examples
#' # Given a VMS dataset with all required columns
#' # results <- run_all_analyses(vms_data)
#' # Access specific results, e.g.:
#' # results$yearly_analysis
#'
#' @export
run_all_analyses <- function(data) {
  cat("========== VMS Data Quality Control Analysis ==========\n\n")
  
  cat("1. Checking for unique IDs...\n")
  id_check <- check_unique_ids(data)
  cat("\n")
  
  cat("2. Analyzing import dates...\n")
  import_analysis <- analyse_import_dates(data)
  cat("\n")
  
  cat("3. Checking record types...\n")
  record_check <- check_record_types(data)
  cat("\n")
  
  cat("4. Analyzing records by year and country...\n")
  yearly_analysis <- records_by_year_country(data)
  cat("\n")
  
  cat("5. Analyzing vessel length categories...\n")
  length_analysis <- analyse_vessel_length(data)
  cat("\n")
  
  cat("6. Analyzing gear codes...\n")
  gear_analysis <- analyse_gear_codes(data)
  cat("\n")
  
  cat("7. Analyzing metier level 5...\n")
  metier_analysis <- analyse_metier_level5(data)
  cat("\n")
  
  cat("8. Checking mesh size information...\n")
  mesh_check <- check_mesh_size(data)
  cat("\n")
  
  cat("9. Checking ICES average fishing speed...\n")
  speed_check <- check_fishing_speed(data)
  cat("\n")
  
  cat("10. Analyzing gear width...\n")
  width_analysis <- analyse_gear_width(data)
  cat("\n")
  
  cat("11. Analyzing fishing speed by metier...\n")
  metier_speed <- analyse_fishing_speed_by_metier(data)
  cat("\n")
  
  cat("12. Analyzing fishing hours...\n")
  hours_analysis <- analyse_fishing_hours(data)
  cat("\n")
  
  cat("13. Analyzing vessel characteristics...\n")
  vessel_analysis <- analyse_vessel_characteristics(data)
  cat("\n")
  
  cat("14. Analyzing average interval...\n")
  interval_analysis <- analyse_average_interval(data)
  cat("\n")
  
  cat("15. Analyzing habitat and depth information...\n")
  habitat_depth <- analyse_habitat_depth(data)
  cat("\n")
  
  cat("16. Analyzing swept area...\n")
  swept_analysis <- analyse_swept_area(data)
  cat("\n")
  
  cat("========== Analysis Complete ==========\n")
  
  # Return all results as a list
  return(list(
    id_check = id_check,
    import_analysis = import_analysis,
    record_check = record_check,
    yearly_analysis = yearly_analysis,
    length_analysis = length_analysis,
    gear_analysis = gear_analysis,
    metier_analysis = metier_analysis,
    mesh_check = mesh_check,
    speed_check = speed_check,
    width_analysis = width_analysis,
    metier_speed = metier_speed,
    hours_analysis = hours_analysis,
    vessel_analysis = vessel_analysis,
    interval_analysis = interval_analysis,
    habitat_depth = habitat_depth,
    swept_analysis = swept_analysis
  ))
}
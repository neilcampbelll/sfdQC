#' Check for Consistency Between Gear Codes and Métiers
#'
#' @description
#' This function checks for consistency between gear codes and the corresponding métier classifications
#' in a fisheries VMS dataset, identifying records where these do not match.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', 'gear_code',
#' and 'LE_MET_level6' columns.
#'
#' @return If inconsistencies are found, returns a list containing a summary of inconsistencies by
#' country, gear code, and métier, and a sample of inconsistent records. Also displays a bar chart
#' visualisation of inconsistencies. If no inconsistencies are found, returns a character string
#' indicating this.
#'
#' @details
#' The function first defines expected mappings between gear codes and métier prefixes (e.g., gear code
#' "OTB" should correspond to métiers starting with "OTB_"). It then identifies records where the
#' métier prefix does not match the corresponding gear code.
#'
#' A bar chart is created to visualise the distribution of inconsistencies across different gear codes
#' and countries, helping to identify systematic issues in gear and métier reporting.
#'
#' This analysis is important for ensuring integrity and consistency in fishing activity classification,
#' which is crucial for accurate fishing effort and impact assessments.
#'
#' @note
#' The function assumes the presence of 'country', 'year', 'gear_code', and 'LE_MET_level6' columns
#' in the input dataset.
#' The function requires the dplyr, tibble, stringr, and ggplot2 packages.
#' The function uses predefined mappings based on standard DCF gear codes and métier classifications.
#'
#' @examples
#' # Create sample VMS data with some gear-métier inconsistencies
#' vms_data <- data.frame(
#'   country = rep(c("FR", "DK", "GB"), each = 3),
#'   year = rep(2020, 9),
#'   gear_code = rep(c("OTB", "PTM", "DRB"), 3),
#'   LE_MET_level6 = c(
#'     "OTB_DEF_70-99_0_0", "PTM_SPF_16-31_0_0", "DRB_MOL_0_0_0",  # Consistent
#'     "OTB_DEF_70-99_0_0", "OTM_SPF_16-31_0_0", "DRB_MOL_0_0_0",  # PTM-OTM mismatch
#'     "OTB_DEF_70-99_0_0", "PTM_SPF_16-31_0_0", "SSC_MOL_0_0_0"   # DRB-SSC mismatch
#'   )
#' )
#'
#' # Check gear-métier consistency
#' result <- check_gear_metier_consistency(vms_data)
#' print(result$summary)
#'
#' @export
check_gear_metier_consistency <- function(data) {
  # Define expected mappings between gear codes and métier prefixes
  expected_mappings <- tribble(
    ~gear_code, ~expected_prefix,
    "OTB", "OTB_",
    "OTM", "OTM_",
    "PTM", "PTM_",
    "PTB", "PTB_",
    "PS", "PS_",
    "SDN", "SDN_",
    "SSC", "SSC_",
    "DRB", "DRB_",
    "TBB", "TBB_",
    "GNS", "GNS_",
    "GND", "GND_",
    "GTR", "GTR_",
    "LLS", "LLS_",
    "LLD", "LLD_"
  )
  
  # Check for inconsistencies
  metier_check <- data %>%
    left_join(expected_mappings, by = "gear_code") %>%
    filter(!is.na(expected_prefix) & !is.na(LE_MET_level6)) %>%
    mutate(
      metier_matches = str_starts(LE_MET_level6, expected_prefix)
    ) %>%
    filter(!metier_matches) %>%
    select(country, year, gear_code, LE_MET_level6, expected_prefix)
  
  if (nrow(metier_check) > 0) {
    cat("Found", nrow(metier_check), "records with inconsistent gear code and métier mapping:\n")
    
    # Summarize by country
    metier_inconsistencies <- metier_check %>%
      group_by(country, gear_code, LE_MET_level6) %>%
      summarise(
        records = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(records))
    
    # Create a visualization
    p <- ggplot(metier_inconsistencies, aes(x = gear_code, y = records, fill = country)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(title = "Gear Code and Métier Inconsistencies",
           x = "Gear Code", y = "Number of Inconsistent Records") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
    
    return(list(
      summary = metier_inconsistencies,
      sample_records = head(metier_check, 20)
    ))
  } else {
    return("No gear code and métier inconsistencies found.")
  }
}
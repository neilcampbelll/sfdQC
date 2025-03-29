#' Check Record Types in VMS Data
#'
#' @description
#' This function examines a fisheries VMS dataset to summarise the different record types present and
#' identify any records that are not of the expected 'VE' type (vessel entries).
#'
#' @param data A data frame or tibble containing VMS data with a 'recordtype' column.
#'
#' @return A summary table of record types and their counts is printed to the console. If non-'VE' records
#' are found, the function returns these records as a data frame. Otherwise, it returns a character
#' string confirming that all records have type 'VE'.
#'
#' @details
#' The function first creates a summary count of each record type present in the dataset.
#' It then identifies any records with a type other than 'VE' (vessel entries), which might
#' indicate data quality issues or unexpected data formats. In VMS datasets, 'VE' typically
#' represents vessel position reports, while other codes might represent different types of data
#' that should be verified.
#'
#' @note
#' The function assumes the presence of a 'recordtype' column in the input dataset.
#' The function requires the dplyr package.
#'
#' @examples
#' # Create sample VMS data with different record types
#' vms_data <- data.frame(
#'   ID = c(
#'     "1E9FE878-43B6-4BA6-BF15-EC91DE86F57F",
#'     "5530B0B1-4F41-4782-955A-7F1C58648366",
#'     "CD10D3E1-A2FE-4D66-A281-51A9C4E0D77C",
#'     "F97BF5E5-A9CD-4B9F-A04B-CB684623CFDF",
#'     "D7E961E0-876C-49B6-8888-247C9C3D33DF",
#'     "BC9F02CB-4D10-4A43-AC98-F3900FFA4AC4"
#'   ),
#'   recordtype = c("VE", "VE", "VE", "LE", "VE", "VE"),
#'   country = c("FR", "DK", "FR", "ES", "GB", "DK"),
#'   year = c(2012, 2009, 2018, 2013, 2009, 2009),
#'   c_square = c(
#'     "7400:495:110:1", "1500:144:227:1", "7400:363:236:4",
#'     "1700:122:102:4", "1500:100:229:4", "1500:144:227:4"
#'   )
#' )
#'
#' # Check record types
#' result <- check_record_types(vms_data)
#' # This should identify and return the record with type "LE"
#'
#' @export
check_record_types <- function(data) {
  record_summary <- data %>%
    group_by(recordtype) %>%
    summarise(count = n(), .groups = "drop")
  
  cat("Record type summary:\n")
  print(record_summary)
  
  non_ve_records <- data %>%
    filter(recordtype != "VE")
  
  if (nrow(non_ve_records) > 0) {
    cat("\nRecords with type other than 'VE':\n")
    return(non_ve_records)
  } else {
    return("All records have type 'VE'.")
  }
}
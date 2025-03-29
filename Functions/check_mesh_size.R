#' Check Mesh Size Information in VMS Data
#'
#' @description
#' This function examines mesh size information in a fisheries VMS dataset, checking for
#' NULL values and analyzing the availability of both lower and upper mesh size data.
#'
#' @param data A data frame or tibble containing VMS data with 'LowerMeshSize' and 'UpperMeshSize' columns.
#'
#' @return A summary of mesh size information is printed to the console. If non-NULL mesh size records
#' are found, the function returns a sample of these records (up to 20). Otherwise, it returns a character
#' string indicating that all mesh size values are NULL or NA.
#'
#' @details
#' The function first counts the total number of records with NULL or NA values for both lower and
#' upper mesh sizes. It then identifies any records with non-NULL values and provides a sample of these
#' for further inspection. Mesh size information is important for understanding the selectivity of
#' fishing gears and for regulatory compliance.
#'
#' @note
#' The function assumes the presence of 'LowerMeshSize' and 'UpperMeshSize' columns in the input dataset.
#' The function handles the special case where NULL values are stored as the string "NULL" rather than
#' actual NULL/NA values in R.
#'
#' @examples
#' # Create sample VMS data with mesh size information
#' vms_data <- data.frame(
#'   ID = paste0("ID", 1:6),
#'   country = c("FR", "DK", "FR", "ES", "GB", "DK"),
#'   year = c(2012, 2009, 2018, 2013, 2009, 2009),
#'   LowerMeshSize = c("NULL", "NULL", 70, "NULL", 80, "NULL"),
#'   UpperMeshSize = c("NULL", "NULL", 79, "NULL", 89, "NULL")
#' )
#'
#' # Check mesh size information
#' mesh_info <- check_mesh_size(vms_data)
#' print(mesh_info)
#'
#' @export
check_mesh_size <- function(data) {
  # Check for non-NULL/NA values
  mesh_summary <- data %>%
    mutate(
      LowerMeshSize = as.character(LowerMeshSize),
      UpperMeshSize = as.character(UpperMeshSize)
    ) %>%
    summarise(
      total_records = n(),
      lower_null = sum(LowerMeshSize == "NULL" | is.na(LowerMeshSize)),
      upper_null = sum(UpperMeshSize == "NULL" | is.na(UpperMeshSize)),
      lower_non_null_percent = 100 * (1 - lower_null / total_records),
      upper_non_null_percent = 100 * (1 - upper_null / total_records)
    )
  
  cat("Mesh size information summary:\n")
  print(mesh_summary)
  
  # If there are some non-NULL values, let's check them
  non_null_mesh <- data %>%
    filter(!(LowerMeshSize == "NULL" | is.na(LowerMeshSize)) | 
             !(UpperMeshSize == "NULL" | is.na(UpperMeshSize))) %>%
    select(country, year, LowerMeshSize, UpperMeshSize)
  
  if (nrow(non_null_mesh) > 0) {
    cat("\nSample of non-NULL mesh size records:\n")
    return(head(non_null_mesh, 20))
  } else {
    return("All mesh size records are NULL or NA.")
  }
}
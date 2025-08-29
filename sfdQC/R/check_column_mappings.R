
#' Check Which Column Mappings May Be Needed
#'
#' @description
#' Identifies potential column mapping issues for QC functions
#'
#' @param data The dataset to check
#'
#' @return List of potential mapping suggestions
check_column_mappings <- function(data) {

  # Expected columns for various QC functions
  expected_columns <- list(
    basic = c("ID", "country", "year", "importDate", "recordtype"),
    vessel = c("vessel_length_category", "avg_kw", "avg_oal"),
    gear = c("gear_code", "LE_MET_level5", "LE_MET_level6"),
    fishing = c("fishing_hours", "avg_fishing_speed", "avg_gearWidth"),
    environmental = c("HabitatType", "DepthRange", "SweptArea"),
    spatial = c("c_square"),
    vessels = c("UniqueVessels", "AnonVessels")
  )

  missing_by_category <- list()

  for (category in names(expected_columns)) {
    missing <- setdiff(expected_columns[[category]], names(data))
    if (length(missing) > 0) {
      missing_by_category[[category]] <- missing
    }
  }

  # Suggest potential mappings based on similar column names
  suggestions <- list()
  for (category in names(missing_by_category)) {
    for (missing_col in missing_by_category[[category]]) {
      # Look for similar column names
      similar <- names(data)[grep(paste0("(?i)", gsub("_", ".*", missing_col)), names(data))]
      if (length(similar) > 0) {
        suggestions[[missing_col]] <- similar
      }
    }
  }

  return(list(
    missing_by_category = missing_by_category,
    mapping_suggestions = suggestions
  ))
}

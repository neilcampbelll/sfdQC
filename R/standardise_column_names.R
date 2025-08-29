
#' Standardise Column Names for QC Functions
#'
#' @description
#' Maps common CSV column variations to the standard names expected by QC functions
#'
#' @param data The dataset to standardise
#'
#' @return Dataset with standardised column names
standardise_column_names <- function(data) {

  # Common column name mappings
  # You may need to adjust these based on your actual CSV structure
  column_mappings <- list(
    # Standard mappings - adjust as needed
    "Year" = "year",
    "Country" = "country",
    "VesselLengthCategory" = "vessel_length_category",
    "GearCode" = "gear_code",
    "MetierL5" = "LE_MET_level5",
    "MetierL6" = "LE_MET_level6",
    "FishingHours" = "fishing_hours",
    "AverageKW" = "avg_kw",
    "AverageOAL" = "avg_oal",
    "AverageFishingSpeed" = "avg_fishing_speed",
    "AverageGearWidth" = "avg_gearWidth",
    "SweptAreaKm2" = "SweptArea",
    "HabitatType" = "HabitatType",
    "DepthRange" = "DepthRange",
    "CSquare" = "c_square",
    "UniqueVessels" = "UniqueVessels",
    "AnonymousVesselID" = "AnonVessels"
  )

  # Apply mappings
  for (old_name in names(column_mappings)) {
    new_name <- column_mappings[[old_name]]
    if (old_name %in% names(data) && !new_name %in% names(data)) {
      names(data)[names(data) == old_name] <- new_name
      cat("Mapped column:", old_name, "->", new_name, "\n")
    }
  }

  return(data)
}

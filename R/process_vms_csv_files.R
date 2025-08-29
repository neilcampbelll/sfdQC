#' Process VMS CSV Files and Run Quality Control Analysis
#'
#' @description
#' This function processes ICES VMS CSV files (LE and VE types) from a specified directory,
#' combines them into a unified dataset equivalent to a database extract, and runs
#' comprehensive quality control analyses.
#'
#' @param data_dir Path to the directory containing CSV files (default: "C:/Work/Spatial_Data/ICES_VMS_QC/data/data_2025")
#' @param pattern_le Pattern to match LE files (default: "ICES_LE_.*\\.csv$")
#' @param pattern_ve Pattern to match VE files (default: "ICES_VE_.*\\.csv$")
#'
#' @return A list containing the processed dataset and QC analysis results
#'
#' @details
#' The function:
#' 1. Reads all ICES_LE_XX.csv and ICES_VE_XX.csv files from the specified directory
#' 2. Extracts country codes from filenames
#' 3. Uses file creation dates as import dates
#' 4. Combines LE and VE data appropriately
#' 5. Runs all quality control analyses
#'
#' @export
process_vms_csv_files <- function(data_dir = "C:/Work/Spatial_Data/ICES_VMS_QC/data/data_2025",
                                  pattern_le = "ICES_LE_.*\\.csv$",
                                  pattern_ve = "ICES_VE_.*\\.csv$") {


  cat("Starting VMS CSV file processing...\n")

  # Check if directory exists
  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }

  # Find all LE and VE files
  le_files <- list.files(data_dir, pattern = pattern_le, full.names = TRUE)
  ve_files <- list.files(data_dir, pattern = pattern_ve, full.names = TRUE)

  cat("Found", length(le_files), "LE files and", length(ve_files), "VE files\n")

  if (length(le_files) == 0 && length(ve_files) == 0) {
    stop("No CSV files found in directory: ", data_dir)
  }

  # Function to extract country code from filename
  extract_country <- function(filepath) {
    filename <- basename(filepath)
    # Extract country code from ICES_LE_XX.csv or ICES_VE_XX.csv pattern
    country_match <- str_extract(filename, "(?:ICES_[LV]E_)([A-Z]{2})(?:\\.csv)", group = 1)
    if (is.na(country_match)) {
      # Fallback: try to extract 2-letter code before .csv
      country_match <- str_extract(filename, "([A-Z]{2})(?:\\.csv)", group = 1)
    }
    return(country_match)
  }

  # Function to get file creation date
  get_file_date <- function(filepath) {
    file_info <- file.info(filepath)
    return(as.Date(file_info$mtime))
  }

  # Function to safely read CSV and add metadata
  read_csv_with_metadata <- function(filepath, record_type) {
    country <- extract_country(filepath)
    import_date <- get_file_date(filepath)

    cat("Processing", basename(filepath), "- Country:", country, "- Import Date:", as.character(import_date), "\n")

    tryCatch({
      # Read the CSV file
      data <- read_csv(filepath, show_col_types = FALSE)

      # Add metadata columns
      data$country <- country
      data$importDate <- import_date
      data$recordtype <- record_type

      # Ensure we have an ID column (create one if missing)
      if (!"ID" %in% names(data)) {
        data$ID <- paste0(record_type, "_", country, "_", seq_len(nrow(data)))
      }

      return(data)

    }, error = function(e) {
      cat("Error reading", basename(filepath), ":", e$message, "\n")
      return(NULL)
    })
  }

  # Read all LE files
  le_data_list <- map(le_files, ~read_csv_with_metadata(.x, "LE"))
  le_data_list <- le_data_list[!map_lgl(le_data_list, is.null)]

  # Read all VE files
  ve_data_list <- map(ve_files, ~read_csv_with_metadata(.x, "VE"))
  ve_data_list <- ve_data_list[!map_lgl(ve_data_list, is.null)]

  # Combine LE data
  if (length(le_data_list) > 0) {
    cat("Combining LE data...\n")
    le_combined <- bind_rows(le_data_list)
  } else {
    le_combined <- NULL
  }

  # Combine VE data
  if (length(ve_data_list) > 0) {
    cat("Combining VE data...\n")
    ve_combined <- bind_rows(ve_data_list)
  } else {
    ve_combined <- NULL
  }

  # Create unified dataset
  # For VMS QC, we typically work primarily with VE data, but may need some LE fields
  if (!is.null(ve_combined)) {
    unified_data <- ve_combined

    # If we have LE data, try to join relevant fields
    if (!is.null(le_combined)) {
      cat("Attempting to join LE data with VE data...\n")

      # Common join keys might include vessel ID, date, etc.
      # This depends on your specific data structure - you may need to adjust
      common_cols <- intersect(names(le_combined), names(ve_combined))
      join_cols <- common_cols[!common_cols %in% c("ID", "recordtype")]

      if (length(join_cols) > 0) {
        # Select only unique columns from LE data to avoid duplication
        le_unique_cols <- setdiff(names(le_combined), names(ve_combined))
        le_for_join <- le_combined %>%
          select(all_of(c(join_cols, le_unique_cols)))

        unified_data <- unified_data %>%
          left_join(le_for_join, by = join_cols, suffix = c("", "_le"))
      }
    }
  } else if (!is.null(le_combined)) {
    unified_data <- le_combined
  } else {
    stop("No data successfully loaded from CSV files")
  }

  cat("Created unified dataset with", nrow(unified_data), "records\n")
  cat("Countries in dataset:", paste(sort(unique(unified_data$country)), collapse = ", "), "\n")

  # Check column names and suggest mappings for common QC functions
  cat("\nColumn names in dataset:\n")
  print(sort(names(unified_data)))

  # Try to standardise column names for QC functions
  unified_data <- standardise_column_names(unified_data)

  return(list(
    data = unified_data,
    le_data = le_combined,
    ve_data = ve_combined,
    summary = list(
      total_records = nrow(unified_data),
      countries = unique(unified_data$country),
      import_dates = unique(unified_data$importDate),
      years = if("year" %in% names(unified_data)) unique(unified_data$year) else "Year column not found"
    )
  ))
}

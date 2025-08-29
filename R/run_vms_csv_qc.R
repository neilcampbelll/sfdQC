#' Run Complete VMS CSV QC Analysis
#'
#' @description
#' Complete pipeline to process CSV files and run all QC analyses
#'
#' @param data_dir Path to directory containing CSV files
#'
#' @return List containing processed data and all QC results
#'
#' @export
run_vms_csv_qc <- function(data_dir = "C:/Work/Spatial_Data/ICES_VMS_QC/data/data_2025") {

  # Process CSV files
  cat("=== STEP 1: Processing CSV Files ===\n")
  processed_data <- process_vms_csv_files(data_dir)

  # Check if we have the minimum required columns for QC
  required_cols <- c("country", "year", "importDate")
  missing_cols <- setdiff(required_cols, names(processed_data$data))

  if (length(missing_cols) > 0) {
    cat("Warning: Missing required columns:", paste(missing_cols, collapse = ", "), "\n")

    # Try to create missing columns if possible
    if ("year" %in% missing_cols) {
      # Try to extract year from other date columns or create a default
      if ("importDate" %in% names(processed_data$data)) {
        processed_data$data$year <- year(processed_data$data$importDate)
        cat("Created 'year' column from importDate\n")
      } else {
        processed_data$data$year <- 2025  # Default year
        cat("Created default 'year' column (2025)\n")
      }
    }
  }

  # Run QC analyses
  cat("\n=== STEP 2: Running QC Analyses ===\n")

  # Check which QC functions we can run based on available columns
  qc_results <- list()

  # Always try these basic checks
  tryCatch({
    qc_results$id_check <- check_unique_ids(processed_data$data)
  }, error = function(e) cat("Could not run ID check:", e$message, "\n"))

  tryCatch({
    qc_results$import_analysis <- analyse_import_dates(processed_data$data)
  }, error = function(e) cat("Could not run import date analysis:", e$message, "\n"))

  tryCatch({
    qc_results$record_check <- check_record_types(processed_data$data)
  }, error = function(e) cat("Could not run record type check:", e$message, "\n"))

  tryCatch({
    qc_results$yearly_analysis <- records_by_year_country(processed_data$data)
  }, error = function(e) cat("Could not run yearly analysis:", e$message, "\n"))

  # Try other analyses based on column availability
  if ("vessel_length_category" %in% names(processed_data$data)) {
    tryCatch({
      qc_results$length_analysis <- analyse_vessel_length(processed_data$data)
    }, error = function(e) cat("Could not run vessel length analysis:", e$message, "\n"))
  }

  if ("LE_MET_level5" %in% names(processed_data$data)) {
    tryCatch({
      qc_results$metier_analysis <- analyse_metier_level5(processed_data$data)
    }, error = function(e) cat("Could not run metier analysis:", e$message, "\n"))
  }

  if (all(c("avg_kw", "avg_oal") %in% names(processed_data$data))) {
    tryCatch({
      qc_results$vessel_analysis <- analyse_vessel_characteristics(processed_data$data)
    }, error = function(e) cat("Could not run vessel characteristics analysis:", e$message, "\n"))
  }

  if (all(c("HabitatType", "DepthRange") %in% names(processed_data$data))) {
    tryCatch({
      qc_results$habitat_depth <- analyse_habitat_depth(processed_data$data)
    }, error = function(e) cat("Could not run habitat/depth analysis:", e$message, "\n"))
  }

  if ("SweptArea" %in% names(processed_data$data)) {
    tryCatch({
      qc_results$swept_analysis <- analyse_swept_area(processed_data$data)
    }, error = function(e) cat("Could not run swept area analysis:", e$message, "\n"))
  }

  # Create summary report
  cat("\n=== STEP 3: Summary ===\n")
  cat("Successfully processed", nrow(processed_data$data), "records\n")
  cat("Countries:", paste(sort(unique(processed_data$data$country)), collapse = ", "), "\n")
  cat("QC analyses completed:", length(qc_results), "\n")
  cat("Available columns:", paste(sort(names(processed_data$data)), collapse = ", "), "\n")

  return(list(
    processed_data = processed_data,
    qc_results = qc_results,
    column_mapping_needed = check_column_mappings(processed_data$data)
  ))
}

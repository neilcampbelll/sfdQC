#' Render the VMS Data Quality Control Report
#'
#' @description
#' This function renders the VMS data quality control report using the provided data
#' and the template R Markdown file included with the package.
#'
#' @param data A data frame or tibble containing VMS data.
#' @param output_file The name of the output HTML file.
#' @param template_path Path to the R Markdown template. If NULL (default), uses the template
#'   bundled with the package.
#'
#' @return The path to the rendered HTML report.
#'
#' @export
render_vms_qc_report <- function(data, output_file = "vms_qc_report.html", 
                                 template_path = NULL) {
  # If no template is provided, use the one bundled with the package
  if (is.null(template_path)) {
    template_path <- system.file("rmd", "vms_qc_report.Rmd", package = "yourpackage")
  }
  
  # Save the data temporarily for the report to use
  temp_data_path <- tempfile(fileext = ".rds")
  saveRDS(data, temp_data_path)
  
  # Set up parameters to pass to the R Markdown document
  params <- list(data_path = temp_data_path)
  
  # Render the report
  rmarkdown::render(template_path, 
                    output_file = output_file,
                    params = params)
  
  # Clean up
  unlink(temp_data_path)
  
  # Return the path to the output file
  return(output_file)
}
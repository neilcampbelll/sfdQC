#' Analyse Swept Area Information
#'
#' @description
#' This function provides a comprehensive analysis of swept area information in a fisheries VMS dataset,
#' evaluating data quality, completeness, and distributional characteristics across countries and years.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', and 'SweptArea' columns.
#'
#' @return A list containing summary tables and plots: a summary table of swept area statistics by country
#' and year, a table of data quality metrics by country, and four visualisations highlighting different
#' aspects of the swept area data. All plots are also saved as PNG files in the working directory.
#'
#' @details
#' The function performs a multi-faceted analysis of swept area data:
#' 
#' 1. It first creates a comprehensive summary of swept area data by country and year, including
#'    completeness metrics, statistics for non-zero values, and flags for potential unit inconsistencies.
#' 
#' 2. It generates a data quality summary highlighting countries with problematic data (missing or zero values).
#' 
#' 3. It attempts to standardise units across countries, converting values to square kilometers based
#'    on the magnitude of the values.
#' 
#' 4. It produces four visualisations:
#'    - A tile plot showing data quality issues by country and year
#'    - A bar chart showing the proportion of problematic data by country
#'    - A histogram showing the distribution of swept area by gear type
#'    - A heatmap showing average swept area by country and year
#' 
#' This analysis is crucial for quality control of swept area data, which is a key indicator of
#' fishing pressure on the seabed. It helps identify inconsistencies in reporting units, missing data,
#' and other quality issues that need to be addressed before using the data for scientific assessments.
#'
#' @note
#' The function assumes the presence of 'country', 'year', and 'SweptArea' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#' The function saves four PNG files in the working directory.
#'
#' @examples
#' # Create sample VMS data with swept area information
#' # Some countries report in sq. meters, others in sq. kilometers
#' vms_data <- data.frame(
#'   country = rep(c("FR", "DK", "GB", "ES"), each = 50),
#'   year = rep(rep(c(2019, 2020), each = 25), 4),
#'   gear_code = rep(c("OTB", "OTB", "DRB", "OTB"), each = 50),
#'   SweptArea = c(
#'     rnorm(50, 2000000, 500000),  # FR: sq. meters
#'     rnorm(50, 2, 0.5),           # DK: sq. kilometers
#'     sample(c(NA, runif(30, 3, 10)), 50, replace = TRUE),  # GB: some missing values
#'     rep(NA, 50)                  # ES: all missing
#'   )
#' )
#'
#' # Analyse swept area
#' # swept_analysis <- analyse_swept_area(vms_data)
#'
#' @export
analyse_swept_area <- function(data) {
  # Create a comprehensive summary of swept area data
  sa_summary <- data %>%
    group_by(country, year) %>%
    summarize(
      records = n(),
      sa_na = sum(is.na(SweptArea)),
      sa_na_pct = round(sa_na / records * 100, 2),
      sa_zero = sum(SweptArea == 0, na.rm = TRUE),
      sa_zero_pct = round(sa_zero / sum(!is.na(SweptArea)) * 100, 2),
      min_sa = min(SweptArea[SweptArea > 0], na.rm = TRUE),
      max_sa = max(SweptArea, na.rm = TRUE),
      avg_sa = mean(SweptArea, na.rm = TRUE),
      median_sa = median(SweptArea, na.rm = TRUE),
      q95_sa = quantile(SweptArea, 0.95, na.rm = TRUE),
      # Create flags for special cases
      all_na = sa_na == records,
      all_zero_or_na = sa_na + sa_zero == records,
      likely_unit = case_when(
        max_sa > 1000000 ~ "Likely sq. meters",
        max_sa > 1000 ~ "Likely sq. kilometers",
        TRUE ~ "Units unknown"
      )
    ) %>%
    ungroup() %>%
    # Replace Inf values with NA for cleaner output
    mutate(
      min_sa = ifelse(is.infinite(min_sa), NA, min_sa),
      max_sa = ifelse(is.infinite(max_sa), NA, max_sa)
    )
  
  # Create a data quality summary to highlight problematic countries
  data_quality_summary <- sa_summary %>%
    group_by(country) %>%
    summarize(
      total_records = sum(records),
      years_with_data = n_distinct(year),
      years_all_na = sum(all_na),
      years_all_zero_or_na = sum(all_zero_or_na),
      pct_years_problematic = round(years_all_zero_or_na / years_with_data * 100, 1)
    ) %>%
    arrange(desc(pct_years_problematic))
  
  # Convert the original data to consistent units (km²)
  data_km2 <- data %>%
    left_join(
      sa_summary %>% select(country, year, likely_unit),
      by = c("country", "year")
    ) %>%
    filter(!is.na(SweptArea), SweptArea > 0) %>%
    mutate(
      swept_area_km2 = case_when(
        likely_unit == "Likely sq. meters" ~ SweptArea / 1000000,
        likely_unit == "Likely sq. kilometers" ~ SweptArea,
        TRUE ~ SweptArea / 1000000  # Default assumption
      )
    )
  
  # Create a plot showing data quality issues by country and year
  p1 <- sa_summary %>%
    mutate(
      data_quality = case_when(
        all_na ~ "All NA",
        all_zero_or_na ~ "All zero or NA",
        sa_na_pct > 75 ~ "Mostly missing",
        sa_na_pct > 50 ~ "Many missing",
        sa_na_pct > 25 ~ "Some missing",
        TRUE ~ "Good data"
      ),
      data_quality = factor(data_quality, 
                            levels = c("Good data", "Some missing", "Many missing", 
                                       "Mostly missing", "All zero or NA", "All NA"))
    ) %>%
    ggplot(aes(x = factor(year), y = country, fill = data_quality)) +
    geom_tile() +
    scale_fill_manual(
      values = c("Good data" = "green3", 
                 "Some missing" = "yellowgreen",
                 "Many missing" = "gold",
                 "Mostly missing" = "orange",
                 "All zero or NA" = "tomato",
                 "All NA" = "red3"),
      name = "Data Quality"
    ) +
    theme_bw() +
    labs(
      title = "Swept Area Data Quality by Country and Year",
      subtitle = "Highlighting countries with only zero or NA values",
      x = "Year",
      y = "Country"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Create a plot showing the proportion of problematic data by country
  p2 <- data_quality_summary %>%
    ggplot(aes(x = reorder(country, pct_years_problematic), 
               y = pct_years_problematic)) +
    geom_bar(stat = "identity", 
             aes(fill = pct_years_problematic > 50)) +
    geom_text(aes(label = paste0(pct_years_problematic, "%")), 
              hjust = -0.1) +
    scale_fill_manual(
      values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
      guide = "none"
    ) +
    coord_flip() +
    theme_bw() +
    labs(
      title = "Percentage of Years with Problematic Swept Area Data by Country",
      subtitle = "Problematic = All values are either zero or missing",
      x = "Country",
      y = "Percentage of Years with Problematic Data"
    ) +
    scale_y_continuous(limits = c(0, max(data_quality_summary$pct_years_problematic) * 1.1))
  
  # Create a plot showing the distribution of swept areas by gear type
  p3 <- data_km2 %>%
    filter(swept_area_km2 < 30) %>%  # Focus on reasonable values
    ggplot(aes(x = swept_area_km2)) +
    geom_histogram(bins = 50, fill = "steelblue") +
    facet_wrap(~gear_code, scales = "free_y") +
    theme_bw() +
    labs(
      title = "Distribution of Swept Area by Gear Type",
      subtitle = "Values converted to square kilometers",
      x = "Swept Area (km²)",
      y = "Count"
    ) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))
  
  # Create a plot showing the average swept area by country and year (only for valid data)
  p4 <- sa_summary %>%
    filter(!all_zero_or_na, !is.na(avg_sa), !is.infinite(avg_sa)) %>%
    # Convert to square kilometers for consistency
    mutate(
      avg_sa_km2 = case_when(
        likely_unit == "Likely sq. meters" ~ avg_sa / 1000000,
        likely_unit == "Likely sq. kilometers" ~ avg_sa,
        TRUE ~ avg_sa / 1000000
      )
    ) %>%
    ggplot(aes(x = factor(year), y = country, fill = avg_sa_km2)) +
    geom_tile() +
    scale_fill_viridis_c(
      trans = "log10", 
      labels = function(x) round(x, 2),
      name = "Avg km²"
    ) +
    theme_bw() +
    labs(
      title = "Average Swept Area by Country and Year",
      subtitle = "Logarithmic color scale, values in square kilometers (only for valid data)",
      x = "Year",
      y = "Country"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save all plots
  ggsave("swept_area_data_quality.png", p1, width = 12, height = 8)
  ggsave("swept_area_problematic_countries.png", p2, width = 10, height = 8)
  ggsave("swept_area_distribution_by_gear.png", p3, width = 12, height = 8)
  ggsave("swept_area_heatmap.png", p4, width = 12, height = 8)
  
  # Print a summary of the problematic countries
  cat("\nCountries with significant data quality issues in swept area:\n")
  print(data_quality_summary %>% filter(pct_years_problematic > 50) %>% 
          select(country, years_with_data, years_all_zero_or_na, pct_years_problematic))
  
  # Return the summary tables and plots
  return(list(
    summary_table = sa_summary,
    data_quality = data_quality_summary,
    data_quality_plot = p1,
    problematic_countries_plot = p2,
    distribution_plot = p3,
    heatmap_plot = p4
  ))
}
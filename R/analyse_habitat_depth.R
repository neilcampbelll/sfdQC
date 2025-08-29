#' Analyse Habitat Type and Depth Information
#'
#' @description
#' This function analyses the availability and distribution of habitat type and depth range
#' information in a fisheries VMS dataset, across different countries and years.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'year', 'HabitatType',
#' and 'DepthRange' columns.
#'
#' @return A list containing three elements: a summary table of missing data percentages, a table
#' of habitat type distributions, and a table of depth range distributions. Additionally, visualisations
#' of habitat and depth distributions are displayed.
#'
#' @details
#' The function first calculates the percentage of missing habitat and depth information by country
#' and year. It then analyses the distribution of different habitat types and depth ranges across
#' countries and years, for records where this information is available.
#'
#' Two bar charts are created to visualise these distributions, allowing for easy identification of
#' patterns in habitat and depth reporting across different countries and years.
#'
#' This analysis is important for assessing the completeness of environmental context in VMS data,
#' which is crucial for evaluating fishing impacts on different marine habitats and understanding
#' the depth distribution of fishing activities.
#'
#' @note
#' The function assumes the presence of 'country', 'year', 'HabitatType', and 'DepthRange' columns
#' in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#'
#' @examples
#' # Create sample VMS data with habitat and depth information
#' habitat_types <- c("HabBenCoarSed", "HabBenMud", "HabBenSand", NA)
#' depth_ranges <- c("A", "B", "C", NA)
#' 
#' vms_data <- data.frame(
#'   country = rep(c("FR", "DK", "GB"), each = 100),
#'   year = rep(c(2019, 2020), each = 50, times = 3),
#'   HabitatType = sample(habitat_types, 300, replace = TRUE, prob = c(0.3, 0.2, 0.2, 0.3)),
#'   DepthRange = sample(depth_ranges, 300, replace = TRUE, prob = c(0.4, 0.3, 0.1, 0.2))
#' )
#'
#' # Analyse habitat and depth information
#' habitat_depth_results <- analyse_habitat_depth(vms_data)
#' print(habitat_depth_results$summary)
#'
#' @export
analyse_habitat_depth <- function(data) {
  # Summary of non-NA values
  habitat_depth_summary <- data %>%
    group_by(country, year) %>%
    summarise(
      records = n(),
      habitat_na = sum(is.na(HabitatType)),
      habitat_na_pct = 100 * habitat_na / records,
      habitat_values = n_distinct(HabitatType, na.rm = TRUE),
      depth_na = sum(is.na(DepthRange)),
      depth_na_pct = 100 * depth_na / records,
      depth_values = n_distinct(DepthRange, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(country, year)
  
  # Distribution of habitat types
  habitat_dist <- data %>%
    filter(!is.na(HabitatType)) %>%
    group_by(country, year, HabitatType) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(country, year, desc(count))
  
  # Distribution of depth ranges
  depth_dist <- data %>%
    filter(!is.na(DepthRange)) %>%
    group_by(country, year, DepthRange) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(country, year, desc(count))
  
  # Create visualizations
  p1 <- ggplot(habitat_dist, aes(x = HabitatType, y = count, fill = factor(year))) +
    geom_col(position = "dodge") +
    facet_wrap(~country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Distribution of Habitat Types by Country and Year",
         x = "Habitat Type", y = "Count",
         fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot(depth_dist, aes(x = DepthRange, y = count, fill = factor(year))) +
    geom_col(position = "dodge") +
    facet_wrap(~country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Distribution of Depth Ranges by Country and Year",
         x = "Depth Range", y = "Count",
         fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p1)
  print(p2)
  
  return(list(
    summary = habitat_depth_summary,
    habitat_dist = habitat_dist,
    depth_dist = depth_dist
  ))
}
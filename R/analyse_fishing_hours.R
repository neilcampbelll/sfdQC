#' Analyse Fishing Hours Distribution
#'
#' @description
#' This function provides a comprehensive analysis of fishing hours distributions across
#' different countries in a fisheries VMS dataset, including various summary statistics
#' and visualisations.
#'
#' @param data A data frame or tibble containing VMS data with 'country' and 'fishing_hours' columns.
#'
#' @return A list containing four ggplot objects (distribution plot, violin plot, mean/median plot,
#' and total hours plot) and a summary statistics data frame. All plots are also saved as PNG files
#' in the working directory.
#'
#' @details
#' The function first calculates summary statistics for fishing hours by country, including the
#' total, mean, median, minimum, and maximum values. It then creates four different visualisations:
#'
#' 1. A histogram showing the distribution of fishing hours by country
#' 2. A violin plot comparing the distributions across countries
#' 3. A bar chart comparing mean and median fishing hours by country
#' 4. A bar chart showing the total fishing hours by country
#'
#' These visualisations help identify potential data quality issues, such as unrealistic fishing
#' hours values or inconsistencies between countries. They also provide insights into fishing
#' patterns and effort distribution.
#'
#' The function focuses on records with reasonable fishing hours (between 0 and 24 hours), filtering
#' out potentially erroneous values.
#'
#' @note
#' The function assumes the presence of 'country' and 'fishing_hours' columns in the input dataset.
#' The function requires the dplyr, tidyr, and ggplot2 packages.
#' The function saves four PNG files in the working directory.
#'
#' @examples
#' # Create sample VMS data with fishing hours
#' vms_data <- data.frame(
#'   country = rep(c("FR", "DK", "GB"), each = 100),
#'   fishing_hours = c(
#'     runif(100, 0, 12),  # FR: uniform distribution 0-12
#'     c(runif(80, 1, 4), runif(20, 8, 12)),  # DK: bimodal distribution
#'     c(rnorm(100, 6, 2))  # GB: normal distribution around 6
#'   )
#' )
#'
#' # Analyse fishing hours
#' result <- analyse_fishing_hours(vms_data)
#' print(result$fishing_hour_stats)
#'
#' @export
analyse_fishing_hours <- function(data) {
  # First, let's look at the distribution of fishing hours
  fishing_hour_stats <- data %>%
    filter(!is.na(fishing_hours), fishing_hours > 0, fishing_hours < 24) %>%
    group_by(country) %>%
    summarize(
      total_hours = sum(fishing_hours, na.rm = TRUE),
      mean_hours = mean(fishing_hours, na.rm = TRUE),
      median_hours = median(fishing_hours, na.rm = TRUE),
      min_hours = min(fishing_hours, na.rm = TRUE),
      max_hours = max(fishing_hours, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(desc(total_hours))
  
  # Create a more informative plot showing actual distributions
  p1 <- ggplot(data %>% filter(!is.na(fishing_hours), 
                               fishing_hours > 0, 
                               fishing_hours < 24), 
               aes(x = fishing_hours)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    facet_wrap(~country, scales = "free_y") +
    theme_bw() +
    labs(
      title = "Distribution of Fishing Hours by Country",
      subtitle = "Showing trips with 0-24 hours of fishing time",
      x = "Fishing Hours per Trip",
      y = "Count of Records"
    ) +
    theme(
      strip.background = element_rect(fill = "lightblue"),
      strip.text = element_text(face = "bold")
    ) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 24))
  
  # Create a violin plot to better compare distributions across countries
  p2 <- ggplot(data %>% filter(!is.na(fishing_hours), 
                               fishing_hours > 0, 
                               fishing_hours < 24), 
               aes(x = reorder(country, fishing_hours, FUN = median), 
                   y = fishing_hours, 
                   fill = country)) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.5) +
    theme_bw() +
    labs(
      title = "Comparison of Fishing Hours Distribution Across Countries",
      subtitle = "Box plots show median, IQR, and range",
      x = "Country",
      y = "Fishing Hours per Trip"
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Create a bar chart of median and mean fishing hours
  stats_for_plot <- fishing_hour_stats %>%
    select(country, median_hours, mean_hours, count) %>%
    pivot_longer(cols = c(median_hours, mean_hours),
                 names_to = "statistic",
                 values_to = "hours")
  
  p3 <- ggplot(stats_for_plot, 
               aes(x = reorder(country, hours, FUN = max), 
                   y = hours, 
                   fill = statistic)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(hours, 1)), 
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 3) +
    theme_bw() +
    labs(
      title = "Median and Mean Fishing Hours by Country",
      subtitle = "Differences between mean and median indicate skew in distribution",
      x = "Country",
      y = "Hours",
      fill = "Statistic"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.title = element_blank()
    ) +
    scale_fill_brewer(palette = "Set1", 
                      labels = c("Mean Hours", "Median Hours"))
  
  # Create a visualization of the total fishing effort by country
  p4 <- ggplot(fishing_hour_stats, 
               aes(x = reorder(country, total_hours), 
                   y = total_hours/1000)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    geom_text(aes(label = paste0(round(total_hours/1000, 0), "K")),
              vjust = -0.5,
              color = "black") +
    theme_bw() +
    labs(
      title = "Total Fishing Hours by Country",
      x = "Country",
      y = "Total Hours (thousands)"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Save all plots
  ggsave("fishing_hours_distribution.png", p1, width = 12, height = 8)
  ggsave("fishing_hours_violin.png", p2, width = 10, height = 6)
  ggsave("fishing_hours_mean_median.png", p3, width = 10, height = 6)
  ggsave("fishing_hours_total.png", p4, width = 10, height = 6)
  
  # Return a list of the plots and stats
  return(list(
    distribution_plot = p1,
    violin_plot = p2,
    mean_median_plot = p3,
    total_hours_plot = p4,
    fishing_hour_stats = fishing_hour_stats
  ))
}
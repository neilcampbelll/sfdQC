#' Analyse Fishing Speed by Métier
#'
#' @description
#' This function conducts a detailed analysis of fishing speeds across different métiers in a
#' fisheries VMS dataset, creating visualisations grouped by gear type and target species.
#'
#' @param data A data frame or tibble containing VMS data with 'LE_MET_level6' and 'avg_fishing_speed' columns.
#'
#' @return A list of ggplot objects displaying fishing speed distributions for different gear groups
#' and target species groups. The plots are also saved as PNG files in the working directory.
#'
#' @details
#' The function first identifies the most common métiers in the dataset (those with more than 1000 records).
#' It then extracts the main gear code and target species from each métier, and groups gear codes into
#' logical categories (bottom trawls, pelagic trawls, seines, dredges, and passive gears).
#'
#' For each gear group, the function creates a plot showing the median fishing speed, along with the
#' 25th and 75th percentiles, for each métier within the group. Similar plots are also created for
#' métiers grouped by target species (DEF, SPF, CRU, MOL, DWS).
#'
#' This analysis is valuable for quality control, as fishing speeds outside the expected range for
#' a given gear type or métier may indicate data quality issues. It also provides useful reference
#' values for estimating fishing activity from VMS data.
#'
#' @note
#' The function assumes the presence of 'LE_MET_level6' and 'avg_fishing_speed' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#' The plots are saved as PNG files with names based on the gear group or target species.
#'
#' @examples
#' # This function works with a large dataset with multiple métiers
#' # Here is a simplified example:
#' metiers <- c("OTB_DEF_70-99_0_0", "PTM_SPF_16-31_0_0", "DRB_MOL_0_0_0")
#' vms_data <- data.frame(
#'   LE_MET_level6 = sample(metiers, 3000, replace = TRUE),
#'   avg_fishing_speed = runif(3000, 2, 5)
#' )
#'
#' # Analyse fishing speeds by métier
#' # Note: In practice, you would need many more records per métier
#' # plots <- analyse_fishing_speed_by_metier(vms_data)
#'
#' @export
analyse_fishing_speed_by_metier <- function(data) {
  # First, get the most common metiers in the data
  metier_counts <- data %>%
    count(LE_MET_level6) %>%
    filter(!is.na(LE_MET_level6)) %>%
    arrange(desc(n)) %>%
    filter(n > 1000)  # Focus on metiers with sufficient data
  
  # Extract the main gear code from each metier
  metier_counts <- metier_counts %>%
    mutate(
      gear_code = substr(LE_MET_level6, 1, 3),
      main_target = substr(LE_MET_level6, 5, 7)
    )
  
  # Group gear codes into logical categories
  gear_groups <- list(
    "Bottom Trawls" = c("OTB", "OTT", "PTB", "TBB"),
    "Pelagic Trawls" = c("OTM", "PTM"),
    "Seines" = c("SDN", "SSC", "PS"),
    "Dredges" = c("DRB"),
    "Passive Gears" = c("GNS", "GTR", "LLS", "FPO", "LHP", "LHM")
  )
  
  # Create a function to plot each gear group
  plot_gear_group <- function(group_name, gear_codes) {
    # Filter data for the selected gear codes
    filtered_metiers <- metier_counts %>%
      filter(gear_code %in% gear_codes) %>%
      pull(LE_MET_level6)
    
    if(length(filtered_metiers) == 0) {
      return(NULL)  # Skip if no metiers in this group
    }
    
    # Filter the data for these metiers and calculate statistics
    metier_data <- data %>%
      filter(LE_MET_level6 %in% filtered_metiers) %>%
      filter(!is.na(avg_fishing_speed)) %>%
      group_by(LE_MET_level6) %>%
      summarize(
        count = n(),
        mean_speed = mean(avg_fishing_speed, na.rm = TRUE),
        median_speed = median(avg_fishing_speed, na.rm = TRUE),
        sd_speed = sd(avg_fishing_speed, na.rm = TRUE),
        q25 = quantile(avg_fishing_speed, 0.25, na.rm = TRUE),
        q75 = quantile(avg_fishing_speed, 0.75, na.rm = TRUE)
      ) %>%
      filter(count > 100) %>%  # Ensure sufficient data for reliable stats
      arrange(desc(median_speed))
    
    # Create more readable labels by shortening metier names
    metier_data <- metier_data %>%
      mutate(
        metier_short = gsub("_0_0", "", LE_MET_level6),  # Remove common suffix
        metier_short = gsub("_0", "", metier_short)      # Remove more suffixes
      )
    
    # Generate the plot
    p <- ggplot(metier_data, aes(x = reorder(metier_short, median_speed), y = median_speed)) +
      geom_point(aes(size = count), color = "steelblue") +
      geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.3, color = "darkblue") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.title = element_text(size = 12)
      ) +
      labs(
        title = paste("Fishing Speeds for", group_name),
        subtitle = paste0(length(unique(metier_data$LE_MET_level6)), " metiers shown"),
        x = "Metier",
        y = "Speed (knots)",
        size = "Record Count"
      ) +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))
    
    return(p)
  }
  
  # Generate plots for each gear group
  plots <- list()
  for(i in seq_along(gear_groups)) {
    group_name <- names(gear_groups)[i]
    gear_codes <- gear_groups[[i]]
    
    p <- plot_gear_group(group_name, gear_codes)
    if(!is.null(p)) {
      plots[[group_name]] <- p
    }
  }
  
  # Create a separate plot for metiers by target groups
  target_groups <- c("DEF", "SPF", "CRU", "MOL", "DWS")
  
  target_plots <- list()
  for(target in target_groups) {
    filtered_target_metiers <- metier_counts %>%
      filter(main_target == target) %>%
      pull(LE_MET_level6)
    
    if(length(filtered_target_metiers) > 0) {
      # Process data for this target group
      target_data <- data %>%
        filter(LE_MET_level6 %in% filtered_target_metiers) %>%
        filter(!is.na(avg_fishing_speed)) %>%
        group_by(LE_MET_level6) %>%
        summarize(
          count = n(),
          mean_speed = mean(avg_fishing_speed, na.rm = TRUE),
          median_speed = median(avg_fishing_speed, na.rm = TRUE),
          sd_speed = sd(avg_fishing_speed, na.rm = TRUE),
          q25 = quantile(avg_fishing_speed, 0.25, na.rm = TRUE),
          q75 = quantile(avg_fishing_speed, 0.75, na.rm = TRUE)
        ) %>%
        filter(count > 100) %>%  # Ensure sufficient data for reliable stats
        arrange(desc(median_speed))
      
      # Create more readable labels
      target_data <- target_data %>%
        mutate(
          metier_short = gsub("_0_0", "", LE_MET_level6),  # Remove common suffix
          metier_short = gsub("_0", "", metier_short)      # Remove more suffixes
        )
      
      # Only create plot if we have metiers to show
      if(nrow(target_data) > 0) {
        p <- ggplot(target_data, aes(x = reorder(metier_short, median_speed), y = median_speed)) +
          geom_point(aes(size = count), color = "darkgreen") +
          geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.3, color = "darkgreen") +
          theme_bw() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            plot.title = element_text(size = 12)
          ) +
          labs(
            title = paste("Fishing Speeds for", target, "Target Group"),
            subtitle = paste0(nrow(target_data), " metiers shown"),
            x = "Metier",
            y = "Speed (knots)",
            size = "Record Count"
          ) +
          scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))
        
        target_plots[[target]] <- p
      }
    }
  }
  
  # Combine all plots into a single list
  all_plots <- c(plots, target_plots)
  
  # Save each plot individually
  for(i in seq_along(all_plots)) {
    plot_name <- names(all_plots)[i]
    ggsave(
      filename = paste0("fishing_speed_", gsub(" ", "_", tolower(plot_name)), ".png"),
      plot = all_plots[[i]],
      width = 10,
      height = 7
    )
  }
  
  # Return the list of plots
  return(all_plots)
}
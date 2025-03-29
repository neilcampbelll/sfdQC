#' Analyse Gear Width by Country and Gear Type
#'
#' @description
#' This function analyses the distribution of gear width values across different countries
#' and gear types in a fisheries VMS dataset, with special attention to placeholder values
#' and seine gears.
#'
#' @param data A data frame or tibble containing VMS data with 'country', 'gear_code', and
#' 'avg_gearWidth' columns.
#'
#' @return A tibble summarising the distribution of gear width categories by country and gear code.
#' Additionally, if seine gears (SDN, SSC) are present in the data, the function generates specific
#' visualisations for these gears and returns detailed information about them.
#'
#' @details
#' The function first categorises gear width values into meaningful ranges, identifying placeholder
#' values (such as -999 or 999999) that might represent missing data. It then creates a visualisation
#' showing the distribution of these categories across different gear types and countries.
#'
#' Special attention is given to seine gears (SDN, SSC), which often require different treatment
#' in gear width analysis due to their large deployment footprint. If these gear types are present,
#' the function produces an additional boxplot visualisation specific to these gears.
#'
#' Understanding gear width distributions is critical for accurate swept area calculations and
#' for estimating fishing pressure on seafloor habitats.
#'
#' @note
#' The function assumes the presence of 'country', 'gear_code', and 'avg_gearWidth' columns in the input dataset.
#' The function requires the dplyr and ggplot2 packages.
#' The function specifically looks for the seine gear codes 'SDN' and 'SSC'.
#'
#' @examples
#' # Create sample VMS data with various gear widths
#' vms_data <- data.frame(
#'   ID = paste0("ID", 1:9),
#'   country = rep(c("FR", "DK", "GB"), each = 3),
#'   gear_code = rep(c("OTB", "SDN", "DRB"), 3),
#'   avg_gearWidth = c(120, 950, 4, 110, 1000, 3, 130, 900, 5)
#' )
#'
#' # Analyse gear width
#' gear_width_summary <- analyse_gear_width(vms_data)
#' head(gear_width_summary)
#'
#' @export
analyse_gear_width <- function(data) {
  # First, check for placeholder values
  placeholders <- c(-999, 999999)
  
  gear_width_summary <- data %>%
    mutate(
      placeholder_flag = avg_gearWidth %in% placeholders | is.na(avg_gearWidth),
      width_category = case_when(
        placeholder_flag ~ "Placeholder/NA",
        avg_gearWidth > 1000 ~ "Very Large (>1000m)",
        avg_gearWidth > 300 ~ "Large (300-1000m)",
        avg_gearWidth > 30 ~ "Medium (30-300m)",
        avg_gearWidth > 5 ~ "Small (5-30m)",
        avg_gearWidth > 0.3 ~ "Very Small (0.3-5m)",
        avg_gearWidth > 0 ~ "Tiny (<0.3m)",
        TRUE ~ "Zero/Negative"
      )
    ) %>%
    group_by(country, gear_code, width_category) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(country, gear_code, width_category)
  
  # Create visualization
  p <- ggplot(gear_width_summary, aes(x = gear_code, y = count, fill = width_category)) +
    geom_col() +
    facet_wrap(~country, scales = "free_y") +
    theme_minimal() +
    labs(title = "Gear Width Categories by Country and Gear Code",
         x = "Gear Code", y = "Count",
         fill = "Width Category") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  # Special check for seine gears (SDN and SSC)
  seine_gears <- data %>%
    filter(gear_code %in% c("SDN", "SSC")) %>%
    filter(!is.na(avg_gearWidth)) %>%
    select(country, year, gear_code, avg_gearWidth)
  
  if (nrow(seine_gears) > 0) {
    cat("\nSeine gear (SDN, SSC) width values:\n")
    print(head(seine_gears, 20))
    
    p2 <- ggplot(seine_gears, aes(x = gear_code, y = avg_gearWidth)) +
      geom_boxplot() +
      facet_wrap(~country, scales = "free_y") +
      theme_minimal() +
      labs(title = "Gear Width for Seine Gears (SDN, SSC)",
           x = "Gear Code", y = "Average Gear Width")
    
    print(p2)
  }
  
  return(gear_width_summary)
}
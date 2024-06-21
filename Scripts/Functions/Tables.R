##########
# table functions: 



# CONTEMP - Australia overall
ERA5_tb_overall <- function(data) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), 
                         Correlation = numeric(), P_value = numeric())
  
  # Summarize Tb min, mean, max by day, year, and species
  summary_tb <- data %>%
    group_by(year, date, species) %>%
    summarise(
      mean_Tb = mean(TC, na.rm = TRUE),
      min_Tb = min(TC, na.rm = TRUE),
      max_Tb = max(TC, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, species) %>%
    summarise(
      mean_Tb = mean(mean_Tb, na.rm = TRUE),
      min_Tb = mean(min_Tb, na.rm = TRUE),
      max_Tb = mean(max_Tb, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get unique species names
  unique_species <- unique(summary_tb$species)
  
  # Function to calculate and store overall statistics
  calculate_overall_stats <- function(y_var, y_label) {
    for (sp in unique_species) {
      subset_data <- subset(summary_tb, species == sp)
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        p_value <- cor.test(subset_data$year, subset_data[[y_var]], 
                            use = "complete.obs")$p.value
        
        # Add to stats data frame
        stats_df <<- rbind(stats_df, data.frame(Species = sp, Variable = y_label, 
                                                Correlation = r, P_value = p_value))
      }
    }
    
    # Calculate and add overall correlation coefficients and p-values
    if (sum(!is.na(summary_tb[[y_var]])) > 1) {
      overall_r <- cor(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")
      overall_p_value <- cor.test(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")$p.value
      
      # Add to stats data frame
      stats_df <<- rbind(stats_df, data.frame(Species = "Overall", Variable = y_label, 
                                              Correlation = overall_r, P_value = overall_p_value))
    }
  }
  
  # Calculate overall statistics for each variable
  calculate_overall_stats("min_Tb", "Min Tb")
  calculate_overall_stats("mean_Tb", "Mean Tb")
  calculate_overall_stats("max_Tb", "Max Tb")
  
  return(stats_df)
}









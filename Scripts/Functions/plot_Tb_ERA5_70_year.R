#######
# TAREF at site (min, mean, max)
#######
ERA5_site_TaREF <- function(environ) {
  
  # Summarize Ta min, mean, max by day then by year
  summary_ta <- environ %>%
    group_by(year, date) %>%
    summarise(
      mean_Ta = mean(TAREF, na.rm = TRUE),
      min_Ta = min(TAREF, na.rm = TRUE),
      max_Ta = max(TAREF, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year) %>%
    summarise(
      mean_Ta = mean(mean_Ta, na.rm = TRUE),
      min_Ta = mean(min_Ta, na.rm = TRUE),
      max_Ta = mean(max_Ta, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate adjusted y-limits
  ylim_adjusted <- c(min(c(summary_ta$min_Ta, summary_ta$mean_Ta, summary_ta$max_Ta)) - 2.0,
                     max(c(summary_ta$min_Ta, summary_ta$mean_Ta, summary_ta$max_Ta)) + 2.0)
  
  # Set up graphical layout to make room for the legend
  old_par <- par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))
  
  # Create the plot
  plot(summary_ta$year, summary_ta$mean_Ta, 
       type = "n", xlim = range(summary_ta$year), 
       ylim = ylim_adjusted,
       main = paste(region,"site", site, ": ERA5 TREF across years"),
       xlab = "Year", ylab = "Temperature (°C)")
  
  # Add points and lines
  points(summary_ta$year, summary_ta$min_Ta, col = "blue", pch = 16)
  points(summary_ta$year, summary_ta$mean_Ta, col = "black", pch = 16)
  points(summary_ta$year, summary_ta$max_Ta, col = "red", pch = 16)
  abline(lm(summary_ta$min_Ta ~ summary_ta$year), col = "blue")
  abline(lm(summary_ta$mean_Ta ~ summary_ta$year), col = "black")
  abline(lm(summary_ta$max_Ta ~ summary_ta$year), col = "red")
  
  # Add correlation coefficients and p-values
  line_count <- -1
  for(metric in c("min_Ta", "mean_Ta", "max_Ta")) {
    r <- cor(summary_ta$year, summary_ta[[metric]], use = "complete.obs")
    p_value <- cor.test(summary_ta$year, summary_ta[[metric]], use = "complete.obs")$p.value
    col_metric <- ifelse(metric == "min_Ta", "blue", ifelse(metric == "mean_Ta", "black", "red"))
    
    mtext(paste(metric, ": r =", round(r, 3), ", p =", round(p_value, 3)),
          side = 1, line = line_count, adj = 0, col = col_metric, cex = 0.7)
    line_count <- line_count - 1
  }
  
  # Add legend outside of the graph
  legend(x = par("usr")[2], y = par("usr")[4], legend = c("Max Ta", "Mean Ta", "Min Ta"), 
         pch = 16, col = c("red", "black", "blue"), xpd = TRUE, inset = c(-0.3, 0),
         box.lty = 0, title = "Legend")
  
  # Restore graphical parameters
  par(old_par)
}



#######
# Tb mean, max, min across years
#######
ERA5_tb <- function(environ) {
  
  # summarize tb min, mean, max by day then by year
  summary_tb <- environ %>%
    group_by(year, date) %>%
    summarise(
      mean_Tb = mean(TC, na.rm = TRUE),
      min_Tb = min(TC, na.rm = TRUE),
      max_Tb = max(TC, na.rm = TRUE)
    ) %>%
    group_by(year) %>%
    summarise(
      mean_Tb = mean(mean_Tb, na.rm = TRUE),
      min_Tb = mean(min_Tb, na.rm = TRUE),
      max_Tb = mean(max_Tb, na.rm = TRUE)
    )
  
  # Create a function to plot the relationship
  plot_relationship <- function(y_var, y_label) {
    ylim_range <- range(summary_tb[[y_var]], na.rm = TRUE)
    
    # Legend: adjust y-limit by .4 unit less for minimum and .4 unit more for maximum
    ylim_range_adjusted <- c(ylim_range[1] - .4, ylim_range[2] + .4)
    
    # Plot initialization
    plot(NA, xlim = range(environ$year), ylim = ylim_range_adjusted, 
         main = paste(genus, species, ":", y_label, "across years"), 
         xlab = "Year", ylab = paste(y_label, "(°C)"), type = "n")
    
    # Add points and regression lines
    points(summary_tb$year, summary_tb[[y_var]], pch = 16)
    abline(lm(summary_tb[[y_var]] ~ summary_tb$year))
    
    r <- cor(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")
    p_value <- cor.test(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")$p.value
    
    mtext(paste("r =", round(r, 3), ", p =", round(p_value, 3)), 
          side = 1, line = -1, adj = 0, cex = 0.7)
  }
  
  par(mfrow = c(1, 3))
  
  # Plot the relationship for min_Tb
  plot_relationship("min_Tb", "Min Tb")
  
  # Plot the relationship for mean_Tb
  plot_relationship("mean_Tb", "Mean Tb")
  
  # Plot the relationship for max_Tb
  plot_relationship("max_Tb", "Max Tb")
  
}


#######
# Activity combine comparison active = (basking + foraging); inactive = inactive
#######
ERA5_comb_beh <- function(environ) {
  
  # Summarizing the data for h/d for each day and then year
  total_annual_time <- environ %>%
    mutate(
      behaviour = case_when(
        ACT %in% c(1, 2) ~ "active",
        ACT == 0 ~ "inactive",
        TRUE ~ as.character(ACT))) %>% 
    group_by(year, date, behaviour) %>%
    summarise(
      daily_hr = n()) %>% 
    group_by(year, behaviour) %>%
    summarise(annual_hr = mean(daily_hr))
  
  behaviors <- c("inactive", "active")
  
  # Set up the layout for 1 row x 2 columns
  par(mfrow = c(1, 2))
  
  # Loop through each behavior
  for(behavior in behaviors) {
    
    # Extract behavior-specific data
    behavior_data <- total_annual_time %>% 
      filter(behaviour == behavior)  # No need for !! before behavior
    
    ylim_range <- range(behavior_data$annual_hr, na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - .1, ylim_range[2] + .1)
    
    # Plot initialization
    plot(NA, xlim = range(environ$year), ylim = ylim_range_adjusted, 
         main = paste(genus, species, ":", behavior, "across years"), 
         xlab = "Year", ylab = "Mean Activity (h/d)", type = "n")
    
    points(behavior_data$year, behavior_data$annual_hr, pch = 16)
    
    # Add regression line
    abline(lm(annual_hr ~ year, data = behavior_data), col = 'blue')
    
    r <- cor(behavior_data$year, behavior_data$annual_hr, use = "complete.obs")
    p_value <- cor.test(behavior_data$year, 
                        behavior_data$annual_hr, use = "complete.obs")$p.value
    
    mtext(paste("r =", round(r, 3), ", p =", round(p_value, 3)), 
          side = 1, line = -1, adj = 0, cex = 0.7)
  }
}


#######
# Seasonal activity combine comparison active = (basking + foraging); inactive = inactive
#######
ERA5_season_comb_beh <- function(environ) {
  # Filter out winter and summer: Oct-Dec
  environ_season <- environ %>%
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(1, 2) ~ "active",
      ACT == 0 ~ "inactive",
      TRUE ~ as.character(ACT)
    ))
  
  # Summarize the data for hours per day (h/d) and then each year
  season_time <- environ_season %>%
    group_by(year, date, ACT_combined, season) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop')
  
  # Colors for each season
  season_colors <- c("summer" = "red", "winter" = "blue")
  
  # Set up the plotting area for side-by-side plots
  par(mfrow=c(1, 2))
  
  # Initialize a counter for the mtext lines
  mtext_line_counter <- 1
  
  # Loop through each behavior type to create separate plots
  for (behavior in unique(season_time$ACT_combined)) {
    # Reset the counter for each new plot
    mtext_line_counter <- 1
    
    # Subset the data for the current behavior
    behavior_data <- subset(season_time, ACT_combined == behavior)
    
    # Set up the plot
    plot(behavior_data$year, behavior_data$annual_hr,
         main=paste(genus, species, ":", behavior),
         xlab="Year", ylab="Mean Activity (h/d)",
         ylim=range(behavior_data$annual_hr) + c(-0.5, 1.4),
         type="n")
    
    # Loop through each season to plot points and fit lines
    for (s in unique(behavior_data$season)) {
      # Subset the data for the current season
      season_data <- subset(behavior_data, season == s)
      
      # Add points to the plot
      points(season_data$year, season_data$annual_hr, 
             col=season_colors[s], pch=ifelse(s == "winter", 3, 16))
      
      # Fit a linear model
      fit <- lm(annual_hr ~ year, data=season_data)
      abline(fit, col=season_colors[s])
      
      # Display correlation and p-value
      correlation <- cor(season_data$year, season_data$annual_hr)
      p_value <- summary(fit)$coefficients[2, 4]
      mtext(paste(s, ": r=", round(correlation, 2), ", p=", round(p_value, 4)),
            side = 1, line = -mtext_line_counter, adj = 0, col = season_colors[s],
            cex = 0.7)
      
      # Update the line counter
      mtext_line_counter <- mtext_line_counter + 1
    }
    
    # Add a legend
    legend("topright", legend=names(season_colors), fill=season_colors, title="Season")
  }
}



#####
# Fraction moving
#####
ERA5_season_frc_foraging <- function(environ) {
  
  # Filter out winter and summer: Oct-Dec
  environ_season <- environ %>%
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(
      ACT_combined = case_when(
        ACT == 0 ~ "inactive",
        ACT == 1 ~ "basking",
        ACT == 2 ~ "foraging"
      )
    )
  
  # environ_season <- environ_season %>% filter(year == "2012")
  # Summarize the data for hours per day (h/d) and then for each year
  season_time <- environ_season %>%
    group_by(year, date, season, ACT_combined) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, season, ACT_combined) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop')
  
  # Calculate the fraction of time spent foraging: foraging/ basking + foraging
  fraction_active <- season_time %>%
    group_by(year, season) %>%
    summarise(
      total_activity = sum(annual_hr[ACT_combined == "basking"]+ 
                             annual_hr[ACT_combined == "foraging"]),
      acivity_foraging = sum(annual_hr[ACT_combined == "foraging"]),
      acitivity_basking = sum(annual_hr[ACT_combined == "basking"]),
      .groups = 'drop'
    ) %>%
    mutate(fraction_activity = ifelse(is.na(total_activity) | total_activity == 0, 0,
                                      acivity_foraging / total_activity))
  
  # Colors and plotting characters for each season
  season_colors <- c("summer" = "red", "winter" = "blue")
  
  # Initialize plot
  par(mfrow=c(1, 1))
  
  # Set up the plot
  plot(fraction_active$year, fraction_active$fraction_activity,
       main=paste(genus, species, ": Fraction of activity time spent foraging"),
       xlab="Year", ylab="% time spent foraging",
       type="n")
  
  i <- 1  # Counter for mtext positioning
  
  # Loop through each season to plot points and fit lines
  for (s in unique(fraction_active$season)) {
    # Subset the data for the current season
    season_data <- subset(fraction_active, season == s)
    
    # Add points to the plot
    points(season_data$year, season_data$fraction_activity,
           col=season_colors[s], pch=ifelse(s == "winter", 3, 16))
    
    # Fit a linear model
    fit <- lm(fraction_activity ~ year, data=season_data)
    abline(fit, col=season_colors[s])
    
    # Display correlation and p-value
    correlation <- cor(season_data$year, season_data$fraction_activity)
    p_value <- summary(fit)$coefficients[2, 4]
    mtext(paste(s, ": r=", round(correlation, 2), ", p=", round(p_value, 4)),
          side = 1, line = -i, adj = 0, col = season_colors[s])
    
    # Update the line counter
    i <- i + 1
  }
  
  # Add a legend
  legend("bottomright", legend=names(season_colors), fill=season_colors,
         inset = c(0.2,0), title="Season")
}

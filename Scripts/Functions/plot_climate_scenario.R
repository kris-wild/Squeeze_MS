library(dplyr)
library(lubridate)
library(tidyverse)

#######
# TAREF at site (min, mean, max)
#######
clm_sce_site_TaREF <- function(metout) {
  
  # List of colors for each scenario for differentiation
  colors <- c("black", "orange", "red2")
  scenarios <- unique(metout$climate_scenario)
  plot_data <- list()
  par(mfrow = c(1, 3))
  
  # Loop through each scenario
  for(scenario in scenarios) {
    data_scenario <- metout %>% 
      filter(climate_scenario == scenario)
    
    # summarise ta min, mean, max by day then by year
    summary_ta <- data_scenario %>%
      group_by(year, date) %>%
      summarise(
        mean_Ta = mean(TAREF, na.rm = TRUE),
        min_Ta = min(TAREF, na.rm = TRUE),
        max_Ta = max(TAREF, na.rm = TRUE)
      ) %>%
      group_by(year) %>%
      summarise(
        mean_Ta = mean(mean_Ta, na.rm = TRUE),
        min_Ta = mean(min_Ta, na.rm = TRUE),
        max_Ta = mean(max_Ta, na.rm = TRUE)
      )
    
    # Store data for plotting later
    plot_data[[scenario]] <- summary_ta
  }
  
  # Create a function to plot the relationship
  plot_relationship <- function(y_var, y_label) {
    ylim_range <- range(unlist(lapply(plot_data, function(data) range(data[[y_var]], na.rm = TRUE))))
    
    # Legend: adjust y-limit by 1 unit less for minimum and 1 unit more for maximum
    ylim_range_adjusted <- c(ylim_range[1] - 1, ylim_range[2] + 1)
    
    # Plot initialization
    plot(NA, xlim = range(metout$year), ylim = ylim_range_adjusted, 
         main = paste(region,"site", site, ":", y_label, "across years"), 
         xlab = "Year", ylab = paste(y_label, "(°C)"), type = "n")
    
    # Loop through each scenario to add points and regression lines
    i <- 1
    for(scenario in scenarios) {
      data <- plot_data[[scenario]]
      points(data$year, data[[y_var]], col = colors[i], pch = 16, bg = colors[i])
      abline(lm(data[[y_var]] ~ data$year), col = colors[i])
      
      r <- cor(data$year, data[[y_var]], use = "complete.obs")
      p_value <- cor.test(data$year, data[[y_var]], use = "complete.obs")$p.value
      
      mtext(paste(scenario, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
            side = 1, line = -i, adj = 0, col = colors[i], cex = 0.7)
      i <- i + 1
    }
    
    legend("topright", legend = scenarios, pch = 16, col = colors)
  }
  
  # Plot the relationship for min_Ta
  plot_relationship("min_Ta", "Min Ta")
  
  # Plot the relationship for mean_Ta
  plot_relationship("mean_Ta", "Mean Ta")
  
  # Plot the relationship for max_Ta
  plot_relationship("max_Ta", "Max Ta")
  
}


#######
# Tb mean, max, min across years
#######
clm_sce_tb_basic <- function(environ) {
  
  # List of colors for each scenario for differentiation
  colors <- c("black", "orange", "red2")
  scenarios <- unique(environ$climate_scenario)
  plot_data <- list()
  par(mfrow = c(1, 3))
  
  # Loop through each scenario
  for(scenario in scenarios) {
    data_scenario <- environ %>% 
      filter(climate_scenario == scenario)
    
    # summarise tb min, mean, max by day then by year
    summary_tb <- data_scenario %>%
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
    
    # Store data for plotting later
    plot_data[[scenario]] <- summary_tb
  }
  
  # Create a function to plot the relationship
  plot_relationship <- function(y_var, y_label) {
    ylim_range <- range(unlist(lapply(plot_data, function(data) range(data[[y_var]], na.rm = TRUE))))
    
    # Legend: adjust y-limit by .4 unit less for minimum and .4 unit more for maximum
    ylim_range_adjusted <- c(ylim_range[1] - .4, ylim_range[2] + .4)
    
    # Plot initialization
    plot(NA, xlim = range(environ$year), ylim = ylim_range_adjusted, 
         main = paste(genus, species, ":", y_label, "across years"), 
         xlab = "Year", ylab = paste(y_label, "(°C)"), type = "n")
    
    # Loop through each scenario to add points and regression lines
    i <- 1
    for(scenario in scenarios) {
      data <- plot_data[[scenario]]
      points(data$year, data[[y_var]], col = colors[i], pch = 16, bg = colors[i])
      abline(lm(data[[y_var]] ~ data$year), col = colors[i])
      
      r <- cor(data$year, data[[y_var]], use = "complete.obs")
      p_value <- cor.test(data$year, data[[y_var]], use = "complete.obs")$p.value
      
      mtext(paste(scenario, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
            side = 1, line = -i, adj = 0, col = colors[i], cex = 0.7)
      i <- i + 1
    }
    
    legend("topright", legend = scenarios, pch = 16, col = colors)
  }
  
  # Plot the relationship for min_Tb
  plot_relationship("min_Tb", "Min Tb")
  
  # Plot the relationship for mean_Tb
  plot_relationship("mean_Tb", "Mean Tb")
  
  # Plot the relationship for max_Tb
  plot_relationship("max_Tb", "Max Tb")
  
}


#######
# Annual activity across years: active = (1=basking, 2=foraging) & inactive = (0=inactive)
#######
clm_sce_comb_beh <- function(environ) {
  
  # Summarizing the data for h/d for each day and then year
  total_annual_time <- environ %>%
    mutate(
      behaviour = case_when(
        ACT %in% c(1, 2) ~ "active",
        ACT == 0 ~ "inactive",
        TRUE ~ as.character(ACT))) %>% 
    group_by(year, date, behaviour, climate_scenario) %>%
    summarise(
      daily_hr = n()) %>% 
    group_by(year, behaviour, climate_scenario) %>%
    summarise(annual_hr = mean(daily_hr))
  
  # List of colors for each scenario for differentiation
  colors <- c("black", "orange", "red2")
  scenarios <- c("current", "future_2", "future_4")
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
    
    # Loop through each scenario to add points and regression lines
    i <- 1
    for(scenario in scenarios) {
      data_scenario <- behavior_data %>% 
        filter(climate_scenario == scenario)
      
      if(nrow(data_scenario) == 0) next
      
      points(data_scenario$year, data_scenario$annual_hr, 
             col = colors[i], pch = 16, bg = colors[i])
      abline(lm(annual_hr ~ year, data = data_scenario), col = colors[i])
      
      r <- cor(data_scenario$year, data_scenario$annual_hr, use = "complete.obs")
      p_value <- cor.test(data_scenario$year, 
                          data_scenario$annual_hr, use = "complete.obs")$p.value
      
      mtext(paste(scenario, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
            side = 1, line = -i, adj = 0, col = colors[i], cex = 0.7)
      i <- i + 1
    }
    
    legend("topright", legend = scenarios, pch = 16, col = colors, title = "Climate Scenario")
  }
}


#######
# Seasonal activity combine comparison active = (basking + foraging); inactive = inactive
#######
clm_sce_season_comb_beh <- function(environ) {
  
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
    group_by(year, date, ACT_combined, season, climate_scenario) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season, climate_scenario) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop')
  
  # Split data by the new combined behavior category and climate scenario
  split_data <- split(season_time, list(season_time$ACT_combined,
                                        season_time$climate_scenario))
  
  # Colors for each season
  season_colors <- c("summer" = "red", "winter" = "blue")
  
  # Inner function to plot for a specific combined behavior
  plot_behavior <- function(behavior) {
    par(mfrow=c(1, 3))
    
    # Legend:calculate global y-axis limits for this behavior across all climates
    behavior_data <- rbind(split_data[[paste(behavior, "current", sep=".")]],
                           split_data[[paste(behavior, "future_2", sep=".")]],
                           split_data[[paste(behavior, "future_4", sep=".")]])
    # legend min/max for placement
    global_y_min <- min(behavior_data$annual_hr) - 0.2
    global_y_max <- max(behavior_data$annual_hr) + 0.4
    
    for (climate in c("current", "future_2", "future_4")) {
      data_subset <- split_data[[paste(behavior, climate, sep=".")]]
      
    # plot relationship
      plot(data_subset$year, data_subset$annual_hr, 
           main=paste(genus, species, ":", behavior, "-", climate), 
           xlab="year", ylab="Mean Activity (h/d)", 
           ylim=c(global_y_min, global_y_max),  # Use the global y-axis limits
           col=season_colors[data_subset$season], 
           pch=ifelse(data_subset$season == "winter", 3, 16))
      
      i <- 1  # Counter for mtext positioning
      
      # Loop through seasons to add lines and get stats
      for (winter in unique(data_subset$season)) {
        season_subset <- subset(data_subset, season == winter)
        fit <- lm(annual_hr ~ year, data=season_subset)
        abline(fit, col=season_colors[winter])
        
        # Get correlation coefficient and p-value
        correlation <- cor(season_subset$year, season_subset$annual_hr)
        p_value <- summary(fit)$coefficients[2, 4]
        
        mtext(paste(winter, ": r=", round(correlation, 2), ", p=", round(p_value, 4)),
              side = 1, line = -i, adj = 0, col = season_colors[winter], cex = 0.7)
        i <- i + 1
      }
      legend("topright", legend=names(season_colors), fill=season_colors, title="Season")
      i <- 1  # Reset counter for next plot
    }
  }
  
  # Loop over the new combined behaviors
  for (behavior in unique(season_time$ACT_combined)) {
    plot_behavior(behavior)
  }
}


#######
# Fraction of activity spent foraging plots of basking & foraging, fraction foraging
#######
clm_sce_season_frc_foraging <- function(environ) {
  
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
        ACT == 2 ~ "foraging",
        TRUE ~ as.character(ACT)
      )
    )
  
  
  # Summarize the behaviour data for hours per day (h/d) and then for each year
  season_time <- environ_season %>%
    group_by(year, date, ACT_combined, season, climate_scenario) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season, climate_scenario) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop')
  
  # Calculate the fraction of time spent in "activity" = basking + foraging
  fraction_active <- season_time %>%
    group_by(year, season, climate_scenario) %>%
    summarise(
      total_activity = sum(annual_hr[ACT_combined == "basking"]+ 
                             annual_hr[ACT_combined == "foraging"]),
      acivity_foraging = sum(annual_hr[ACT_combined == "foraging"]),
      .groups = 'drop') %>%
    mutate(fraction_activity = ifelse(is.na(total_activity) | total_activity == 0, 0,
                                      acivity_foraging / total_activity))
  
  # Split data by climate scenario
  split_data <- split(fraction_active, fraction_active$climate_scenario)
  
  # Colors and plotting characters for each season
  season_colors <- c( "summer" = "red", "winter" = "blue")
  season_pch <- c("summer" = 16, "winter" = 3)
  
  # Legend placement: rounded global y-axis limits across all climate scenarios
  global_y_min <- round(min(fraction_active$fraction_activity) - 0.1, 1)
  global_y_max <- round(max(fraction_active$fraction_activity) + 0.1, 1)
  
  # Define y-axis ticks
  y_ticks <- seq(global_y_min, global_y_max, by = 0.1)
  
  # Plot for different climate scenarios
  par(mfrow=c(1, 3))
  
  for (climate in c("current", "future_2", "future_4")) {
    data_subset <- split_data[[climate]]
    
    # Legend: initialize the plot with the global y-axis limits
    plot(0, 0,
         main=paste(genus, species,": Fraction of activity time spent foraging -", climate), 
         xlab="year", ylab="% time spent foraging", 
         xlim=range(data_subset$year), ylim=c(global_y_min, global_y_max),
         type="n", yaxt="n")  # Suppress default y-axis
    
    # Add custom y-axis
    axis(side=2, at=y_ticks, las=1)
    
    i <- 1  # Counter for mtext positioning
    
    # Explicitly loop through the defined seasons
    for (season in c("winter", "summer")) {
      season_subset <- data_subset[data_subset$season == season,]
      
      if(nrow(season_subset) > 0) {  
        # Add a slight jitter for x-values
        jittered_years <- jitter(season_subset$year, amount = 0.15)
        
        points(jittered_years, season_subset$fraction_activity, 
               col=season_colors[season], pch=season_pch[season])
        
        # Adding regression line
        fit <- lm(fraction_activity ~ year, data=season_subset)
        coef_fit <- coef(fit)
        abline(a = coef_fit[1], b = coef_fit[2], col=season_colors[season])
        
        # Get correlation coefficient and p-value
        correlation <- cor(season_subset$year, season_subset$fraction_activity)
        p_value <- summary(fit)$coefficients[2, 4]
        
        mtext(paste(season, ": r=", round(correlation, 2), ", p=", round(p_value, 4)),
              side = 1, line = -i, adj = 0, col = season_colors[season], cex = 0.7)
        i <- i + 1
      }
    }
    
    # Add legend
    legend("topright", inset = c(0.2,0), legend=names(season_colors), 
           pch=c(16, 3), col=season_colors, title="Season")
  }
}








########################## ########################## ########################## 
########################## Old plots: that include just basking alone
########################## ########################## ########################## 
#######
# Annual activity across years: activity state (0=inactive, 1=basking, 2=foraging)
#######
clm_sce_beh_basic <- function(environ) {
  
  # Summarizing the data for h/d for each day and then year
  total_annual_time <- environ %>%
    group_by(year, date, ACT, climate_scenario) %>%
    summarise(
      daily_hr = n()) %>% 
    group_by(year, ACT, climate_scenario) %>%
    summarise(annual_hr = mean(daily_hr)) %>% 
    mutate(
      behavior = case_when(
        ACT == 0 ~ "inactive",
        ACT == 1 ~ "basking",
        ACT == 2 ~ "foraging",
        TRUE ~ as.character(ACT)
      )
    )
  
  # List of colors for each scenario for differentiation
  colors <- c("black", "orange", "red2")
  scenarios <- c("current", "future_2", "future_4")
  behaviors <- c("inactive", "basking", "foraging")
  
  # Loop through each behavior
  for(behavior in behaviors) {
    
    # Extract behavior-specific data once outside of the scenario loop
    behavior_data <- total_annual_time %>% 
      filter(behavior == !!behavior)
    
    ylim_range <- range(behavior_data$annual_hr, na.rm = TRUE)
    
    # Legend: adjust y-limit by .1 unit less for minimum and .1 unit more for maximum
    ylim_range_adjusted <- c(ylim_range[1] - .1, ylim_range[2] + .1)
    
    # Plot initialization
    plot(NA, xlim = range(environ$year), ylim = ylim_range_adjusted, 
         main = paste(genus, species,":", behavior, "across years"), 
         xlab = "Year", ylab = "Mean Activity (h/d)", type = "n")
    
    # Loop through each scenario to add points and regression lines
    i <- 1
    for(scenario in scenarios) {
      data_scenario <- behavior_data %>% 
        filter(climate_scenario == scenario)
      
      # If there's no data for this behavior and scenario combination, 
      # skip to the next iteration
      if(nrow(data_scenario) == 0) next
      
      points(data_scenario$year, data_scenario$annual_hr, 
             col = colors[i], pch = 16, bg = colors[i])
      abline(lm(annual_hr ~ year, data = data_scenario), col = colors[i])
      
      r <- cor(data_scenario$year, data_scenario$annual_hr, use = "complete.obs")
      p_value <- cor.test(data_scenario$year, 
                          data_scenario$annual_hr, use = "complete.obs")$p.value
      
      mtext(paste(scenario, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
            side = 1, line = -i, adj = 0, col = colors[i], cex = 0.7)
      i <- i + 1
    }
    
    legend("topright", legend = scenarios, pch = 16, col = colors, title = "Climate Scenario")
  }
}

######
# Seasonal activity across years: activity state (0=inactive, 1=basking, 2=foraging) 
######
clm_sce_season_beh_basic <- function(environ) {
  
  # Filter out winter and summer: Oct-Dec
  environ_season <- environ %>%
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other")
  
  # Summarize the data for hours per day (h/d) by day and then each year
  season_time <- environ_season %>%
    group_by(year, date, ACT, season, climate_scenario) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT, season, climate_scenario) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop') %>% 
    mutate(
      behavior = case_when(
        ACT == 0 ~ "inactive",
        ACT == 1 ~ "basking",
        ACT == 2 ~ "foraging",
        TRUE ~ as.character(ACT)
      )
    )
  
  # Split data by behavior and climate scenario
  split_data <- split(season_time, list(season_time$behavior,
                                        season_time$climate_scenario))
  
  # Colors for each season
  season_colors <- c("summer" = "red", "winter" = "blue")
  
  # Inner function to plot for a specific behavior
  plot_behavior <- function(behavior) {
    par(mfrow=c(1, 3))
    
    # Legend: calculate global y-axis limits for this behavior across all climates
    behavior_data <- rbind(split_data[[paste(behavior, "current", sep=".")]],
                           split_data[[paste(behavior, "future_2", sep=".")]],
                           split_data[[paste(behavior, "future_4", sep=".")]])
    # legend placement by behaviour
    global_y_min <- min(behavior_data$annual_hr) - 0.2
    global_y_max <- max(behavior_data$annual_hr) + 0.4
    
    for (climate in c("current", "future_2", "future_4")) {
      data_subset <- split_data[[paste(behavior, climate, sep=".")]]
      
      plot(data_subset$year, data_subset$annual_hr, 
           main=paste(genus, species, ":", behavior, "-", climate), 
           xlab="year", ylab="Mean Activity (h/d)", 
           ylim=c(global_y_min, global_y_max),  # Use the global y-axis limits
           col=season_colors[data_subset$season], 
           pch=ifelse(data_subset$season == "winter", 3, 16))
      
      i <- 1  # Counter for mtext positioning
      
      # Loop through seasons to add lines and get stats
      for (winter in unique(data_subset$season)) {
        season_subset <- subset(data_subset, season == winter)
        fit <- lm(annual_hr ~ year, data=season_subset)
        abline(fit, col=season_colors[winter])
        
        # Get correlation coefficient and p-value
        correlation <- cor(season_subset$year, season_subset$annual_hr)
        p_value <- summary(fit)$coefficients[2, 4]
        
        mtext(paste(winter, ": r=", round(correlation, 2), ", p=", round(p_value, 4)),
              side = 1, line = -i, adj = 0, col = season_colors[winter], cex = 0.7)
        i <- i + 1
      }
      legend("topright", legend=names(season_colors), fill=season_colors, title="Season")
      i <- 1  # Reset counter for next plot
    }
  }
  
  # Loop over behaviors
  for (behavior in unique(season_time$behavior)) {
    plot_behavior(behavior)
  }
}

#######
# Repo plots
#######
# !!!! Note: should we add argument with different repo seasons by species according to pianka data?

climate_scenario_repo_beh <- function(environ) {
  
  # Label the reproductive season: Oct-Dec
  environ_repo <- environ
  environ_repo$repo_season <- ifelse(environ_repo$month %in% c(10, 11, 12), "repo", "other")
  
  # Summarizing the data for hours per day (h/d) for each year
  repo_time <- environ_repo %>%
    group_by(year, date, ACT, repo_season, climate_scenario) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT, repo_season, climate_scenario) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop') %>% 
    mutate(
      behavior = case_when(
        ACT == 0 ~ "inactive",
        ACT == 1 ~ "basking",
        ACT == 2 ~ "foraging",
        TRUE ~ as.character(ACT)
      )
    )
  
  # Split data by behavior and climate scenario
  split_data <- split(repo_time, list(repo_time$behavior, repo_time$climate_scenario))
  
  # Colors for each reproductive season
  repo_colors <- c("other" = "black", "repo" = "red")
  
  # Inner function to plot for a specific behavior
  plot_behavior <- function(behavior) {
    par(mfrow=c(1, 3))
    
    i <- 1  # Counter for mtext positioning
    
    for (climate in c("current", "future_2", "future_4")) {
      data_subset <- split_data[[paste(behavior, climate, sep=".")]]
      
      # Calculate ylim adjusted to .2 unit less than the minimum and .4 unit more than the maximum
      ylim_adjusted <- c(min(data_subset$annual_hr) - .2, max(data_subset$annual_hr) + .4)
      
      plot(data_subset$year, data_subset$annual_hr, 
           main=paste(behavior, "-", climate), 
           xlab="year", ylab="Annual_hr", 
           ylim=ylim_adjusted,  # Use the adjusted ylim
           col=repo_colors[data_subset$repo_season], 
           pch=ifelse(data_subset$repo_season == "repo", 3, 16))
      
      # Loop through reproductive seasons to add lines and get stats
      for (repo in unique(data_subset$repo_season)) {
        repo_subset <- subset(data_subset, repo_season == repo)
        fit <- lm(annual_hr ~ year, data=repo_subset)
        abline(fit, col=repo_colors[repo])
        
        # Get correlation coefficient and p-value
        correlation <- cor(repo_subset$year, repo_subset$annual_hr)
        p_value <- summary(fit)$coefficients[2, 4]
        
        mtext(paste(repo, ": r=", round(correlation, 2), ", p=", round(p_value, 4)), side = 1, line = -i, adj = 0, col = repo_colors[repo], cex = 0.8)
        i <- i + 1
      }
      legend("topright", legend=names(repo_colors), fill=repo_colors, 
             title="Repo Season")
      i <- 1  # Reset counter for next plot
    }
  }
  
  # Loop over behaviors
  for (behavior in unique(repo_time$behavior)) {
    plot_behavior(behavior)
  }
}



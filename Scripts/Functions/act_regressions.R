############ Annual & seasonal activity function
###### Function that returns predicted foraging time sum

###########
# upload packages
pacman::p_load(dplyr, patchwork)

net_activity_plot <- function(data, plot_title = NULL) {
  
  # Calculating annual activity costs
  Annual_activity <- data %>%
    mutate(ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, # renaming activity for sum cal
                                    ACT == 2 ~ 1, # foraging 
                                    TRUE ~ as.numeric(ACT))) %>% 
    group_by(species, year) %>%
    summarise(Sum_activity_yr = sum(ACT_combined)) 
  
  # Get a list of all unique species in the data
  unique_species <- unique(data$species)
  unique_region <- unique(data$region)
  
  # Initialize results dataframe
  results <- data.frame(Region = character(), 
                        Species = character(), 
                        Annual_Activity_hr = numeric(),
                        Correlation = numeric(), 
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
  # List to store plots
  plot_list <- list()
  
  # Calculate linear relationship and mean activity for each species
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- Annual_activity %>% filter(species == sp)
    
    # Fit a linear model
    model <- lm(Sum_activity_yr ~ year, data = data_sp)
    
    # Get correlation and p-value
    cor_test <- cor.test(~ Sum_activity_yr + year, data = data_sp)
    
    # Calculate mean activity
    Annual_Activity <- mean(data_sp$Sum_activity_yr)
    
    coefficients <- coef(model)
    slope <- coefficients[2] * 10
    summary_model <- summary(model)
    r_squared <- summary_model$r.squared
    p <- summary_model$coefficients
    p_value <- p[2,4]
    
    # Save the results
    results <- rbind(results, data.frame(Region = unique_region, Species = sp, 
                                         Annual_Activity_hr = Annual_Activity, 
                                         Correlation = cor_test$estimate,
                                         R_2 = r_squared,
                                         Slope_10_Year = slope, 
                                         P_value = p_value)) %>% 
      remove_rownames()
    
    # Plotting with base R
    if (is.null(plot_title)) {
      plot(data_sp$year, data_sp$Sum_activity_yr, main = paste("Species:", sp),
           xlab = "Year", ylab = "Annual Activity",
           pch = 16, col = "blue")
    } else {
      plot(data_sp$year, data_sp$Sum_activity_yr, main = paste(plot_title),
           xlab = "Year", ylab = "Annual Activity",
           pch = 16, col = "blue")
    }
    
    abline(model, col = "red")
    
    legend_text <- paste("y =", round(coefficients[1], 2), "+", round(coefficients[2], 2), "x\n",
                         "R-squared =", round(r_squared, 2), "P-value =", round(p_value, 4), "\n",
                         "Decade Change =", round(slope, 2))
    legend("topleft", legend = legend_text, bty = "n", cex = 0.8)
    
    # Save the plot as an R base plot object
    plot_list[[sp]] <- recordPlot()
  }
  
  return(list(results = results, plots = plot_list))
}

seasonal_activity_plots <- function(data, plot_title = NULL) {
  
  # Filter out 'other' seasons
  data <- data %>% 
    mutate(season = case_when(month %in% c(6, 7, 8) ~ "winter", 
                              month %in% c(12, 1, 2) ~ "summer",
                              month %in% c(11, 10, 9) ~ "spring", 
                              TRUE ~ "other")) %>% 
    filter(season != "other")
  
  # Calculate seasonal activity costs
  Seasonal_activity <- data %>%
    mutate(ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, # renaming activity for sum cal
                                    ACT == 2 ~ 1, # foraging 
                                    TRUE ~ as.numeric(ACT))) %>% 
    group_by(species, year, season) %>%
    summarise(Sum_activity_season = sum(ACT_combined))
  
  # Get a list of all unique species, seasons, and regions in the data
  unique_species <- unique(data$species)
  unique_seasons <- c("winter", "spring", "summer")
  unique_region <- unique(data$region)
  
  # Initialize results dataframe
  results <- data.frame(Region = character(), 
                        Species = character(), 
                        Season = character(),  
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
  # List to store plots
  plot_list <- list()
  
  plot_for_species <- function(sp) {
    par(mfrow = c(1, 3))  # Set up a 3-panel plot
    for (se in unique_seasons) {
      # Subset the data for each species and season
      data_sp_se <- Seasonal_activity %>% filter(species == sp, season == se)
      
      # Fit a linear model
      model <- lm(Sum_activity_season ~ year, data = data_sp_se)
      coefficients <- coef(model)
      slope <- coefficients[2] * 10
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      p <- summary_model$coefficients
      p_value <- p[2,4]
      
      # Save the results
      results <<- rbind(results, data.frame(Region = unique_region, 
                                            Species = sp, 
                                            Season = se, 
                                            R_2 = r_squared,
                                            Slope_10_Year = slope, 
                                            P_value = p_value)) %>% 
        remove_rownames()
      
      # Plotting with base R
      if (is.null(plot_title)) {
        plot(data_sp_se$year, data_sp_se$Sum_activity_season, 
             main = paste("Season:", se),
             xlab = "Year", ylab = "Seasonal Activity",
             pch = 16, col = "blue", ylim = c(400, 1300))
      } else {
        plot(data_sp_se$year, data_sp_se$Sum_activity_season, 
             main = paste(plot_title, "\nSeason:", se),
             xlab = "Year", ylab = "Seasonal Activity",
             pch = 16, col = "blue", ylim = c(400, 1300))
      }
      
      abline(model, col = "red")
      
      legend_text <- paste("y =", round(coefficients[1], 2), "+", round(coefficients[2], 2), "x\n",
                           "R-squared =", round(r_squared, 2), "P-value =", round(p_value, 4), "\n",
                           "Decade Change =", round(slope, 2))
      legend("topleft", legend = legend_text, bty = "n", cex = 0.8)
    }
    par(mfrow = c(1, 1))  # Reset plotting layout to default
  }
  
  # Calculate linear relationship and mean activity for each species and season
  for (sp in unique_species) {
    plot_for_species(sp)
    plot_list[[sp]] <- recordPlot()
  }
  
  return(list(results = results, plots = plot_list))
}

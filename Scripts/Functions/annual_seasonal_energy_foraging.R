

# annual matinence costs
plot_annual_m_rate_kJ <- function(data) {
  
  # Create a season column and adding activity vs inactive column
  ectotherm <- data %>% 
    group_by(species) %>% 
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(0, 1) ~ "inactive", # includes basking and inactive
      ACT == 2 ~ "active", # foraging
      TRUE ~ as.character(ACT)),
      m_rate_J = O2_ml*20.08)
  
  # these are food parameters for an ant 
  w <- 0.0122  # weight of an ant in grams
  ant_kJ <- 27.3
  energy_per_ant <- ant_kJ*w 
  
  # Calculating annual maintenance metabolic costs
  Annual_maintenance <- ectotherm %>% 
    group_by(species, year) %>% 
    summarise(Sum_mrate_J = sum(m_rate_J)) %>% # summing annual metabolic rate
    mutate(m_rate_kJ = Sum_mrate_J*0.001, # converting metabolism J to kJ
           ant_number = m_rate_kJ/energy_per_ant) # calculating the annual ant consumption biased off m_rate_kJ
  
  
  # Get a list of all unique species in the data
  unique_species <- unique(ectotherm$species)
  
  results <- data.frame(Species = character(), Correlation = numeric(), P_value = numeric())
  
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- Annual_maintenance %>% filter(species == sp)
    
    # Fit a linear model
    model <- lm(m_rate_kJ ~ year, data = data_sp)
    
    # Get correlation and p-value
    cor_test <- cor.test(~ m_rate_kJ + year, data = data_sp)
    
    # Save the results
    results <- rbind(results, data.frame(Species = sp, r = cor_test$estimate, 
                                         P_value = cor_test$p.value))
    
    # Plotting
    plot(data_sp$year, data_sp$m_rate_kJ, main = paste("Species:", sp),
         xlab = "Year", ylab = "m_rate_kJ", pch = 19)
    abline(model, col = "blue") # Add linear fit
    
    # Add correlation coefficient and p-value to the plot
    legend("topleft", legend = paste("Correlation:", round(cor_test$estimate, 2), 
                                     "\nP-value:", round(cor_test$p.value, 4)), 
           box.lty = 0, bg = "transparent")
    
    # Pause to allow review of each plot
    readline(prompt="Press [enter] to continue")
  }
  
  return(results)
}

######################
# Seasonal matinence costs by year
plot_seasonal_annual_m_rate_kJ <- function(data) {
  
  # Create a season column and adding activity vs inactive column
  ectotherm <- data %>% 
    group_by(species) %>% 
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(0, 1) ~ "inactive", # includes basking and inactive
      ACT == 2 ~ "active", # foraging
      TRUE ~ as.character(ACT)),
      m_rate_J = O2_ml*20.08)
  
  # Calculating annual maintenance metabolic costs by season
  Annual_maintenance <- ectotherm %>% 
    group_by(species, season, year) %>% 
    summarise(Sum_mrate_J = sum(m_rate_J)) %>% # summing annual metabolic rate
    mutate(m_rate_kJ = Sum_mrate_J*0.001) # converting J to kJ
  
  # Get a list of all unique species in the data
  unique_species <- unique(ectotherm$species)
  
  # Colors for each season
  season_colors <- c("summer" = "red", "winter" = "blue")
  
  results <- data.frame(Species = character(), Season = character(), Correlation = numeric(), P_value = numeric())
  
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- Annual_maintenance %>% filter(species == sp)
    
    # Set up the plot
    plot(NA, xlim = range(data_sp$year), ylim = range(data_sp$m_rate_kJ), 
         main = paste("Species:", sp), xlab = "Year", ylab = "m_rate_kJ", 
         type = "n")
    
    text_offset <- 0.8  # Positioning offset for the text
    
    for (se in c("summer", "winter")) {
      # Subset the data for each season
      season_data <- data_sp %>% filter(season == se)
      
      # Fit a linear model
      model <- lm(m_rate_kJ ~ year, data = season_data)
      
      # Get correlation and p-value
      cor_test <- cor.test(~ m_rate_kJ + year, data = season_data)
      
      # Save the results
      results <- rbind(results, data.frame(Species = sp, Season = se, 
                                           Correlation = cor_test$estimate, 
                                           P_value = cor_test$p.value))
      
      # Add points and lines to the plot
      points(season_data$year, season_data$m_rate_kJ, col = season_colors[se], pch = 19)
      abline(model, col = season_colors[se])
      
      # Adjust the position for the summer text
      if (se == "summer") {
        text_offset <- text_offset - 0.1  # Additional adjustment for summer
      }
      
      # Add correlation coefficient and p-value to the plot
      text(min(data_sp$year), max(data_sp$m_rate_kJ) * text_offset, 
           paste(se, ": r:", round(cor_test$estimate, 2), 
                 "\np-value:", round(cor_test$p.value, 4)), 
           col = season_colors[se], adj = 0)
      
      text_offset <- text_offset - 0.2  # Adjust the position for the next text
    }
    
    # Pause to allow review of each plot
    readline(prompt="Press [enter] to continue")
  }
  
  return(results)
}


# Define a function called plot_foraging_rate with two parameters: J_per_g and food_type
food_requirments_hr <- function(data, J_per_g, food_type) {
  
  # Create a season column and adding activity vs inactive column
  ectotherm <- data %>% 
    group_by(species) %>% 
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(0, 1) ~ "inactive", # includes basking and inactive
      ACT == 2 ~ "active", # foraging
      TRUE ~ as.character(ACT)),
      m_rate_J = O2_ml*20.08)
  
  
  # Calculating g crickets / h needed for meeting maintenance metabolic rate costs
  m_rate_O2 <- ectotherm$O2_ml # add units ml/hr
  m_rate_J <- m_rate_O2 * 20.08 # conversion for oxygen j per mil
  
  # Counting the number of active hours per season and year
  active_hours_season_yearly <- ectotherm %>% 
    filter(ACT_combined == "active") %>% 
    group_by(species, year, season) %>% 
    summarise(Active_Hours = n())
  
  # Calculate the amount of food in grams using m_rate_J and food - 
  # depends on diet 
  food_g <- m_rate_J / J_per_g
  
  # Aggregate the food consumed per day, season, and year 
  # using ectotherm data frame
  food_sum_per_day_season_year <- aggregate(food_g, 
                                            by = list(ectotherm$species,
                                                      ectotherm$DAY,
                                                      ectotherm$year,
                                                      ectotherm$season),
                                            FUN = sum)
  
  # Rename the columns of the aggregated data frame
  colnames(food_sum_per_day_season_year) <- c("species", "Day", "year", "season", "Daily_Food_g")
  
  # Calculating the total amount of dry food required for each season and year
  daymon <- c(31, 28, 30, 31, 31, 31) # check dates for month days
  
  # Aggregate the food consumed per season and year, 
  # scaling by daymon 
  season_year_food_list <- aggregate(food_sum_per_day_season_year$Daily_Food_g, 
                                     by = list(food_sum_per_day_season_year$species,
                                               food_sum_per_day_season_year$year,
                                               food_sum_per_day_season_year$season),
                                     FUN = function(x) {sum(x * daymon)})
  
  # Rename the columns of the aggregated data frame
  colnames(season_year_food_list) <- c("species","year","season", "Seasonal_Yearly_Food_g")
  
  # Merge the aggregated seasonal and yearly food data with active_hours_season_yearly 
  # data frame based on year and season
  foraging_rate_per_hour_season_year <- merge(season_year_food_list, 
                                              active_hours_season_yearly, 
                                              by = c("species", "year", "season"))
  
  # Calculate Foraging Success Rate Per Hour by dividing seasonal 
  # and yearly food consumption by active hours
  foraging_rate_per_hour_season_year$Foraging_Success_Rate_Per_Hour <- foraging_rate_per_hour_season_year$Seasonal_Yearly_Food_g / foraging_rate_per_hour_season_year$Active_Hours
  
  # number of food items baised off g per hour estimate using the weight of an ant measured by Dickman and Withers 
  weight_of_one_ant_mg <- 12.2   # Assuming the weight of one ant is 12.2 mg
  weight_of_one_ant_g <- weight_of_one_ant_mg / 1000 # Converting the weight of one ant to grams
  foraging_rate_per_hour_season_year <- foraging_rate_per_hour_season_year %>%
    mutate(Ants_Required_Per_Hour = Foraging_Success_Rate_Per_Hour / weight_of_one_ant_g)
  
  # Get a list of all unique species in the data
  unique_species <- unique(foraging_rate_per_hour_season_year$species)
  
  # table with stats results
  stats_results <- data.frame(Species = character(),
                              Season = character(),
                              Pearson_r = numeric(),
                              P_value = numeric())
  
  
  # Loop over each species and plot
  for (species_name in unique_species) {
    # Resetting layout at the start of each iteration
    layout(matrix(1))
    
    # Subset the data for the current species
    species_data <- subset(foraging_rate_per_hour_season_year, species == species_name)
    
    # Plot: x-axis as year and y-axis as Foraging Success Rate Per Hour
    plot(0, 0, xlim = range(species_data$year), 
         ylim = range(species_data$Foraging_Success_Rate_Per_Hour),
         xlab = "Year", ylab = "Food requirments (g per hour active)", type = "n", 
         main = paste("Annual Food Requirments by season for", species_name))
    
    # Function to add correlation details to plot and save to stats_results
    add_corr_details <- function(subset_data, season, color) {
      cor_test <- cor.test(subset_data$year, subset_data$Foraging_Success_Rate_Per_Hour)
      abline(lm(Foraging_Success_Rate_Per_Hour ~ year, data = subset_data), col = color)
      text(x = min(subset_data$year), y = max(subset_data$Foraging_Success_Rate_Per_Hour), 
           labels = paste("r:", signif(cor_test$estimate, 3),
                          "\np-value:", signif(cor_test$p.value, 3)), 
           col = color, adj = c(0, 1), cex = 0.8, pos = 4)
      
      # Adjusting the margins to make space for the secondary Y-axis label
      par(mar = c(5, 4, 4, 4) + 0.1)
      
      # Adding a secondary Y-axis for Ants Required Per Hour
      axis(side = 4, at = axTicks(2), 
           labels =  sprintf("%.2f", axTicks(2) / weight_of_one_ant_g))
      mtext("Ants Required Per hr", side = 4, line = 3)
      
      # stat DF
      stats <- data.frame(Species = species_name,
                          Season = season,
                          Pearson_r = cor_test$estimate,
                          P_value = cor_test$p.value)
      return(stats)
    }
    
    # Subset the data for summer season and add correlation details
    subset_data_summer <- subset(species_data, season == "summer")
    if (nrow(subset_data_summer) > 0) {
      points(subset_data_summer$year, subset_data_summer$Foraging_Success_Rate_Per_Hour, col = "red", pch = 16)
      stats_summer <- add_corr_details(subset_data_summer, "Summer", "red")
      stats_results <- rbind(stats_results, stats_summer)
    }
    
    # Subset the data for winter season and add correlation details
    subset_data_winter <- subset(species_data, season == "winter")
    if (nrow(subset_data_winter) > 0) {
      points(subset_data_winter$year, subset_data_winter$Foraging_Success_Rate_Per_Hour, col = "blue", pch = 17)
      stats_winter <- add_corr_details(subset_data_winter, "Winter", "blue")
      stats_results <- rbind(stats_results, stats_winter)
    }
    
    # Add a legend to the top right of the plot
    legend("topright", legend = c("Summer", "Winter"), col = c("red", "blue"), pch = c(16, 17), lwd = 2)
    
    # png(filename=paste("plot_for_", species_name, ".png", sep=""))
    # dev.off()
    # Pause to allow review of each plot
    readline(prompt="Press [enter] to continue")
  }
  
  return(stats_results)
}


food_requirments_day <- plot_foraging_rates <- function(data, J_per_g, food_type) {
  
  # Create a season column and adding activity vs inactive column
  ectotherm <- data %>% 
    group_by(species) %>% 
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(0, 1) ~ "inactive", # includes basking and inactive
      ACT == 2 ~ "active", # foraging
      TRUE ~ as.character(ACT)),
      m_rate_J = O2_ml*20.08)
  
  
  # Calculating maintenance metabolic rate costs
  m_rate_O2 <- ectotherm$O2_ml # add units ml/hr
  m_rate_J <- m_rate_O2 * 20.08 # conversion for oxygen j per mil
  
  # Counting the number of active hours per season and year
  active_hours_season_yearly <- ectotherm %>% 
    filter(ACT_combined == "active") %>% 
    group_by(species, year, season) %>% 
    summarise(Active_Hours = n())
  
  # Calculate the amount of food in grams using m_rate_J and food - 
  # depends on diet 
  food_g <- m_rate_J / J_per_g
  
  # Aggregate the food consumed per day, season, and year 
  # using ectotherm data frame
  food_sum_per_day_season_year <- aggregate(food_g, 
                                            by = list(ectotherm$species,
                                                      ectotherm$DAY,
                                                      ectotherm$year,
                                                      ectotherm$season),
                                            FUN = sum)
  
  # Rename the columns of the aggregated data frame
  colnames(food_sum_per_day_season_year) <- c("species", "Day", "year", "season", "Daily_Food_g")
  
  # Calculating the total amount of dry food required for each season and year
  daymon <- c(31, 28, 30, 31, 31, 31) # check dates for month days
  
  # Aggregate the food consumed per season and year, 
  # scaling by daymon 
  season_year_food_list <- aggregate(food_sum_per_day_season_year$Daily_Food_g, 
                                     by = list(food_sum_per_day_season_year$species,
                                               food_sum_per_day_season_year$year,
                                               food_sum_per_day_season_year$season),
                                     FUN = function(x) {sum(x * daymon)})
  
  # Rename the columns of the aggregated data frame
  colnames(season_year_food_list) <- c("species","year","season", "Seasonal_Yearly_Food_g")
  
  # Merge the aggregated seasonal and yearly food data with active_hours_season_yearly 
  # data frame based on year and season
  foraging_rate_per_hour_season_year <- merge(season_year_food_list, 
                                              active_hours_season_yearly, 
                                              by = c("species", "year", "season"))
  
  # Calculate Foraging Success Rate Per dat by dividing seasonal 
  # and yearly food consumption by active hours and multiplying by 30
  foraging_rate_per_hour_season_year$Foraging_Success_Rate_Per_Hour <- (foraging_rate_per_hour_season_year$Seasonal_Yearly_Food_g / foraging_rate_per_hour_season_year$Active_Hours)*30 
  
  # number of food items baised off g per hour estimate using the weight of an ant measured by Dickman and Withers 
  weight_of_one_ant_mg <- 12.2   # Assuming the weight of one ant is 12.2 mg
  weight_of_one_ant_g <- weight_of_one_ant_mg / 1000 # Converting the weight of one ant to grams
  foraging_rate_per_hour_season_year <- foraging_rate_per_hour_season_year %>%
    mutate(Ants_Required_Per_Hour = Foraging_Success_Rate_Per_Hour / weight_of_one_ant_g)
  
  
  # Get a list of all unique species in the data
  unique_species <- unique(foraging_rate_per_hour_season_year$species)
  
  # table with stats results
  stats_results <- data.frame(Species = character(),
                              Season = character(),
                              Pearson_r = numeric(),
                              P_value = numeric())
  
  
  # Loop over each species and plot
  for (species_name in unique_species) {
    
    # Resetting layout at the start of each iteration
    layout(matrix(1))
    
    # Subset the data for the current species
    species_data <- subset(foraging_rate_per_hour_season_year, species == species_name)
    
    # Plot: x-axis as year and y-axis as Foraging Success Rate Per Hour
    plot(species_data$year, species_data$Foraging_Success_Rate_Per_Hour, 
         xlim = range(species_data$year), 
         ylim = range(species_data$Foraging_Success_Rate_Per_Hour),
         xlab = "Year", ylab = "Food requirements while active (g/day)", 
         pch = ifelse(species_data$season == "summer", 16, 17), 
         col = ifelse(species_data$season == "summer", "red", "blue"),
         main = paste("Annual Food Requirements by season for", species_name))
    
    # Function to add correlation details to plot and save to stats_results
    add_corr_details <- function(subset_data, season, color) {
      cor_test <- cor.test(subset_data$year, subset_data$Foraging_Success_Rate_Per_Hour)
      abline(lm(Foraging_Success_Rate_Per_Hour ~ year, data = subset_data), col = color)
      text(x = min(subset_data$year), y = max(subset_data$Foraging_Success_Rate_Per_Hour), 
           labels = paste("r:", signif(cor_test$estimate, 3),
                          "\np-value:", signif(cor_test$p.value, 3)), 
           col = color, adj = c(0, 1), cex = 0.8, pos = 4)
      
    # Adjusting the margins to make space for the secondary Y-axis label
        par(mar = c(5, 4, 4, 4) + 0.1)
        
        # Adding a secondary Y-axis for Ants Required Per Hour
        axis(side = 4, at = axTicks(2), 
             labels = round(axTicks(2) / weight_of_one_ant_g))
        mtext("Ants Required Per Day", side = 4, line = 3)
        
        # stat DF
      stats <- data.frame(Species = species_name,
                          Season = season,
                          Pearson_r = cor_test$estimate,
                          P_value = cor_test$p.value)
      return(stats)
    }
    
    # Subset the data for summer season and add correlation details
    subset_data_summer <- subset(species_data, season == "summer")
    if (nrow(subset_data_summer) > 0) {
      points(subset_data_summer$year, subset_data_summer$Foraging_Success_Rate_Per_Hour, col = "red", pch = 16)
      stats_summer <- add_corr_details(subset_data_summer, "Summer", "red")
      stats_results <- rbind(stats_results, stats_summer)
    }
    
    # Subset the data for winter season and add correlation details
    subset_data_winter <- subset(species_data, season == "winter")
    if (nrow(subset_data_winter) > 0) {
      points(subset_data_winter$year, subset_data_winter$Foraging_Success_Rate_Per_Hour, col = "blue", pch = 17)
      stats_winter <- add_corr_details(subset_data_winter, "Winter", "blue")
      stats_results <- rbind(stats_results, stats_winter)
    }
    
    # Add a legend to the top right of the plot
    legend("topright", legend = c("Summer", "Winter"), col = c("red", "blue"), pch = c(16, 17), lwd = 2)
    
    # png(filename=paste("plot_for_", species_name, ".png", sep=""))
    # dev.off()
    # Pause to allow review of each plot
    readline(prompt="Press [enter] to continue")
  }
  
  return(stats_results)
}
# plot_foraging_rate(27300.0, "Ants")

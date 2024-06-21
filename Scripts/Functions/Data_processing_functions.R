############################################
###########################################
# Data processing functions

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary warming - Ta function
contemp_ta_site  <- function(data) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), 
                         Mean_value = numeric(), Slope = numeric(), R_2 <- numeric(),
                         Correlation = numeric(), P_value = numeric())
  data <- data %>% select(-species)
  data <- data %>% rename(species = site)
  # Summarize Tb min, mean, max by day, year, and species
  summary_ta <- data %>%
    group_by(year, date, species) %>%
    summarise(
      mean_ta = mean(TAREF, na.rm = TRUE),
      min_ta = min(TAREF, na.rm = TRUE),
      max_ta = max(TAREF, na.rm = TRUE),
      .groups = 'drop') %>%
    group_by(year, species) %>%
    summarise(
      mean_ta = mean(mean_ta, na.rm = TRUE),
      min_ta = mean(min_ta, na.rm = TRUE),
      max_ta = mean(max_ta, na.rm = TRUE),
      .groups = 'drop')
  
  # Get unique species names
  unique_species <- unique(summary_ta$species)
  
  # Function to calculate and store overall statistics
  calculate_overall_stats <- function(y_var, y_label) {
    for (sp in unique_species) {
      subset_data <- subset(summary_ta, species == sp)
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        mean_value <- mean(subset_data[[y_var]], na.rm = TRUE)
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        # p_value <- cor.test(subset_data$year, subset_data[[y_var]], use = "complete.obs")$p.value
        model <- lm(subset_data[[y_var]] ~ subset_data$year)
        coefficients <- coef(model)
        slope <- coefficients[2]
        summary_model <- summary(model)
        r_squared <- summary_model$r.squared
        p <- summary_model$coefficients
        p_value <- p[2,4]
        
        # Add to stats data frame
        stats_df <<- rbind(stats_df, data.frame(Species = sp, Variable = y_label, 
                                                Mean_value = mean_value, 
                                                Slope = slope, R_2 = r_squared,
                                                Correlation = r, P_value = p_value))
      }
    }
    
    # Calculate and add overall correlation coefficients and p-values
    if (sum(!is.na(summary_ta[[y_var]])) > 1) {
      overall_mean <- mean(summary_ta[[y_var]], na.rm = TRUE)
      overall_r <- cor(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")
      # overall_p_value <- cor.test(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")$p.value
      overall_model <- lm(summary_ta[[y_var]] ~ summary_ta$year)
      overall_coefficients <- coef(overall_model)
      overall_slope <- overall_coefficients[2]
      overall_summary_model <- summary(overall_model)
      overall_r_squared <- overall_summary_model$r.squared
      overall_p <- overall_summary_model$coefficients
      overall_p_value <- overall_p[2,4]
      # Add to stats data frame
      stats_df <<- rbind(stats_df, data.frame(Species = "Overall", Variable = y_label,
                                              Mean_value = overall_mean, 
                                              Slope = overall_slope, R_2 = overall_r_squared,
                                              Correlation = overall_r, P_value = overall_p_value)) %>% 
        remove_rownames()
    }
  }
  
  # Calculate overall statistics for each variable
  calculate_overall_stats("min_ta", "Min Ta")
  calculate_overall_stats("mean_ta", "Mean Ta")
  calculate_overall_stats("max_ta", "Max Ta")
  
  return(stats_df)
}





####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary warming - Tb function
Contemp_tb_overall <- function(data) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), 
                         Mean_value = numeric(), Slope = numeric(),
                         R_2 = numeric(), Correlation = numeric(), 
                         P_value = numeric())
  
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
        mean_value <- mean(subset_data[[y_var]], na.rm = TRUE)
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        #p_value <- cor.test(subset_data$year, subset_data[[y_var]], use = "complete.obs")$p.value
        model <- lm(subset_data[[y_var]] ~ subset_data$year)
        coefficients <- coef(model)
        slope <- coefficients[2]
        summary_model <- summary(model)
        r_squared <- summary_model$r.squared
        p <- summary_model$coefficients
        p_value <- p[2,4]
        # Add to stats data frame
        stats_df <<- rbind(stats_df, data.frame(Species = sp, Variable = y_label, 
                                                Mean_value = mean_value, 
                                                Slope = slope, R_2 = r_squared,
                                                Correlation = r, P_value = p_value))
      }
    }
    
    # Calculate and add overall correlation coefficients and p-values
    if (sum(!is.na(summary_tb[[y_var]])) > 1) {
      overall_mean <- mean(summary_tb[[y_var]], na.rm = TRUE)
      overall_r <- cor(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")
      #overall_p_value <- cor.test(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")$p.value
      
      overall_model <- lm(summary_tb[[y_var]] ~ summary_tb$year)
      overall_coefficients <- coef(overall_model)
      overall_slope <- overall_coefficients[2]
      overall_summary_model <- summary(overall_model)
      overall_r_squared <- overall_summary_model$r.squared
      overall_p <- overall_summary_model$coefficients
      overall_p_value <- overall_p[2,4]
      # Add to stats data frame
      stats_df <<- rbind(stats_df, data.frame(Species = "Overall", Variable = y_label,
                                              Mean_value = overall_mean, 
                                              Slope = overall_slope, R_2 = overall_r_squared,
                                              Correlation = overall_r, 
                                              P_value = overall_p_value)) %>% 
        remove_rownames()
    }
  }
  
  # Calculate overall statistics for each variable
  calculate_overall_stats("min_Tb", "Min Tb")
  calculate_overall_stats("mean_Tb", "Mean Tb")
  calculate_overall_stats("max_Tb", "Max Tb")
  
  return(stats_df)
}



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## CONTEMPORARY warming - Ta by site and season function
contemp_ta_season  <-  function(data) {
  # Initialize an empty data frame to store overall statistics
  stats_df <- data.frame(Species = character(), 
                         Season = character(), Variable = character(), 
                         Mean_Value = numeric(), Correlation = numeric(),
                         P_value = numeric(), R_2 = numeric(), Slope = numeric())
  
  data <- data %>% select(-species)
  data <- data %>% rename(species = site)
  
  # Summarize Tb min, mean, max by day, year, season, and site
  summary_ta <- data %>%
    group_by(year, season, date, species) %>%
    summarise(
      mean_ta = mean(TAREF, na.rm = TRUE),
      min_ta = min(TAREF, na.rm = TRUE),
      max_ta = max(TAREF, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, season, species) %>%
    summarise(
      mean_ta = mean(mean_ta, na.rm = TRUE),
      min_ta = mean(min_ta, na.rm = TRUE),
      max_ta = mean(max_ta, na.rm = TRUE),
      .groups = 'drop')
  
  # Function to calculate and store overall statistics for each species and season
  calculate_overall_stats <- function(y_var, y_label) {
    unique_seasons <- unique(summary_ta$season)
    unique_species <- unique(summary_ta$species)
    
    for (seas in unique_seasons) {
      for (spec in unique_species) {
        subset_data <- subset(summary_ta, species == spec & season == seas)
        if (sum(!is.na(subset_data[[y_var]])) > 1) {
          overall_r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
          # overall_p_value <- cor.test(subset_data$year, subset_data[[y_var]], use = "complete.obs")$p.value
          overall_mean <- mean(subset_data[[y_var]], na.rm = TRUE)
          overall_model <- lm(subset_data[[y_var]] ~ subset_data$year)
          overall_coefficients <- coef(overall_model)
          overall_slope <- overall_coefficients[2]
          overall_summary_model <- summary(overall_model)
          overall_r_squared <- overall_summary_model$r.squared
          overall_p <- overall_summary_model$coefficients
          overall_p_value <- overall_p[2,4]
          
          # Add to stats data frame
          stats_df <<- rbind(stats_df, data.frame(Species = spec, season = seas, 
                                                  Variable = y_label, 
                                                  Mean_value = overall_mean,
                                                  Correlation = overall_r, 
                                                  P_value = overall_p_value,
                                                  Slope = overall_slope,
                                                  R_2 = overall_r_squared)) %>% 
            remove_rownames()
        }
      }
    }
  }
  
  # Calculate overall statistics for each variable, species, and season
  calculate_overall_stats("min_ta", "Min Ta")
  calculate_overall_stats("mean_ta", "Mean Ta")
  calculate_overall_stats("max_ta", "Max Ta")
  
  return(stats_df)
}


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Tb by species and season
# TB --- by season and species
Contemp_Tb_season_spp <-function(data) {
  # Initialize an empty data frame to store overall statistics
  stats_df <- data.frame(Species = character(), 
                         Season = character(), Variable = character(), 
                         Mean_Value = numeric(), Correlation = numeric(),
                         P_value = numeric(), R_2 = numeric(), Slope = numeric())
  
  # Summarize Tb min, mean, max by day, year, season, site, and species
  summary_tb <- data %>%
    group_by(year, season, date, site, species) %>%
    summarise(
      mean_tb = mean(TC, na.rm = TRUE),
      min_tb = min(TC, na.rm = TRUE),
      max_tb = max(TC, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, season, site, species) %>%
    summarise(
      mean_tb = mean(mean_tb, na.rm = TRUE),
      min_tb = mean(min_tb, na.rm = TRUE),
      max_tb = mean(max_tb, na.rm = TRUE),
      .groups = 'drop')
  
  # Function to calculate and store overall statistics for each species and season
  calculate_overall_stats <- function(y_var, y_label) {
    unique_seasons <- unique(summary_tb$season)
    unique_species <- unique(summary_tb$species)
    
    for (seas in unique_seasons) {
      for (spec in unique_species) {
        subset_data <- subset(summary_tb, species == spec & season == seas)
        if (sum(!is.na(subset_data[[y_var]])) > 1) {
          overall_r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
          # overall_p_value <- cor.test(subset_data$year, subset_data[[y_var]], use = "complete.obs")$p.value
          overall_mean <- mean(subset_data[[y_var]], na.rm = TRUE)
          overall_model <- lm(subset_data[[y_var]] ~ subset_data$year)
          overall_coefficients <- coef(overall_model)
          overall_slope <- overall_coefficients[2]
          overall_summary_model <- summary(overall_model)
          overall_r_squared <- overall_summary_model$r.squared
          overall_p <- overall_summary_model$coefficients
          overall_p_value <- overall_p[2,4]
          # Add to stats data frame
          stats_df <<- rbind(stats_df, data.frame(Species = spec, season = seas, 
                                                  Variable = y_label, 
                                                  Mean_value = overall_mean,
                                                  Correlation = overall_r, 
                                                  P_value = overall_p_value,
                                                  Slope = overall_slope,
                                                  R_2 = overall_r_squared)) %>% 
            remove_rownames()
        }
      }
    }
  }
  
  # Calculate overall statistics for each variable, species, and season
  calculate_overall_stats("min_tb", "Min Tb")
  calculate_overall_stats("mean_tb", "Mean Tb")
  calculate_overall_stats("max_tb", "Max Tb")
  
  return(stats_df)
}


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  ACT year function
contemp_sum_annual_activity <- function(data) {
  # Create a season column and adding activity vs inactive column
  ectotherm <- data 
  
  # Calculating annual activity costs
  Annual_activity <- ectotherm %>%
    group_by(species, year) %>%
    summarise(Sum_activity_yr = sum(ACT_combined)) 
  
  # Get a list of all unique species in the data
  unique_species <- unique(ectotherm$species)
  unique_region <- unique(ectotherm$region)
  # Initialize results dataframe
  results <- data.frame(Region = character(), 
                        Species = character(), 
                        Correlation = numeric(), 
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
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
    slope <- coefficients[2]*10
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
                                         P_value = p_value))  %>% 
      remove_rownames()
    
  }
  
  return(results)
}


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Year metabolic rate (J/g/year): species
contemp_sum_annual_m_rate_J <- function(data = data) {
  
  # Use the dynamic column name in summarise and mutate
  Annual_maintenance <- data %>% 
    mutate(m_rate_J_Wwg = m_rate_J_adj/Ww_g) %>%   # converting metabolism kJ/g
    group_by(species, year) %>% 
    summarise(m_rate_J_Wwg = sum(m_rate_J_Wwg))
  
  # Get a list of all unique species in the data
  unique_species <- unique(data$species)
  # Initialize results dataframe
  results <- data.frame(Species = character(),
                        Correlation = numeric(), 
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
  # Calculate linear relationship and mean metabolic rate for each species
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- Annual_maintenance %>% filter(species == sp)
    # Fit a linear model
    model <- lm(m_rate_J_Wwg ~ year, data = data_sp)
    coefficients <- coef(model)
    slope <- coefficients[2]*10
    summary_model <- summary(model)
    r_squared <- summary_model$r.squared
    p <- summary_model$coefficients
    p_value <- p[2,4]
    
    # Save the results
    results <- rbind(results, data.frame(Species = sp, 
                                         P_value = p_value,
                                         R_2 = r_squared, 
                                         Slope_10_Year = slope)) %>% 
      remove_rownames()
  }
  return(results)
}


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Feeding demand: mrate (kJ/g/h) per hour active across years
contemp_feeding_demand <- function(data = data) {
  
  # Use the dynamic column name in summarise and mutate
  mrate_hr_active <- data %>%
    mutate(m_rate_J_Wwg = m_rate_J_adj/Ww_g) %>% # metabolic rate (J/g) 
    group_by(species, year) %>%
    summarise(Sum_activity_yr = sum(ACT_combined), #
              Sum_mrate = sum(m_rate_J_Wwg)) %>% 
    ungroup() %>% 
    mutate(m_rate_J_g_hr_year = Sum_mrate/Sum_activity_yr) %>% 
    mutate(m_rate_J_g_hr_year = ifelse(is.infinite(m_rate_J_g_hr_year), NA, 
                                       m_rate_J_g_hr_year)) # make inf to NA; "issue with 1997 M. horridus inf"
  
  # Get a list of all unique species in the data
  unique_species <- unique(data$species)
  # Initialize results dataframe
  results <- data.frame(Species = character(),  
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
  # Calculate linear relationship and mean metabolic rate for each species
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- mrate_hr_active %>% filter(species == sp)
    # Fit a linear model
    model <- lm(m_rate_J_g_hr_year ~ year, data = data_sp, na.action = na.omit)
    coefficients <- coef(model)
    slope <- coefficients[2]*10
    summary_model <- summary(model)
    r_squared <- summary_model$r.squared
    p <- summary_model$coefficients
    p_value <- p[2,4]
    
    # Save the results
    results <- rbind(results, data.frame(Species = sp, 
                                         P_value = p_value,
                                         R_2 = r_squared, 
                                         Slope_10_Year = slope)) %>% 
      remove_rownames()
  }
  
  return(results)
}


####################   ####################   ####################   #################### 
####################   ####################   ####################   ####################
####################               SEASONAL FUNCTIONS
####################   ####################   ####################   #################### 
####################   ####################   ####################   ####################


####################   ####################   ####################   #################### 
####################   ####################   ####################   #################### 
# ACT Seasonal 
contemp_sum_seasonal_activity <- function(data) {
  # Adding activity vs inactive column
  data <- data %>% 
    filter(season != "other") # remove other seasons: spring/autumn
  
  # Calculating seasonal activity costs
  Seasonal_activity <- data %>%
    group_by(species, year, season) %>%
    summarise(Sum_activity_season = sum(ACT_combined)) 
  
  # Get a list of all unique species and seasons in the data
  unique_species <- unique(data$species)
  unique_seasons <- unique(data$season)
  unique_region <- unique(data$region)
  
  # Initialize results dataframe
  results <- data.frame(Region = character(), 
                        Species = character(), 
                        Season = character(),  
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
  # Calculate linear relationship and mean activity for each species and season
  for (sp in unique_species) {
    for (se in unique_seasons) {
      # Subset the data for each species and season
      data_sp_se <- Seasonal_activity %>% filter(species == sp, season == se)
      # Fit a linear model
      model <- lm(Sum_activity_season ~ year, data = data_sp_se)
      coefficients <- coef(model)
      slope <- coefficients[2]*10
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      p <- summary_model$coefficients
      p_value <- p[2,4]
      
      
      # Save the results
      results <- rbind(results, data.frame(Region = unique_region, 
                                           Species = sp, 
                                           Season = se, 
                                           R_2 = r_squared,
                                           Slope_10_Year = slope, 
                                           P_value = p_value)) %>% 
        remove_rownames()
    }
  }
  
  return(results)
}



####################   ####################   ####################   #################### 
####################   ####################   ####################   #################### 
############ Contemporary MR by season and species function
seasonal_annual_m_rate_J <- function(data = data) {
  
  
  # Calculating annual maintenance metabolic costs by season
  seasonal_maintenance <- data %>% 
    mutate(m_rate_kJ_Wwg = m_rate_J_adj/Ww_g) %>%  # metabolic rate (kJ/g)
    group_by(species, year, season) %>%
    summarise(Sum_activity_yr = sum(ACT_combined), 
              m_rate_kJ_g = sum(m_rate_kJ_Wwg))
  
  # Get a list of all unique species in the data
  unique_species <- unique(data$species)
  unique_region <- unique(data$region)
  results <- data.frame(Region = character(), 
                        Species = character(), 
                        Season = character(), 
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
  # Calculate linear relationship and mean metabolic rate for each species
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- seasonal_maintenance %>% filter(species == sp)
    for (se in c("summer", "winter", "spring")) {
      # Subset the data for each season
      season_data <- data_sp %>% filter(season == se)
      # Fit a linear model
      model <- lm(m_rate_kJ_g ~ year, data = season_data)
      coefficients <- coef(model)
      slope <- coefficients[2]*10
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      p <- summary_model$coefficients
      p_value <- p[2,4]
      
      # Save the results
      results <- rbind(results, data.frame(Species = sp, 
                                           Season = se, 
                                           Region = unique_region, 
                                           P_value = p_value, 
                                           R_2 = r_squared, 
                                           Slope_10_Year = slope)) %>% 
        remove_rownames()
    }
  }
  
  
  return(results)
}


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary feeding requirements by season function: 
# 1) metabolic demands per hr active
# 2) number of ants


####################
## Seasonal ants function
seasonal_feeding_demand <- function(data = data) {
  
  ## Africa and Australia data summary : Year and season by spp
  Region_foraging_year_season <- data %>% 
    mutate(m_rate_J_Wwg = m_rate_J_adj/Ww_g) %>%  
    group_by(species, year, season) %>%
    summarise(Sum_activity = sum(ACT_combined), 
              m_rate_kJ_g = sum(m_rate_J_Wwg)) %>% 
    ungroup() %>% 
    mutate(m_rate_kJ_g_ACT_season = m_rate_kJ_g/Sum_activity) %>% 
    mutate(m_rate_kJ_g_ACT_season = ifelse(is.infinite(m_rate_kJ_g_ACT_season), NA, 
                                           m_rate_kJ_g_ACT_season)) # make inf to NA; "
  
  
  # Get a list of all unique species in the data
  unique_species <- unique(Region_foraging_year_season$species)
  unique_seasons <- unique(Region_foraging_year_season$season)
  results <- data.frame(Species = character(), 
                        Season = character(), 
                        P_value = numeric(),
                        R_2 = numeric(), 
                        Slope_10_Year = numeric())
  
  # Calculate linear relationship and mean activity for each species and season
  for (sp in unique_species) {
    for (se in unique_seasons) {
      # Subset the data for each species and season
      season_data <- Region_foraging_year_season %>% 
        filter(species == sp, season == se)
      model <- lm(m_rate_kJ_g_ACT_season ~ year, 
                  data = season_data)
      coefficients <- coef(model)
      slope <- coefficients[2]*10
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      p <- summary_model$coefficients
      p_value <- round(p[2,4], digits = 4)
      # Calculate mean food for the season
      # Save the results
      results <- rbind(results, data.frame(Species = sp, 
                                           Season = se, 
                                           P_value = p_value, 
                                           R_2 = r_squared, 
                                           Slope_10_Year = slope)) %>% 
        remove_rownames()
    }
  }
  
  return(results)
}

pacman::p_load(maps, dplyr, tidyr, tidyverse, flextable, knitr, here, ggplot2, purrr, rnaturalearth, rnaturalearthdata, grid, cowplot, simplecolors, ggnewscale, gridExtra, grid)

############################################################################################################
#### 

# Change for simulation +2/-2 CTmin or CTmax
ctqu_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Ctenotus_quatt_L_environ_CTmax_47.RDS")%>% 
  mutate(Ww_g = 4)# mean mass from field data




########################## This all will remain the same
ctis_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Ctenophorus_isolepis_L_environ.RDS")%>% 
  mutate(Ww_g = 5) # mean mass from field data
geva_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Gehyra_variegata_L_environ.RDS")%>% 
  mutate(Ww_g = 4)# mean mass from field data
moho_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Moloch_horridus_R_environ.RDS")%>% 
  mutate(Ww_g = 39)# mean mass from field data
pomi_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Pogona_minor_A_environ.RDS")%>% 
  mutate(Ww_g = 36)# mean mass from field data
aus_contemp <- rbind(pomi_contemp, ctqu_contemp, 
                     moho_contemp, geva_contemp, ctis_contemp) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species),
         season = case_when(month %in% c(6, 7, 8) ~ "winter", 
                            month %in% c(12, 1, 2) ~ "summer",
                            month %in% c(11, 10, 9) ~ "spring", 
                            TRUE ~ "other"), 
         O2_ml_h = 0.013 * Ww_g^0.800 * 10^(0.038 * TC) * 10^(.14 * 0),# O2ml/h calculation
         m_rate_J = O2_ml_h*20.08, #O2_ml conversion - no adjustment to M rate and behaviour
         m_rate_J_adj = case_when(ACT == 1 ~ m_rate_J * 1.5, # basking metabolism adjustment
                                  ACT == 2 ~ m_rate_J * 4, # foraging metabolism adjustment
                                  TRUE ~ m_rate_J),
         ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, #  renaming activitiy for sum cal
                                  ACT == 2 ~ 1, # foraging 
                                  TRUE ~ as.numeric(ACT)),
         week = paste0("Week_", week(date), "_", year(date)), 
         fortnight = floor((yday(date) - 1) / 14) + 1, # fortnight column
         fortnight_period = paste0("Fortnight_", year, "_", fortnight),
         month = month(date), # month column number
         month_ch = month(date, label=TRUE))




# Africa
agac_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Agama_aculeata_X_environ.RDS") %>% 
  mutate(Ww_g = 31.9) # mean mass from field data
chan_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Chondrodactylus_angulifer_A_environ.RDS")%>%
  mutate(Ww_g = 21.2) # mean mass from field data
mesu_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Meroles_suborbitalis_L_environ.RDS") %>%
  mutate(Ww_g = 5.5) # mean mass from field data
peli_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Pedioplanis_lineoocellata_A_environ.RDS")%>%
  mutate(Ww_g = 5.2) # mean mass from field data
trsp_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Trachylepis_sparsa_B_environ.RDS")%>% 
  mutate(Ww_g = 21.3) # mean mass from field data
kal_contemp <- rbind(agac_contemp, chan_contemp, mesu_contemp, peli_contemp, trsp_contemp) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species),# defining seasons
         season = case_when(month %in% c(6, 7, 8) ~ "winter", 
                            month %in% c(12, 1, 2) ~ "summer",
                            month %in% c(11, 10, 9) ~ "spring", 
                            TRUE ~ "other"), 
         O2_ml_h = 0.013 * Ww_g^0.800 * 10^(0.038 * TC) * 10^(.14 * 0),# O2ml/h calculation
         m_rate_J = O2_ml_h*20.08, #O2_ml conversion - no adjustment to M rate and behaviour
         m_rate_J_adj = case_when(ACT == 1 ~ m_rate_J * 1.5, # basking metabolism adjustment
                                  ACT == 2 ~ m_rate_J * 4, # foraging metabolism adjustment
                                  TRUE ~ m_rate_J),
         ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, #  renaming activity for sum cal
                                  ACT == 2 ~ 1, # foraging 
                                  TRUE ~ as.numeric(ACT)),
         week = paste0("Week_", week(date), "_", year(date)), 
         fortnight = floor((yday(date) - 1) / 14) + 1, # fortnight column
         fortnight_period = paste0("Fortnight_", year, "_", fortnight),
         month = month(date), # month column number
         month_ch = month(date, label=TRUE)) %>% 
  mutate(region = if_else(region == "Kalahari", "Africa", region)) # rename kalhari to "Africa"


######### Metabolism and foraging notes: 
# Bennett & Gleeson 1979 " At normal field-foraging velocity (0.14 km/h), metabolic rate is approximately 4 times resting values - Whiptales
# Wan et al., 1997; Monitors Iguanas Fig 3. 3 to 4 times higher at moderate walking speeds
# Huey and pianka 1984 foraging vs sit and wait: widely foraging lizards appear to be about 1.3—1.5 


############
# Final merged df 
contemp <- rbind(kal_contemp, aus_contemp) 

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by year  
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
#### Contemporary warming - Ta function
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
# data
aus_ta_contemp <- contemp_ta_site(data = aus_contemp) %>% 
  filter(Species != "Overall") %>% 
  mutate(Region = "Australia", Climate_scenario = "contemporary",
         Species = paste0("Site_AU", Species))
kal_ta_contemp <- contemp_ta_site(data = kal_contemp) %>% 
  filter(Species != "Overall") %>%  
  mutate(Region = "Africa", Climate_scenario = "contemporary",
         Species = paste0("Site_AF", Species))
contemp_Ta_overall <- rbind(kal_ta_contemp, aus_ta_contemp) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Slope, Variable, Correlation, P_value) %>% 
  mutate(Slope_10_Year = Slope*10)


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Contemporary warming - Tb function
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
# Tb data - all
aus_Tb_contemp_overall_spp <- Contemp_tb_overall(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") 
kal_Tb_contemp_overall_spp <- Contemp_tb_overall(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")
contemp_Tb_overall_spp <- rbind(aus_Tb_contemp_overall_spp, kal_Tb_contemp_overall_spp)%>% 
  filter(Species != "Overall") %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Variable, Slope, Correlation, P_value) %>% 
  mutate(Slope_10_Year = Slope*10)


############ SAVE AND READ IN DATA
# final data - Australia
contemp_ta_tb_australia <- rbind(contemp_Ta_overall, contemp_Tb_overall_spp)  %>% filter(Region == "Australia")
contemp_ta_tb_australia <- contemp_ta_tb_australia %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max"
  )) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
saveRDS(contemp_ta_tb_australia, "Raw_data/contemp_ta_tb_australia_C_quatt_CTmax_47.rds")



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
# data
Aus_ACT_contemp_yr_spp <- contemp_sum_annual_activity(data = aus_contemp) %>%
  mutate(Test = "ACT", Region = "Australia")
Kal_ACT_contemp_yr_spp <- contemp_sum_annual_activity(data = kal_contemp) %>% 
  mutate(Test = "ACT", Region = "Africa")
contemp_ACT_contemp_yr_spp <- rbind(Aus_ACT_contemp_yr_spp, Kal_ACT_contemp_yr_spp)%>%
  filter(Species != "Overall") %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, R_2, Slope_10_Year, Test, P_value)


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
# data
Aus_MR_Sum_contemp_overall_spp <- contemp_sum_annual_m_rate_J(data = aus_contemp) %>%
  mutate(Test = "M_Rate", Region = "Australia")
Kal_MR_Sum_contemp_overall_spp <- contemp_sum_annual_m_rate_J(data = kal_contemp) %>% 
  mutate(Test = "M_Rate", Region = "Africa")

contemp_MR_Sum_overall_spp <- rbind(Aus_MR_Sum_contemp_overall_spp, Kal_MR_Sum_contemp_overall_spp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Test, R_2, Slope_10_Year, P_value)


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary feeding requirements by year function: 
# 1) metabolic demands (J/g) per hr active 
# 2) number of ants

####### 
# bring in get ants function
source("Scripts/Functions/get_ants.R")
# apply the hourly ant estimate to data "contemp" that has both regions
foraging_success_contemp <- contemp %>%
  group_by(species) %>% 
  rowwise() %>% # apply function by row
  mutate(ants_consumed_hourly = get_ants_hourly(m_rate_J_h = m_rate_J_adj)) %>% 
  ungroup()

####### Filter data for functions: Mrate per hr active and average ants
## data
africa_annual_foraging <- foraging_success_contemp %>% 
  filter(region == "Africa")
australia_annual_foraging <- foraging_success_contemp %>% 
  filter(region == "Australia")



####################
## 1) Feeding demand: mrate (kJ/g/h) per hour active across years
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

## annual feeding demands - region 
africa_annual_feeding_summary <- contemp_feeding_demand(africa_annual_foraging) %>% 
  mutate(Region = "Africa") 
australia_annual_feeding_summary <- contemp_feeding_demand(australia_annual_foraging)%>%
  mutate(Region = "Australia")

# final df for merge
annual_feeding_plot_data <- rbind(africa_annual_feeding_summary, 
                                  australia_annual_feeding_summary) %>%
  select(Region, Species, R_2, Slope_10_Year, P_value) %>% 
  mutate(Test = "Feeding_demand")

############### 
### Final DF and arranging data for plot
ACT_MRate_Foraging_contemp <- rbind(contemp_ACT_contemp_yr_spp, # annual activity (h)
                                    contemp_MR_Sum_overall_spp, # annual metabolic rate (J/g)
                                    annual_feeding_plot_data) # annual feeding per hr active (J/g/h)


############ SAVE AND READ IN DATA
saveRDS(ACT_MRate_Foraging_contemp, "Raw_data/ACT_MRate_Foraging_contemp_C_quatt_CTmax_47.rds")



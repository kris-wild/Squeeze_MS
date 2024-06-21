######### Ta, Tb, activity, MR, and foraging by region
## Contemp = "contemporary warming" - 70 year data 1950 - 2020
## Terra = "TerraClimate data" - 25 year 1985 - 2015
pacman::p_load( ggplot2, dplyr, tidyr)
############################################################################################################
#### Load in data
## contemporary
agac_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Agama_aculeata_X_environ.RDS")
chan_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Chondrodactylus_angulifer_A_environ.RDS")
mesu_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Meroles_suborbitalis_L_environ.RDS")
peli_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Pedioplanis_lineoocellata_A_environ.RDS")
trsp_contemp<-readRDS("output/environ/ERA5_70YR_Kalahari_Trachylepis_sparsa_B_environ.RDS")
kal_contemp <- rbind(agac_contemp, chan_contemp, mesu_contemp, peli_contemp, trsp_contemp) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species))%>% 
  mutate(season = case_when(
    month %in% c(6, 7, 8) ~ "winter",
    month %in% c(12, 1, 2) ~ "summer",
    TRUE ~ "other"))
pomi_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Pogona_minor_A_environ.RDS")
ctqu_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Ctenotus_quatt_L_environ.RDS")
moho_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Moloch_horridus_R_environ.RDS")
geva_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Gehyra_variegata_L_environ.RDS")
ctis_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Ctenophorus_isolepis_L_environ.RDS")
aus_contemp <- rbind(pomi_contemp, ctqu_contemp, moho_contemp, geva_contemp, ctis_contemp) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species))%>% 
  mutate(season = case_when(
    month %in% c(6, 7, 8) ~ "winter",
    month %in% c(12, 1, 2) ~ "summer",
    TRUE ~ "other")) 
contemp <- rbind(kal_contemp, aus_contemp) 

## +2 & + 4 data
agac_terra<-readRDS("output/climate_scenarios/climate_scenarioKalahari_Agama_aculeata_X_environ.RDS")
mesu_terra<-readRDS("output/climate_scenarios/climate_scenarioKalahari_Meroles_suborbitalis_L_environ.RDS")
peli_terra<-readRDS("output/climate_scenarios/climate_scenarioKalahari_Pedioplanis_lineoocellata_A_environ.RDS")
trsp_terra<-readRDS("output/climate_scenarios/climate_scenarioKalahari_Trachylepis_sparsa_B_environ.RDS")
chan_terra<-readRDS("output/climate_scenarios/climate_scenarioKalahari_Chondrodactylus_angulifer_A_environ.RDS")
kal_terra <- rbind(agac_terra, chan_terra, mesu_terra, peli_terra, trsp_terra) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species)) %>% mutate(region = "Africa")%>% 
  mutate(season = case_when(
    month %in% c(6, 7, 8) ~ "winter",
    month %in% c(12, 1, 2) ~ "summer",
    TRUE ~ "other"))
pomi_terra<- readRDS("output/climate_scenarios/climate_scenarioAustralia_Pogona_minor_A_environ.RDS")
ctqu_terra<-readRDS("output/climate_scenarios/climate_scenarioAustralia_Ctenotus_quatt_L_environ.RDS")
moho_terra<-readRDS("output/climate_scenarios/climate_scenarioAustralia_Moloch_horridus_R_environ.RDS")
geva_terra<-readRDS("output/climate_scenarios/climate_scenarioAustralia_Gehyra_variegata_L_environ.RDS")
ctis_terra<-readRDS("output/climate_scenarios/climate_scenarioAustralia_Ctenophorus_isolepis_L_environ.RDS")
aus_terra <- rbind(pomi_terra, ctqu_terra, moho_terra, geva_terra, ctis_terra) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species))%>% 
  mutate(season = case_when(
    month %in% c(6, 7, 8) ~ "winter",
    month %in% c(12, 1, 2) ~ "summer",
    TRUE ~ "other"))
terra <- rbind(kal_terra, aus_terra) %>% filter(climate_scenario != "current") 



############################################################################################################
#########
# Contemp Ta: overall 
contemp_overall_ta <-  function(data) {
  # Initialize an empty data frame to store overall statistics
  stats_df <- data.frame(Variable = character(), MeanValue = numeric(),
                         Correlation = numeric(), P_value = numeric())
  # Summarize Tb min, mean, max by day, year, and species
  summary_ta <- data %>%
    group_by(year, date, site) %>%
    summarise(
      mean_Ta = mean(TAREF, na.rm = TRUE),
      min_Ta = min(TAREF, na.rm = TRUE),
      max_Ta = max(TAREF, na.rm = TRUE),
      .groups = 'drop') %>%
    group_by(year, site) %>%
    summarise(
      mean_Ta = mean(mean_Ta, na.rm = TRUE),
      min_Ta = mean(min_Ta, na.rm = TRUE),
      max_Ta = mean(max_Ta, na.rm = TRUE),
      .groups = 'drop')
  
  # Function to calculate and store overall statistics
  calculate_overall_stats <- function(y_var, y_label) {
    if (sum(!is.na(summary_ta[[y_var]])) > 1) {
      overall_r <- cor(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")
      overall_p_value <- cor.test(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")$p.value
      overall_mean <- mean(summary_ta[[y_var]], na.rm = TRUE)
      
      # Add to stats data frame
      stats_df <<- rbind(stats_df, data.frame(Variable = y_label, Mean_value = overall_mean,
                                              Correlation = overall_r, P_value = overall_p_value))
    }
  }
  
  # Calculate overall statistics for each variable
  calculate_overall_stats("min_Ta", "Min Ta")
  calculate_overall_stats("mean_Ta", "Mean Ta")
  calculate_overall_stats("max_Ta", "Max Ta")
  
  return(stats_df)
}

# data
aus_ta_contemp <- contemp_overall_ta(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary")
kal_ta_contemp <- contemp_overall_ta(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")
contemp_ta_overall <- rbind(kal_ta_contemp, aus_ta_contemp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Climate_scenario, Variable, Mean_value, Correlation, P_value)


#########
# Terra: Overall
terra_ta_overall <- terra  %>% 
  group_by(region, climate_scenario, site, year, date) %>% # added site 
  summarise(daily_mean = mean(TAREF), 
            daily_min = min(TAREF), 
            daily_max = max(TAREF), 
            .groups = 'drop') %>%
  group_by(region, climate_scenario, year, site) %>% # added site
  summarise(annual_mean = mean(daily_mean),
            annual_min = mean(daily_min),
            annual_max = mean(daily_max),
            .groups = 'drop') %>% 
  group_by(region, climate_scenario, year) %>%
  summarise(annual_mean = mean(annual_mean),
            annual_min = mean(annual_min),
            annual_max = mean(annual_max),
            .groups = 'drop') %>% 
  group_by(region, climate_scenario) %>%
  summarise(Mean_Ta = mean(annual_mean),
            Min_Ta = mean(annual_min),
            Max_Ta = mean(annual_max),
            .groups = 'drop') %>% 
  rename(Region = region, Climate_scenario= climate_scenario)
# final terra dataframe
terra_ta_overall <- terra_ta_overall %>%
  pivot_longer(cols = c(Mean_Ta, Min_Ta, Max_Ta),
               names_to = "Variable",
               values_to = "Mean_value") %>%
  mutate(Variable = case_when(Variable == "Min_Ta"  ~ "Min Ta", 
                              Variable == "Mean_Ta" ~ "Mean Ta",
                              Variable == "Max_Ta"  ~ "Max Ta",
                              TRUE ~ Variable))


#########
# Overall TA - final df
Ta_overall_yr <- bind_rows(terra_ta_overall, contemp_ta_overall) %>% 
  arrange(Region,
          match(Variable, c("Min Ta", "Mean Ta", "Max Ta")),
          match(Climate_scenario, c("contemporary", "future_2", "future_4")))
Ta_overall_yr_contemp <- Ta_overall_yr %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Variable, Mean_value) %>%
  rename(Contemporary_Mean = Mean_value)
Ta_overall_yr_final <- Ta_overall_yr %>%
  left_join(Ta_overall_yr_contemp, by = c("Region", "Variable"))
Ta_overall_yr_final <- Ta_overall_yr_final %>%
  mutate(New_Column = case_when(
    Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value > Contemporary_Mean ~ "+",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value < Contemporary_Mean ~ "-",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value > Contemporary_Mean ~ "++",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value < Contemporary_Mean ~ "--",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value > Contemporary_Mean ~ "+",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value < Contemporary_Mean ~ "-",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value > Contemporary_Mean ~ "++",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value < Contemporary_Mean ~ "--",
    TRUE ~ NA_character_ ))



############################################################################################################
#########
# Contemp Ta: seasonal 
contemp_seasonal_ta <-  function(data) {
  # Initialize an empty data frame to store overall statistics
  stats_df <- data.frame(Season = character(), Variable = character(), Mean_Value = numeric(),
                         Correlation = numeric(), P_value = numeric())
  # Summarize Tb min, mean, max by day, year, season, and site
  summary_ta <- data %>%
    group_by(year, season, date, site) %>%
    summarise(
      mean_Ta = mean(TAREF, na.rm = TRUE),
      min_Ta = min(TAREF, na.rm = TRUE),
      max_Ta = max(TAREF, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, season, site) %>%
    summarise(
      mean_Ta = mean(mean_Ta, na.rm = TRUE),
      min_Ta = mean(min_Ta, na.rm = TRUE),
      max_Ta = mean(max_Ta, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Function to calculate and store overall statistics for each season
  calculate_overall_stats <- function(y_var, y_label) {
    unique_seasons <- unique(summary_ta$season)
    
    for (seas in unique_seasons) {
      subset_data <- subset(summary_ta, season == seas)
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        overall_r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        overall_p_value <- cor.test(subset_data$year, subset_data[[y_var]], use = "complete.obs")$p.value
        overall_mean <- mean(subset_data[[y_var]], na.rm = TRUE)
        
        # Add to stats data frame
        stats_df <<- rbind(stats_df, data.frame(season = seas, Variable = y_label, Mean_value = overall_mean,
                                                Correlation = overall_r, P_value = overall_p_value))
      }
    }
  }
  
  # Calculate overall statistics for each variable and season
  calculate_overall_stats("min_Ta", "Min Ta")
  calculate_overall_stats("mean_Ta", "Mean Ta")
  calculate_overall_stats("max_Ta", "Max Ta")
  
  return(stats_df)
}

# data
aus_ta_season_contemp <- contemp_seasonal_ta(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") %>% 
  filter(season != "other")
kal_ta_season_contemp <- contemp_seasonal_ta(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")  %>% 
  filter(season != "other")
contemp_ta_season <- rbind(kal_ta_season_contemp, aus_ta_season_contemp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Climate_scenario, season, Variable, Mean_value, Correlation, P_value)

#########
# Terra: Ta season
terra_ta_season <- terra  %>% 
  group_by(region, climate_scenario, season, site, date) %>%
  summarise(daily_mean = mean(TAREF, na.rm = TRUE),
            daily_min = min(TAREF, na.rm = TRUE),
            daily_max = max(TAREF, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(region, climate_scenario, season) %>%
  summarise(Mean_Ta = mean(daily_mean, na.rm = TRUE),
            Min_Ta = mean(daily_min, na.rm = TRUE),
            Max_Ta = mean(daily_max, na.rm = TRUE),
            .groups = 'drop') %>%
  rename(Region = region, Climate_scenario = climate_scenario) %>%
  pivot_longer(
    cols = c(Mean_Ta, Min_Ta, Max_Ta),
    names_to = "Variable",
    values_to = "Mean_value") %>%
  mutate(Variable = case_when(
      Variable == "Min_Ta"  ~ "Min Ta", 
      Variable == "Mean_Ta" ~ "Mean Ta",
      Variable == "Max_Ta"  ~ "Max Ta",
      TRUE ~ Variable)) %>% 
  filter(season != "other") 

######### Final dataframe with +/-
Ta_seasonal <- bind_rows(terra_ta_season, contemp_ta_season)
Ta_seasonal$Variable <- factor(Ta_seasonal$Variable, levels = c("Min Ta", "Mean Ta", "Max Ta"))
Ta_seasonal <- Ta_seasonal %>%
  arrange(Region, season, Variable, Climate_scenario)
Ta_seasonal_contemp <- Ta_seasonal %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Variable, Mean_value, season) %>%
  rename(Contemporary_Mean = Mean_value)

# Create a unique combination of Region, Variable, and Season
unique_combinations <- unique(Ta_seasonal[ , c("Region", "Variable", "season")])

# Initialize an empty dataframe to store results
Ta_seasonal_final <- data.frame()
for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  # Filter the original and contemporary dataframes for the current combination
  filtered_data <- Ta_seasonal %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           season == current_combination$season)
  filtered_contemporary <- Ta_seasonal_contemp %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           season == current_combination$season)
  # Left join the filtered data
  joined_data <- left_join(filtered_data, filtered_contemporary, by = c("Region", "Variable"))
  # Apply the case_when mutations
  processed_data <- joined_data %>%
    mutate(New_Column = case_when(
      Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value < Contemporary_Mean ~ "--",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value < Contemporary_Mean ~ "--",
      TRUE ~ NA_character_ )) 
  
  # Bind the processed data to the final dataframe
  Ta_seasonal_final <- rbind(Ta_seasonal_final, processed_data)
}
# final dataframe
Ta_seasonal_final <- Ta_seasonal_final %>% rename(season = season.x) %>% select(-season.y)

##################################################################################################
# Contemp Tb: overall 
Contemp_tb_overall <- function(data) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), Mean_value = numeric(), 
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
        mean_value <- mean(subset_data[[y_var]], na.rm = TRUE)
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        p_value <- cor.test(subset_data$year, subset_data[[y_var]], 
                            use = "complete.obs")$p.value
        
        # Add to stats data frame
        stats_df <<- rbind(stats_df, data.frame(Species = sp, Variable = y_label, Mean_value = mean_value,
                                                Correlation = r, P_value = p_value))
      }
    }
    
    # Calculate and add overall correlation coefficients and p-values
    if (sum(!is.na(summary_tb[[y_var]])) > 1) {
      overall_mean <- mean(summary_tb[[y_var]], na.rm = TRUE)
      overall_r <- cor(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")
      overall_p_value <- cor.test(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")$p.value
      
      # Add to stats data frame
      stats_df <<- rbind(stats_df, data.frame(Species = "Overall", Variable = y_label, Mean_value = overall_mean,
                                              Correlation = overall_r, P_value = overall_p_value))
    }
  }
  
  # Calculate overall statistics for each variable
  calculate_overall_stats("min_Tb", "Min Tb")
  calculate_overall_stats("mean_Tb", "Mean Tb")
  calculate_overall_stats("max_Tb", "Max Tb")
  
  return(stats_df)
}
# data
aus_Tb_contemp_overall <- Contemp_tb_overall(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") %>% 
  filter(Species == "Overall")
kal_Tb_contemp_overall <- Contemp_tb_overall(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")%>% 
  filter(Species == "Overall")
contemp_Tb_overall <- rbind(kal_Tb_contemp_overall, aus_Tb_contemp_overall)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Climate_scenario, Variable, Mean_value, Correlation, P_value)

#########
# Terra Tb: Overall
terra_Tb_overall_sum <- terra %>% 
  group_by(region, climate_scenario,species, year, date) %>%
  summarise(daily_mean = mean(TC), 
            daily_min = min(TC), 
            daily_max = max(TC), 
            .groups = 'drop') %>%
  group_by(region, climate_scenario, year, species) %>%
  summarise(annual_mean = mean(daily_mean),
            annual_min = mean(daily_min),
            annual_max = mean(daily_max),
            .groups = 'drop') %>% 
  group_by(region, climate_scenario, year) %>%
  summarise(annual_mean = mean(annual_mean),
            annual_min = mean(annual_min),
            annual_max = mean(annual_max),
            .groups = 'drop') %>%
  group_by(region, climate_scenario) %>%
  summarise(Mean_Tb = mean(annual_mean),
            Min_Tb = mean(annual_min),
            Max_Tb = mean(annual_max),
            .groups = 'drop') %>%
  rename(Region = region, Climate_scenario= climate_scenario)
# final terra dataframe
terra_Tb_overall <- terra_Tb_overall_sum %>%
  pivot_longer(cols = c(Mean_Tb, Min_Tb, Max_Tb),
               names_to = "Variable",
               values_to = "Mean_value") %>%
  mutate(Variable = case_when(Variable == "Min_Tb"  ~ "Min Tb", 
                              Variable == "Mean_Tb" ~ "Mean Tb",
                              Variable == "Max_Tb"  ~ "Max Tb",
                              TRUE ~ Variable))


#########
# Overall Tb - final df
Tb_overall_yr <- bind_rows(terra_Tb_overall, contemp_Tb_overall) %>% 
  arrange(Region,
          match(Variable, c("Min Tb", "Mean Tb", "Max Tb")),
          match(Climate_scenario, c("contemporary", "future_2", "future_4")))
Tb_overall_yr_contemp <- Tb_overall_yr %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Variable, Mean_value) %>%
  rename(Contemporary_Mean = Mean_value)
# final dataframe
Tb_overall_yr_final <- Tb_overall_yr %>%
  left_join(Tb_overall_yr_contemp, by = c("Region", "Variable"))
Tb_overall_yr_final <- Tb_overall_yr_final %>%
  mutate(New_Column = case_when(
    Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value > Contemporary_Mean ~ "+",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value < Contemporary_Mean ~ "-",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value > Contemporary_Mean ~ "++",
    Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value < Contemporary_Mean ~ "--",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) <= 2 ~ "0",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value > Contemporary_Mean ~ "+",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value < Contemporary_Mean ~ "-",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value > Contemporary_Mean ~ "++",
    Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value < Contemporary_Mean ~ "--",
    TRUE ~ NA_character_ ))  

Tb_overall_yr_final


############################################################################################################
#########
# Contemp Tb: seasonal 
Contemp_tb_season  <-  function(data) {
  # Initialize an empty data frame to store overall statistics
  stats_df <- data.frame(Season = character(), Variable = character(), 
                         Mean_Value = numeric(),
                         Correlation = numeric(), P_value = numeric())
  # Summarize Tb min, mean, max by day, year, season, and site
  summary_tb <- data %>%
    group_by(year, season, date, site) %>%
    summarise(
      mean_tb = mean(TC, na.rm = TRUE),
      min_tb = min(TC, na.rm = TRUE),
      max_tb = max(TC, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, season, site) %>%
    summarise(
      mean_tb = mean(mean_tb, na.rm = TRUE),
      min_tb = mean(min_tb, na.rm = TRUE),
      max_tb = mean(max_tb, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Function to calculate and store overall statistics for each season
  calculate_overall_stats <- function(y_var, y_label) {
    unique_seasons <- unique(summary_tb$season)
    
    for (seas in unique_seasons) {
      subset_data <- subset(summary_tb, season == seas)
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        overall_r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        overall_p_value <- cor.test(subset_data$year, subset_data[[y_var]], use = "complete.obs")$p.value
        overall_mean <- mean(subset_data[[y_var]], na.rm = TRUE)
        
        # Add to stats data frame
        stats_df <<- rbind(stats_df, data.frame(season = seas, 
                                                Variable = y_label, 
                                                Mean_value = overall_mean,
                                                Correlation = overall_r, 
                                                P_value = overall_p_value))
      }
    }
  }
  
  # Calculate overall statistics for each variable and season
  calculate_overall_stats("min_tb", "Min Tb")
  calculate_overall_stats("mean_tb", "Mean Tb")
  calculate_overall_stats("max_tb", "Max Tb")
  
  return(stats_df)
}
# data
aus_Tb_contemp_season <- Contemp_tb_season(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") %>% 
  filter(season != "other")
kal_Tb_contemp_season <- Contemp_tb_season(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")%>% 
  filter(season != "other")
contemp_tb_season <- rbind(kal_Tb_contemp_season, aus_Tb_contemp_season)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, season, Climate_scenario, Variable, Mean_value, 
         Correlation, P_value) 

#########
# Terra: Tb season
terra_tb_season <- terra  %>% 
  group_by(region, climate_scenario, season, site, date) %>%
  summarise(daily_mean = mean(TC, na.rm = TRUE),
            daily_min = min(TC, na.rm = TRUE),
            daily_max = max(TC, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(region, climate_scenario, season) %>%
  summarise(Mean_tb = mean(daily_mean, na.rm = TRUE),
            Min_tb = mean(daily_min, na.rm = TRUE),
            Max_tb = mean(daily_max, na.rm = TRUE),
            .groups = 'drop') %>%
  rename(Region = region, Climate_scenario = climate_scenario) %>%
  pivot_longer(
    cols = c(Mean_tb, Min_tb, Max_tb),
    names_to = "Variable",
    values_to = "Mean_value") %>%
  mutate(Variable = case_when(
    Variable == "Min_tb"  ~ "Min Tb", 
    Variable == "Mean_tb" ~ "Mean Tb",
    Variable == "Max_tb"  ~ "Max Tb",
    TRUE ~ Variable)) %>% 
  filter(season != "other") 

#########
# Seasonal Tb - final df
Tb_seasonal <- bind_rows(terra_tb_season, contemp_tb_season)
Tb_seasonal$Variable <- factor(Tb_seasonal$Variable, levels = c("Min Tb", "Mean Tb", "Max Tb"))
Tb_seasonal <- Tb_seasonal %>%
  arrange(Region, season, Variable, Climate_scenario)
Tb_seasonal_contemp <- Tb_seasonal %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Variable, Mean_value, season) %>%
  rename(Contemporary_Mean = Mean_value)

# Create a unique combination of Region, Variable, and Season
unique_combinations <- unique(Tb_seasonal[ , c("Region", "Variable", "season")])

# Initialize an empty dataframe to store results
Tb_seasonal_final <- data.frame()
for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  # Filter the original and contemporary dataframes for the current combination
  filtered_data <- Tb_seasonal %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           season == current_combination$season)
  filtered_contemporary <- Tb_seasonal_contemp %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           season == current_combination$season)
  # Left join the filtered data
  joined_data <- left_join(filtered_data, filtered_contemporary, by = c("Region", "Variable"))
  # Apply the case_when mutations
  processed_data <- joined_data %>%
    mutate(New_Column = case_when(
      Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value < Contemporary_Mean ~ "--",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value < Contemporary_Mean ~ "--",
      TRUE ~ NA_character_ ))
  
  # Bind the processed data to the final dataframe
  Tb_seasonal_final <- rbind(Tb_seasonal_final, processed_data)
}
# final dataframe
Tb_seasonal_final <- Tb_seasonal_final %>% rename(season = season.x) %>% select(-season.y)


############################################################################################################
#### Contemp Tb: overall BY SPP
# data
aus_Tb_contemp_overall_spp <- Contemp_tb_overall(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") %>% 
  filter(Species != "Overall")
kal_Tb_contemp_overall_spp <- Contemp_tb_overall(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")%>% 
  filter(Species != "Overall")
contemp_Tb_overall_spp <- rbind(aus_Tb_contemp_overall_spp, kal_Tb_contemp_overall_spp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Climate_scenario, 
         Variable, Mean_value, Correlation, P_value)

########## 
# Terra Tb: Overall
terra_Tb_overall_spp_sum <- terra %>% 
  group_by(region, climate_scenario, species, year, date) %>%
  summarise(daily_mean = mean(TC), 
            daily_min = min(TC), 
            daily_max = max(TC), 
            .groups = 'drop') %>%
  group_by(region, climate_scenario, year, species) %>%
  summarise(annual_mean = mean(daily_mean),
            annual_min = mean(daily_min),
            annual_max = mean(daily_max),
            .groups = 'drop') %>% 
  group_by(region, climate_scenario, species) %>%
  summarise(Mean_Tb = mean(annual_mean),
            Min_Tb = mean(annual_min),
            Max_Tb = mean(annual_max),
            .groups = 'drop') %>% 
  rename(Region = region, Climate_scenario= climate_scenario,
         Species = species)

# final terra dataframe
terra_Tb_overall_spp <- terra_Tb_overall_spp_sum %>%
  pivot_longer(cols = c(Mean_Tb, Min_Tb, Max_Tb),
               names_to = "Variable",
               values_to = "Mean_value") %>%
  mutate(Variable = case_when(Variable == "Min_Tb"  ~ "Min Tb", 
                              Variable == "Mean_Tb" ~ "Mean Tb",
                              Variable == "Max_Tb"  ~ "Max Tb",
                              TRUE ~ Variable))

########
# Overall Tb by spp - final df
Tb_spp_overall_yr <- bind_rows(terra_Tb_overall_spp, contemp_Tb_overall_spp) %>% 
  arrange(Region, Species,
          match(Variable, c("Min Tb", "Mean Tb", "Max Tb")),
          match(Climate_scenario, c("contemporary", "future_2", "future_4"))) %>% 
  rename(species = Species)
Tb_overall_yr_contemp <- Tb_spp_overall_yr %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Variable, Mean_value, species) %>%
  rename(Contemporary_Mean = Mean_value)

# Create a unique combination of Region, Variable, and Season
unique_combinations <- unique(Tb_spp_overall_yr[ , c("Region", "species", "Variable")])

# Initialize an empty dataframe to store results
Tb_spp_overall_yr_final <- data.frame()
for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  # Filter the original and contemporary dataframes for the current combination
  filtered_data <- Tb_spp_overall_yr %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           species == current_combination$species)
  
  filtered_contemporary <- Tb_overall_yr_contemp %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           species == current_combination$species)
  
  # Left join the filtered data
  joined_data <- left_join(filtered_data, filtered_contemporary, by = c("Region", "Variable", "species"))
  
  # Apply the case_when mutations
  processed_data <- joined_data %>%
    mutate(New_Column = case_when(
      Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value < Contemporary_Mean ~ "--",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) <= 2 ~ "0",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value < Contemporary_Mean ~ "--",
      TRUE ~ NA_character_ )) 
  
  # Bind the processed data to the final dataframe
  Tb_spp_overall_yr_final <- rbind(Tb_spp_overall_yr_final, processed_data)
}
Tb_spp_overall_yr_final 




####################################
# TB --- by season and species
Contemp_Tb_season_spp <-function(data) {
  # Initialize an empty data frame to store overall statistics
  stats_df <- data.frame(Species = character(), Season = character(), Variable = character(), 
                         Mean_Value = numeric(), Correlation = numeric(), P_value = numeric())
  
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
      .groups = 'drop'
    )
  
  # Function to calculate and store overall statistics for each species and season
  calculate_overall_stats <- function(y_var, y_label) {
    unique_seasons <- unique(summary_tb$season)
    unique_species <- unique(summary_tb$species)
    
    for (seas in unique_seasons) {
      for (spec in unique_species) {
        subset_data <- subset(summary_tb, species == spec & season == seas)
        if (sum(!is.na(subset_data[[y_var]])) > 1) {
          overall_r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
          overall_p_value <- cor.test(subset_data$year, subset_data[[y_var]], use = "complete.obs")$p.value
          overall_mean <- mean(subset_data[[y_var]], na.rm = TRUE)
          
          # Add to stats data frame
          stats_df <<- rbind(stats_df, data.frame(Species = spec, season = seas, 
                                                  Variable = y_label, 
                                                  Mean_value = overall_mean,
                                                  Correlation = overall_r, 
                                                  P_value = overall_p_value))
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
aus_Tb_contemp_spp_season <- Contemp_Tb_season_spp(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") %>% 
  filter(season != "other")
kal_Tb_contemp_spp_season <- Contemp_Tb_season_spp(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")%>% 
  filter(season != "other")
contemp_tb_season_spp_season <- rbind(kal_Tb_contemp_spp_season, aus_Tb_contemp_spp_season)%>%
  mutate(species = Species, across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, species, season, Climate_scenario, Variable, Mean_value, 
         Correlation, P_value) 

############
### Terra by season and species
terra_tb_spp_season <- terra  %>% 
  group_by(region, climate_scenario, season, species, site, date) %>%
  summarise(daily_mean = mean(TC, na.rm = TRUE),
            daily_min = min(TC, na.rm = TRUE),
            daily_max = max(TC, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(region, climate_scenario, season, species) %>%
  summarise(Mean_tb = mean(daily_mean, na.rm = TRUE),
            Min_tb = mean(daily_min, na.rm = TRUE),
            Max_tb = mean(daily_max, na.rm = TRUE),
            .groups = 'drop') %>%
  rename(Region = region, Climate_scenario = climate_scenario) %>%
  pivot_longer(
    cols = c(Mean_tb, Min_tb, Max_tb),
    names_to = "Variable",
    values_to = "Mean_value") %>%
  mutate(Variable = case_when(
    Variable == "Min_tb"  ~ "Min Tb", 
    Variable == "Mean_tb" ~ "Mean Tb",
    Variable == "Max_tb"  ~ "Max Tb",
    TRUE ~ Variable)) %>% 
  filter(season != "other") 

#########
# Seasonal Tb - final df
Tb_spp_seasonal <- bind_rows(terra_tb_spp_season, contemp_tb_season_spp_season)
Tb_spp_seasonal$Variable <- factor(Tb_spp_seasonal$Variable, levels = c("Min Tb", "Mean Tb", "Max Tb"))
Tb_spp_seasonal <- Tb_spp_seasonal %>%
  arrange(Region, species, season, Variable, Climate_scenario)
Tb_seasonal_spp_contemp <- Tb_spp_seasonal %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Variable, Mean_value, season, species) %>%
  rename(Contemporary_Mean = Mean_value)

# Create a unique combination of Region, Variable, and Season
unique_combinations <- unique(Tb_spp_seasonal[ , c("Region", "Variable", "season", "species")])
# Initialize an empty dataframe to store results
Tb_spp_seasonal_final <- data.frame()
for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  # Filter the original and contemporary dataframes for the current combination
  filtered_data <- Tb_spp_seasonal %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           season == current_combination$season,
           species == current_combination$species)
  
  filtered_contemporary <- Tb_seasonal_spp_contemp %>%
    filter(Region == current_combination$Region, 
           Variable == current_combination$Variable,
           season == current_combination$season,
           species == current_combination$species)
  
  # Left join the filtered data
  joined_data <- left_join(filtered_data, filtered_contemporary, by = c("Region", "Variable","season", "species"))
  
  # Apply the case_when mutations
  processed_data <- joined_data %>%
    mutate(New_Column = case_when(
      Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) <= 1 ~ "0",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 1 & abs(Mean_value - Contemporary_Mean) <= 2 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_2" & abs(Mean_value - Contemporary_Mean) > 2 & Mean_value < Contemporary_Mean ~ "--",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) <= 2 ~ "0",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value > Contemporary_Mean ~ "+",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 2 & abs(Mean_value - Contemporary_Mean) <= 4 & Mean_value < Contemporary_Mean ~ "-",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value > Contemporary_Mean ~ "++",
      Climate_scenario == "future_4" & abs(Mean_value - Contemporary_Mean) > 4 & Mean_value < Contemporary_Mean ~ "--",
      TRUE ~ NA_character_ )) 
  
  # Bind the processed data to the final dataframe
  Tb_spp_seasonal_final <- rbind(Tb_spp_seasonal_final, processed_data)
}
Tb_spp_seasonal_final




################################################
### ACT--- Contemporary sum annual activity
contemp_sum_annual_activity <- function(data) {
  # Create a season column and adding activity vs inactive column
  ectotherm <- data %>% 
    group_by(species) %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(0, 1) ~ 0, # includes basking and inactive
      ACT == 2 ~ 1, # foraging hr
      TRUE ~ as.numeric(ACT)))# foraging

  # Calculating annual activity costs
  Annual_activity <- ectotherm %>%
    group_by(species, year) %>%
    summarise(Sum_activity_yr = sum(ACT_combined)) 
  
  # Get a list of all unique species in the data
  unique_species <- unique(ectotherm$species)
  unique_region <- unique(ectotherm$region)
  # Initialize results dataframe
  results <- data.frame(Region = character(), Species = character(), 
                        Annual_Activity_hr = numeric(),
                        Correlation = numeric(), P_value = numeric())
  
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
    # Save the results
    results <- rbind(results, data.frame(Region = unique_region, Species = sp, 
                                         Annual_Activity_hr = Annual_Activity, 
                                         Correlation = cor_test$estimate, 
                                         P_value = cor_test$p.value))
  }
  # Calculate overall linear relationship and mean activity for all species combined
  overall_model <- lm(Sum_activity_yr ~ year, data = Annual_activity)
  overall_cor_test <- cor.test(~ Sum_activity_yr + year, data = Annual_activity)
  overall_mean_activity <- mean(Annual_activity$Sum_activity_yr)
  results <- rbind(results, data.frame(Region = unique_region, Species = "Overall", 
                                       Annual_Activity_hr = overall_mean_activity, 
                                       Correlation = overall_cor_test$estimate, 
                                       P_value = overall_cor_test$p.value)) %>% 
    remove_rownames()
  
  return(results)
}
# data
Aus_ACT_contemp_yr_spp <- contemp_sum_annual_activity(data = aus_contemp) %>%
  mutate(Climate_scenario = "contemporary") 
Kal_ACT_contemp_yr_spp <- contemp_sum_annual_activity(data = kal_contemp) %>% 
  mutate(Climate_scenario = "contemporary", Region = "Africa")
contemp_ACT_contemp_yr_spp <- rbind(Aus_ACT_contemp_yr_spp, Kal_ACT_contemp_yr_spp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Climate_scenario, 
         Annual_Activity_hr, Correlation, P_value)

################################################
### Terraclimate sum annual activity
terra_ACT_spp<- terra  %>%
  group_by(species) %>%
  mutate(ACT_combined = case_when(
    ACT %in% c(0, 1) ~ 0, # includes basking and inactive
    ACT == 2 ~ 1, # foraging hr
    TRUE ~ as.numeric(ACT)))# foraging

# Calculating annual activity 
Annual_activity_terra <- terra_ACT_spp %>%
  group_by(region, climate_scenario, species, year) %>%
  summarise(Sum_activity_yr = sum(ACT_combined)) 

# mean sum across years
Sum_ACT_terra_yr_overall <- Annual_activity_terra %>% 
  group_by(region, climate_scenario) %>%
  summarise(Annual_Activity_hr = mean(Sum_activity_yr, na.rm = TRUE)) %>%
  mutate(species = "Overall") %>% 
  rename(Region = region, Climate_scenario = climate_scenario, Species = species) 

# by sepcies
Sum_ACT_terra_yr_spp <- Annual_activity_terra %>% 
  group_by(region, climate_scenario, species) %>%
  summarise(Annual_Activity_hr = mean(Sum_activity_yr, na.rm = TRUE)) %>%
  rename(Region = region, Climate_scenario = climate_scenario, Species = species) %>% 
  rbind(Sum_ACT_terra_yr_overall)


######
# Final data frame
Activity_yr_spp <- bind_rows(contemp_ACT_contemp_yr_spp, Sum_ACT_terra_yr_spp)
Activity_yr_spp <- Activity_yr_spp %>%
  arrange(Region, Species) %>% 
  rename(species = Species)

ACT_overall_yr_contemp <- Activity_yr_spp %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Annual_Activity_hr, species) %>%
  rename(Contemporary_Mean = Annual_Activity_hr)

# Create a unique combination of Region,  and Season
unique_combinations <- unique(Activity_yr_spp[ , c("Region", "species")])

# Initialize an empty dataframe to store results
ACT_spp_overall_yr_final <- data.frame()
for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  # Filter the original and contemporary dataframes for the current combination
  filtered_data <- Activity_yr_spp %>%
    filter(Region == current_combination$Region,
           species == current_combination$species)
  
  filtered_contemporary <- ACT_overall_yr_contemp %>%
    filter(Region == current_combination$Region, 
           species == current_combination$species)
  
  # Left join the filtered data
  joined_data <- left_join(filtered_data, filtered_contemporary, by = c("Region", "species"))
  
  # Apply the case_when mutations
  processed_data <- joined_data %>%
    mutate(New_Column = case_when(
      Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
      Climate_scenario == "future_2" & abs(Annual_Activity_hr - Contemporary_Mean) <= 109.5 ~ "0", # 30 mins a day of activity
      Climate_scenario == "future_2" & abs(Annual_Activity_hr - Contemporary_Mean) > 109.5 & abs(Annual_Activity_hr - Contemporary_Mean) <= 365 & Annual_Activity_hr > Contemporary_Mean ~ "+",
      Climate_scenario == "future_2" & abs(Annual_Activity_hr - Contemporary_Mean) > 109.5 & abs(Annual_Activity_hr - Contemporary_Mean) <= 365 & Annual_Activity_hr < Contemporary_Mean ~ "-",
      Climate_scenario == "future_2" & abs(Annual_Activity_hr - Contemporary_Mean) > 365 & Annual_Activity_hr > Contemporary_Mean ~ "++",
      Climate_scenario == "future_2" & abs(Annual_Activity_hr - Contemporary_Mean) > 365 & Annual_Activity_hr < Contemporary_Mean ~ "--",
      Climate_scenario == "future_4" & abs(Annual_Activity_hr - Contemporary_Mean) <= 365 ~ "0",
      Climate_scenario == "future_4" & abs(Annual_Activity_hr - Contemporary_Mean) > 365 & abs(Annual_Activity_hr - Contemporary_Mean) <= 730 & Annual_Activity_hr > Contemporary_Mean ~ "+",
      Climate_scenario == "future_4" & abs(Annual_Activity_hr - Contemporary_Mean) > 365 & abs(Annual_Activity_hr - Contemporary_Mean) <= 730 & Annual_Activity_hr < Contemporary_Mean ~ "-",
      Climate_scenario == "future_4" & abs(Annual_Activity_hr - Contemporary_Mean) > 730 & Annual_Activity_hr > Contemporary_Mean ~ "++",
      Climate_scenario == "future_4" & abs(Annual_Activity_hr - Contemporary_Mean) > 730 & Annual_Activity_hr < Contemporary_Mean ~ "--",
      TRUE ~ NA_character_ )) 
  
  # Bind the processed data to the final dataframe
  ACT_spp_overall_yr_final <- rbind(ACT_spp_overall_yr_final, processed_data)
}
ACT_spp_overall_yr_final 


################################################
### Contemporary sum seasonal activity
contemp_sum_seasonal_activity <- function(data) {
  # Adding activity vs inactive column
  ectotherm <- data %>% 
    filter(season != "other") %>% 
    group_by(species, season) %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(0, 1) ~ 0, # includes basking and inactive
      ACT == 2 ~ 1, # active foraging hour
      TRUE ~ as.numeric(ACT)))
  
  # Calculating seasonal activity costs
  Seasonal_activity <- ectotherm %>%
    group_by(species, year, season) %>%
    summarise(Sum_activity_season = sum(ACT_combined)) 
  
  # Get a list of all unique species and seasons in the data
  unique_species <- unique(ectotherm$species)
  unique_seasons <- unique(ectotherm$season)
  unique_region <- unique(ectotherm$region)
  
  # Initialize results dataframe
  results <- data.frame(Region = character(), Species = character(), 
                        Season = character(), 
                        Seasonal_Activity_hr = numeric(),
                        Correlation = numeric(), P_value = numeric())
  
  # Calculate linear relationship and mean activity for each species and season
  for (sp in unique_species) {
    for (se in unique_seasons) {
      # Subset the data for each species and season
      data_sp_se <- Seasonal_activity %>% filter(species == sp, season == se)
      # Fit a linear model
      model <- lm(Sum_activity_season ~ year, data = data_sp_se)
      # Get correlation and p-value
      cor_test <- cor.test(~ Sum_activity_season + year, data = data_sp_se)
      # Calculate mean activity
      Seasonal_Activity <- mean(data_sp_se$Sum_activity_season)
      # Save the results
      results <- rbind(results, data.frame(Region = unique_region, Species = sp, Season = se, 
                                           Seasonal_Activity_hr = Seasonal_Activity, 
                                           Correlation = cor_test$estimate, 
                                           P_value = cor_test$p.value)) %>% 
        remove_rownames()
    }
  }
  # Calculate overall relationship for summer and winter across all species
  for (se in unique_seasons) {
    overall_data <- Seasonal_activity %>% filter(season == se)
    overall_model <- lm(Sum_activity_season ~ year, data = overall_data)
    overall_cor_test <- cor.test(~ Sum_activity_season + year, data = overall_data)
    overall_mean_activity <- mean(overall_data$Sum_activity_season)
    results <- rbind(results, data.frame(Region = unique_region,
                                         Species = "Overall", Season = se, 
                                         Seasonal_Activity_hr = overall_mean_activity, 
                                         Correlation = overall_cor_test$estimate, 
                                         P_value = overall_cor_test$p.value))
  }
  
  return(results)
}
# data
Aus_ACT_contemp_season_spp <- contemp_sum_seasonal_activity(data = aus_contemp) %>%
  mutate(Climate_scenario = "contemporary") 
Kal_ACT_contemp_season_spp <- contemp_sum_seasonal_activity(data = kal_contemp) %>% 
  mutate(Climate_scenario = "contemporary", Region = "Africa")
contemp_ACT_contemp_season_spp <- rbind(Aus_ACT_contemp_season_spp, Kal_ACT_contemp_season_spp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Season, Climate_scenario, 
         Seasonal_Activity_hr, Correlation, P_value)

################################################
### TerraClimate sum seasonal activity
terra_ACT_spp_season <- terra  %>%
  group_by(species) %>%
  filter(season != "other") %>% 
  mutate(ACT_combined = case_when(
    ACT %in% c(0, 1) ~ 0, # includes basking and inactive
    ACT == 2 ~ 1, # foraging hr
    TRUE ~ as.numeric(ACT)))# foraging

# Calculating seasonal activity by year
Seasonal_activity_terra <- terra_ACT_spp_season %>%
  group_by(region, climate_scenario, season, species, year) %>%
  summarise(Sum_activity_yr = sum(ACT_combined)) 

# mean sum across season and years
Sum_ACT_terra_season_spp <- Seasonal_activity_terra %>% 
  group_by(region, climate_scenario, season, species) %>%
  summarise(Seasonal_Activity_hr = mean(Sum_activity_yr, na.rm = TRUE)) %>%
  rename(Region = region, Climate_scenario = climate_scenario, Species = species, Season = season) 

######
# Final data frame
Activity_season_spp <- bind_rows(contemp_ACT_contemp_season_spp, Sum_ACT_terra_season_spp)
Activity_season_spp <- Activity_season_spp %>%
  arrange(Region, Species, Season) %>% 
  rename(species = Species, season = Season)
# contemporary means df for change
Activity_season_spp_contemp <- Activity_season_spp %>%
  filter(Climate_scenario == "contemporary") %>%
  select(Region, Seasonal_Activity_hr,  species, season) %>%
  rename(Contemporary_Mean = Seasonal_Activity_hr)

# Create a unique combination of Region,  and Season
unique_combinations <- unique(Activity_season_spp[ , c("Region", "species", "season")])
# Initialize an empty dataframe to store results
ACT_spp_season_final <- data.frame()
for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  # Filter the original and contemporary dataframes for the current combination
  filtered_data <- Activity_season_spp %>%
    filter(Region == current_combination$Region,
           species == current_combination$species, 
           season == current_combination$season)
  # mean difference
  filtered_contemporary <- Activity_season_spp_contemp %>%
    filter(Region == current_combination$Region, 
           species == current_combination$species,
           season == current_combination$season)
  # Left join the filtered data
  joined_data <- left_join(filtered_data, filtered_contemporary, by = c("Region", "species", "season"))
  # Apply the case_when mutations
  processed_data <- joined_data %>%
    mutate(New_Column = case_when(
      Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
      Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
      Climate_scenario == "future_2" & abs(Seasonal_Activity_hr - Contemporary_Mean) <= 22.5 ~ "0", # 15 mins a day of activity
      Climate_scenario == "future_2" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 22.5 & abs(Seasonal_Activity_hr - Contemporary_Mean) <= 45 & Seasonal_Activity_hr > Contemporary_Mean ~ "+", 
      Climate_scenario == "future_2" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 22.5 & abs(Seasonal_Activity_hr - Contemporary_Mean) <= 45 & Seasonal_Activity_hr < Contemporary_Mean ~ "-",
      Climate_scenario == "future_2" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 45 & Seasonal_Activity_hr > Contemporary_Mean ~ "++",
      Climate_scenario == "future_2" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 45 & Seasonal_Activity_hr < Contemporary_Mean ~ "--",
      Climate_scenario == "future_4" & abs(Seasonal_Activity_hr - Contemporary_Mean) <= 45 ~ "0",
      Climate_scenario == "future_4" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 45 & abs(Seasonal_Activity_hr - Contemporary_Mean) <= 90 & Seasonal_Activity_hr > Contemporary_Mean ~ "+",
      Climate_scenario == "future_4" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 45 & abs(Seasonal_Activity_hr - Contemporary_Mean) <= 90 & Seasonal_Activity_hr < Contemporary_Mean ~ "-",
      Climate_scenario == "future_4" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 90 & Seasonal_Activity_hr > Contemporary_Mean ~ "++",
      Climate_scenario == "future_4" & abs(Seasonal_Activity_hr - Contemporary_Mean) > 90 & Seasonal_Activity_hr < Contemporary_Mean ~ "--",
      TRUE ~ NA_character_ )) 
  # Bind the processed data to the final dataframe
  ACT_spp_season_final <- rbind(ACT_spp_season_final, processed_data)
}
ACT_spp_season_final 



##################################################
# contemporary metabolic rate: overall and species
contemp_sum_annual_m_rate_kJ <- function(data) {
  # Create a season column and adding activity vs inactive column
  ectotherm <- data %>% 
    group_by(species) %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(0, 1) ~ "inactive", # includes basking and inactive
      ACT == 2 ~ "active", # foraging
      TRUE ~ as.character(ACT)),
      m_rate_J = O2_ml*20.08)
  
  # Calculating annual maintenance metabolic costs
  Annual_maintenance <- ectotherm %>% 
    group_by(species, year) %>% 
    summarise(Sum_mrate_J = sum(m_rate_J), # summing annual metabolic rate
              Mean_mrate_J = mean(m_rate_J)) %>% # calculating mean annual metabolic rate
    mutate(m_rate_kJ = Sum_mrate_J*0.001) # converting metabolism J to kJ
  
  # Get a list of all unique species in the data
  unique_species <- unique(ectotherm$species)
  # Initialize results dataframe
  results <- data.frame(Species = character(), Annual_MR_kJ = numeric(),
                        Correlation = numeric(), P_value = numeric())
  
  # Calculate linear relationship and mean metabolic rate for each species
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- Annual_maintenance %>% filter(species == sp)
    # Fit a linear model
    model <- lm(m_rate_kJ ~ year, data = data_sp)
    # Get correlation and p-value
    cor_test <- cor.test(~ m_rate_kJ + year, data = data_sp)
    # Calculate mean metabolic rate
    Annual_MR <- mean(data_sp$Mean_mrate_J)
    # Save the results
    results <- rbind(results, data.frame(Species = sp, Annual_MR_kJ = Annual_MR, 
                                         Correlation = cor_test$estimate, 
                                         P_value = cor_test$p.value))
  }
  # Calculate overall linear relationship and mean metabolic rate for all species combined
  overall_model <- lm(m_rate_kJ ~ year, data = Annual_maintenance)
  overall_cor_test <- cor.test(~ m_rate_kJ + year, data = Annual_maintenance)
  overall_mean_metabolic_rate <- mean(Annual_maintenance$Mean_mrate_J)
  results <- rbind(results, data.frame(Species = "Overall", Annual_MR_kJ = overall_mean_metabolic_rate, 
                                       Correlation = overall_cor_test$estimate, 
                                       P_value = overall_cor_test$p.value)) %>% 
    remove_rownames()
  
  return(results)
}
# data
Aus_MR_Sum_contemp_overall_spp <- contemp_sum_annual_m_rate_kJ(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") 
Kal_MR_Sum_contemp_overall_spp <- contemp_sum_annual_m_rate_kJ(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")
contemp_MR_Sum_overall_spp <- rbind(Aus_MR_Sum_contemp_overall_spp, Kal_MR_Sum_contemp_overall_spp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Climate_scenario, 
         Annual_MR_kJ, Correlation, P_value)
# final coded df
contemp_MR_Sum_overall_spp_final <- contemp_MR_Sum_overall_spp %>%
  mutate(New_Column = case_when(
    Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
    TRUE ~ NA_character_ ))
contemp_MR_Sum_overall_spp_final


##########################
# Contemporary MR by season and species
seasonal_annual_m_rate_kJ <- function(data) {
  
  # Create a season column and adding activity vs inactive column
  ectotherm <- data %>% 
    group_by(species) %>% 
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
  unique_region <- unique(ectotherm$region)
  results <- data.frame(Region = character(), Species = character(), Season = character(), 
                        Seasonal_MR_kJ = numeric(),Correlation = numeric(), P_value = numeric())
  
  # Calculate linear relationship and mean metabolic rate for each species
  for (sp in unique_species) {
    # Subset the data for each species
    data_sp <- Annual_maintenance %>% filter(species == sp)
    for (se in c("summer", "winter")) {
      # Subset the data for each season
      season_data <- data_sp %>% filter(season == se)
      # Fit a linear model
      model <- lm(m_rate_kJ ~ year, data = season_data)
      # Get correlation and p-value
      cor_test <- cor.test(~ m_rate_kJ + year, data = season_data)
      # Calculate mean metabolic rate for the season
      mean_mr_kJ <- mean(season_data$m_rate_kJ, na.rm = TRUE)
      # Save the results
      results <- rbind(results, data.frame(Species = sp, Season = se, Seasonal_MR_kJ = mean_mr_kJ,
                                           Correlation = cor_test$estimate, Region = unique_region, 
                                           P_value = cor_test$p.value))
    }
  }
  
  # Calculate overall linear relationship and mean metabolic rate for all species combined by season
  for (se in c("summer", "winter")) {
    season_data_all_species <- Annual_maintenance %>% filter(season == se)
    overall_model <- lm(m_rate_kJ ~ year, data = season_data_all_species)
    overall_cor_test <- cor.test(~ m_rate_kJ + year, data = season_data_all_species)
    overall_mean_mr_kJ <- mean(season_data_all_species$m_rate_kJ, na.rm = TRUE)
    
    results <- rbind(results, data.frame(Species = "Overall", Season = se, Seasonal_MR_kJ = overall_mean_mr_kJ, 
                                         Correlation = overall_cor_test$estimate, Region = unique_region, 
                                         P_value = overall_cor_test$p.value)) %>% 
      remove_rownames()
  }
  
  return(results)
}
# data
Aus_MR_Sum_contemp_seasonal_spp <- seasonal_annual_m_rate_kJ(data = aus_contemp) %>%
  mutate(Climate_scenario = "contemporary") 
Kal_MR_Sum_contemp_seasonal_spp <- seasonal_annual_m_rate_kJ(data = kal_contemp) %>% 
  mutate(Climate_scenario = "contemporary")
contemp_MR_Sum_seasonal_spp <- rbind(Aus_MR_Sum_contemp_seasonal_spp, Kal_MR_Sum_contemp_seasonal_spp)%>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Season, Species, Climate_scenario, 
         Seasonal_MR_kJ, Correlation, P_value)
# final coded df
contemp_MR_Sum_season_spp_final <- contemp_MR_Sum_seasonal_spp %>%
  group_by(Season, Species) %>%
  mutate(New_Column = case_when(
    Climate_scenario == "contemporary" & P_value > 0.05 ~ "0",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation > 0 ~ "+",
    Climate_scenario == "contemporary" & P_value <= 0.05 & Correlation < 0 ~ "-",
    TRUE ~ NA_character_ )) %>% 
  ungroup()
contemp_MR_Sum_season_spp_final




# Ta and Tb: year (all pulled from sites or species) and seasonal
# future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)
# future +4: within 2= 0; between 2-4 (+/-); >4 (++/--)
Ta_overall_yr_final_simp <- Ta_overall_yr_final %>% select(Region, Climate_scenario, Variable, New_Column)
Ta_seasonal_final_simp <- Ta_seasonal_final %>% select(Region, season, Climate_scenario, Variable, New_Column)
Tb_overall_yr_final_simp <- Tb_overall_yr_final %>% select(Region, Climate_scenario, Variable, New_Column)
Tb_seasonal_final_simp <- Tb_seasonal_final %>% select(Region, season, Climate_scenario, Variable, New_Column)

# Tb by species year and Tb by species X season
# future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)
# future +4: within 2= 0; between 2-4 (+/-); >4 (++/--)
Tb_spp_overall_yr_final_simp <- Tb_spp_overall_yr_final %>% select(Region, species, Climate_scenario, Variable, New_Column)
Tb_spp_seasonal_final_simp <- Tb_spp_seasonal_final %>% select(Region, species, season, Climate_scenario, Variable, New_Column)

# Sum annual activity year by species
# future +2: within 30min/d= 0; between 30min-1hr/d (+/-); >1hr/d (++/--)
# future +4: within 1hr/d = 0; between 1-2hr/d (+/-); >2/hr/d (++/--)
ACT_spp_overall_yr_final_simp <- ACT_spp_overall_yr_final%>% select(Region, species, Climate_scenario, New_Column)
ACT_spp_season_final_simp <- ACT_spp_season_final %>% select(Region, species, season, Climate_scenario, New_Column)

# Metabolic rate: all and by species - missing terraclimate data
contemp_MR_Sum_overall_spp_final_simp <- contemp_MR_Sum_overall_spp_final %>% select(Region, Species, Climate_scenario, New_Column)

# Metabolic rate: season overall and season by species - missing terraclimate data
contemp_MR_Sum_season_spp_final_simp <- contemp_MR_Sum_season_spp_final %>% select(Region, Species, Season, Climate_scenario, New_Column)





########################################################################
######################## Finished data tables ##########################
########################################################################
# Ta and Tb: year (all pulled from sites or species) and seasonal
# future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)
# future +4: within 2= 0; between 2-4 (+/-); >4 (++/--)
Ta_overall_yr_final_simp
Ta_seasonal_final_simp
Tb_overall_yr_final_simp
Tb_seasonal_final_simp

# Tb by species year and Tb by species X season
# future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)
# future +4: within 2= 0; between 2-4 (+/-); >4 (++/--)
Tb_spp_overall_yr_final_simp
Tb_spp_seasonal_final_simp

# Sum annual activity year by species
# future +2: within 30min/d= 0; between 30min-1hr/d (+/-); >1hr/d (++/--)
# future +4: within 1hr/d = 0; between 1-2hr/d (+/-); >2/hr/d (++/--)
ACT_spp_overall_yr_final_simp
ACT_spp_season_final_simp

# Metabolic rate: all and by species - missing terraclimate data
contemp_MR_Sum_overall_spp_final_simp

# Metabolic rate: season overall and season by species - missing terraclimate data
contemp_MR_Sum_season_spp_final_simp












#######################################################################################
# all data table with raw values/stats
# Ta and Tb: year (all pulled from sites or species) and seasonal
# future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)
# future +4: within 2= 0; between 2-4 (+/-); >4 (++/--)
Ta_overall_yr_final
Ta_seasonal_final
Tb_overall_yr_final
Tb_seasonal_final

# Tb by species year and Tb by species X season
# future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)
# future +4: within 2= 0; between 2-4 (+/-); >4 (++/--)
Tb_spp_overall_yr_final
Tb_spp_seasonal_final

# Sum annual activity year by species
# future +2: within 30min/d= 0; between 30min-1hr/d (+/-); >1hr/d (++/--)
# future +4: within 1hr/d = 0; between 1-2hr/d (+/-); >2/hr/d (++/--)
ACT_spp_overall_yr_final
ACT_spp_season_final

# Metabolic rate: all and by species - missing terraclimate data
contemp_MR_Sum_overall_spp_final

# Metabolic rate: season overall and season by species - missing terraclimate data
contemp_MR_Sum_season_spp_final











########################################################################################
# wide conversions??
# Ta and Tb: year (all pulled from sites or species) and seasonal
Ta_overall_yr_final_wide <- Ta_overall_yr_final %>% 
  select(New_Column, Region, Variable, Climate_scenario) %>% 
  pivot_wider(names_from = Climate_scenario, 
    values_from = c(New_Column))
Ta_seasonal_final_wide <-  Ta_seasonal_final %>% 
  select(New_Column, Region, Variable, Climate_scenario, season) %>% 
  pivot_wider(
    names_from = c(season, Climate_scenario),
    values_from = New_Column,
    names_sep = "_") %>%
  select(Region, Variable, starts_with("summer"), starts_with("winter"))
Tb_overall_yr_final_wide <- Tb_overall_yr_final %>% 
  select(New_Column, Region, Variable, Climate_scenario) %>% 
  pivot_wider(names_from = Climate_scenario, 
              values_from = c(New_Column))
Tb_seasonal_final_wide<- Tb_seasonal_final %>% 
  select(New_Column, Region, Variable, Climate_scenario, season) %>% 
  pivot_wider(
    names_from = c(season, Climate_scenario),
    values_from = New_Column,
    names_sep = "_") %>%
  select(Region, Variable, starts_with("summer"), starts_with("winter"))

# Tb by species year and Tb by species X season
Tb_spp_overall_yr_final_wide <- Tb_spp_overall_yr_final %>% 
  select(New_Column, Region, Variable, Climate_scenario, species)
Tb_spp_seasonal_final_wide <-  Tb_spp_seasonal_final %>% 
  select(New_Column, Region, Variable, Climate_scenario, species, season) %>% 
  mutate(Scenario_Season = paste(Climate_scenario, season, sep = "_")) %>% 
  pivot_wider(
    names_from = Scenario_Season,
    values_from = New_Column
  ) %>%
  select(Region, Variable, species, ends_with("summer"), ends_with("winter"))


# Sum annual activity year by species
ACT_spp_overall_yr_final
ACT_spp_season_final



# Metabolic rate: all and by species - missing terraclimate data
contemp_MR_Sum_overall_spp_final

# Metabolic rate: season overall and season by species - missing terraclimate data
contemp_MR_Sum_season_spp_final


########################################################################
######################## Wide data tables ##########################
########################################################################
view(Ta_overall_yr_final_wide) # future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)
view(Ta_seasonal_final_wide) # future +4: within 2= 0; between 2-4 (+/-); >4 (++/--)
view(Tb_overall_yr_final_wide) # future +2: within 1= 0; between 1-2 (+/-); >2 (++/--)


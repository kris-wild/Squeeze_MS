pacman::p_load(maps, dplyr, tidyr, tidyverse, flextable, knitr, here, ggplot2, purrr, rnaturalearth, rnaturalearthdata, grid, cowplot, simplecolors, ggnewscale, gridExtra, grid)

############################################################################################################
#### Load in contemporary data for each region
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

# Australian species
ctis_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Ctenophorus_isolepis_L_environ.RDS")%>% 
  mutate(Ww_g = 5) # mean mass from field data
ctqu_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Ctenotus_quatt_L_environ.RDS")%>% 
  mutate(Ww_g = 4)# mean mass from field data
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
# final data - Africa
contemp_ta_tb_africa <- rbind(contemp_Ta_overall, contemp_Tb_overall_spp)  %>% filter(Region == "Africa")
contemp_ta_tb_africa <- contemp_ta_tb_africa %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max"
  )) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
saveRDS(contemp_ta_tb_africa, "Raw_data/contemp_ta_tb_africa.rds")

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
saveRDS(contemp_ta_tb_australia, "Raw_data/contemp_ta_tb_australia.rds")


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by season and species  
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   


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
# data
aus_ta_season_contemp <- contemp_ta_season(data = aus_contemp) %>% 
  filter(season != "other") %>% 
  mutate(Region = "Australia",
         Species = paste0("Site_AU", Species))
kal_ta_season__contemp <- contemp_ta_season(data = kal_contemp) %>% 
  filter(season != "other") %>% 
  mutate(Region = "Africa",
         Species = paste0("Site_AF", Species))
contemp_ta_season_spp <- rbind(kal_ta_season__contemp, aus_ta_season_contemp) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species,Slope, R_2, season,Variable, Correlation, P_value)%>% 
  mutate(Slope_10_Year = Slope*10)



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
aus_Tb_contemp_spp_season <- Contemp_Tb_season_spp(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") %>% 
  filter(season != "other")
kal_Tb_contemp_spp_season <- Contemp_Tb_season_spp(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")%>% 
  filter(season != "other")
contemp_tb_season_spp <- rbind(kal_Tb_contemp_spp_season,
                               aus_Tb_contemp_spp_season)%>%
  mutate(species = Species, across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, season, Slope, R_2, Variable, Correlation, P_value) %>% 
  mutate(Slope_10_Year = Slope*10)


########
#### final df and plot prep by region
contemp_ta_tb_season_spp <- rbind(contemp_ta_season_spp, contemp_tb_season_spp)


############ SAVE AND READ IN DATA
contemp_ta_tb_season_spp <- saveRDS(contemp_ta_tb_season_spp, "Raw_data/contemp_ta_tb_season_spp.rds")
contemp_ta_tb_season_spp <- readRDS("Raw_data/contemp_ta_tb_season_spp.rds")

## final seasonal data - Africa
contemp_ta_tb_season_spp_africa <- contemp_ta_tb_season_spp %>% filter(Region == "Africa")
# set up data
contemp_ta_tb_season_spp_africa <- contemp_ta_tb_season_spp_africa %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max")) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
# Set up data - Convert 'Combined' and 'season' to factors with specified levels
contemp_ta_tb_season_spp_africa$Combined <- factor(contemp_ta_tb_season_spp_africa$Combined, levels = c("Min Ta", "Mean Ta", "Max Ta", "Min Tb", "Mean Tb",  "Max Tb"))
#levels = c("Min Ta", "Min Tb", "Mean Ta", "Mean Tb", "Max Ta", "Max Tb"))
contemp_ta_tb_season_spp_africa$Season <- factor(contemp_ta_tb_season_spp_africa$season, levels = c("winter", "summer"))
species_order <- c("C. angulifer", "A. aculeata", "M. suborbitalis", 
                   "P. lineoocellata", "T. sparsa", "Site_AFA", "Site_AFB", 
                   "Site_AFL", "Site_AFX")
contemp_ta_tb_season_spp_africa$Species <- factor(contemp_ta_tb_season_spp_africa$Species, 
                                                  levels = species_order)
# Determine significance
contemp_ta_tb_season_spp_africa$Significant <- contemp_ta_tb_season_spp_africa$P_value < 0.05


## final seasonal data - Australia
contemp_ta_tb_season_spp_australia <- contemp_ta_tb_season_spp  %>% filter(Region == "Australia")
## final seasonal data - Africa
contemp_ta_tb_season_spp_australia <- contemp_ta_tb_season_spp_australia %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max")) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
# Set up data - Convert 'Combined' and 'season' to factors with specified levels
contemp_ta_tb_season_spp_australia$Combined <- factor(contemp_ta_tb_season_spp_australia$Combined, levels = c("Min Ta", "Mean Ta", "Max Ta", "Min Tb", "Mean Tb",  "Max Tb"))
#levels = c("Min Ta", "Min Tb", "Mean Ta", "Mean Tb", "Max Ta", "Max Tb"))
# Species order
species_order <- c("G. variegata", "P. minor", "C. isolepis", "C. quatt", "M. horridus",
                   "Site_AUA", "Site_AUL", "Site_AUR")
contemp_ta_tb_season_spp_australia$Species <- factor(contemp_ta_tb_season_spp_australia$Species, 
                                                     levels = species_order)
# Season order
contemp_ta_tb_season_spp_australia$Season <- factor(contemp_ta_tb_season_spp_australia$season, 
                                                    levels = c("winter", "summer"))
# Determine significance
contemp_ta_tb_season_spp_australia$Significant <- contemp_ta_tb_season_spp_australia$P_value < 0.05


########
# filter season data for plots for each region
contemp_ta_tb_winter_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "winter")
contemp_ta_tb_summer_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "summer")
contemp_ta_tb_winter_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "winter")
contemp_ta_tb_summer_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "summer")




####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
### ACT--- Contemporary sum annual activity
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 

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
# save file
# saveRDS(foraging_success_contemp, file = "output/foraging_success/Africa_Australia_dat.RDS")

####### 
# Read in data that was saved above
foraging_success_contemp <- readRDS("output/foraging_success/Africa_Australia_dat.RDS")

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
saveRDS(ACT_MRate_Foraging_contemp, "Raw_data/ACT_MRate_Foraging_contemp.rds")






####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary sum seasonal activity 
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
####################   ####################   ####################   #################### 
############  ACT Seasonal function
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
# data
Aus_ACT_contemp_season_spp <- contemp_sum_seasonal_activity(data = aus_contemp) %>%
  mutate(Test = "ACT", Region = "Australia")
Kal_ACT_contemp_season_spp <- contemp_sum_seasonal_activity(data = kal_contemp) %>% 
  mutate(Test = "ACT", Region = "Africa")
contemp_ACT_contemp_season_spp <- rbind(Aus_ACT_contemp_season_spp, Kal_ACT_contemp_season_spp)%>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Season, Test, Slope_10_Year, R_2, P_value) 



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
# data
Aus_MR_Sum_contemp_seasonal_spp <- seasonal_annual_m_rate_J(data = aus_contemp) %>%
  mutate(Test = "M_Rate", Region = "Australia")
Kal_MR_Sum_contemp_seasonal_spp <- seasonal_annual_m_rate_J(data = kal_contemp) %>% 
  mutate(Test = "M_Rate", Region = "Africa")
contemp_MR_Sum_seasonal_spp <- rbind(Aus_MR_Sum_contemp_seasonal_spp, 
                                     Kal_MR_Sum_contemp_seasonal_spp) %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))%>% 
  select(Region, Species, Season, Test, Slope_10_Year, R_2, P_value) 




####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary feeding requirements by season function: 
# 1) metabolic demands per hr active
# 2) number of ants

####################
# data
africa_seasonal_foraging <- foraging_success_contemp %>% 
  filter(region == "Africa" & season!= "other")
australia_seasonal_foraging <- foraging_success_contemp %>% 
  filter(region == "Australia"& season!= "other")


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

####################
## Seasonal feeding summary
africa_seasonal_feeding_summary <- seasonal_feeding_demand(africa_seasonal_foraging) %>% 
  mutate(Region ="Africa")
australia_seasonal_feeding_summary <- seasonal_feeding_demand(australia_seasonal_foraging)%>% 
  mutate(Region = "Australia")
contemp_Foraging_seasonal_spp <- rbind(africa_seasonal_feeding_summary,
                                       australia_seasonal_feeding_summary) %>% 
  mutate(across(where(is.numeric), ~ round(., 4))) %>% 
  select(Region, Species, Season, R_2, Slope_10_Year, P_value) %>% 
  mutate(Test = "Feeding_demand")



##########
### Final data merge and data prep for plots
contemp_ACT_MRate_Foraging_season<- rbind(contemp_ACT_contemp_season_spp,
                                          contemp_MR_Sum_seasonal_spp,
                                          contemp_Foraging_seasonal_spp)
saveRDS(contemp_ACT_MRate_Foraging_season, "Raw_data/contemp_ACT_MRate_Foraging_season.rds")


######################################################################################## 
###################   #####   TERRA CLIMATE DATA  ################   ########      
######################################################################################## 
###### 
##########
## Load Climate scenario files for African species 
agac_clim<-readRDS(here("output/climate_scenarios/climate_scenarioKalahari_Agama_aculeata_X_environ.RDS"))%>% 
  mutate(Ww_g = 31.9) # mean mass from field data
chan_clim<-readRDS(here("output/climate_scenarios/climate_scenarioKalahari_Chondrodactylus_angulifer_A_environ.RDS"))%>% 
  mutate(Ww_g = 21.2) # mean mass from field data
mesu_clim<-readRDS(here("output/climate_scenarios/climate_scenarioKalahari_Meroles_suborbitalis_L_environ.RDS"))%>% 
  mutate(Ww_g = 5.5) # mean mass from field data
peli_clim<-readRDS(here("output/climate_scenarios/climate_scenarioKalahari_Pedioplanis_lineoocellata_A_environ.RDS"))%>% 
  mutate(Ww_g = 5.2) # mean mass from field data
trsp_clim<-readRDS(here("output/climate_scenarios/climate_scenarioKalahari_Trachylepis_sparsa_B_environ.RDS"))%>% 
  mutate(Ww_g = 21.3) # mean mass from field data
kal_clim <- rbind(agac_clim, chan_clim, trsp_clim, mesu_clim, peli_clim) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species),
         site = paste0("Site_AF", site),
         region = "Africa")%>% # adding site id by region
  dplyr::select(climate_scenario, region, site, date, year, month,
                hr, genus, species, Ww_g, TC, TAREF, O2_ml, ACT, DEP, SHADE) %>% 
  mutate(O2_ml_h = 0.013 * Ww_g^0.800 * 10^(0.038 * TC) * 10^(.14 * 0),# O2ml/h calculation
         m_rate_J = O2_ml_h*20.08, #O2_ml conversion - no adjustment to M rate and behaviour
         m_rate_J_adj = case_when(ACT == 1 ~ m_rate_J * 1.5, # basking metabolism adjustment
                                  ACT == 2 ~ m_rate_J * 4, # foraging metabolism adjustment
                                  TRUE ~ m_rate_J),
         ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, #  renaming activity for sum cal
                                  ACT == 2 ~ 1, # foraging 
                                  TRUE ~ as.numeric(ACT)),
         fortnight = floor((yday(date) - 1) / 14) + 1, # fortnight 
         fortnight_period = paste0("Fortnight_", year, "_", fortnight),# fortnight column
         month = month(date), # month column number
         month_ch = month(date, label=TRUE)) %>% 
  mutate(region = if_else(region == "Kalahari", "Africa", region))

####################
## Load Climate scenario for Australian species
pomi_clim<-readRDS(here("output/climate_scenarios/climate_scenarioAustralia_Pogona_minor_A_environ.RDS")) %>% 
  mutate(Ww_g = 36) # mean mass from field data
ctqu_clim<-readRDS(here("output/climate_scenarios/climate_scenarioAustralia_Ctenotus_quatt_L_environ.RDS")) %>% 
  mutate(Ww_g = 4) # mean mass from field data
moho_clim<-readRDS(here("output/climate_scenarios/climate_scenarioAustralia_Moloch_horridus_R_environ.RDS")) %>% 
  mutate(Ww_g = 39) # mean mass from field data
geva_clim<-readRDS(here("output/climate_scenarios/climate_scenarioAustralia_Gehyra_variegata_L_environ.RDS")) %>% 
  mutate(Ww_g = 4) # mean mass from field data
ctis_clim<-readRDS(here("output/climate_scenarios/climate_scenarioAustralia_Ctenophorus_isolepis_L_environ.RDS")) %>% 
  mutate(Ww_g = 5) # mean mass from field data
# combine species
aus_clim <- rbind(pomi_clim, ctqu_clim, moho_clim, geva_clim, ctis_clim) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species),
         site = paste0("Site_AU", site)) %>% # adding site id by region
  dplyr::select(climate_scenario, region, site, date, year, month, 
                hr, genus, species, Ww_g, TC, TAREF, O2_ml, ACT, DEP, SHADE) %>% 
  mutate(O2_ml_h = 0.013 * Ww_g^0.800 * 10^(0.038 * TC) * 10^(.14 * 0),# O2ml/h calculation
         m_rate_J = O2_ml_h*20.08, #O2_ml conversion - no adjustment to M rate and behaviour
         m_rate_J_adj = case_when(ACT == 1 ~ m_rate_J * 1.5, # basking metabolism adjustment
                                  ACT == 2 ~ m_rate_J * 4, # foraging metabolism adjustment
                                  TRUE ~ m_rate_J),
         ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, #  renaming activity for sum cal
                                  ACT == 2 ~ 1, # foraging 
                                  TRUE ~ as.numeric(ACT)),
         fortnight = floor((yday(date) - 1) / 14) + 1, # fortnight 
         fortnight_period = paste0("Fortnight_", year, "_", fortnight),# fortnight column
         month = month(date), # month column number
         month_ch = month(date, label=TRUE))


####################
## final dataframe adding season for later and changing climate_scenario name
terra_data <- rbind(kal_clim, aus_clim) %>% 
  mutate(climate_scenario = case_when(climate_scenario == "current" ~ "future_0", 
                                      TRUE ~ climate_scenario),
         season = case_when(month %in% c(6, 7, 8) ~ "winter",
                            month %in% c(12, 1, 2) ~ "summer",
                            month %in% c(11, 10, 9) ~ "spring", 
                            TRUE ~ "other")) 

####################
## adding foraging rates using get ant function 
source("Scripts/Functions/get_ants.R")
# apply the hourly ant estimate to data "contemp" that has both regions
terra_data <- terra_data %>%
  group_by(species) %>% 
  rowwise() %>% # apply function by row
  mutate(ants_consumed_hourly = get_ants_hourly(m_rate_J_h = m_rate_J_adj)) %>%
  ungroup()

# save terra data
#
saveRDS(terra_data, "output/foraging_success/TERRA_Results_all.RDS")
terra_data <- readRDS("output/foraging_success/TERRA_Results_all.RDS")



########################################
#### Data summary - Tb/Ta overall
# Ta summary : site
Ta_summary <- terra_data  %>% 
  group_by(region, climate_scenario,site, year, date) %>% # by date 
  summarise(daily_mean = mean(TAREF), 
            daily_min = min(TAREF), 
            daily_max = max(TAREF), 
            .groups = 'drop') %>%
  group_by(region, climate_scenario, year, site) %>% # by year
  summarise(annual_mean = mean(daily_mean),
            annual_min = mean(daily_min),
            annual_max = mean(daily_max),
            .groups = 'drop') %>% 
  group_by(region, climate_scenario, site) %>% # site
  summarise(annual_min = mean(annual_min),
            annual_mean = mean(annual_mean),
            annual_max = mean(annual_max),
            .groups = 'drop') %>% 
  mutate(Test = "Ta") %>% # add test for filtering for later
  rename(species = site) 

# Tb summary - Spp
Tb_summary <- terra_data %>% 
  group_by(region, climate_scenario,species, year, date) %>% # group by date
  summarise(daily_mean = mean(TC), 
            daily_min = min(TC), 
            daily_max = max(TC), 
            .groups = 'drop') %>%
  group_by(region, climate_scenario, year, species) %>%# group by year and spp
  summarise(annual_mean = mean(daily_mean),
            annual_min = mean(daily_min),
            annual_max = mean(daily_max),
            .groups = 'drop') %>% 
  group_by(region, climate_scenario, species) %>%# group by year - overall
  summarise(annual_min = mean(annual_min),
            annual_mean = mean(annual_mean),
            annual_max = mean(annual_max),
            .groups = 'drop') %>% 
  mutate(Test = "Tb") # add test for filtering for later

# final data and pivot for figures
data_summary_final_spp <- rbind(Ta_summary, Tb_summary)  %>% 
  pivot_longer(cols = c(annual_min, annual_mean, annual_max), # Columns to be reshaped
               names_to = "Variable", # Name of the new variable column
               values_to = "Value" ) %>% # Name of the new value column
  mutate(Variable = paste(Variable, Test),
         Variable = case_when(Variable == "annual_min Ta" ~ "Min_Ta",
                              Variable == "annual_mean Ta" ~ "Mean_Ta",
                              Variable == "annual_max Ta" ~ "Max_Ta",
                              Variable == "annual_min Tb" ~ "Min_Tb",
                              Variable == "annual_mean Tb" ~ "Mean_Tb",
                              Variable == "annual_max Tb" ~ "Max_Tb",
                              TRUE ~ climate_scenario))

data_summary_final_spp


####################
## Average of change from future_0 to _2 &_4
# data - pivot for calculation & Calculate Delta
Tb_Ta_change_spp_1 <- data_summary_final_spp %>% 
  pivot_wider(names_from = climate_scenario, values_from = Value) %>%
  mutate(future_2_delta = future_2 - future_0,
         future_4_delta = future_4 - future_0) %>% 
  dplyr::select(region, species, Variable, future_2_delta, future_4_delta) %>% 
  rename(future_2 = future_2_delta, future_4 = future_4_delta) # renaming for plotting
Tb_Ta_change_spp_final <- pivot_longer(Tb_Ta_change_spp_1, 
                                       cols = c(future_2, future_4), names_to = "climate_scenario", values_to = "value")
Tb_Ta_change_spp_final$Variable <- factor(Tb_Ta_change_spp_final$Variable,  # for grouping
                                          levels = c("Min_Ta", "Mean_Ta","Max_Ta",
                                                     "Min_Tb", "Mean_Tb", "Max_Tb"))



############ SAVE
saveRDS(Tb_Ta_change_spp_final, "Raw_data/Tb_Ta_change_spp_final.rds")


####################################################################################################
####################################################################################################
####################################################################################################
## Data summary - Tb/Ta seasonal
# Ta summary : site
Ta_summary_season <- terra_data  %>% 
  group_by(region, season, climate_scenario, site, year, date) %>% # by date 
  summarise(daily_mean = mean(TAREF), 
            daily_min = min(TAREF), 
            daily_max = max(TAREF), 
            .groups = 'drop') %>%
  group_by(region, season, climate_scenario, year, site) %>% # by year
  summarise(annual_mean = mean(daily_mean),
            annual_min = mean(daily_min),
            annual_max = mean(daily_max),
            .groups = 'drop') %>% 
  group_by(region, season, climate_scenario, site) %>% # site
  summarise(annual_min = mean(annual_min),
            annual_mean = mean(annual_mean),
            annual_max = mean(annual_max),
            .groups = 'drop') %>% 
  mutate(Test = "Ta") %>% # add test for filtering for later
  rename(species = site) 

# Tb summary - Spp
Tb_summary_season <- terra_data %>% 
  group_by(region, season, climate_scenario,species, year, date) %>% # group by date
  summarise(daily_mean = mean(TC), 
            daily_min = min(TC), 
            daily_max = max(TC), 
            .groups = 'drop') %>%
  group_by(region, season, climate_scenario, year, species) %>%# group by year and spp
  summarise(annual_mean = mean(daily_mean),
            annual_min = mean(daily_min),
            annual_max = mean(daily_max),
            .groups = 'drop') %>% 
  group_by(region, season, climate_scenario, species) %>%# group by year - overall
  summarise(annual_min = mean(annual_min),
            annual_mean = mean(annual_mean),
            annual_max = mean(annual_max),
            .groups = 'drop') %>% 
  mutate(Test = "Tb") # add test for filtering for later

# final data and pivot for figures
data_summary_final_spp_season <- rbind(Ta_summary_season, Tb_summary_season)  %>% 
  filter(season != "other") %>% 
  pivot_longer(cols = c(annual_min, annual_mean, annual_max), # Columns to be reshaped
               names_to = "Variable", # Name of the new variable column
               values_to = "Value" ) %>% # Name of the new value column
  mutate(Variable = paste(Variable, Test),
         Variable = case_when(Variable == "annual_min Ta" ~ "Min_Ta",
                              Variable == "annual_mean Ta" ~ "Mean_Ta",
                              Variable == "annual_max Ta" ~ "Max_Ta",
                              Variable == "annual_min Tb" ~ "Min_Tb",
                              Variable == "annual_mean Tb" ~ "Mean_Tb",
                              Variable == "annual_max Tb" ~ "Max_Tb",
                              TRUE ~ climate_scenario))


####################
## Average of change from future_0 to _2 &_4
# data - pivot for calculation & Calculate Delta
Tb_Ta_change_spp_season_1 <- data_summary_final_spp_season %>% 
  pivot_wider(names_from = climate_scenario, values_from = Value) %>%
  group_by(species, season) %>% #group by species and season
  mutate(future_2_delta = future_2 - future_0,
         future_4_delta = future_4 - future_0) %>% 
  dplyr::select(region, species, season, Variable, future_2_delta, future_4_delta) %>% 
  rename(future_2 = future_2_delta, future_4 = future_4_delta) # renaming for plotting
Tb_Ta_change_spp_season_final <- pivot_longer(Tb_Ta_change_spp_season_1, 
                                              cols = c(future_2, future_4), 
                                              names_to = "climate_scenario", values_to = "value")
Tb_Ta_change_spp_season_final$Variable <- factor(Tb_Ta_change_spp_season_final$Variable,  # for grouping
                                                 levels = c("Min_Ta", "Mean_Ta",
                                                            "Max_Ta","Min_Tb", 
                                                            "Mean_Tb", "Max_Tb"))


############ SAVE AND READ IN DATA
saveRDS(Tb_Ta_change_spp_season_final, "Raw_data/Tb_Ta_change_spp_season_final.rds")



################################################################
################################################################
################################################################
########### TERRA delta change: Activity  Metabolic rates and foraging
#### Data summary - Annual sum activity
Terra_ACT_yr_mean <- terra_data %>% 
  group_by(region, year, species, climate_scenario) %>% # grouping to get year sum by year
  summarise(Sum_activity_scenario = sum(ACT_combined)) %>% # summing annual metabolic rate
  ungroup() %>% 
  group_by(region, species, climate_scenario) %>% # grouping to mean annual sum to calculate differences between 
  summarise(Value = mean(Sum_activity_scenario)) %>% 
  mutate(Test = "ACT_hr")


# Data summary - year maintenance metabolic costs by climate scenario
Terra_MR_yr_mean <- terra_data %>% 
  mutate(m_rate_J_Wwg = m_rate_J_adj/Ww_g) %>% # metabolic rate (J/g)
  group_by(region, year, species, climate_scenario) %>% # grouping to get year sum by year
  summarise(Sum_mrate_J_scenario = sum(m_rate_J_Wwg)) %>% # summing annual metabolic rate
  ungroup() %>% 
  group_by(region, species, climate_scenario) %>% # grouping to mean annual sum to calculate differences between 
  summarise(Value = mean(Sum_mrate_J_scenario)) %>% 
  mutate(Test = "M_Rate_J")


# Data summary - year foraging rates by climate scenario ants_consumed_hourly
Terra_foraging_hr_mean <- terra_data %>% 
  mutate(m_rate_J_Wwg = m_rate_J_adj/Ww_g) %>% # metabolic rate (J/g)
  group_by(region, climate_scenario, species, year, month_ch) %>% 
  summarise(Sum_activity = sum(ACT_combined), 
            m_rate_J_g = sum(m_rate_J_Wwg)) %>% 
  ungroup() %>% 
  mutate(m_rate_J_g_ACT = m_rate_J_g/Sum_activity)  %>%
  filter(is.finite(m_rate_J_g_ACT)) %>%  # M. horrindus in July 1997 had 0 activity
  group_by(region, species, climate_scenario) %>% # grouping to mean annual sum to calculate differences between 
  summarise(Value = mean(m_rate_J_g_ACT)) %>% 
  mutate(Test = "Feeding_demand") 

################
# final df be fore pivot for delta calculation
Terra_ACT_MR_Forage_yr_mean <- rbind(Terra_ACT_yr_mean, 
                              Terra_MR_yr_mean, 
                              Terra_foraging_hr_mean)
# pivot for calculation
ACT_MR_Forage_change_yr_1<- Terra_ACT_MR_Forage_yr_mean %>% 
  pivot_wider(names_from = climate_scenario, values_from = Value) %>%
  group_by(species) %>% #group by species
  mutate(future_2_delta = future_2 - future_0,
         future_4_delta = future_4 - future_0) %>% 
  dplyr::select(region, species, Test, future_2_delta, future_4_delta) %>% 
  rename(future_2 = future_2_delta, future_4 = future_4_delta) # renaming for plotting
# final df - will piviot with analysis table
ACT_MR_Forage_change_yr_final <- pivot_longer(ACT_MR_Forage_change_yr_1, 
                                       cols = c(future_2, future_4), 
                                       names_to = "Climate_Scenario", 
                                       values_to = "Value") %>% 
  dplyr::rename(Region = region, Species = species)


############ SAVE AND READ IN DATA
saveRDS(ACT_MR_Forage_change_yr_final, "Raw_data/ACT_MR_Forage_change_yr_final.rds")




#################################################
#################################################
########### TERRA delta SEASONAL change: Activity and Metabolic rates 
# Data summary - SEASONAL sum activity
Terra_ACT_season_mean <- terra_data %>% 
  filter(season != "other") %>% 
  group_by(region, year, season, species, climate_scenario) %>% # grouping to get seasonal sum by year
  summarise(Sum_activity_scenario = sum(ACT_combined)) %>% # summing annual metabolic rate
  ungroup() %>% 
  group_by(region, season, species, climate_scenario) %>% # grouping to mean annual sum to calculate differences between 
  summarise(Value = mean(Sum_activity_scenario)) %>% 
  mutate(Test = "ACT_hr")


# Data summary - Seasonal maintenance metabolic costs by climate scenario
Terra_MR_season_mean <- terra_data %>%
  filter(season != "other") %>% 
  mutate(m_rate_J_Wwg = m_rate_J_adj/Ww_g) %>% # metabolic rate (J/g)
  group_by(region, year,season, species, climate_scenario) %>% # grouping to get year sum by year
  summarise(Sum_mrate_J_scenario = sum(m_rate_J_Wwg)) %>% # summing annual metabolic rate
  ungroup() %>% 
  group_by(region, season, species, climate_scenario) %>% # grouping to mean annual sum to calculate differences between 
  summarise(Value = mean(Sum_mrate_J_scenario))%>% 
  mutate(Test = "M_Rate_J")  


# Data summary - year foraging rates by climate scenario ants_consumed_hourly
Terra_Foraging_hr_season_mean <- terra_data %>%
  filter(season != "other") %>%
  mutate(m_rate_J_Wwg = m_rate_J_adj/Ww_g) %>% # metabolic rate (J/g)
  group_by(region, climate_scenario, species, season, year, month_ch) %>% 
  summarise(Sum_activity = sum(ACT_combined), 
            m_rate_J_g = sum(m_rate_J_Wwg)) %>% 
  ungroup() %>% 
  mutate(m_rate_J_g_ACT = m_rate_J_g/Sum_activity) %>% 
  filter(is.finite(m_rate_J_g_ACT)) %>%  # M. horrindus in July 1997 had 0 activity
  ungroup() %>% 
  group_by(region, species, season, climate_scenario) %>% # grouping to mean annual sum to calculate differences between 
  summarise(Value = mean(m_rate_J_g_ACT)) %>% 
  mutate(Test = "Foraging")



####################
## Final dataframe
# merge before  pivot for calculation
Terra_ACT_MR_Foraging_season_mean <- rbind(Terra_ACT_season_mean, 
                                  Terra_MR_season_mean,
                                  Terra_Foraging_hr_season_mean)


# data - pivot for calculation & Calculate Delta
ACT_MR_Foraging_change_yr_1 <- Terra_ACT_MR_Foraging_season_mean %>% 
  pivot_wider(names_from = climate_scenario, values_from = Value) %>%
  group_by(species, season) %>% #group by species and season
  mutate(future_2_delta = future_2 - future_0,
         future_4_delta = future_4 - future_0) %>% 
  dplyr::select(region, species, season, Test, future_2_delta, future_4_delta) %>% 
  rename(future_2 = future_2_delta, future_4 = future_4_delta) # renaming for plotting
# final df - will piviot with analysis table
ACT_MR_Foraging_change_season_final <- pivot_longer(ACT_MR_Foraging_change_yr_1, 
                                           cols = c(future_2, future_4), 
                                           names_to = "Climate_Scenario", 
                                           values_to = "Value") %>% 
  dplyr::rename(Species = species, Region = region)


############ SAVE AND READ IN DATA: ANNIUAL AND SEASONAL
saveRDS(ACT_MR_Foraging_change_season_final, "Raw_data/ACT_MR_Foraging_change_season_final.rds")


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary feeding requirements by season function: 
## Annual Ants Calculation - table for paper
# data
africa_ants_terra <- terra_data %>% 
  filter(region == "Africa")
australia_ants_terra <- terra_data %>% 
  filter(region == "Australia")


###########
# Function for calculation
annual_req_ants_Terra_tbl <- function(data = data) {
  Region_foraging_year <- data %>%
    group_by(region, climate_scenario, species, year, month_ch) %>% 
    summarise(Sum_activity = sum(ACT_combined), 
              sum_ants = sum(ants_consumed_hourly)) %>% 
    ungroup() %>% 
    mutate(ants_hourly = sum_ants/Sum_activity,
           ants_month = ants_hourly*Sum_activity)  %>%
    filter(is.finite(ants_hourly)) %>%  # M. horrindus in July 1997 had 0 activity
    group_by(region, species, climate_scenario) %>% # grouping to mean annual sum to calculate differences between 
    summarise(ants_hour = mean(ants_hourly))
  
  return(Region_foraging_year)
}

###########
## annual ants consumed
africa_ants_terra_data_terra <- annual_req_ants_Terra_tbl(africa_ants_terra) %>% 
  mutate(Region = "Africa")
australia_annual_ant_summary_terra <- annual_req_ants_Terra_tbl(australia_ants_terra)%>% 
  mutate(Region = "Australia")

# final df for merge - annual ants required per hr active (ants/hr)
annual_ants_TERRA <- rbind(africa_ants_terra_data_terra,
                           australia_annual_ant_summary_terra) %>% 
  mutate(season = "OVERALL")


####################
## 2) Seasonal ants - 
####################
## Annual Ants Calculation - table for paper
africa_seasonal_foraging <- terra_data %>% 
  filter(region == "Africa" & season!= "other")
australia_seasonal_foraging <- terra_data %>% 
  filter(region == "Australia"& season!= "other")

###########
# Function for calculation
season_req_ants_Terra_tbl <- function(data = data) {
  Region_foraging_season <- data %>%
    group_by(region, climate_scenario, species, year, month_ch, season) %>% 
    summarise(Sum_activity = sum(ACT_combined), 
              sum_ants = sum(ants_consumed_hourly)) %>% 
    ungroup() %>% 
    mutate(ants_hourly = sum_ants/Sum_activity,
           ants_month = ants_hourly*Sum_activity)  %>%
    filter(is.finite(ants_hourly)) %>%  # M. horrindus in July 1997 had 0 activity
    group_by(region, species, climate_scenario, season) %>% # grouping to mean annual sum to calculate differences between 
    summarise(ants_hour = mean(ants_hourly)) 
  
  return(Region_foraging_season)
}



####################
## SEASON ants consumed
africa_season_ant_summary <- season_req_ants_Terra_tbl(africa_seasonal_foraging) %>% 
  mutate(Region = "Africa")
australia_season_ant_summary <- season_req_ants_Terra_tbl(australia_seasonal_foraging)%>% 
  mutate(Region = "Australia")

# final df for merge - annual ants required per hr active (ants/hr)
seasonal_ants_plot_data_TERRA <- rbind(africa_season_ant_summary, 
                                 australia_season_ant_summary) 


## Ants per hr ACTIVE
ants_per_hr_active_TERRA <- rbind(annual_ants_TERRA,
                                  seasonal_ants_plot_data_TERRA)

saveRDS(ants_per_hr_active_TERRA, "Raw_data/ants_per_hr_active_TERRA.RDS")



#######################################################
#######################################################
##### Depth differences for supplementary information
terra_data <- terra_data %>%  
  mutate(decade = paste0("decade_", ceiling((year - 1984) / 10)))
kal_depth <- terra_data %>% filter(region == "Africa")
aus_depth <- terra_data %>% filter(region == "Australia")

####################
# summarise by decade and climate secnario
kal_depth_dat <- kal_depth %>% 
  group_by(species, climate_scenario, decade, month, hr) %>% 
  summarise(depth = mean(DEP)) %>% 
  group_by(species, climate_scenario, month, hr) %>% 
  summarise(depth = mean(depth))%>% 
  arrange(climate_scenario, month, hr) %>%
  mutate(MonthHour = (month - 1) * 24 + hr)

aus_depth_dat <- aus_depth %>% 
  group_by(species, climate_scenario, decade, month, hr) %>% 
  summarise(depth = mean(DEP)) %>% 
  group_by(species, climate_scenario, month, hr) %>% 
  summarise(depth = mean(depth)) %>% 
arrange(climate_scenario, month, hr) %>%
  mutate(MonthHour = (month - 1) * 24 + hr)


min(aus_depth$DEP)
min(kal_depth$DEP)

####################
# Define the colors for the climate scenarios
climate_colors <- c("future_0" = "#D8A428", "future_2" = "#C6272E", "future_4" = "#712B89")

####################
# Plotting with ggplot2
ggplot(kal_depth_dat, aes(x = MonthHour, y = depth, 
               group = interaction(climate_scenario, month), 
               color = climate_scenario)) +
  geom_line(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(12, 12 * 24, by = 24), labels = 1:12) +
  scale_color_manual(values = climate_colors) + # Set custom colors
  labs(x = "Month of Year", y = "Depth| Height (cm)", color = "Climate Scenario") +
  theme_minimal() +
  facet_wrap(~species, scales = "free") + # You have a facet_wrap here which is great for multiple species
  theme(legend.position = "top") # Adjust legend position as needed

ggplot(aus_depth_dat, aes(x = MonthHour, y = depth, 
                          group = interaction(climate_scenario, month), 
                          color = climate_scenario)) +
  geom_line(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(12, 12 * 24, by = 24), labels = 1:12) +
  scale_color_manual(values = climate_colors) + # Set custom colors
  labs(x = "Month of Year", y = "Depth | Height (cm)", color = "Climate Scenario") +
  theme_minimal() +
  facet_wrap(~species, scales = "free") + # You have a facet_wrap here which is great for multiple species
  theme(legend.position = "top") # Adjust legend position as needed




####################
# summarise shade by decade and climate secnario
kal_shade_dat <- kal_depth %>% 
  group_by(species, climate_scenario, decade, month, hr) %>% 
  summarise(shade = mean(SHADE)) %>% 
  group_by(species, climate_scenario, month, hr) %>% 
  summarise(shade = mean(shade))%>% 
  arrange(climate_scenario, month, hr) %>%
  mutate(MonthHour = (month - 1) * 24 + hr)

aus_shade_dat <- aus_depth %>% 
  group_by(species, climate_scenario, decade, month, hr) %>% 
  summarise(shade = mean(SHADE)) %>% 
  group_by(species, climate_scenario, month, hr) %>% 
  summarise(shade = mean(shade)) %>% 
  arrange(climate_scenario, month, hr) %>%
  mutate(MonthHour = (month - 1) * 24 + hr)


####################
# Plotting with ggplot2
ggplot(kal_shade_dat, aes(x = MonthHour, y = shade, 
                          group = interaction(climate_scenario, month), 
                          color = climate_scenario)) +
  geom_line(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(12, 12 * 24, by = 24), labels = 1:12) +
  scale_color_manual(values = climate_colors) + # Set custom colors
  labs(x = "Month of Year", y = "Depth (cm)", color = "Climate Scenario") +
  theme_minimal() +
  facet_wrap(~species, scales = "free") + # You have a facet_wrap here which is great for multiple species
  theme(legend.position = "top") # Adjust legend position as needed

ggplot(aus_shade_dat, aes(x = MonthHour, y = shade, 
                          group = interaction(climate_scenario, month), 
                          color = climate_scenario)) +
  geom_line(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(12, 12 * 24, by = 24), labels = 1:12) +
  scale_color_manual(values = climate_colors) + # Set custom colors
  labs(x = "Month of Year", y = "Depth (cm)", color = "Climate Scenario") +
  theme_minimal() +
  facet_wrap(~species, scales = "free") + # You have a facet_wrap here which is great for multiple species
  theme(legend.position = "top") # Adjust legend position as needed





















################################################################################### #
################################################################################### 
############# ############## CLIMB MISMATCH Scenarios ############# ############## 
# here animals that would climb have been changed to only burrow vise versa #######
################################################################################### #
################################################################################### #
###### !!! add CLIMB_ for climbing scenarios files
##########
## Load Climate scenario files for African species 
agac_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioKalahari_Agama_aculeata_X_environ.RDS"))%>% 
  mutate(Ww_g = 11.6) # mean mass from field data
chan_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioKalahari_Chondrodactylus_angulifer_A_environ.RDS"))%>% 
  mutate(Ww_g = 16) # mean mass from field data
mesu_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioKalahari_Meroles_suborbitalis_L_environ.RDS"))%>% 
  mutate(Ww_g = 4.01) # mean mass from field data
peli_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioKalahari_Pedioplanis_lineoocellata_A_environ.RDS"))%>% 
  mutate(Ww_g = 3.18) # mean mass from field data
trsp_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioKalahari_Trachylepis_sparsa_B_environ.RDS"))%>% 
  mutate(Ww_g = 11.3) # mean mass from field data
kal_clim_Climb<- rbind(agac_clim_Climb, chan_clim_Climb, trsp_clim_Climb, mesu_clim_Climb, peli_clim_Climb) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species),
         site = paste0("Site_AF", site),
         region = "Africa")%>% # adding site id by region
  dplyr::select(climate_scenario, region, site, date, year, month,
                hr, genus, species, Ww_g, TC, TAREF, O2_ml, ACT, DEP, SHADE) %>% 
  mutate(O2_ml_h = 0.013 * Ww_g^0.800 * 10^(0.038 * TC) * 10^(.14 * 0),# O2ml/h calculation
         m_rate_J = O2_ml_h*20.08, #O2_ml conversion - no adjustment to M rate and behaviour
         m_rate_J_adj = case_when(ACT == 1 ~ m_rate_J * 1.5, # basking metabolism adjustment
                                  ACT == 2 ~ m_rate_J * 4, # foraging metabolism adjustment
                                  TRUE ~ m_rate_J),
         ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, #  renaming activity for sum cal
                                  ACT == 2 ~ 1, # foraging 
                                  TRUE ~ as.numeric(ACT)),
         fortnight = floor((yday(date) - 1) / 14) + 1, # fortnight 
         fortnight_period = paste0("Fortnight_", year, "_", fortnight),# fortnight column
         month = month(date), # month column number
         month_ch = month(date, label=TRUE))

####################
## Load Climate scenario for Australian species
pomi_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioAustralia_Pogona_minor_A_environ.RDS")) %>% 
  mutate(Ww_g = 36) # mean mass from field data
ctqu_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioAustralia_Ctenotus_quatt_L_environ.RDS")) %>% 
  mutate(Ww_g = 4) # mean mass from field data
moho_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioAustralia_Moloch_horridus_R_environ.RDS")) %>% 
  mutate(Ww_g = 39) # mean mass from field data
geva_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioAustralia_Gehyra_variegata_L_environ.RDS")) %>% 
  mutate(Ww_g = 4) # mean mass from field data
ctis_clim_Climb<-readRDS(here("output/climate_scenarios/CLIMB_climate_scenarioAustralia_Ctenophorus_isolepis_L_environ.RDS")) %>% 
  mutate(Ww_g = 5) # mean mass from field data
# combine species
aus_clim_Climb <- rbind(pomi_clim_Climb, ctqu_clim_Climb, moho_clim_Climb, geva_clim_Climb, ctis_clim_Climb) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species),
         site = paste0("Site_AU", site)) %>% # adding site id by region
  dplyr::select(climate_scenario, region, site, date, year, month, 
                hr, genus, species, Ww_g, TC, TAREF, O2_ml, ACT, DEP, SHADE) %>% 
  mutate(O2_ml_h = 0.013 * Ww_g^0.800 * 10^(0.038 * TC) * 10^(.14 * 0),# O2ml/h calculation
         m_rate_J = O2_ml_h*20.08, #O2_ml conversion - no adjustment to M rate and behaviour
         m_rate_J_adj = case_when(ACT == 1 ~ m_rate_J * 1.5, # basking metabolism adjustment
                                  ACT == 2 ~ m_rate_J * 4, # foraging metabolism adjustment
                                  TRUE ~ m_rate_J),
         ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, #  renaming activity for sum cal
                                  ACT == 2 ~ 1, # foraging 
                                  TRUE ~ as.numeric(ACT)),
         fortnight = floor((yday(date) - 1) / 14) + 1, # fortnight 
         fortnight_period = paste0("Fortnight_", year, "_", fortnight),# fortnight column
         month = month(date), # month column number
         month_ch = month(date, label=TRUE))


####################
## final dataframe adding season for later and changing climate_scenario name
terra_data_Climb <- rbind(kal_clim_Climb, aus_clim_Climb) %>% 
  mutate(climate_scenario = case_when(climate_scenario == "current" ~ "future_0", 
                                      TRUE ~ climate_scenario),
         season = case_when(month %in% c(6, 7, 8) ~ "winter",
                            month %in% c(12, 1, 2) ~ "summer",
                            TRUE ~ "other")) 



################
# Normal data: terra_data
# Climbing data: terra_data_Climb
terra_data_final <- terra_data %>% mutate(climb_parameter ="TRUE")
terra_data_Climb_final <- terra_data_Climb %>% mutate(climb_parameter ="FALSE")
# climing df with both
climb_data <- rbind(terra_data_final, terra_data_Climb_final) %>% 
  select(climate_scenario, region, date, month, hr, species, 
         month_ch, climb_parameter, DEP)

# summarise data
climb_data_summary <- climb_data %>% 
  group_by(region, climate_scenario,species, climb_parameter, month,month_ch, hr) %>% 
  summarise(mean_depth = mean(DEP)) %>% 
  mutate(MonthHour = (month - 1) * 24 + hr)



###############################
##### Depth plotting
### Data by region 
climb_data_summary <- readRDS(file = "Raw_data/Climb_data_summary.RDS") %>% 
  mutate(climate_scenario = if_else(climate_scenario == "future_0", 
                                    "current", climate_scenario))

aus_dig <- climb_data_summary %>% filter(region == "Australia")
afr_dig <- climb_data_summary %>% filter(region == "Africa")

# Define the colors for the climate scenarios
climate_colors <- c("current" = "#FFCC00", "future_2" = "#FF3333", "future_4" = "#FF33CC")
climbing_linetypes <- c("TRUE" = "solid", "FALSE" = "dashed")


# Plotting with ggplot2, adding the linetype aesthetic
ggplot(afr_dig, aes(x = MonthHour, y = mean_depth, 
                    group = interaction(climate_scenario, species, 
                                        month, climb_parameter), 
                    color = climate_scenario, linetype = climb_parameter)) +
  geom_line(alpha = 0.4) + 
  scale_x_continuous(breaks = seq(12, 12 * 24, by = 24), labels = 1:12) +
  scale_color_manual(values = climate_colors) + # Set custom colors for climate scenario
  scale_linetype_manual(values = climbing_linetypes) + # Set custom linetypes for climbing_parameter
  labs(x = "Month of Year", y = "Depth | Height (cm)", color = "Climate Scenario", linetype = "Climbing Parameter") +
  theme_minimal() +
  facet_wrap(~species, scales = "free", ncol = 3) + # Adjust for your species variable
  theme(legend.position = c(1, 0), # Position top right
        legend.justification = c(1, 0)) # Anchor top right# Adjust legend position as needed

# Plotting with ggplot2, adding the linetype aesthetic
ggplot(aus_dig, aes(x = MonthHour, y = mean_depth, 
                    group = interaction(climate_scenario, species, 
                                        month, climb_parameter), 
                    color = climate_scenario, linetype = climb_parameter)) +
  geom_line(alpha = 0.4) + 
  scale_x_continuous(breaks = seq(12, 12 * 24, by = 24), labels = 1:12) +
  scale_color_manual(values = climate_colors) + # Set custom colors for climate scenario
  scale_linetype_manual(values = climbing_linetypes) + # Set custom linetypes for climbing_parameter
  labs(x = "Month of Year", y = "Depth | Height (cm)", color = "Climate Scenario", linetype = "Climbing Parameter") +
  theme_minimal() +
  facet_wrap(~species, scales = "free", ncol = 3) + # Adjust for your species variable
  theme(legend.position = c(1, 0), # Position top right
        legend.justification = c(1, 0)) # Anchor top right# Adjust legend position as needed





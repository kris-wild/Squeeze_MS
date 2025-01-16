
############################################
############################################
### SI Tables all in one place - these will be found in markdown doc


pacman::p_load(maps, dplyr,ggplot2, flextable, lubridate, knitr, here, stringr, broom, tibble, tidyverse, cowplot)


############################################
############################################
# Table S1
source(here("Scripts/Functions/Results.R"))
# Field body temperature (Tb) and ambient temperature (Ta)observations by species and region
Aus_Tb_data <-read.csv(here("Raw_data/Aus_Ta_Tb_Clean.csv")) %>% 
  arrange(Genus.species) %>%
  rename(Genus_species = Genus.species,
         Tb = bt,
         Ta = at,
         Site = site) %>% 
  mutate(dmy = dmy(date),
         year = year(dmy),
         year_cat = as.factor(year),
         Year = as.numeric(as.character(year_cat)),
         Region = "Australia") %>%
  filter(Genus_species == "Ctenotus quatt" |
           Genus_species == "Gehyra variegata"|
           Genus_species == "Moloch horridus" |
           Genus_species == "Pogona minor"|
           Genus_species == "Ctenophorus isolepis") %>% 
  mutate(Genus_species = str_replace(Genus_species, "^(\\w)\\w+\\s(\\w+)$", "\\1. \\2")) %>% 
  select(Region, Site, Genus_species, Year, Tb, Ta)

Kal_Tb_data <- read.csv(here(file = "Raw_data/Kal_Ta_Tb_Clean.csv")) %>% 
  mutate(Genus.species = gsub(" ", "_", species)) %>% 
  arrange(Genus.species) %>%
  rename(Genus_species = Genus.species,
         Tb = bt,
         Ta = at,
         Site = site) %>% 
  mutate(dmy = dmy(date),
         year = year(dmy),
         year_cat = as.factor(year),
         Year = as.numeric(as.character(year_cat)), 
         Region = "Africa") %>%
  filter(Genus_species == "Chondrodactylus_angulifer" |
           Genus_species == "Trachylepis_sparsa"|
           Genus_species == "Agama_aculeata" |
           Genus_species == "Meroles_suborbitalis"|
           Genus_species == "Pedioplanis_lineoocellata") %>% 
  mutate(Genus_species = str_replace(Genus_species, "^(\\w)\\w+_(\\w+)$", "\\1. \\2")) %>% 
  select(Region, Site, Genus_species, Year, Tb, Ta)

# combinde data
Tb_data <- rbind(Kal_Tb_data, Aus_Tb_data)

## All Ta and Tb for supplementary
Tb_Site_range_all  <- Tb_data %>%
  group_by(Region, Site, Genus_species) %>%
  summarize(
    year_range = paste(min(Year, na.rm = TRUE), 
                       max(Year, na.rm = TRUE), sep = " - "),
    Tb_sample_size = sum(!is.na(Tb)),
    Ta_sample_size = sum(!is.na(Ta))) %>% 
  ungroup() %>% 
  arrange(Region, Genus_species, desc(Tb_sample_size)) %>% 
  filter(Tb_sample_size >= 10)

## Tb data used in comparisions:
Tb_Site_range_filtered <- Tb_Site_range_all %>%
  filter(
    (Genus_species == "A. aculeata" & Site %in% c("KLX", "KLB")) |
      (Genus_species == "C. angulifer" & Site %in% c("KLA", "KLL")) |
      (Genus_species == "M. suborbitalis" & Site %in% c("KLA", "KLT"))|
      (Genus_species == "P. lineoocellata" & Site %in% c("KLA", "KLT")) |
      (Genus_species == "T. sparsa" & Site %in% c("KLB", "KLK")) |
      (Genus_species == "C. isolepis" & Site %in% c("AUSR", "AUSL")) |
      (Genus_species == "C. quatt" & Site %in% c("AUSR", "AUSL")) |
      (Genus_species == "G. variegata" & Site %in% c("AUSR", "AUSL")) |
      (Genus_species == "M. horridus" & Site %in% c("AUSD", "AUSE", "AUSL")) |
      (Genus_species == "P. minor" & Site %in% c("AUSA", "AUSI", "AUSL", "AUSR")))

# Data for Tbl 1 data
Table_S1 <- Tb_Site_range_filtered %>%
  group_by(Region, Genus_species) %>%
  summarize(
    Total_Site_Sample_Size = n(),  # Assuming each row is a unique site sample
    Total_Tb_Sample_Size = sum(Tb_sample_size, na.rm = TRUE),
    Total_Ta_Sample_Size = sum(Ta_sample_size, na.rm = TRUE)) %>% 
  ungroup() 

Table_S1 <- Table_S1[,c(1, 2, 3, 4, 5)]

TblS1<-flextable(Table_S1) %>% 
  italic(i = 1:10, j = 2, italic = TRUE) %>% 
  hline(i=5, j = 1:5, part="body") %>% 
  merge_v(j = c("Region", "Genus_species")) %>% 
  flextable::font(part = "all", fontname = "Times New Roman") %>% 
  set_table_properties(layout = "autofit") %>% 
  fix_border_issues(part = "body")

############################################
############################################
# Table S2 - parameter table in summary sheet folder



############################################
############################################
#Table S3 - Ta differences
source(here("Scripts/Functions/SI_functions.R"))
Africa_diffs <- read.csv(here("output/checking Tbs/Af_Ta_Tb_check_master.csv")) %>% 
  filter(!is.na(TA_interp) & !is.na(AT) & !is.na(TC_interp), !is.na(BT)) 

Australia_diffs <- read.csv(here("output/checking Tbs/AU_Ta_Tb_check_master.csv")) %>% 
  filter(!is.na(TA_interp) & !is.na(AT) & !is.na(TC_interp), !is.na(BT))


##### Table stats for Africa and Australia
# Africa - Ta/Tb
Africa_Ta_diffs_tbl <- get_regression(Africa_diffs, 
                                      interp_col = "AT", 
                                      response_col = "TA_interp") %>%
  mutate(Region = "Africa")
Australia_Ta_diffs_tbl <- get_regression(Australia_diffs, 
                                         interp_col = "AT", 
                                         response_col = "TA_interp") %>% 
  mutate(Region = "Australia")

# change pvalues to <01 if values are 0
Table_S3 <- rbind(Africa_Ta_diffs_tbl, Australia_Ta_diffs_tbl) 
Table_S3 <- Table_S3[,c(6, 1, 2, 3, 4, 5)]

TblS3<-flextable(Table_S3) %>% 
  italic(i = 1:12, j = 2, italic = TRUE) %>% 
  hline(i=6, j = 1:6, part="body") %>% 
  merge_v(j = c("Region", "Genus_species")) %>% 
  flextable::font(part = "all", fontname = "Times New Roman") %>% 
  set_table_properties(layout = "autofit") %>% 
  fix_border_issues(part = "body")



############################################
############################################
# Table S4 - Tb differences
source(here("Scripts/Functions/SI_functions.R"))
Africa_diffs <- read.csv(here("output/checking Tbs/Af_Ta_Tb_check_master.csv")) %>% 
  filter(!is.na(TA_interp) & !is.na(AT) & !is.na(TC_interp), !is.na(BT)) 

Australia_diffs <- read.csv(here("output/checking Tbs/AU_Ta_Tb_check_master.csv")) %>% 
  filter(!is.na(TA_interp) & !is.na(AT) & !is.na(TC_interp), !is.na(BT)) 

##### Table stats for Africa and Australia
# Africa - Ta/Tb
Africa_Ta_diffs_tbl <- get_regression(Africa_diffs, 
                                      interp_col = "BT", 
                                      response_col = "TC_interp") %>%
  mutate(Region = "Africa")
Australia_Ta_diffs_tbl <- get_regression(Australia_diffs, 
                                         interp_col = "BT", 
                                         response_col = "TC_interp") %>% 
  mutate(Region = "Australia")

# change pvalues to <01 if values are 0
Table_S4 <- rbind(Africa_Ta_diffs_tbl, Australia_Ta_diffs_tbl) 
Table_S4 <- Table_S4[,c(6, 1, 2, 3, 4, 5)]

TblS4<-flextable(Table_S4) %>% 
  italic(i = 1:12, j = 2, italic = TRUE) %>% 
  hline(i=6, j = 1:6, part="body") %>% 
  merge_v(j = c("Region", "Genus_species")) %>% 
  flextable::font(part = "all", fontname = "Times New Roman") %>% 
  set_table_properties(layout = "autofit") %>% 
  fix_border_issues(part = "body")





############################################
############################################
# Table S5 - Table of the number of ants required per hour active for each diel category
ants_per_h_active <- readRDS(here("Raw_data/ants_per_hr_active_TERRA.RDS")) %>% 
  mutate(diel = case_when(species %in% c("C. angulifer", "G. variegata") ~ "nocturnal",
                          TRUE ~ "diurnal")) 

#############
## Overall - ant change by diel
overall_diel_ant_change <- ants_per_h_active %>% 
  filter(season == "OVERALL") %>% # filter out the seasonal trends
  group_by(Region, climate_scenario, diel) %>% 
  summarise(ant_hr = mean(ants_hour))

# Reshape the data
overall_diel_ant_change_wide <- overall_diel_ant_change %>%
  pivot_wider(id_cols = c("Region", "diel"),
              names_from = c("climate_scenario"),
              values_from = c("ant_hr"),
              names_glue = "{climate_scenario}_{.value}") %>%
  ungroup() %>% 
  select(Region, diel, 
         future_0_ant_hr,
         future_2_ant_hr,
         future_4_ant_hr) %>% 
  rename(Diel = diel, 
         'Current' = future_0_ant_hr,
         '+2°C' = future_2_ant_hr,
         '+4°C' = future_4_ant_hr) 

############
# final data table
Table_S5 <- overall_diel_ant_change_wide %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
TblS5 <-  flextable(Table_S5) %>% 
  hline_top(border = fp_border_default(width = 0), 
            part = "header") %>% 
  hline(i =2, j = 1:5, part = "body", border = fp_border_default(width = 1)) %>% 
  fontsize(size = 10, part = "all") %>%
  merge_v(j = c("Region", "Diel"), part = "header") %>%
  merge_v(j = c("Region", "Diel"), part = "body") %>%
  flextable::font(part = "all", fontname = "Times New Roman") %>%
  set_table_properties(layout = "autofit") %>%
  fix_border_issues(part = "body") %>%
  set_table_properties(layout = "autofit") %>% 
  merge_h(part = "header") %>% 
  flextable::align(align = "center", part = "all", j = 3:5)  # Center al



############################################
############################################
# Table S6. Differences in estimated food requirements per hour active by diurnal and nocturnal category for each season by region (Kalahari Desert, Africa and Great Victoria Desert, Australia).
ants_per_h_active <- readRDS(here("Raw_data/ants_per_hr_active_TERRA.RDS")) %>% 
  mutate(diel = case_when(species %in% c("C. angulifer", "G. variegata") ~ "nocturnal",
                          TRUE ~ "diurnal")) 

# diel by season
seasonal_diel_ant_change <- ants_per_h_active %>% 
  filter(season != "OVERALL") %>% # filter out the annual
  group_by(Region, climate_scenario, season, diel) %>% 
  summarise(ant_hr = mean(ants_hour))

season_diel_ant_change_wide <- seasonal_diel_ant_change %>%
  pivot_wider(id_cols = c("Region", "diel", 'season'),
              names_from = c("climate_scenario"),
              values_from = c("ant_hr"),
              names_glue = "{climate_scenario}_{.value}") %>%
  ungroup() %>% 
  select(Region, diel, season,
         future_0_ant_hr, 
         future_2_ant_hr,
         future_4_ant_hr) %>% 
  rename(Diel = diel, 
         'Current' = future_0_ant_hr,
         '+2°C' = future_2_ant_hr,
         '+4°C' = future_4_ant_hr) %>% 
  rename(Season = season)
# arranging season
season_diel_ant_change_wide$Season <- factor(season_diel_ant_change_wide$Season,
                                             levels = c("winter", "spring", "summer"))

# Now reorder the dataframe based on the 'Season' factor
season_diel_ant_change_wide <- season_diel_ant_change_wide %>%
  arrange(Region, Season)

#########
# Final table
Table_S6 <- season_diel_ant_change_wide %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

TblS6 <- flextable(Table_S6) %>% 
  hline_top(border = fp_border_default(width = 0), 
            part = "header") %>% 
  fontsize(size = 10, part = "all") %>%
  hline(i =6, j = 1:6, part = "body", border = fp_border_default(width = 1.5)) %>% 
  merge_v(j = c("Region", "Diel", "Season"), part = "header") %>%
  merge_v(j = c("Region", "Diel", "Season"), part = "body") %>%
  flextable::font(part = "all", fontname = "Times New Roman") %>%
  hline(i =c(2, 4, 8, 10), j = 1:6, part = "body") %>% 
  set_table_properties(layout = "autofit") %>%
  fix_border_issues(part = "body") %>%
  flextable::align(j = 3:6, align = "center", part = "all") %>%
  set_table_properties(layout = "autofit")




############################################
############################################
# Table S7. Differences in estimated food requirements per hour active by species by region (Kalahari Desert, Africa and Great Victoria Desert, Australia). 
ants_per_h_active <- readRDS(here("Raw_data/ants_per_hr_active_TERRA.RDS")) %>% 
  mutate(diel = case_when(species %in% c("C. angulifer", "G. variegata") ~ "nocturnal",
                          TRUE ~ "diurnal")) 


#############
overall_spp_ant_change <- ants_per_h_active %>% 
  filter(season == "OVERALL") %>% # filter out the seasonal trends
  group_by(Region, climate_scenario, diel, species) %>% 
  summarise(ant_hr = mean(ants_hour))

# Reshape the data
overall_diel_ant_change_wide <- overall_spp_ant_change %>%
  pivot_wider(id_cols = c("Region", "species", "diel"),
              names_from = c("climate_scenario"),
              values_from = c("ant_hr"),
              names_glue = "{climate_scenario}_{.value}") %>%
  ungroup() %>% 
  select(Region, species, diel, 
         future_0_ant_hr, 
         future_2_ant_hr,
         future_4_ant_hr) %>% 
  rename(Diel = diel, 
         'Current' = future_0_ant_hr,
         '+2°C' = future_2_ant_hr,
         '+4°C' = future_4_ant_hr) 

############
# final data table
Table_S7 <- overall_diel_ant_change_wide %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
TblS7 <-  flextable(Table_S7) %>% 
  fontsize(size = 10, part = "all") %>%
  italic(i = c(1:10), j = 2, part = "body") %>%   
  hline(i =5, j = 1:6, part = "body", border = fp_border_default(width = 1.5)) %>% 
  merge_v(j = c("Region"), part = "header") %>%
  merge_v(j = c("Region"), part = "body") %>%
  flextable::font(part = "all", fontname = "Times New Roman") %>%
  set_table_properties(layout = "autofit") %>%
  fix_border_issues(part = "body") %>%
  flextable::align(j = 3:6, align = "center", part = "all") %>%
  set_table_properties(layout = "autofit") 


############################################
############################################
# Table S8 Differences in estimated food requirements per hour active by species for each season in Kalahari. Food requirements were estimated by ants required per hour (ants/h) for each year for each species. 
ants_per_h_active <- readRDS(here("Raw_data/ants_per_hr_active_TERRA.RDS")) %>% 
  mutate(diel = case_when(species %in% c("C. angulifer", "G. variegata") ~ "nocturnal",
                          TRUE ~ "diurnal")) 

#############
## seasonal ant change by species 
africa_season_spp_ant_change <- ants_per_h_active %>% 
  filter(season != "OVERALL") %>% # filter out the seasonal trends
  group_by(Region, climate_scenario, diel, species, season) %>% 
  summarise(ant_hr = mean(ants_hour))  %>% 
  filter(Region == "Africa")

# wide data for table
africa_seasonal_spp_ant_change_wide <- africa_season_spp_ant_change %>%
  pivot_wider(id_cols = c("Region", "species", "season",  "diel"),
              names_from = c("climate_scenario"),
              values_from = c("ant_hr"),
              names_glue = "{climate_scenario}_{.value}") %>%
  ungroup() %>% 
  select(species, season, diel,
         future_0_ant_hr, 
         future_2_ant_hr,
         future_4_ant_hr) %>% 
  rename(Season = season, 
         Species = species,
         Diel = diel, 
         'Current' = future_0_ant_hr,
         '+2°C' = future_2_ant_hr,
         '+4°C' = future_4_ant_hr) 
# arranging season
africa_seasonal_spp_ant_change_wide$Season <- factor(africa_seasonal_spp_ant_change_wide$Season,
                                                     levels = c("winter", "spring", "summer"))

# Now reorder the dataframe based on the 'Season' factor
africa_seasonal_spp_ant_change_wide <- africa_seasonal_spp_ant_change_wide %>%
  arrange(Species, Season)


#########
# Final table
TableS8 <- africa_seasonal_spp_ant_change_wide %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

TblS8 <- flextable(TableS8) %>% 
  fontsize(size = 10, part = "all") %>%
  italic(i = c(1:15), j = 1, part = "body") %>%   
  hline(i =c(3,6,9,12), j = 1:6, part = "body") %>% 
  merge_v(j = c("Species"), part = "header") %>%
  merge_v(j = c("Species"), part = "body") %>%
  flextable::font(part = "all", fontname = "Times New Roman") %>%
  set_table_properties(layout = "autofit") %>%
  fix_border_issues(part = "body") %>%
  flextable::align(j = 3:6, align = "center", part = "all") %>%
  set_table_properties(layout = "autofit")




############################################
############################################
# Table S9 Differences in estimated food requirements per hour active by species for each season in Australia. Food requirements were estimated by ants required per hour (ants/h) for each year for each species

#### Ants for metabolic demand using terra climate data: current, +2, +4
ants_per_h_active <- readRDS(here("Raw_data/ants_per_hr_active_TERRA.RDS")) %>% 
  mutate(diel = case_when(species %in% c("C. angulifer", "G. variegata") ~ "nocturnal",
                          TRUE ~ "diurnal")) 

#############
## seasonal ant change by species 
australia_season_spp_ant_change <- ants_per_h_active %>% 
  filter(season != "OVERALL") %>% # filter out the seasonal trends
  group_by(Region, climate_scenario, diel, species, season) %>% 
  summarise(ant_hr = mean(ants_hour))  %>% 
  filter(Region == "Australia")

# wide data for table
australia_seasonal_spp_ant_change_wide <- australia_season_spp_ant_change %>%
  pivot_wider(id_cols = c("Region", "species", "season",  "diel"),
              names_from = c("climate_scenario"),
              values_from = c("ant_hr"),
              names_glue = "{climate_scenario}_{.value}") %>%
  ungroup() %>% 
  select(species, season, diel,
         future_0_ant_hr, 
         future_2_ant_hr,
         future_4_ant_hr) %>% 
  rename(Season = season, 
         Species = species,
         Diel = diel, 
         'Current' = future_0_ant_hr,
         '+2°C' = future_2_ant_hr,
         '+4°C' = future_4_ant_hr) 
# arranging season
australia_seasonal_spp_ant_change_wide$Season <- factor(australia_seasonal_spp_ant_change_wide$Season,
                                                        levels = c("winter", "spring", "summer"))

# Now reorder the dataframe based on the 'Season' factor
australia_seasonal_spp_ant_change_wide <- australia_seasonal_spp_ant_change_wide %>%
  arrange(Species, Season)


#########
# Final table
TableS9 <- australia_seasonal_spp_ant_change_wide %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

TblS9 <- flextable(TableS9) %>% 
  hline_top(border = fp_border_default(width = 0), 
            part = "header") %>% 
  fontsize(size = 10, part = "all") %>%
  italic(i = c(1:15), j = 1, part = "body") %>%   
  hline(i =c(3,6,9,12), j = 1:6, part = "body") %>% 
  merge_v(j = c("Species"), part = "header") %>%
  merge_v(j = c("Species"), part = "body") %>%
  flextable::font(part = "all", fontname = "Times New Roman") %>%
  set_table_properties(layout = "autofit") %>%
  fix_border_issues(part = "body") %>%
  flextable::align(j = 3:6, align = "center", part = "all") %>%
  set_table_properties(layout = "autofit")
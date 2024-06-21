#############
### Field metabolic rate - Pedioplanis lineoocellata
## Nagy, K. A., Huey, R. B., & Bennett, A. F. (1984). 
## Field energetics and foraging mode of Kalahari lacertid lizards. 
## Ecology, 65(2), 588-596.
# see Table 1 for mass of animals and FMR estimation (kJ/d) in late November
# mass of animal: 3.27g
# energy expendature: 739 ± 68SE J/d

### Predicted data
peli_contemp_DLW <-readRDS("output/environ/ERA5_70YR_Kalahari_Pedioplanis_lineoocellata_L_environ_FMR_EXAMPLE.RDS")%>%
  mutate(Ww_g = 3.27, # adjust mass observed in field during that time (Table 1)
         O2_ml_h = 0.013 * Ww_g^0.800 * 10^(0.038 * TC) * 10^(.14 * 0),# O2ml/h calculation
         m_rate_J = O2_ml_h*20.08, #O2_ml conversion - no adjustment to M rate and behaviour
         m_rate_J_adj = case_when(ACT == 1 ~ m_rate_J * 1.5, # basking metabolism adjustment
                                  ACT == 2 ~ m_rate_J * 4, # foraging metabolism adjustment
                                  TRUE ~ m_rate_J)) 

### predicted summary
peli_DLW_pred <- peli_contemp_DLW %>% 
  filter(date >= "1981-11-15" & date <= "1981-11-30") %>% 
  group_by(date) %>% 
  summarise(sum_kJ_d = sum(m_rate_J_adj)) %>% 
  ungroup() %>% 
  summarise(predicted_kJ_d = mean(sum_kJ_d),
            predicted_KJ_d_SD = sd(sum_kJ_d))
peli_DLW_pred



#############
### Predicted ants consumed - Moloch horridus
## Withers, P. C., & Dickman, C. R. (1995). 
## The role of diet in determining water, energy and 
## salt intake in the thorny devil Moloch horridus (Lacertilia: Agamidae). 
## Journal of the Royal Society of Western Australia, 78, 3.
# Range 750 -1500  

## Predicted data across 70 years 
moho_contemp<-readRDS("output/environ/ERA5_70YR_Australia_Moloch_horridus_R_environ.RDS")%>% 
  mutate(Ww_g = 35, # mass of individual in study
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

# bring in get ants function
source("Scripts/Functions/get_ants.R")
# apply the hourly ant estimate to data "contemp" that has both regions
moho_feeding_demand <- moho_contemp %>%
  group_by(species) %>% 
  rowwise() %>% # apply function by row
  mutate(ants_consumed_hourly = get_ants_hourly(m_rate_J_h = m_rate_J_adj)) %>%
  ungroup()

# summparise mean feeding demand
moho_ants_pred_mean <- moho_feeding_demand %>% 
  group_by(date) %>% 
  summarise(sum_ants_d = sum(ants_consumed_hourly)) %>% 
  ungroup() %>% 
  summarise(mean_day_ants = mean(sum_ants_d))
moho_ants_pred_mean


hist(moho_ants_pred_mean$sum_ants_d)

########################################
########################################
############ FIGURE S1-S9 Code: this will be put in markdown doc
# to quickly skip to specific code comand find the following: 
# Figure S1.
# Figure S2.
# Figure S3.
# Figure S4.
# Figure S5.
# Figure S6.
# Figure S7.
# Figure S8.
# Figure S9.

pacman::p_load(maps, dplyr,ggplot2, flextable, lubridate, knitr, here, stringr, broom, tibble,tidyverse, cowplot)

############################
############################
# FIGURE S1 is a map of sites. See map folder for Evenza map

############################
############################
# FIGURE S2.  The relationship between observed (y axis) and predicted (x axis) field observations for air (Ta - blue) and lizard body temperatures (Tb - red)  in the Kalahari.  
pacman::p_load(dplyr, ggplot2, tidyverse, here)
#######
### Read in regional data
# Africa
Af_ta_temps<- read.csv(here("output/checking Tbs/Af_Ta_Tb_check_master.csv")) %>% 
  select(Genus_species, datetime2, AT, TA_interp) %>% 
  rename(predicted = TA_interp, 
         observed = AT) %>% 
  mutate(Test = "Ta")
Af_tb_temps<- read.csv(here("output/checking Tbs/Af_Ta_Tb_check_master.csv")) %>% 
  select(Genus_species, datetime2, BT, TC_interp) %>% 
  rename(predicted = TC_interp, 
         observed = BT) %>% 
  mutate(Test = "Tb")
Af_temps <- rbind(Af_ta_temps, Af_tb_temps)%>% 
  filter(!is.na(Genus_species))

#######
### Plotting:  TA and TB by species
# plot set up
my_colors <- c("tomato1", "dodgerblue") 
Af_temps$Test <- factor(Af_temps$Test, levels = c("Tb", "Ta"))

# Africa plot
Af_temps_plot <- ggplot(Af_temps, aes(x = observed, y = predicted, color = Test)) +
  geom_point(alpha = 0.3, size = 2) +
  scale_color_manual(values = my_colors, 
                     labels = c(expression(T[b]), expression(T[a])), 
                     breaks = c("Tb", "Ta")) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  scale_x_continuous(name = expression(paste("Predicted", 
                                             " (", degree, "C)", sep = "")),
                     limits = c(10, 48), breaks = seq(15, 45, by = 5)) +
  scale_y_continuous(name = expression(paste("Observed", 
                                             " (", degree, "C)", 
                                             sep = "")),
                     limits = c(10, 48), breaks = seq(15, 45, by = 5)) +
  theme_minimal() +
  facet_wrap(~Genus_species, 
             scales = "free", ncol = 3) + # Adjust for your species variable
  theme(legend.position = c(.85, .1), # Position top right
        legend.justification = c(1, 0), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        strip.text = element_text(face = "italic", size = 20), 
        legend.title = element_text(size = 30, face = "bold"), #
        legend.text = element_text(size = 24))



############################
############################
# FIGURE S3.The relationship between observed (y axis) and predicted (x axis) field observations for air (Ta - blue) and lizard body temperatures (Tb - red) in the Great Victorian Desert.  
pacman::p_load(dplyr, ggplot2, tidyverse, here)
#######
### Read in regional data
# Australia
Au_ta_temps<- read.csv(here("output/checking Tbs/Au_Ta_Tb_check_master.csv")) %>% 
  select(Genus_species, datetime2, AT, TA_interp) %>% 
  rename(predicted = TA_interp, 
         observed = AT) %>% 
  mutate(Test = "Ta")
Au_tb_temps<- read.csv(here("output/checking Tbs/Au_Ta_Tb_check_master.csv")) %>% 
  select(Genus_species, datetime2, BT, TC_interp) %>% 
  rename(predicted = TC_interp, 
         observed = BT) %>% 
  mutate(Test = "Tb") 
Au_temps <- rbind(Au_ta_temps, Au_tb_temps)%>% 
  dplyr::filter(Genus_species != "" & trimws(Genus_species) != "")

#######
### Plotting:  TA and TB by species
# plot set up
my_colors <- c("tomato1", "dodgerblue") 
Au_temps$Test <- factor(Au_temps$Test, levels = c("Tb", "Ta"))

# Australia plot
Au_tb_temps_plot <- ggplot(Au_temps, aes(x = observed, y = predicted, color = Test)) +
  geom_point(alpha = 0.3, size = 2) +
  scale_color_manual(values = my_colors, 
                     labels = c(expression(T[b]), expression(T[a])), 
                     breaks = c("Tb", "Ta")) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  scale_x_continuous(name = expression(paste("Predicted", 
                                             " (", degree, "C)", sep = "")),
                     limits = c(10, 48), breaks = seq(15, 45, by = 5)) +
  scale_y_continuous(name = expression(paste("Observed", 
                                             " (", degree, "C)", 
                                             sep = "")),
                     limits = c(10, 48), breaks = seq(15, 45, by = 5)) +
  theme_bw() +
  theme_minimal() +
  facet_wrap(~Genus_species, 
             scales = "free", ncol = 3) + # Adjust for your species variable
  theme(legend.position = c(.85, .1), # Position top right
        legend.justification = c(1, 0), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        strip.text = element_text(face = "italic", size = 20), 
        legend.title = element_text(size = 30, face = "bold"), #
        legend.text = element_text(size = 24)) 



############################
############################
# FIGURE S4. Average climb height (cm) or average burrow depth (cm) when comparing future climate scenarios for lizards in the Kalahari. Responses to climate warming of +2 °C and +4 °C were simulated using TerraClimate data from 1985 to 2015. Values were summarized by taking hourly mean predicted depth/height for each month under each future climate scenario (Current, +2 °C or +4 °C) for each species.  Values above zero indicate climbing behavior and values below indicate burrow behavior. Breaks in lines are from summary differences between each month.
terra_data <- readRDS(here("output/foraging_success/TERRA_Results_all.RDS"))

################
# summarise data
climb_data_summary <- terra_data %>% 
  group_by(region, climate_scenario,species, month,month_ch, hr) %>% 
  summarise(mean_depth = mean(DEP)) %>% 
  mutate(MonthHour = (month - 1) * 24 + hr) 
climb_data_summary$climate_scenario[climb_data_summary$climate_scenario == "future_0"] <- "current"


aus_dig <- climb_data_summary %>% filter(region == "Australia")
afr_dig <- climb_data_summary %>% filter(region == "Africa")

# Define the colors for the climate scenarios
climate_colors <- c("current" = "#666666", "future_2" = "#8B0000", "future_4" = "purple")
climbing_linetypes <- c("current" = "solid", "future_2" = "dashed", "future_4" = "dotdash")

########
# Africa
# Plotting with ggplot2, adding the linetype aesthetic
afr_dig_plot <- ggplot(afr_dig, aes(x = MonthHour, y = mean_depth, 
                                    group = interaction(climate_scenario, species, month), 
                                    color = climate_scenario)) +
  geom_line(alpha = 0.4) + 
  scale_x_continuous(breaks = (1:12 - 1) * 24 + 12, # Adjust this if the hour 'hr' isn't 12 for each month
                     labels = month.abb) + # Use month.abb for standard abbreviations
  scale_color_manual(values = climate_colors) +
  labs(x = "Month of Year", y = "Depth | Height (cm)", color = "Climate Scenario", linetype = "Climbing Parameter") +
  theme_minimal() +
  facet_wrap(~species, scales = "free", ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.position = c(1, .1), # Position top right
        legend.justification = c(1, 0), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20),
        strip.text = element_text(face = "italic", size = 20), 
        legend.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 24)) 

############################
############################
# FIGURE S5 Climb height (cm) or burrow depth (cm) changes when comparing future climate scenarios for lizards in the Great Victorian Desert. Responses to climate warming of +2 °C and +4 °C were simulated using TerraClimate data from 1985 to 2015. Values were summarized by taking hourly mean predicted depth/height for each month under each future climate scenario (Current, +2 °C or +4 °C) for each species.  Values above zero indicate climbing behavior and values below indicate burrow behavior. Breaks in lines are from summary differences between each month. 
terra_data <- readRDS(here("output/foraging_success/TERRA_Results_all.RDS"))

################
# summarise data
climb_data_summary <- terra_data %>% 
  group_by(region, climate_scenario,species, month,month_ch, hr) %>% 
  summarise(mean_depth = mean(DEP)) %>% 
  mutate(MonthHour = (month - 1) * 24 + hr) 
climb_data_summary$climate_scenario[climb_data_summary$climate_scenario == "future_0"] <- "current"


aus_dig <- climb_data_summary %>% filter(region == "Australia")

# Define the colors for the climate scenarios
climate_colors <- c("current" = "#666666", "future_2" = "#8B0000", "future_4" = "purple")
climbing_linetypes <- c("current" = "solid", "future_2" = "dashed", "future_4" = "dotdash")

########
# Australia
# Plotting with ggplot2, adding the linetype aesthetic
aus_dig_plot <- ggplot(aus_dig, aes(x = MonthHour, y = mean_depth, 
                                    group = interaction(climate_scenario, species, month), 
                                    color = climate_scenario)) +
  geom_line(alpha = 0.4) + 
  scale_x_continuous(breaks = (1:12 - 1) * 24 + 12, # Adjust this if the hour 'hr' isn't 12 for each month
                     labels = month.abb) + # Use month.abb for standard abbreviations
  scale_color_manual(values = climate_colors) +
  labs(x = "Month of Year", y = "Depth | Height (cm)", color = "Climate Scenario", linetype = "Climbing Parameter") +
  theme_minimal() +
  facet_wrap(~species, scales = "free", ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.position = c(1, .1), # Position top right
        legend.justification = c(1, 0), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 20),
        strip.text = element_text(face = "italic", size = 20), 
        legend.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 24)) 

############################
############################
# FIGURE S6. Example showing foraging activity restriction (orange) throughout the year and how the hours of restriction for each month compare from our estimates using NicheMapR (black) and the equation developed by Sinervo et al.(2) (red). In this example, we used parameters for the Australian species Ctenotus quatt. at site L and utilized the micro_global function to estimate microclimate under historical long-term average monthly conditions (1960-1990). The comparison illustrates that biophysical calculations produce similar estimates of activity restriction to those of Sinervo et al.(2) empirical function.
library(NicheMapR)
species <- "Ctenotus quatt"
source(paste0('Scripts/parameters/', species, '/parameters_biophys.R'))

sites <- read.csv("Sites/Aus_KL_Sites.csv")
site_name <- "Australia_L"
location <- subset(sites, Site.name == site_name)
loc <- c(location$Long, location$Lat)

warm <- 1
Usrhyt <- 0.005
maxshade <- .050
micro <- micro_global(loc = loc,
                      warm = warm,
                      Usrhyt = Usrhyt,
                      maxshade = maxshade,
                      cap = 0)
# retrieve output
metout <- as.data.frame(micro$metout) # above ground microclimatic conditions, min shade
shadmet <- as.data.frame(micro$shadmet) # above ground microclimatic conditions, max shade
soil <- as.data.frame(micro$soil) # soil temperatures, minimum shade
shadsoil <- as.data.frame(micro$shadsoil) # soil temperatures, maximum shade

# append dates
dates <- micro$dates
metout <- cbind(dates, metout)
soil <- cbind(dates, soil)
shadmet <- cbind(dates, shadmet)
shadsoil <- cbind(dates, shadsoil)

ecto <- ectotherm(Ww_g = Ww_g,
                  shape = shape,
                  CT_min = CT_min, 
                  T_RB_min = T_RB_min,
                  T_B_min = T_B_min,
                  T_F_min = T_F_min,
                  T_pref = T_pref,
                  T_F_max = T_F_max,
                  CT_max = CT_max,
                  alpha_max = alpha_max,
                  alpha_min = alpha_min,
                  diurn = diurn,
                  nocturn = nocturn,
                  shade_seek = shade_seek,
                  burrow = burrow,
                  climb = climb,
                  custom_shape = c(10.4713, 0.688, 0.425, 0.85, 3.798, 0.683, 10.4713, 0.688),
                  minshades = micro$minshade,
                  maxshades = micro$maxshade,
                  shdburrow = shdburrow,
                  mindepth = mindepth,
                  maxdepth = maxdepth,
                  pct_wet = pct_wet,
                  pct_eyes = pct_eyes)


# retrieve output
environ <- as.data.frame(ecto$environ) # activity, Tb and environment
enbal <- as.data.frame(ecto$enbal) # energy balance values
masbal <- as.data.frame(ecto$masbal) # mass balance value (note most missing if DEB model not running)

# append dates
environ <- cbind(dates, environ)
masbal <- cbind(dates, masbal)
enbal <- cbind(dates, enbal)

############### plot results ######################
forage <- subset(environ, ACT == 2)
bask <- subset(environ, ACT == 1)
night <- subset(metout, ZEN == 90)
day <- subset(metout, ZEN != 90)
with(night, plot(TIME / 60 ~ DOY, ylab = "Hour of Day", xlab = "Day of Year", pch = 15, cex = 2, col = 'dark blue', main = paste('warm =', warm, 'deg C')))
# nighttime hours
with(forage, points(TIME ~ DOY, pch = 15, cex = 2, col = 'orange')) # foraging Tbs
with(bask, points(TIME ~ DOY, pch = 15, cex = 2, col = 'light blue')) # basking Tbs

warming <- 0.1
doys <- unique(metout$DOY)
environ$toohot <- 0
environ$toohot[environ$ACT == 0 & environ$ZEN < 90 & soil$D0cm > T_F_min] <- 1
hr_actual <- aggregate(environ$toohot, by = list(metout$DOY), FUN = sum)[, 2]
Tmaxs <- aggregate(metout$TAREF, by = list(metout$DOY), FUN = max)[, 2]
hr1 <- 6.12 + 0.74 * (Tmaxs - T_pref)
hr1[hr1 < 0] <- 0
text(x = doys, y = 14,labels = round(hr1, 1), col = 'red')
text(x = doys, y = 12,labels = round(hr_actual, 0))
text(x = 25, y = 15, labels = 'Sinervo', col = 'red')
text(x = 25, y = 13, labels = 'NicheMapR', col = 'black')





############################
############################
# FIGURE S7. Activity time (A), metabolic (B), and feeding demand (C) changes when comparing future climate scenarios for nocturnal (grey) and diurnal ectotherms in the Kalahari and the Great Victorian Desert. Responses to climate warming of +2°C (circles) and +4°C (triangles) where climate warming was simulated using TerraClimate data from 1985 to 2015. Change in response variables was derived by the overall average difference between the trait estimations under Recent  conditions and projected trait estimations under each future climate scenario (+2°C or +4°C) for each species. Feeding demand was estimated as described in Figure 3. Due to strong patterns by nocturnal species (grey color), these data were averaged for each activity pattern (nocturnal vs diurnal) across species (Figure 5).  
### READ IN DATA: ANNIUAL AND SEASONAL
ACT_MR_Forage_change_yr_final <- readRDS(here("Raw_data/ACT_MR_Forage_change_yr_final.rds"))

####################
## data prep for Mrate and ACT plots
MR_change_yr_terra_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "M_Rate_J")
ACT_change_yr_terra_ACT_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "ACT_hr")
Forage_change_yr_terra_ACT_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "Feeding_demand")

####################   ####################   ####################   ####################   
#################### Plotting
# Define the order of the species
species_order <- c("C. angulifer", "A. aculeata", "M. suborbitalis", 
                   "P. lineoocellata","T. sparsa", "G. variegata", 
                   "P. minor", "C. isolepis", "C. quatt",
                   "M. horridus")

####################   ####################   ####################   ####################   
#################### ACT Plot
# Function to determine shape
# Create the plot
ACT_change_yr_terra_plot <- ggplot(ACT_change_yr_terra_ACT_plot_dat, 
                                   aes(x = Region, y = Value, 
                                       color = Species, fill = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick2"),
                    limits = species_order) +
  guides(color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =  "(h/yr)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-450, 820), breaks = seq(-400, 800, by = 200)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 800, label = expression(Delta * "Activity Time"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.7, .18),
        legend.text = element_text(size = 20, face = "italic"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA, color = "black", size = .5), # Adding a border
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 18),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 18),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend 
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 

####################   ####################   ####################   ####################   
#################### M Rate Plot
MR_change_yr_terra_plot_dat_plot <-  ggplot(MR_change_yr_terra_plot_dat, 
                                            aes(x = Region, y = Value, 
                                                color = Species, 
                                                fill = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5)+
  scale_shape_identity() + 
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2",
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick2"),  
                    limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(J/yr)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 3000)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 12000, label = expression(Delta * "Metabolic Rate"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 

####################   ####################   ####################   ####################   
#################### Foraging rate change plot 
Forage_yr_terra_plot_dat_plot <-  ggplot(Forage_change_yr_terra_ACT_plot_dat, 
                                         aes(x = Region, y = Value, 
                                             color = Species,
                                             fill = Species)) +
  geom_point(position = position_dodge(width = .5), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5)+
  scale_shape_identity() + 
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2",
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick2"), 
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(J/g/h)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-6, 5), breaks = seq(-6,5, by = 2)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 5, label = expression(Delta * "Feeding Demand"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines'))+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 

####################   ####################   ####################   ####################   
#################### Final ACT/MR plot 25x14 pdf
ACT_M_Rate_Forage_change_yr <- plot_grid(ACT_change_yr_terra_plot,
                                         MR_change_yr_terra_plot_dat_plot,
                                         Forage_yr_terra_plot_dat_plot,
                                         nrow = 1, ncol = 3, 
                                         labels = c("A", "B", "C"))





############################
############################
# FIGURE S8. Results from a simulation in which animals could burrow only to 20cm but could not thermoregulate up and down within the burrow. Historical decadal changes in calculated air temperature (Ta) and calculated body temperature of thermoregulating lizards (Tb) at field locations in the Kalahari (A) and the Great Victorian Desert (B) based on historical climate data (ERA5) between 1950 and 2020. Filled points indicate statistically significant (p <0.05) linear regressions with time. Ta and Tb data (min, mean, max) were summarized by day and then by year (top panels) or by season (bottom panels). Points are jittered for visual clarity. Values above the dashed line indicate an overall increase in the response variable with time and vice versa. 
source('Scripts/Functions/Data_processing_functions.R')
#### Load in contemporary data for each region
# Africa
agac_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Kalahari_Agama_aculeata_X_environ.RDS") %>% 
  mutate(Ww_g = 31.9) # mean mass from field data
chan_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Kalahari_Chondrodactylus_angulifer_A_environ.RDS")%>%
  mutate(Ww_g = 21.2) # mean mass from field data
mesu_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Kalahari_Meroles_suborbitalis_L_environ.RDS") %>%
  mutate(Ww_g = 5.5) # mean mass from field data
peli_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Kalahari_Pedioplanis_lineoocellata_A_environ.RDS")%>%
  mutate(Ww_g = 5.2) # mean mass from field data
trsp_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Kalahari_Trachylepis_sparsa_B_environ.RDS")%>% 
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
ctis_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Australia_Ctenophorus_isolepis_L_environ.RDS")%>% 
  mutate(Ww_g = 5) # mean mass from field data
ctqu_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Australia_Ctenotus_quatt_L_environ.RDS")%>% 
  mutate(Ww_g = 4)# mean mass from field data
geva_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Australia_Gehyra_variegata_L_environ.RDS")%>% 
  mutate(Ww_g = 4)# mean mass from field data
moho_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Australia_Moloch_horridus_R_environ.RDS")%>% 
  mutate(Ww_g = 39)# mean mass from field data
pomi_contemp<-readRDS("output/environ/burrow_20cm_ERA5_70YR_Australia_Pogona_minor_A_environ.RDS")%>% 
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
burrow_20cm_contemp <- rbind(kal_contemp, aus_contemp) 
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
# Ta DATA - all
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
burrow_20cm_contemp_ta_tb_africa <- rbind(contemp_Ta_overall, contemp_Tb_overall_spp)  %>% filter(Region == "Africa")
burrow_20cm_contemp_ta_tb_africa <- burrow_20cm_contemp_ta_tb_africa %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max"
  )) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
saveRDS(burrow_20cm_contemp_ta_tb_africa, "Raw_data/burrow_20cm_contemp_ta_tb_africa.rds")

# final data - Australia
burrow_20cm_contemp_ta_tb_australia <- rbind(contemp_Ta_overall, contemp_Tb_overall_spp)  %>% filter(Region == "Australia")
burrow_20cm_contemp_ta_tb_australia <- burrow_20cm_contemp_ta_tb_australia %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max"
  )) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
saveRDS(burrow_20cm_contemp_ta_tb_australia, "Raw_data/burrow_20cm_contemp_ta_tb_australia.rds")


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by season and species  
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
# Ta by site and season 
aus_ta_season_contemp <- contemp_ta_season(data = aus_contemp) %>% 
  filter(season != "other") %>% 
  mutate(Region = "Australia",
         Species = paste0("Site_AU", Species))
kal_ta_season_contemp <- contemp_ta_season(data = kal_contemp) %>% 
  filter(season != "other") %>% 
  mutate(Region = "Africa",
         Species = paste0("Site_AF", Species))
burrow_20cm_contemp_ta_season_spp <- rbind(kal_ta_season_contemp, aus_ta_season_contemp) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species,Slope, R_2, season,Variable, Correlation, P_value)%>% 
  mutate(Slope_10_Year = Slope*10)



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
# CONTEMPORARY WARMING Tb by species and season
aus_Tb_contemp_spp_season <- Contemp_Tb_season_spp(data = aus_contemp) %>%
  mutate(Region = "Australia", Climate_scenario = "contemporary") %>% 
  filter(season != "other")
kal_Tb_contemp_spp_season <- Contemp_Tb_season_spp(data = kal_contemp) %>% 
  mutate(Region = "Africa", Climate_scenario = "contemporary")%>% 
  filter(season != "other")
burrow_20cm_contemp_tb_season_spp <- rbind(kal_Tb_contemp_spp_season,
                                           aus_Tb_contemp_spp_season)%>%
  mutate(species = Species, across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, season, Slope, R_2, Variable, Correlation, P_value) %>% 
  mutate(Slope_10_Year = Slope*10)


########
#### final df and plot prep by region
burrow_20cm_contemp_ta_tb_season_spp <- rbind(burrow_20cm_contemp_ta_season_spp,
                                              burrow_20cm_contemp_tb_season_spp)
############ SAVE AND READ IN DATA
saveRDS(burrow_20cm_contemp_ta_tb_season_spp, "Raw_data/burrow_20cm_contemp_ta_tb_season_spp.rds")



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
### ACT--- burrow_20cm_Contemporary sum annual activity
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
# ACT year 
# data
Aus_ACT_contemp_yr_spp <- contemp_sum_annual_activity(data = aus_contemp) %>%
  mutate(Test = "ACT", Region = "Australia")
Kal_ACT_contemp_yr_spp <- contemp_sum_annual_activity(data = kal_contemp) %>% 
  mutate(Test = "ACT", Region = "Africa")

burrow_20cm_contemp_ACT_contemp_yr_spp <- rbind(Aus_ACT_contemp_yr_spp, Kal_ACT_contemp_yr_spp)%>%
  filter(Species != "Overall") %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, R_2, Slope_10_Year, Test, P_value)


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
# CONTEMPORARY WARMING Year metabolic rate (J/g/year): species
Aus_MR_Sum_contemp_overall_spp <- contemp_sum_annual_m_rate_J(data = aus_contemp) %>%
  mutate(Test = "M_Rate", Region = "Australia")
Kal_MR_Sum_contemp_overall_spp <- contemp_sum_annual_m_rate_J(data = kal_contemp) %>% 
  mutate(Test = "M_Rate", Region = "Africa")

burrow_20cm_contemp_MR_Sum_overall_spp <- rbind(Aus_MR_Sum_contemp_overall_spp, Kal_MR_Sum_contemp_overall_spp)%>%
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
burrow_20cm_contemp_foraging_success_contemp <- burrow_20cm_contemp %>%
  group_by(species) %>% 
  rowwise() %>% # apply function by row
  mutate(ants_consumed_hourly = get_ants_hourly(m_rate_J_h = m_rate_J_adj)) %>% 
  ungroup()
# save file
# 
saveRDS(burrow_20cm_contemp_foraging_success_contemp, file = "output/foraging_success/burrow_20cm_contemp_foraging_success_contemp.RDS")

####### 
# Read in data that was saved above
burrow_20cm_contemp_foraging_success_contemp <- readRDS("output/foraging_success/burrow_20cm_contemp_foraging_success_contemp.RDS")

####### Filter data for functions: Mrate per hr active and average ants
## data
burrow_20cm_contemp_africa_annual_foraging <- burrow_20cm_contemp_foraging_success_contemp %>% 
  filter(region == "Africa")
burrow_20cm_contemp_australia_annual_foraging <- burrow_20cm_contemp_foraging_success_contemp %>% 
  filter(region == "Australia")



####################
## 1) Feeding demand: mrate (kJ/g/h) per hour active across years
## annual feeding demands - region 
burrow_20cm_contemp_africa_annual_feeding_summary <- contemp_feeding_demand(burrow_20cm_contemp_africa_annual_foraging) %>% 
  mutate(Region = "Africa") 
burrow_20cm_contemp_australia_annual_feeding_summary <- contemp_feeding_demand(burrow_20cm_contemp_australia_annual_foraging)%>%
  mutate(Region = "Australia")

# final df for merge
burrow_20cm_annual_feeding_plot_data <- rbind(burrow_20cm_contemp_africa_annual_feeding_summary, 
                                              burrow_20cm_contemp_australia_annual_feeding_summary) %>%
  select(Region, Species, R_2, Slope_10_Year, P_value) %>% 
  mutate(Test = "Feeding_demand")

############### 
### Final DF and arranging data for plot
burrow_20cm_ACT_MRate_Foraging_contemp <- rbind(burrow_20cm_contemp_ACT_contemp_yr_spp, # annual activity (h)
                                                burrow_20cm_contemp_MR_Sum_overall_spp, # annual metabolic rate (J/g)
                                                burrow_20cm_annual_feeding_plot_data) # annual feeding per hr active (J/g/h)


############ SAVE AND READ IN DATA
saveRDS(burrow_20cm_ACT_MRate_Foraging_contemp, "Raw_data/burrow_20cm_ACT_MRate_Foraging_contemp.rds")






####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary sum seasonal activity 
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
####################   ####################   ####################   #################### 
# ACT Seasonal 
Aus_ACT_contemp_season_spp <- contemp_sum_seasonal_activity(data = aus_contemp) %>%
  mutate(Test = "ACT", Region = "Australia")
Kal_ACT_contemp_season_spp <- contemp_sum_seasonal_activity(data = kal_contemp) %>% 
  mutate(Test = "ACT", Region = "Africa")
burrow_20cm_contemp_ACT_contemp_season_spp <- rbind(Aus_ACT_contemp_season_spp, Kal_ACT_contemp_season_spp)%>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>% 
  select(Region, Species, Season, Test, Slope_10_Year, R_2, P_value) 



####################   ####################   ####################   #################### 
####################   ####################   ####################   #################### 
# Seasonal Mrate
Aus_MR_Sum_contemp_seasonal_spp <- seasonal_annual_m_rate_J(data = aus_contemp) %>%
  mutate(Test = "M_Rate", Region = "Australia")
Kal_MR_Sum_contemp_seasonal_spp <- seasonal_annual_m_rate_J(data = kal_contemp) %>% 
  mutate(Test = "M_Rate", Region = "Africa")
burrow_20cm_contemp_MR_Sum_seasonal_spp <- rbind(Aus_MR_Sum_contemp_seasonal_spp, 
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
burrow_20cm_africa_seasonal_foraging <- burrow_20cm_contemp_foraging_success_contemp %>% 
  filter(region == "Africa" & season!= "other")
burrow_20cm_australia_seasonal_foraging <- burrow_20cm_contemp_foraging_success_contemp %>% 
  filter(region == "Australia"& season!= "other")


####################
## Seasonal feeding summary
africa_seasonal_feeding_summary <- seasonal_feeding_demand(burrow_20cm_africa_seasonal_foraging) %>% 
  mutate(Region ="Africa")
australia_seasonal_feeding_summary <- seasonal_feeding_demand(burrow_20cm_australia_seasonal_foraging)%>% 
  mutate(Region = "Australia")
burrow_20cm_contemp_Foraging_seasonal_spp <- rbind(africa_seasonal_feeding_summary,
                                                   australia_seasonal_feeding_summary) %>% 
  mutate(across(where(is.numeric), ~ round(., 4))) %>% 
  select(Region, Species, Season, R_2, Slope_10_Year, P_value) %>% 
  mutate(Test = "Feeding_demand")



##########
### Final data merge and data prep for plots
burrow_20cm_contemp_ACT_MRate_Foraging_season<- rbind(burrow_20cm_contemp_ACT_contemp_season_spp,
                                                      burrow_20cm_contemp_MR_Sum_seasonal_spp,
                                                      burrow_20cm_contemp_Foraging_seasonal_spp)
saveRDS(burrow_20cm_contemp_ACT_MRate_Foraging_season, "Raw_data/burrow_20cm_contemp_ACT_MRate_Foraging_season.rds")










################################################
# INSET MAPS FOR PLOTS for each contenent 
################################################ 
pacman::p_load(maps, dplyr, tidyr, tidyverse, flextable, knitr, here, ggplot2, purrr, rnaturalearth, rnaturalearthdata, grid, cowplot, simplecolors, ggnewscale, gridExtra, grid, ggbreak, sf)
### Australia map: 
world <- ne_countries(scale = "medium", returnclass = "sf")
australia <- world[world$admin == "Australia", ]
# Create a map grob
australia_map <- ggplot() +
  geom_sf(data = australia, fill = NA, color = "black") + 
  theme_void()
# Convert the map to a grob
australia_grob <- ggplotGrob(australia_map)

### Africa map:
# Load world countries
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world[world$continent == "Africa", ]
# Clean the geometries
africa_clean <- africa %>% st_make_valid()
# Dissolve all internal borders by unifying all geometries into one
africa_unified <- africa_clean %>% 
  st_geometry() %>%
  st_union() %>% 
  st_collection_extract("POLYGON")
# Create a single feature for the continent of Africa
africa_sf <- st_sf(geometry = st_sfc(africa_unified))
# Create a map grob with only the outline
africa_map <- ggplot() +
  geom_sf(data = africa_sf, fill = "white", color = "black") +
  theme_void()

###  Function to add an inset map to a plot
add_inset_map <- function(plot, inset_map, x, y, width, height) {
  inset_grob <- ggplotGrob(inset_map)
  inset_grob <- gtable::gtable_filter(inset_grob, "panel")
  
  plot +
    annotation_custom(
      grob = inset_grob,
      xmin = x, xmax = x + width,
      ymin = y, ymax = y + height
    )
}
# Create a ggplot object for the map
australia_plot <- ggplot(data = australia) +
  geom_sf(fill = "transparent", color = "black") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))
africa_plot <- ggplot(data = africa_sf) +
  geom_sf(fill = "white", color = "black") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Convert the ggplot map objects to grobs
australia_grob <- ggplotGrob(australia_plot)
africa_grob <- ggplotGrob(africa_plot)
# Define viewport for the inset maps
vp <- viewport(x = 0.1, y = 0.95, just = c("left", "top"), 
               width = unit(0.25, "npc"), height = unit(0.25, "npc"))



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by year  
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Figure TA and TB - Africa
contemp_ta_tb_africa <- readRDS("Raw_data/burrow_20cm_contemp_ta_tb_africa.rds") 
# Replace _AF with _Af
contemp_ta_tb_africa$Species <- gsub("_AF", "_Af", contemp_ta_tb_africa$Species)
# Add a "_" after every "f" in the Species column
contemp_ta_tb_africa$Species <- gsub("Site_Af", "Site_Af_", contemp_ta_tb_africa$Species)
# Remove "Site_" from the Species column
contemp_ta_tb_africa$Species <- gsub("Site_", "", contemp_ta_tb_africa$Species)
# Define factor levels
contemp_ta_tb_africa$Combined <- factor(contemp_ta_tb_africa$Combined, 
                                        levels = c("Min Ta", "Mean Ta", "Max Ta", 
                                                   "Min Tb", "Mean Tb",  "Max Tb"))
species_order_AF = c("Af_A", "Af_B", "Af_L", "Af_X",
                     "C. angulifer", "A. aculeata", "M. suborbitalis", "P. lineoocellata",
                     "T. sparsa")
colors <- c( "#9DB9F1", "#4479E4", "#16439C", "#0D2659",
             "grey40","darkgoldenrod3", "coral","green3", "firebrick4")

###########
### Plotting - Africa
# Create the plot with custom legend
Ta_Tb_contemp_africa <- ggplot(contemp_ta_tb_africa, aes(x = Combined, y = Slope_10_Year, 
                                                         color = Species, shape = Significance)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_shape_manual(values = c("p < 0.05" = 16, "p ≥ 0.05" = 1), 
                     guide = FALSE) + 
  scale_color_manual(values =  colors, breaks = species_order_AF) + 
  theme_bw() +
  labs(y = "Temperature Change by Decade (°C)", color = "Site/Species", 
       x = NULL) +
  ylim(0, .5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.18, .70),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA, 
                                         color = "black", 
                                         size = 0.5), # Adjust color and size as needed
        legend.box.spacing = unit(0, 'lines'))  + 
  guides(color = guide_legend(ncol = 2, title.position = "top", )) +  
  annotate("text",  x = 1.4, y = .50, label = "● P-value < 0.05",  size = 5)+
  annotate("text", x = 1.4, y = .47, label = "○ P-value > 0.05", size = 5) +
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 40, unit = "pt"))
Ta_Tb_contemp_africa
# add inset map
Ta_Tb_contemp_africa_final <- add_inset_map(Ta_Tb_contemp_africa, africa_plot, 
                                            x = 5, y = 0.16, 
                                            width = 1.2, height = .5)

Ta_Tb_contemp_africa_final




####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Plotting - Australia
contemp_ta_tb_australia <- readRDS("Raw_data/burrow_20cm_contemp_ta_tb_australia.rds")
# Replace _AU with _Au
contemp_ta_tb_australia$Species <- gsub("_AU", "_Au", contemp_ta_tb_australia$Species)
# Add a "_" after every "f" in the Species column
contemp_ta_tb_australia$Species <- gsub("Site_Au", "Site_Au_", contemp_ta_tb_australia$Species)
# Remove "Site_" from the Species column
contemp_ta_tb_australia$Species <- gsub("Site_", "", contemp_ta_tb_australia$Species)
contemp_ta_tb_australia$Combined <- factor(contemp_ta_tb_australia$Combined, 
                                           levels = c("Min Ta", "Mean Ta", "Max Ta", 
                                                      "Min Tb", "Mean Tb",  "Max Tb"))

species_order_AU = c( "Au_A", "Au_L", "Au_R", 
                      "G. variegata", "P. minor", "C. isolepis", "C. quatt",
                      "M. horridus")
colors <- c( "#9DB9F1", "#4479E4", "#16439C","grey40","darkgoldenrod3", "coral","green3", "firebrick4")
###########
### Plotting - TA/TB australia 
Ta_Tb_contemp_australia <- ggplot(contemp_ta_tb_australia, 
                                  aes(x = Combined, y = Slope_10_Year,
                                      color = Species, shape = Significance)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_shape_manual(values = c("p < 0.05" = 16, "p ≥ 0.05" = 1), 
                     guide = FALSE) + 
  scale_color_manual(values =  colors, breaks = species_order_AU) + 
  theme_bw() +
  labs(y = NULL, color = "Site/Species", 
       x = NULL) +
  ylim(0, .5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.18, .7),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA, 
                                         color = "black", 
                                         size = 0.5), # Adjust color and size as needed
        legend.box.spacing = unit(0, 'lines'))  + 
  guides(color = guide_legend(ncol = 2, title.position = "top", 
                              nrow = 5)) +  
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 40, unit = "pt"))
Ta_Tb_contemp_australia

# add inset map
Ta_Tb_contemp_australia_final <- add_inset_map(Ta_Tb_contemp_australia, australia_plot, 
                                               x = 5, y = 0.16, 
                                               width = 1.2, height = .5)
Ta_Tb_contemp_australia_final


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Ta/Tb contemp year final plot
Ta_Tb_contemp_year_plot <- plot_grid(Ta_Tb_contemp_africa_final,
                                     Ta_Tb_contemp_australia_final,
                                     ncol = 2, nrow = 1, align = "h", 
                                     labels = c("A", "B"),
                                     label_size = 24)
Ta_Tb_contemp_year_plot







####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by season and species  
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   

############ READ IN DATA
contemp_ta_tb_season_spp <- readRDS("Raw_data/burrow_20cm_contemp_ta_tb_season_spp.rds")

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

# Species order for plotting
species_order <- c("G. variegata", "P. minor", "C. isolepis", "C. quatt", "M. horridus",
                   "Site_AUA", "Site_AUL", "Site_AUR")
contemp_ta_tb_season_spp_australia$Species <- factor(contemp_ta_tb_season_spp_australia$Species, 
                                                     levels = species_order)
# Season order
contemp_ta_tb_season_spp_australia$Season <- factor(contemp_ta_tb_season_spp_australia$season, 
                                                    levels = c("winter", "summer", "spring"))
# Determine significance
contemp_ta_tb_season_spp_australia$Significant <- contemp_ta_tb_season_spp_australia$P_value < 0.05

########
# filter season data for plots for each region
contemp_ta_tb_winter_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "winter")
contemp_ta_tb_summer_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "summer")
contemp_ta_tb_spring_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "spring")
contemp_ta_tb_winter_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "winter")
contemp_ta_tb_summer_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "summer")
contemp_ta_tb_spring_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "spring")


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by season and species  Plots
# Colors for plots
semi_transparent_blue <- "#0000FF15"
semi_transparent_red <- "#FF999915"
semi_transparent_green <- "#00FF080F"

####################   ####################   
########   Africa plots Winter and summer
Ta_Tb_contemp_winter_africa_plot <- ggplot(contemp_ta_tb_winter_africa, aes(x = Combined, 
                                                                            y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "summer" = 21)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", "T. sparsa" = "firebrick4", 
                                "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                                "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", "T. sparsa" = "firebrick4", 
                               "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                               "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .6)+
  labs(y = "Temperature Change by Decade (°C)", color = "Species/Site", 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "winter", 
           size = 6,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))



####################   ####################   ####################  
######## Africa Spring
Ta_Tb_contemp_spring_africa_plot <- ggplot(contemp_ta_tb_spring_africa, aes(x = Combined, 
                                                                            y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("spring" = 21, "spring" = 21)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", "T. sparsa" = "firebrick4", 
                                "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                                "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", "T. sparsa" = "firebrick4", 
                               "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                               "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .6)+
  labs(y = NULL, 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "spring", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

####################   ####################   ####################  
######## Africa SUMMER
Ta_Tb_contemp_summer_africa_plot <- ggplot(contemp_ta_tb_summer_africa, aes(x = Combined, 
                                                                            y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("summer" = 21, "summer" = 21)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", "T. sparsa" = "firebrick4", 
                                "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                                "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", "T. sparsa" = "firebrick4", 
                               "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                               "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .6)+
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "summer", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))


####################   ####################   ####################   ####################   
######### Africa Ta/Tb seasonal plot final
Tb_Ta_contemporary_season_africa <- plot_grid(Ta_Tb_contemp_winter_africa_plot,
                                              Ta_Tb_contemp_spring_africa_plot,
                                              Ta_Tb_contemp_summer_africa_plot,
                                              nrow = 1, ncol = 3)



####################   ####################   ####################   ####################      
########  Australia Contemporary warming  Plots - Winter and Summer
#### Australia Winter plot
Ta_Tb_contemp_winter_australia_plot <- ggplot(contemp_ta_tb_winter_australia, aes(x = Combined, 
                                                                                  y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "summer" = 21)) +
  scale_color_manual(values = c("G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral", 
                                "C. quatt" = "green3", "M. horridus" = "firebrick4",
                                "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                                "Site_AUR" = "#16439C")) +
  scale_fill_manual(values = c("G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral", 
                               "C. quatt" = "green3", "M. horridus" = "firebrick4",
                               "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                               "Site_AUR" = "#16439C"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .6)+
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "winter", 
           size = 6,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))


####################   ####################   ####################   ####################   
########  Australia spring
# plot
Ta_Tb_contemp_spring_australia_plot <- ggplot(contemp_ta_tb_spring_australia, aes(x = Combined, 
                                                                                  y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "spring" = 21)) +
  scale_color_manual(values = c("G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral", 
                                "C. quatt" = "green3", "M. horridus" = "firebrick4",
                                "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                                "Site_AUR" = "#16439C")) +
  scale_fill_manual(values = c("G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral", 
                               "C. quatt" = "green3", "M. horridus" = "firebrick4",
                               "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                               "Site_AUR" = "#16439C"), na.value = NA) + 
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .6)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "spring", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

####################   ####################   ####################   ####################   
########  Australia SUMMER
# plot
Ta_Tb_contemp_summer_australia_plot <- ggplot(contemp_ta_tb_summer_australia, aes(x = Combined, 
                                                                                  y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "summer" = 21)) +
  scale_color_manual(values = c("G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral", 
                                "C. quatt" = "green3", "M. horridus" = "firebrick4",
                                "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                                "Site_AUR" = "#16439C")) +
  scale_fill_manual(values = c("G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral", 
                               "C. quatt" = "green3", "M. horridus" = "firebrick4",
                               "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                               "Site_AUR" = "#16439C"), na.value = NA) + 
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .6)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "summer", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

####################   ####################   ####################   ####################   
######## australia Ta/Tb seasonal plot
Tb_Ta_contemporary_season_australia <- plot_grid(Ta_Tb_contemp_winter_australia_plot,
                                                 Ta_Tb_contemp_spring_australia_plot,
                                                 Ta_Tb_contemp_summer_australia_plot,
                                                 nrow = 1, ncol =3)




####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  ***FINAL PLOT CONTEMPORARY CLIMATE: Ta/Tb
# 1700 X1100 -png or eps
# 16 x 12 - pdf 
Figure_S8 <- grid.arrange(
  Ta_Tb_contemp_year_plot,
  Tb_Ta_contemporary_season_africa,
  Tb_Ta_contemporary_season_australia,
  ncol = 2,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)
)
#ggsave("../../Pianka_figs/Figure_2.pdf", Figure_2, device = "pdf", width = 25, height =16)


ggsave("../../Pianka_figs/Figure_S8.pdf", Figure_S8, device = "pdf", width = 26, height =14)






############################
############################
# FIGURE S9. Results from a simulation in which animals could burrow only to 20cm but could not thermoregulate up and down within the burrow. Activity time (A), metabolic (B), and feeding demand (C) changes when comparing future climate scenarios for nocturnal (grey) and diurnal ectotherms in the Kalahari and the Great Victorian Desert. Responses to climate warming of +2°C (circles) and +4°C (triangles) where climate warming was simulated using TerraClimate data from 1985 to 2015. Change in response variables was derived by the overall average difference between the trait estimations under Recent  conditions and projected trait estimations under each future climate scenario (+2°C or +4°C) for each species. Feeding demand was estimated as described in Figure 3. Due to strong patterns by nocturnal species (grey color), these data were averaged for each activity pattern (nocturnal vs diurnal) across species (Figure 5). 
ACT_MRate_Foraging_contemp <- readRDS("Raw_data/burrow_20cm_ACT_MRate_Foraging_contemp.rds") %>% 
  filter(Species != "Overall")

# data prep
ACT_MRate_Foraging_contemp$Test <- factor(ACT_MRate_Foraging_contemp$Test, levels = c("ACT", "M_Rate", "Feeding_demand"))
# Create a new column for significance
ACT_MRate_Foraging_contemp$Significant <- ACT_MRate_Foraging_contemp$P_value < 0.05

# filter data by "Test" for plots
ACT_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "ACT")
MRate_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "M_Rate")
Foraging_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "Feeding_demand")

# Define the order of the species for plots
species_order <- c("C. angulifer", "A. aculeata", "M. suborbitalis", 
                   "P. lineoocellata","T. sparsa", "G. variegata", 
                   "P. minor", "C. isolepis", "C. quatt",
                   "M. horridus")


####################   ####################   ####################   ####################   
########  ACT Plot
# Create the plot
ACT_contemp_plot <- ggplot(ACT_contemp_plot_dat, 
                           aes(x = Region, y = Slope_10_Year, 
                               color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5)  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "h/decade", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-30, 35), breaks = seq(-30, 30, by = 10)) + 
  annotate("text", x = 1.5, y = 33, label = expression(Delta * "Activity time"), 
           fontface = "bold", size = 10.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  annotate("text",  x = 2.05, y = -3, label = "● P-value < 0.05",  size = 5)+
  annotate("text", x = 2.05, y = -6, label = "○ P-value > 0.05", size = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.78, .2),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA, 
                                         color = "black", 
                                         size = 0.5), # Adjust color and size as needed
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2), fill = "none")+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
ACT_contemp_plot


# diel Activity difference between diurnal and nocturnal change for 
overall_ACT_change_value <-  ACT_contemp_plot_dat%>% 
  mutate(diel = if_else(Species %in% c("G. variegata", "C. angulifer"), "nocturnal", # australian sites
                        "diurnal")) %>% 
  group_by(diel) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year))



####################   ####################   ####################   ####################   
######## M Rate Plot
MRate_contemp_plot <- ggplot(MRate_contemp_plot_dat, 
                             aes(x = Region, y = Slope_10_Year, 
                                 color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5)  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "J/g/decade", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-100, 700), breaks = seq(-100, 700, by = 100)) + 
  annotate("text", x = 1.5, y = 690, label = expression(Delta * "Metabolic Rate"), 
           fontface = "bold", size = 10.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2), fill = "none")+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
MRate_contemp_plot

overall_Mrate_change_value <-  ACT_contemp_plot_dat%>% 
  group_by(Region) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year))



####################   ####################   ####################   ####################   
######## Foraging Rate Plot
Foraging_contemp_plot <- ggplot(Foraging_contemp_plot_dat, 
                                aes(x = Region, y = Slope_10_Year, 
                                    color = Species, fill = Species)) + # Assuming 'fill' is mapped to 'Species' as well
  geom_point(position = position_dodge(width = .3), alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 6, stroke = 1.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA, limits = species_order) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA, limits = species_order) +
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3)))  +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  labs(y = "J/g/h/decade", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-.10, .2), breaks = seq(-.1, .20, by = .05)) + 
  annotate("text", x = 1.5, y = .195, label = expression(Delta * "Feeding Demand"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 11), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 11),
                              ncol = 2), fill = "none")+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
Foraging_contemp_plot

overall_feeding_demand_change_value <-  Foraging_contemp_plot_dat%>% 
  filter(Species != "C. angulifer" & Species != "G. variegata" ) %>% 
  group_by(Region) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year))

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Activity and Metabolic rate year final plot
ACT_MRate_Foraging_contemp_plot <- plot_grid(ACT_contemp_plot,
                                             MRate_contemp_plot,
                                             Foraging_contemp_plot,
                                             ncol = 3, nrow = 1, align = "h", 
                                             labels = c("A", "B", "C"), 
                                             label_size = 24)
ACT_MRate_Foraging_contemp_plot


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary sum seasonal activity  Metabolic rate, Feeding rate
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
############  READ IN DATA
contemp_ACT_MRate_Foraging_season <- readRDS("Raw_data/burrow_20cm_contemp_ACT_MRate_Foraging_season.rds")

### arrange data for plots
contemp_ACT_MRate_Foraging_season$Test <- factor(contemp_ACT_MRate_Foraging_season$Test, levels = c("ACT", "M_Rate", "Feeding_demand"))
# Create a new column for significance
contemp_ACT_MRate_Foraging_season$Significant <- contemp_ACT_MRate_Foraging_season$P_value < 0.05

### Filter ACT and MRate data for plots
contemp_ACT_winter_plot_dat <- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "winter"& Test == "ACT")
contemp_ACT_summer_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "summer"& Test == "ACT")
contemp_ACT_spring_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "spring"& Test == "ACT")
contemp_MRate_winter_plot_dat <- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "winter"& Test == "M_Rate")
contemp_MRate_summer_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "summer" & Test == "M_Rate")
contemp_MRate_spring_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "spring" & Test == "M_Rate")
contemp_Foraging_winter_plot_dat <- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "winter"& Test == "Feeding_demand")
contemp_Foraging_summer_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "summer" & Test == "Feeding_demand")
contemp_Foraging_spring_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "spring" & Test == "Feeding_demand")

# plotting colors
semi_transparent_blue<- "#0000FF15"
semi_transparent_red <- "#FF999915"
semi_transparent_green <- "#00FF080F"
# Define the order of the species for plots
species_order <- c("C. angulifer", "A. aculeata", 
                   "M. suborbitalis", "P. lineoocellata",
                   "T. sparsa", "G. variegata", 
                   "P. minor", "C. isolepis", 
                   "C. quatt", "M. horridus")

####################   ####################   ####################   ####################   
######## ACT Winter plots
contemp_ACT_winter_plot <- ggplot(contemp_ACT_winter_plot_dat, 
                                  aes(x = Region, y = Slope_10_Year, 
                                      color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =  "h/decade", 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-0, 20), breaks = c(0, 5, 10, 15, 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 20, label = "winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") + # 1.7 - too far left  
  #annotate("text", x = 1.5, y = 10, 
  #label = "+", 
  #size = 12,
  #fontface = "bold") 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
contemp_ACT_winter_plot


####################   ####################   ####################   ####################   
######## ACT spring plots
contemp_ACT_spring_plot <- ggplot(contemp_ACT_spring_plot_dat, 
                                  aes(x = Region, y = Slope_10_Year, 
                                      color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =NULL, 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-12, 14), breaks = c(-10, -5, 0, 5, 10)) + 
  annotate("text", x = 1.5, y = 14, label = "spring", 
           size = 8,  
           fontface = "bold") +    
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
#annotate("text", x = 1, y = 1.5, label = " -", 
#size = 12,
#fontface = "bold") +
#annotate("text", x = 2, y = 5, label = " +", 
#size = 12,
#fontface = "bold") 
contemp_ACT_spring_plot

####################   ####################   ####################   ####################   
######## ACT Summer plot
contemp_ACT_summer_plot <- ggplot(contemp_ACT_summer_plot_dat, 
                                  aes(x = Region, y = Slope_10_Year, 
                                      color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =  NULL, 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-15, 6), breaks = c(-15,-10, -5, 0, 5)) + 
  annotate("text", x = 1.5, y = 6, label = "summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # This line removes the fill legend
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  #annotate("text", x = 1.5, y = 0, label = "-", 
  #size = 12,
  #fontface = "bold") +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
contemp_ACT_summer_plot

# Seasonal Activity difference
ACT_change_season <-  contemp_ACT_MRate_Foraging_season%>% 
  filter(Test == "ACT") %>% 
  mutate(diel = if_else(Species %in% c("G. variegata", "C. angulifer"), "nocturnal", # australian sites
                        "diurnal")) %>% 
  group_by(Species, Season) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year)) %>% 
  filter(Season == "spring" | Season == "summer")
range(ACT_change_season$ACT_mean_decade)



####################   ####################   ####################   ####################   
######## MRate Winter plots
contemp_MRate_winter_plot <- ggplot(contemp_MRate_winter_plot_dat, 
                                    aes(x = Region, y = Slope_10_Year, 
                                        color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "J/g/decade", 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-180, 350), breaks = seq(-150, 350, by = 50)) +
  annotate("text", x = 1.5, y = 350, label = "winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
#annotate("text", x = 1.5, y = 2.4, label = "+", 
#size = 12,
#fontface = "bold")
contemp_MRate_winter_plot



####################   ####################   ####################   ####################   
######## MRate spring plots
contemp_MRate_spring_plot <- ggplot(contemp_MRate_spring_plot_dat, 
                                    aes(x = Region, y = Slope_10_Year, 
                                        color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-180, 350), breaks = seq(-150, 350, by = 50))  +
  annotate("text", x = 1.5, y = 350, label = "spring", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
contemp_MRate_spring_plot

####################   ####################   ####################   ####################   
######## MRate Summer plot
contemp_MRate_summer_plot <- ggplot(contemp_MRate_summer_plot_dat, 
                                    aes(x = Region, y = Slope_10_Year, 
                                        color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-180, 350), breaks = seq(-150, 350, by = 50))  +
  annotate("text", x = 1.5, y = 350, label = "summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
contemp_MRate_summer_plot



####################   ####################   ####################   ####################   
######## Foraging Winter plots
contemp_Foraging_winter_plot_1 <- ggplot(contemp_Foraging_winter_plot_dat, 
                                         aes(x = Region, y = Slope_10_Year, 
                                             color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  scale_y_continuous(limits = c(-.29, .25), breaks = c(-.25, -.2, -.15, -.1, -.05, 0, 0.05, .1, .15, .2, .25)) +
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
  annotate("text", x = 1.5, y = .25, label = "winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) +  # This line removes the fill legend
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 11.5, unit = "mm")) 
contemp_Foraging_winter_plot_1

contemp_Foraging_winter_plot_2 <- ggplot(contemp_Foraging_winter_plot_dat, 
                                         aes(x = Region, y = Slope_10_Year, 
                                             color = Species)) +
  geom_point(position = position_dodge(width = 0), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  scale_y_continuous(limits = c(-1.99, -.65), breaks = seq(-1.8, -.8, by = .4))  +
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) +  # This line removes the fill legend
  theme(plot.margin = margin(t = 0, r = 2, b = 5, l = 13.8, unit = "mm")) 
#annotate("text", x = 1, y = -16.05, label = "-", 
#size = 12,  # Size 8 in ggplot2 corresponds to 20pt font size
#fontface = "bold")
contemp_Foraging_winter_plot_2

# Final
contemp_Foraging_winter_combined_plot <- plot_grid(
  contemp_Foraging_winter_plot_1, 
  contemp_Foraging_winter_plot_2,
  nrow = 2,  # Specify two rows
  rel_heights = c(2, 1)  # First plot has double the height of the second
) 

# adding y axis
contemp_Foraging_winter_plot_final <- ggdraw() +
  draw_plot(contemp_Foraging_winter_combined_plot) +
  draw_label("J/g/h/decade", 
             x = 0, 
             y = 0.6, 
             hjust = 0.5, 
             vjust = 2,
             angle = 90, # Rotate the label for y-axis
             size = 14) # Adjust size according to your preference
contemp_Foraging_winter_plot_final


####################   ####################   ####################   ####################   
######## Foraging spring plots
contemp_Foraging_spring_plot <- ggplot(contemp_Foraging_spring_plot_dat, 
                                       aes(x = Region, y = Slope_10_Year, 
                                           color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  annotate("text", x = 1.5, y = .32, label = "spring", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  scale_y_continuous(limits = c(-.2, .32), breaks = c( -.2, -.1, 0, .1, .2, .3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Dashed line at y = 0
#annotate("text", x = 1, y = .60, label = "+", 
#size = 12,
#fontface = "bold") 
contemp_Foraging_spring_plot


####################   ####################   ####################   ####################   
######## Foraging Summer plot
contemp_Foraging_summer_plot <- ggplot(contemp_Foraging_summer_plot_dat, 
                                       aes(x = Region, y = Slope_10_Year, 
                                           color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral", 
                                "P. lineoocellata" = "green3", 
                                "T. sparsa" = "firebrick4",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral", 
                                "C. quatt" = "green3", 
                                "M. horridus" = "firebrick4"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral", 
                               "P. lineoocellata" = "green3", 
                               "T. sparsa" = "firebrick4",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral", 
                               "C. quatt" = "green3", 
                               "M. horridus" = "firebrick4"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  scale_y_continuous(limits = c(-.2, .32), breaks = c( -.2, -.1, 0, .1, .2, .3)) +
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x = 1.5, y = .32, label = "summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
#annotate("text", x = 1, y = 0.5, label = "+", 
#size = 12,
#fontface = "bold") 
contemp_Foraging_summer_plot

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## ACT/Mrate seasonal plots
ACT_contemporary_season_plot <- plot_grid(contemp_ACT_winter_plot,
                                          contemp_ACT_spring_plot,
                                          contemp_ACT_summer_plot,
                                          nrow = 1, ncol = 3)

MRate_contemporary_season_plot <- plot_grid(contemp_MRate_winter_plot,
                                            contemp_MRate_spring_plot,
                                            contemp_MRate_summer_plot,
                                            nrow = 1, ncol = 3)

Foraging_contemporary_season_plot <- plot_grid(contemp_Foraging_winter_plot_final,
                                               contemp_Foraging_spring_plot,
                                               contemp_Foraging_summer_plot,
                                               nrow = 1, ncol = 3)




####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  ***FINAL PLOT CONTEMPORARY CLIMATE: Activity and Metabolic rate
# save: 2200 X 1200
# 26X14 PDF
Figure_S9 <-  grid.arrange(
  ACT_MRate_Foraging_contemp_plot,
  ACT_contemporary_season_plot,
  MRate_contemporary_season_plot,
  Foraging_contemporary_season_plot,
  ncol = 3,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 1, 2, 3, 4), 
                         nrow = 2, ncol = 3, byrow = TRUE)
)
ggsave("../../Pianka_figs/Figure_S9.pdf", Figure_S9, device = "pdf", width = 26, height =14)

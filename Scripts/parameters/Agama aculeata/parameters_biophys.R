library(dplyr)

# bring in thermal and morph information from Summary_sheets folder
thermal_morph <- read.csv(file = "Summary_sheets/KL_Thermal_beh_traits_by_Spp.csv") %>% 
  filter(Genus_species== "Agama_aculeata")

shape <- 3 # lizard
Ww_g <- thermal_morph$mass_g # avg in between W_p and W_i
CT_min <- thermal_morph$CT_min # critical thermal minimum (deg C)
T_RB_min <- thermal_morph$T_RB_min # minimum emergence (retreat to bask) body temperature (deg C) observed in field 
T_B_min <- thermal_morph$T_B_min # minimum basking body temperature (deg C) observed in field -
T_F_min <- thermal_morph$T_F_min # minimum feeding/foraging body temperature (deg C) - 
T_pref <- thermal_morph$Tpref # preferred body temperature (deg C) - 
T_F_max <- thermal_morph$T_F_max # maximum feeding/foraging temperature (deg C) 
CT_max <-  thermal_morph$CT_max  # critical thermal maximum (deg C); T_F_max

# behav - check table for descriptions(eg. diel patterns), skin properties, and water loss
alpha_min <- thermal_morph$ABSMIN #
alpha_max <- thermal_morph$ABSMAX #
diurn <- 1
nocturn <- 0
crepus <- 0
shade_seek <- 0
burrow <- 1
climb <- 1
shdburrow <- 2
mindepth <- 1
maxdepth <- 6
pct_wet <- 0.02 
pct_eyes <- 0.03

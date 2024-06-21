library(dplyr)

# bring in thermal and morph information from Summary_sheets folder
thermal_morph <- read.csv(file = "Summary_sheets/KL_Thermal_beh_traits_by_Spp.csv") %>% 
  filter(Genus_species== "Chondrodactylus_angulifer")

shape <- 3 # lizard
Ww_g <- thermal_morph$mass_g #  avg in between W_p and W_i
CT_min <- thermal_morph$CT_min # critical thermal minimum (deg C)
T_RB_min <- thermal_morph$T_RB_min # minimum emergence (retreat to bask) body temperature (deg C) observed in field  - +3 CTmin
T_B_min <- thermal_morph$T_RB_min # minimum basking body temperature (deg C) observed in field - ******* Tb_min value is smaller than RB CHECK
T_F_min <- thermal_morph$T_F_min # minimum feeding/foraging body temperature (deg C) - 
T_pref <- thermal_morph$Tpref # preferred body temperature (deg C) - 
T_F_max <- thermal_morph$T_F_max # maximum feeding/foraging temperature (deg C) 
CT_max <- thermal_morph$CT_max # T_F_max# critical thermal maximum (deg C) 

# behav - check table for descriptions(eg. diel patterns), skin properties, and water loss
alpha_min <- thermal_morph$ABSMIN # C. bibronii; Clusella-Trullas unpub.
alpha_max <- thermal_morph$ABSMAX # C. bibronii; Clusella-Trullas unpub.
diurn <- 0
nocturn <- 1
crepus <- 0
shade_seek <- 0
burrow <- 1
climb <- 0
shdburrow <- 2
mindepth <- 1
maxdepth <- 6
pct_wet <- 0.02 
pct_eyes <- 0.03

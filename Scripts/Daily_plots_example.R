############################################
############################################
###### Daily checks recent simulation data 
###### and recent simulation where shade is zero 
###### which is what Sinervo et al. 2010
############ bring in functions
source('Scripts/Functions/act_regressions.R') # annual & seasonal regressions
source('Scripts/Functions/day_checks.R') # daily plotting & annual Tb differences

# recent simulation data 
pomi_norm <- readRDS("output/environ/ERA5_70YR_Australia_Pogona_minor_A_environ.RDS")
ctis_norm <- readRDS("output/environ/ERA5_70YR_Australia_Ctenophorus_isolepis_L_environ.RDS")
ctqu_norm <- readRDS("output/environ/ERA5_70YR_Australia_Ctenotus_quatt_L_environ.RDS")
moho_norm <- readRDS("output/environ/ERA5_70YR_Australia_Moloch_horridus_R_environ.RDS")
ghva_norm <- readRDS("output/environ/ERA5_70YR_Australia_Gehyra_variegata_L_environ.RDS")
agac_norm<-readRDS("output/environ/ERA5_70YR_Kalahari_Agama_aculeata_X_environ.RDS")  
chan_norm<-readRDS("output/environ/ERA5_70YR_Kalahari_Chondrodactylus_angulifer_A_environ.RDS")
mesu_norm<-readRDS("output/environ/ERA5_70YR_Kalahari_Meroles_suborbitalis_L_environ.RDS") 
peli_norm<-readRDS("output/environ/ERA5_70YR_Kalahari_Pedioplanis_lineoocellata_A_environ.RDS")
trsp_norm<-readRDS("output/environ/ERA5_70YR_Kalahari_Trachylepis_sparsa_B_environ.RDS") 



####### Pogona minor example ########
###############
#### check regression net annual and seasonal
# annual
net_activity_plot(pomi_norm, plot_title = "P. minor normal simulation")
# seasonal 
seasonal_activity_plots(pomi_norm, plot_title = "P. minor normal simulation")


###############
#### day plots looks 
# like 2001 they were moving a bunch
daily_checks_plot(pomi_norm, '2001-01-01', title_suffix = "normal simulation")








########################################################################################
########################################################################################
net_activity_plot(ctqu_norm, plot_title = "C. quatt normal simulation") # normal
net_activity_plot(ctqu_zero_shade, plot_title = "C. quatt zero shade simulation") # shade
# looks like in the normal simulation they move more overall, but
# the rate of change is higher with the zero shade example. So maybe
# figure 3 plots arent the right way to look at it because 
# it's decade change in activity and not 'sum' of activity. Just the rate is changing? 

# seasonal 
seasonal_activity_plots(ctqu_norm, plot_title = "C. quatt normal simulation")
seasonal_activity_plots(ctqu_zero_shade, plot_title = "C. quatt zero shade simulation")
# so here again they are starting off with not much activity under zero shade (e.g. summer),
# if you compare summer change between 'normal' simulation vs 'zero shade' simulations the
# regression lines are going opposite directions but the shade activity values are nowhere 
# near as high of activity where our 'normal' simulation values(mean = ~3796hr) Vs
# 'zero shade' simulation value (mean = 3182hr)


###############
#### day plots looks 
# like 2001 they were moving a bunch
daily_checks_plot(ctqu_norm, '2001-01-01', title_suffix = "normal simulation")
daily_checks_plot(ctqu_zero_shade, '2001-01-01', title_suffix = "zero shade simulation")
plot_temperature_summary(ctqu_norm, ctqu_zero_shade)



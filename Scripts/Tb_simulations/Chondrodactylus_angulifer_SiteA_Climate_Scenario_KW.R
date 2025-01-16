source('scripts/Functions/run_sim_long.R')
source('scripts/Functions/plot_climate_scenario.R')
source('scripts/Functions/plot_Ta_Tb_time.R')

plottime <- FALSE # plot time series?
burrow_bask <- 0 # Tb delta to put in shallow burrow

genus <- 'Chondrodactylus'
species <- 'angulifer'
loadlizard <- FALSE

####################################
# microclimate
region <- 'Kalahari'
site <- 'A'
ystart <- 1985
yfinish <- 2015
maxshade <- 50
warm <- 0 
Usrhyt <- 0.015
cap <- 0
ERR <- 1.5 
loadmicro <- TRUE


####################################
# current scenario = 0
scenario <- 0 
current_out <- run_sim_long(genus = genus,
                            species = species,
                            region = region,
                            site = site,
                            ystart = ystart,
                            yfinish = yfinish,
                            maxshade = maxshade,
                            warm = warm,
                            scenario = scenario,
                            Usrhyt = Usrhyt,
                            #tree = tree,
                            cap = cap,
                            ERR = ERR,
                            loadmicro = loadmicro,
                            loadlizard = loadlizard,
                            soilnode = 2)

current_micro <- current_out$micro
current_ecto <- current_out$ecto
current_dates <- current_micro$dates
# retrieve output
current_environ <- as.data.frame(current_ecto$environ) %>%  # beh, Tb and env
  mutate(climate_scenario = "current") 
current_enbal <- as.data.frame(current_ecto$enbal) %>% # heat balance outputs 
  mutate(climate_scenario = "current") 
current_masbal <- as.data.frame(current_ecto$masbal) %>% # mass balance outputs
  mutate(climate_scenario = "current")
current_metout <- as.data.frame(current_micro$metout) %>%  # above ground micro
  mutate(climate_scenario = "current")
current_environ <- cbind(current_environ,current_metout$SOLR, current_metout$TAREF) # add solar rad and Taref for plots
current_environ <- current_environ %>% dplyr::rename(Solar = "current_metout$SOLR",
                                                     TAREF = "current_metout$TAREF")

# add mass bal
current_environ <- cbind(current_environ, current_masbal)

# add dates
current_environ['dates'] <- current_dates
current_enbal['dates'] <- current_dates
current_masbal['dates'] <- current_dates

# add final lables
current_environ <- current_environ %>%
  mutate(region = region,
         site = site,
         genus = genus,
         species = species)


####################################
# future scenario = 2
scenario = 2
future_2_out <- run_sim_long(genus = genus,
                             species = species,
                             region = region,
                             site = site,
                             ystart = ystart,
                             yfinish = yfinish,
                             maxshade = maxshade,
                             warm = warm,
                             scenario = scenario,
                             Usrhyt = Usrhyt,
                             #tree = tree,
                             cap = cap,
                             ERR = ERR,
                             loadmicro = loadmicro,
                             loadlizard = loadlizard,
                             soilnode = 2)

future_2_micro <- future_2_out$micro
future_2_ecto <- future_2_out$ecto
future_2_dates <- future_2_micro$dates
# retrieve output
future_2_environ <- as.data.frame(future_2_ecto$environ) %>%  # beh, Tb and env
  mutate(climate_scenario = "future_2") 
future_2_enbal <- as.data.frame(future_2_ecto$enbal) %>% # heat balance outputs 
  mutate(climate_scenario = "future_2") 
future_2_masbal <- as.data.frame(future_2_ecto$masbal) %>% # mass balance outputs
  mutate(climate_scenario = "future_2")
future_2_metout <- as.data.frame(future_2_micro$metout) %>%  # above ground micro
  mutate(climate_scenario = "future_2")
future_2_environ <- cbind(future_2_environ,future_2_metout$SOLR, future_2_metout$TAREF) # add solar rad and Taref for plots
future_2_environ <- future_2_environ %>% dplyr::rename(Solar = "future_2_metout$SOLR",
                                                       TAREF = "future_2_metout$TAREF")
# add mass bal
future_2_environ <- cbind(future_2_environ, future_2_masbal)

# add dates
future_2_environ['dates'] <- future_2_dates
future_2_enbal['dates'] <- future_2_dates
future_2_masbal['dates'] <- future_2_dates

# add final lables
future_2_environ <- future_2_environ %>%
  mutate(region = region,
         site = site,
         genus = genus,
         species = species)
####################################
# future scenario = 4
scenario = 4
future_4_out <- run_sim_long(genus = genus,
                             species = species,
                             region = region,
                             site = site,
                             ystart = ystart,
                             yfinish = yfinish,
                             maxshade = maxshade,
                             warm = warm,
                             scenario = scenario,
                             Usrhyt = Usrhyt,
                             #tree = tree,
                             cap = cap,
                             ERR = ERR,
                             loadmicro = loadmicro,
                             loadlizard = loadlizard,
                             soilnode = 2)

future_4_micro <- future_4_out$micro
future_4_ecto <- future_4_out$ecto
future_4_dates <- future_4_micro$dates


####################################
# retrieve, merge for analysis and plots
# retrieve output
future_4_environ <- as.data.frame(future_4_ecto$environ) %>%  # beh, Tb and env
  mutate(climate_scenario = "future_4") 
future_4_enbal <- as.data.frame(future_4_ecto$enbal) %>% # heat balance outputs 
  mutate(climate_scenario = "future_4") 
future_4_masbal <- as.data.frame(future_4_ecto$masbal) %>% # mass balance outputs
  mutate(climate_scenario = "future_4")
future_4_metout <- as.data.frame(future_4_micro$metout) %>%  # above ground micro
  mutate(climate_scenario = "future_4")
future_4_environ <- cbind(future_4_environ,future_4_metout$SOLR, future_4_metout$TAREF) # add solar rad and Taref for plots
future_4_environ <- future_4_environ %>% dplyr::rename(Solar = "future_4_metout$SOLR",
                                                       TAREF = "future_4_metout$TAREF")
# add mass bal
future_4_environ <- cbind(future_4_environ, future_4_masbal)

# add dates
future_4_environ['dates'] <- future_4_dates
future_4_enbal['dates'] <- future_4_dates
future_4_masbal['dates'] <- future_4_dates

# add final labels
future_4_environ <- future_4_environ %>%
  mutate(region = region,
         site = site,
         genus = genus,
         species = species)

# merge
environ <- rbind(current_environ, future_2_environ, future_4_environ)
# arrange dates and time
environ$hr <- lubridate::hour(environ$dates)
environ$date <- lubridate::date(environ$dates)
environ$yr <- lubridate::year(environ$dates)

### reorganize year so that we're not splitting summer or winter months
environ$month <- lubridate::month(environ$dates)
environ$year <- ifelse(environ$month < 10, 
                       environ$yr - 1, environ$yr) 
# !!!! limited data for 2015 because of grouping - some weird summaries for plots
environ <- environ %>% filter(year<2015)

# merge above ground microclimate
metout <- rbind(current_metout, future_2_metout, future_4_metout)
metout["dates"] <- current_dates
# arrange dates and time
metout$hr <- lubridate::hour(metout$dates)
metout$date <- lubridate::date(metout$dates)
metout$yr <- lubridate::year(metout$dates)

### reorganize year so that we're not splitting summer or winter months
metout$month <- lubridate::month(metout$dates)
metout$year <- ifelse(metout$month < 10, 
                      metout$yr - 1, metout$yr) 
# !!!! limited data for 2015 because of grouping - some weird summaries for plots
metout <- metout %>% filter(year<2015)


# save environ ouptut
# 
saveRDS(environ, file = paste0("output/climate_scenarios/","climate_scenario",region, "_",genus,"_",species,"_",site,"_environ.RDS"))

source('scripts/Functions/run_sim_long.R')
source('scripts/Functions/plot_Tb_ERA5_70_year.R')
source('scripts/Functions/plot_Ta_Tb_time.R')

plottime <- FALSE # plot time series?
burrow_bask <- 0 # Tb delta to put in shallow burrow

genus <- 'Ctenotus'
species <- 'quatt'
loadlizard <- FALSE


####################################
# microclimate
region <- 'Australia'
site <- 'L'
ystart <- 1950
yfinish <- 2021
maxshade <- 50
warm <- 0 # warm offset - different than climate scenario
Usrhyt <- 0.005
scenario <- 0
cap <- 0
ERR <- 1.5 
loadmicro <- TRUE

####################################
# ERA5 simulation 1950-2020
out <- run_sim_long(genus = genus,
                            species = species,
                            region = region,
                            site = site,
                            ystart = ystart,
                            yfinish = yfinish,
                            maxshade = maxshade,
                            warm = warm,
                            scenario = scenario,
                            Usrhyt = Usrhyt,
                            cap = cap,
                            ERR = ERR,
                            loadmicro = loadmicro,
                            loadlizard = loadlizard,
                            soilnode = 2)
micro <- out$micro
ecto <- out$ecto
dates <- micro$dates
# retrieve output
environ <- as.data.frame(ecto$environ) 
enbal <- as.data.frame(ecto$enbal) 
masbal <- as.data.frame(ecto$masbal) 
metout <- as.data.frame(micro$metout) 
environ <- cbind(environ, metout, masbal, enbal)

# add dates
environ['dates'] <- dates
environ$hr <- lubridate::hour(environ$dates)
environ$date <- lubridate::date(environ$dates)
environ$yr <- lubridate::year(environ$dates)

### reorganize year so that we're not splitting summer or winter months
environ$month <- lubridate::month(environ$dates)
environ$year <- ifelse(environ$month < 10, 
                       environ$yr - 1, environ$yr) 

# filter years between 1950-2020
environ <- environ %>% filter(year<2021 & year >1949)
min(environ$year)
max(environ$year)

# add site info
environ <- environ %>% 
  mutate(region = region,
         site = site,
         genus = genus,
         species = species)

# save environ ouptut
# 
saveRDS(environ, file = paste0("output/environ/","ERA5_70YR_",region, "_",genus,"_",species,"_",site,"_environ.RDS"))

run_sim_long <- function(genus = 'Meroles',
                    species = 'suborbitalis',
                    region = 'Kalahari',
                    site = 'L',
                    ystart = 1965,
                    yfinish = 1965,
                    minshade = 0,
                    maxshade = 50,
                    warm = 0,
                    scenario = 0,
                    Usrhyt = 0.005,
                    cap = 0,
                    ERR = 1.5,
                    slope = 0,
                    aspect = 0,
                    windfac = 1,
                    wind.mult = 1,
                    tree = 0,
                    loadmicro = TRUE,
                    loadlizard = FALSE,
                    soilnode = 2){
  
  source('scripts/Functions/sim_micro.R')
  source('scripts/Functions/sim_lizard.R')
  
  sites <- read.csv("Sites/Aus_KL_Sites.csv")
  site_name <- paste0(region, "_", site)
  location <- subset(sites, Site.name == site_name)
  loc <- c(location$Long, location$Lat)
  
  micro <- sim_micro(region = region,
                     site = site,
                     loc = loc,
                     ystart = ystart,
                     yfinish = yfinish,
                     minshade = minshade,
                     maxshade = maxshade,
                     warm = warm,
                     scenario = scenario,
                     Usrhyt = Usrhyt,
                     cap = cap,
                     ERR = ERR,
                     slope = slope,
                     aspect = aspect,
                     windfac = windfac,
                     loadmicro = loadmicro)

  ecto <- sim_lizard(genus = genus,
                     species = species,
                     region = region,
                     site = site,
                     loc = loc,
                     ystart = ystart,
                     yfinish = yfinish,
                     maxshade = maxshade,
                     warm = warm,
                     scenario = scenario,
                     Usrhyt = Usrhyt,
                     cap = cap,
                     ERR = ERR,
                     wind.mult = wind.mult,
                     tree = tree,
                     slope = slope,
                     aspect = aspect,
                     loadlizard = loadlizard)
  
  environ <- as.data.frame(ecto$environ)
  dates <- micro$dates
  
  return(list(environ = environ, micro = micro, ecto = ecto))
}
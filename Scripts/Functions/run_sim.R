run_sim <- function(genus = 'Meroles',
                    species = 'suborbitalis',
                    region = 'Kalahari',
                    site = 'L',
                    ystart = 1965,
                    yfinish = 1965,
                    minshade = 0,
                    maxshade = 50,
                    maxshade.lizard = 50,
                    warm = 0,
                    scenario = 0,
                    Usrhyt = 0.005,
                    canopy_height = 0,
                    cap = 0,
                    ERR = 1.5,
                    slope = 0,
                    aspect = 0,
                    wind.mult = 1,
                    tree = 0,
                    windfac = 1,
                    soilnode = 2,
                    loadmicro = TRUE,
                    loadlizard = FALSE){
  
  source('scripts/Functions/sim_micro.R')
  source('scripts/Functions/sim_lizard.R')
  source('scripts/Functions/merge_obs_Kalahari.R')
  source('scripts/Functions/merg_obs_Australia.R')
  
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
                     canopy_height = canopy_height,
                     cap = cap,
                     ERR = ERR,
                     slope = slope,
                     aspect = aspect,
                     loadmicro = loadmicro,
                     windfac = windfac)
  
  micro_1m <- sim_micro(region = region,
                        site = site,
                        loc = loc,
                        ystart = ystart,
                        yfinish = yfinish,
                        minshade = minshade,
                        maxshade = maxshade,
                        warm = warm,
                        scenario = scenario,
                        Usrhyt = 1,
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
                     maxshade.lizard = maxshade.lizard,
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
  
  # Conditional execution based on the region value
  if(region == "Australia") {
    merged_data <- merge_obs_Australia(site = site,
                                       loc = loc,
                                       genus = genus,
                                       species = species,
                                       ystart = ystart,
                                       yfinish = yfinish,
                                       micro = micro_1m,
                                       environ = environ,
                                       soilnode = soilnode)
  } else {
    merged_data <- merge_obs_Kalahari(site = site,
                                      loc = loc,
                                      genus = genus,
                                      species = species,
                                      ystart = ystart,
                                      yfinish = yfinish,
                                      micro = micro_1m,
                                      environ = environ,
                                      soilnode = soilnode)
  }
  
  environ <- merged_data[[1]]
  merged <- merged_data[[2]]
  return(list(environ = environ, merged = merged, micro = micro, ecto = ecto, micro_1m = micro_1m))
}
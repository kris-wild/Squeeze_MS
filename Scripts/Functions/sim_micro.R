sim_micro <- function(region = 'Kalahari',
                      site = 'A',
                      loc = c(20.70, -27.340),
                      ystart = 1970,
                      yfinish = 1970,
                      minshade = 0,
                      maxshade = 50,
                      warm = 0,
                      scenario = 0,
                      Usrhyt = 0.005,
                      canopy_height = 0,
                      cap = 0,
                      ERR = 1.5,
                      slope = 0,
                      aspect = 0,
                      windfac = 1,
                      loadmicro = FALSE){
  pacman::p_load("NicheMapR", "mcera5", "knitr", "lubridate")
  if(slope == 0){
    microfile <- paste0('output/microclimates/micro_', region, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', scenario,'scenario_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap.Rda')
  }else{
    microfile <- paste0('output/microclimates/micro_', region, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', scenario,'scenario_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap_', slope, 'slp_', aspect, 'asp.Rda')
  }
  if(loadmicro) {
    micro <- readRDS(file = microfile)
  } else {
    # Check if ystart is after 79 and region is Australia, or if ystart is 1950
    if(ystart > 1979 && region == 'Australia') {
      spatial_folder <- paste0('ERA5/', region, '_ERA5/ERA5')  # Replace with the correct folder name
    } else if (ystart == 1950) {
      spatial_folder <- paste0('ERA5/', region, '_ERA5/ERA5')
    } else {
      spatial_folder <- paste0('ERA5/', region, '_ERA5/ERA5')}
    ZM <- 0.1 * canopy_height
    d <- 0.5 * canopy_height
    ZH <- 0.2 * ZM # heat transfer roughness height (m) for Campbell and Norman air temperature/wind speed profile (invoked if greater than 1, 0.02 * canopy height in m if unknown)
    D0 <- d # zero plane displacement correction factor (m) for Campbell and Norman air temperature/wind speed profile (0.6 * canopy height in m if unknown)
    micro <- micro_era5(windfac = windfac, 
                        warm = warm,
                        scenario = scenario,
                        minshade = minshade,
                        maxshade = maxshade, 
                        loc = loc, 
                        dstart = paste0('01/01/', ystart), 
                        dfinish = paste0('31/12/', yfinish), 
                        Usrhyt = Usrhyt,
                        ZH = ZH,
                        D0 = D0,
                        spatial = spatial_folder, 
                        cap = cap,
                        ERR = ERR, 
                        slope = slope,
                        rainhour = 1,
                        soilgrids = 1,
                        snowmodel = 0)

    # need to change the dates from UTC
    longlat <- loc
    if(sign(loc[1]) == -1){
      gmtzone <- "+"
    }else{
      gmtzone <- ""
    }
    tz <- paste0("Etc/GMT", gmtzone, round(loc[1]/15*-1, 0))
    attr(micro$dates, "tzone") <- tz
    
    saveRDS(micro, file = microfile)
  }
  return(micro)
}
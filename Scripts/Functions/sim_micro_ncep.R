sim_micro <- function(region = 'Kalahari',
                      site = 'A',
                      loc = c(20.70, -27.340),
                      ystart = 1970,
                      yfinish = 1970,
                      maxshade = 50,
                      warm = 0,
                      Usrhyt = 0.005,
                      cap = 0,
                      loadmicro = FALSE){
  pacman::p_load("NicheMapR", "mcera5", "knitr", "lubridate")
  microfile <- paste0('output/microclimates/micro_', region, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap_NCEP.Rda')
  if(loadmicro){
    micro <- readRDS(file = microfile) 
  }else{
    micro <- micro_ncep(windfac = 1, 
                      warm = warm,
                      maxshade = maxshade, 
                      loc = loc, 
                      dstart = paste0('01/01/', ystart), 
                      dfinish = paste0('31/12/', yfinish), 
                      Usrhyt = Usrhyt, 
                      spatial = '/srv/6300-predecol/data/NCEP', 
                      cap = cap, 
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
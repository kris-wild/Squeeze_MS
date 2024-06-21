merge_obs_Kalahari <- function(site = site,
                         loc = loc,
                         genus = genus,
                         species = species,
                         ystart = 1970,
                         yfinish = 1970,
                         environ = environ,
                         micro = micro,
                         soilnode = 2){
  
  longlat <- loc
  if(sign(loc[1]) == -1){
    gmtzone <- "+"
  }else{
    gmtzone <- ""
  }
  tz <- paste0("Etc/GMT", gmtzone, round(loc[1]/15*-1, 0))
  
  Kalahari_Tb <- read.csv(file = "Raw_data/kalahari.csv") %>% 
    arrange(species) %>%
    rename(Genus_species = species,
           Site = site,
           BT = bt,
           AT = at,
           Time = time_orig) %>% 
    mutate(date = dmy(date, tz = "Etc/GMT-1"),
           year = year(date),
           year_cat = as.factor(year)) %>%
    filter(!is.na(BT)) %>% 
    filter(Site == paste0("KL", site), 
           year %in% ystart:yfinish,
           Genus_species == paste(genus, species))
  ####
  # date time column - Convert 'Time' to 'HH:MM:SS' format
  Kalahari_Tb$time_formatted <- sprintf('%04d', Kalahari_Tb$Time) 
  Kalahari_Tb$time_formatted <- substr(Kalahari_Tb$time_formatted, 1, 2) %>%
    paste0(':', substr(Kalahari_Tb$time_formatted, 3, 4), ':00')
  # Combine 'date' and 'time_formatted' into a single datetime column
  Kalahari_Tb$datetime <- paste(Kalahari_Tb$date, Kalahari_Tb$time_formatted)
  # Convert the datetime character column to POSIXct with GMT+1 timezone
  Kalahari_Tb$dates <- as.POSIXct(Kalahari_Tb$datetime, format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT-1')
  Kalahari_Tb$dates <- round_date(Kalahari_Tb$dates, unit = "hour")
  
  dates <- as.data.frame(micro$dates)
  metout <- as.data.frame(micro$metout) # file with TA Ref ~2m
  soil <- as.data.frame(micro$soil)
  
  # make df with dates and temps and arrange cols for filter
  environ['dates'] <- dates
  environ$hr <- lubridate::hour(environ$dates)
  environ$date <- lubridate::date(environ$dates)
  
  # cbind TA Ref from microclimate and rename TAREF col
  environ <- cbind(environ, metout$TALOC) 
  environ <- environ %>% 
    rename(TA1m = `metout$TALOC`)
  environ <- cbind(environ, metout$SOLR) 
  environ <- environ %>% 
    rename(SOLR = `metout$SOLR`)
  environ <- cbind(environ, soil[, soilnode + 2]) 
  colnames(environ)[ncol(environ)] <- "Tsoil"
  
  # environ and Pianka data merge from site A
  Kalahari_Tb <- merge(environ, Kalahari_Tb, by="dates", all=TRUE) 
  Kalahari_Tb$datetime2 <- as.POSIXct(Kalahari_Tb$datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  return(list(environ = environ, Kalahari_Tb = Kalahari_Tb))
}
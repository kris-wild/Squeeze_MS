get_ERA5 <- function(loc = loc, dstart = dstart, dfinish = dfinish, spatial = spatial){
  library(microclima)
  library(mcera5)
  lat <- loc[2]
  long <- loc[1]
  dem.res <- 30
  pixels <- 100
  dem <- microclima::get_dem(lat = lat, long = long, resolution = dem.res, xdims = pixels, ydims = pixels) # mercator equal area projection
  tme <- seq(as.Date(dstart, format = "%d/%m/%Y"), as.Date(dfinish, format = "%d/%m/%Y"), "days")
  dstart.splt <- strsplit(dstart, '/')
  dfinish.splt <- strsplit(dfinish, '/')
  st_time <- lubridate::ymd(paste0(dstart.splt[[1]][3], ":", dstart.splt[[1]][2], ":", dstart.splt[[1]][1]))
  en_time <- lubridate::ymd(paste0(dfinish.splt[[1]][3], ":", dfinish.splt[[1]][2], ":", dfinish.splt[[1]][1]))
  years <- as.numeric(unique(format(tme, "%Y")))
  longitude <- loc[1]
  latitude <- loc[2]
  
  if(length(years)==1){
    hourlydata <- mcera5::extract_clim(nc = paste0(spatial, '_', years, '.nc'), long = loc[1], lat = loc[2],
                                       start_time = st_time,  end_time = en_time)
  }else{
    for(i in 1:length(years)){
      if(i==1){
        hourlydata <- mcera5::extract_clim(nc = paste0(spatial, '_', years[i], '.nc'), long = loc[1], lat = loc[2],
                                           start_time = st_time,
                                           end_time = as.Date(paste(years[i],'12','31', sep='-')))
      }
      if(i!=1 & i!=length(years)){
        hourlydata.i <- mcera5::extract_clim(nc = paste0(spatial, '_', years[i], '.nc'), long = loc[1], lat = loc[2],
                                             start_time = as.Date(paste(years[i],'01','01', sep='-')),
                                             end_time = as.Date(paste(years[i],'12','31', sep='-')))
        hourlydata <- dplyr::bind_rows(hourlydata, hourlydata.i)
      }
      if(i==length(years)){
        hourlydata.i <- mcera5::extract_clim(nc = paste0(spatial, '_', years[i], '.nc'), long = loc[1], lat = loc[2],
                                             start_time = as.Date(paste(years[i],'01','01', sep='-')),
                                             end_time = en_time)
        hourlydata <- dplyr::bind_rows(hourlydata, hourlydata.i)
      }
    }
  }
  if(length(years)==1){
    dailyprecip <- mcera5::extract_precip(nc = paste0(spatial, '_', years, '.nc'), long = loc[1], lat = loc[2],
                                          start_time = st_time,
                                          end_time = en_time)
  }else{
    for(i in 1:length(years)){
      if(i==1){
        dailyprecip <- mcera5::extract_precip(nc = paste0(spatial, '_', years[i], '.nc'), long = loc[1], lat = loc[2],
                                              start_time = st_time,
                                              end_time = as.Date(paste(years[i],'12','31', sep='-')))
      }
      if(i!=1 & i!=length(years)){
        dailyprecip.i <- mcera5::extract_precip(nc = paste0(spatial, '_', years[i], '.nc'), long = loc[1], lat = loc[2],
                                                start_time = as.Date(paste(years[i],'01','01', sep='-')),
                                                end_time = as.Date(paste(years[i],'12','31', sep='-')))
        dailyprecip <- c(dailyprecip, dailyprecip.i)
      }
      if(i==length(years)){
        dailyprecip.i <- mcera5::extract_precip(nc = paste0(spatial, '_', years[i], '.nc'), long = loc[1], lat = loc[2],
                                                start_time = as.Date(paste(years[i],'01','01', sep='-')),
                                                end_time = en_time)
        dailyprecip <- c(dailyprecip, dailyprecip.i)
      }
    }
  }
  microclima.out <- microclima::microclimaforNMR(lat = loc[2],
                                                 long = loc[1],
                                                 dstart = dstart,
                                                 dfinish = dfinish,
                                                 l = 0,
                                                 x = 1,
                                                 coastal = F,
                                                 hourlydata = as.data.frame(hourlydata),
                                                 dailyprecip = dailyprecip,
                                                 dem = dem,
                                                 demmeso = dem,
                                                 albr = 0,
                                                 resolution = 30,
                                                 slope = 0,
                                                 aspect = 0,
                                                 windthresh = 4.5,
                                                 emthresh = 0.78,
                                                 reanalysis2 = TRUE,
                                                 difani = FALSE,
                                                 weather.elev = 'era5',
                                                 cad.effects = TRUE,
                                                 zmin = 0)
  return(microclima.out)
}
source('scripts/check ERA5/get_ERA5.R')
source('scripts/Functions/get_tz.R')
spatial <- 'ERA5/Australia_ERA5/ERA5'

sites <- c("LEONORA", "YEELIRRIE", "YAMARNA", "LAVERTON")
lats <- c(-28.89, -27.28, -28.17, -28.63)
lons <- c(121.33, 120.09, 123.66, 122.41)        
stations <- c(12046, 12090, 12219, 12045)
years <- c(1950:2020) # years <- c(1965:1990, 1976:1977)

i <- 1
for(i in 1:length(sites)){
  loc <- c(lons[i], lats[i])
  
  for(j in 1:length(years)){
    dstart <- paste0("01/01/", years[j])
    dfinish <- paste0("31/12/", years[j])
    dates.daily <- seq(as.POSIXct(dstart, format = "%d/%m/%Y", tz = 'UTC'), as.POSIXct(dfinish, format = "%d/%m/%Y", tz = 'UTC')+23*3600, by = 'days')
    dates.hourly <- seq(as.POSIXct(dstart, format = "%d/%m/%Y", tz = 'UTC'), as.POSIXct(dfinish, format = "%d/%m/%Y", tz = 'UTC')+23*3600, by = 'hours')
    dates.daily <- get_tz(dates.daily, loc)
    dates.hourly <- get_tz(dates.hourly, loc)
    ERA5.data <- get_ERA5(loc = loc, dstart = dstart, dfinish = dfinish, spatial = spatial)
    
    hourlydata <- ERA5.data$hourlydata
    dailyprecip <- ERA5.data$dailyprecip
    hourlyradwind <- ERA5.data$hourlyradwind
    
    plot(dates.hourly, hourlydata$temperature, type = 'l')
    plot(dates.daily, dailyprecip, type = 'h')
    
    write.csv(hourlydata, paste0("Output/check ERA5/hourlydata", sites[i], "_", years[j], ".csv"))
    write.csv(dailyprecip, paste0("Output/check ERA5/dailyprecip", sites[i], "_", years[j], ".csv"))
  }
}

source('scripts/check ERA5/get_ERA5.R')
source('scripts/Functions/get_tz.R')
spatial <- 'ERA5/Kalahari_ERA5/ERA5'

sites <- c("UPINGTON")
lats <- c(-28.4)
lons <- c(21.2670)        
years <- c(1950:2020)
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

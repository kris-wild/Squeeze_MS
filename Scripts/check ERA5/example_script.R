source('scripts/Functions/get_ERA5.R')
source('scripts/Functions/get_tz.R')
loc <- c(21.2670, -28.4)
dstart <- "01/01/1969"
dfinish <- "31/12/1970"
spatial <- 'ERA5/Kalahari_ERA5/ERA5'
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

write.csv(hourlydata, "Scripts/check ERA5/hourlydataUpington.csv")
write.csv(dailyprecip, "Scripts/check ERA5/dailyprecipUpington.csv")


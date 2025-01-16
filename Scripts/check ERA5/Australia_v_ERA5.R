library(tidyverse)
library(lubridate)
source('scripts/Functions/get_tz.R')

metfiles <- list.files('data/BoM data Australia/')
sites <- c("LEONORA", "YEELIRRIE", "YAMARNA", "LAVERTON")
stations <- c(12046, 12090, 12219, 12045)
lats <- c(-28.89, -27.28, -28.17, -28.63)
lons <- c(121.33, 120.09, 123.66, 122.41) 

years <- c(1969:2020)

pdf(file = 'MS/SI_Files/Au_ERA5_min_max_test.pdf', width = 12, height = 11.69, paper = "a4r")
for(i in 1:length(sites)){
  loc <- c(lons[i], lats[i])
  sub.metfiles <- subset(metfiles, substr(metfiles, 12, 16) == stations[i] & substr(metfiles, 23, 26) == 'Data')
  
  obs.Tmax <- read.csv(paste0('data/BoM data Australia/', sub.metfiles[2]))
  obs.Tmin <- read.csv(paste0('data/BoM data Australia/', sub.metfiles[3]))
  
  obs.Tmax$date <- as.Date(get_tz(as.POSIXct(paste0(obs.Tmax$Year, '-', obs.Tmax$Month, '-', obs.Tmax$Day)), loc))
  obs.Tmin$date <- as.Date(get_tz(as.POSIXct(paste0(obs.Tmin$Year, '-', obs.Tmin$Month, '-', obs.Tmin$Day)), loc)) - 1 # need to get Tmin of previous day
  
  for(j in 1:length(years)){
    dstart2 <- paste0(years[j], "-01-01")
    dfinish2 <- paste0(years[j], "-12-31")
    dstart <- paste0("01/01/", years[j])
    dfinish <- paste0("31/12/", years[j])
    dates.daily <- seq(as.POSIXct(dstart, format = "%d/%m/%Y", tz = 'UTC'), as.POSIXct(dfinish, format = "%d/%m/%Y", tz = 'UTC')+23*3600, by = 'days')
    dates.hourly <- seq(as.POSIXct(dstart, format = "%d/%m/%Y", tz = 'UTC'), as.POSIXct(dfinish, format = "%d/%m/%Y", tz = 'UTC')+23*3600, by = 'hours')
    dates.daily <- get_tz(dates.daily, loc)
    dates.hourly <- get_tz(dates.hourly, loc)
    
    
    obs.Tmax.sub <- obs.Tmax |>
      filter(date <= dfinish2 & date >= dstart2)
    obs.Tmin.sub <- obs.Tmin |>
      filter(date <= dfinish2 & date >= dstart2)
    
    #
    if(nrow(obs.Tmax.sub) > 0){
      pred_ERA5 <- read.csv(paste0("Output/check ERA5/hourlydata", sites[i], "_", years[j], ".csv"))
      pred_ERA5$date <-  get_tz(as.POSIXct(pred_ERA5$obs_time), loc)
      
      
      
     
      
      # get max min by date for ERA5
      # 
      pred_ERA5_day <- pred_ERA5 |>
        mutate(date = as.Date(date)) |>
        group_by(date) |>
        summarise(Tmax = max(temperature),
                  Tmin = min(temperature))
      
      obs_ERA_Tmax <- full_join(
        obs.Tmax.sub, pred_ERA5_day, by = "date")
      obs_ERA_Tmin <- full_join(
        obs.Tmin.sub, pred_ERA5_day, by = "date")
      
      
      obs_ERA_Tmax$year <- year(obs_ERA_Tmax$date)
      obs_ERA_Tmin$year <- year(obs_ERA_Tmin$date)
     
      
      obs_ERA_Tmax <- subset(obs_ERA_Tmax, year %in% years)
      obs_ERA_Tmin <- subset(obs_ERA_Tmin, year %in% years)
      
      
    
      # 
      # for(j in 1:length(years)){
      #   if(j == length(years)){
      obs_ERA_Tmax_sub <- obs_ERA_Tmax
      obs_ERA_Tmin_sub <- obs_ERA_Tmin
      
      # }else{
      #   obs_ERA_Tmax_sub <- subset(obs_ERA_Tmax, year == years[j])
      #   obs_ERA_Tmin_sub <- subset(obs_ERA_Tmin, year == years[j])
      
      # }
      obspred_Tmax <- na.omit(cbind(obs_ERA_Tmax_sub$Tmax, obs_ERA_Tmax_sub$Maximum.temperature..Degree.C.))
      if(nrow(obspred_Tmax) > 0){
        r.Tmax <- cor(obspred_Tmax[, 1], obspred_Tmax[, 2])
        p.Tmax <- cor.test(obspred_Tmax[, 1], obspred_Tmax[, 2])$p.value
        rmsd.Tmax <- sqrt(mean(((obspred_Tmax[, 1] - obspred_Tmax[, 2]) ^ 2), na.rm = TRUE))
        bias.Tmax <- mean(obspred_Tmax[, 1] - obspred_Tmax[, 2])
        
        obspred_Tmin <- na.omit(cbind(obs_ERA_Tmin_sub$Tmin, obs_ERA_Tmin_sub$Minimum.temperature..Degree.C.))
        r.Tmin <- cor(obspred_Tmin[, 1], obspred_Tmin[, 2])
        p.Tmin <- cor.test(obspred_Tmin[, 1], obspred_Tmin[, 2])$p.value
        rmsd.Tmin <- sqrt(mean(((obspred_Tmin[, 1] - obspred_Tmin[, 2]) ^ 2), na.rm = TRUE))
        bias.Tmin <- mean(obspred_Tmin[, 1] - obspred_Tmin[, 2])
        
        
        
        results <- cbind(sites[i], years[j], r.Tmax, p.Tmax, rmsd.Tmax, bias.Tmax, r.Tmin, p.Tmin, rmsd.Tmin, bias.Tmin)
        if(j == 1){
          allresults <- results
        }else{
          allresults <- rbind(allresults, results)
        }
        
        par(oma = c(2, 1, 2, 2) + 0.1)
        par(mar = c(3, 3, 1.5, 1) + 0.1)
        par(mgp = c(2, 1, 0))
        par(mfrow = c(1, 2))
        with(obs_ERA_Tmin_sub,
             plot(Tmin, Minimum.temperature..Degree.C., pch = 16, cex = 0.6,
                  xlim = c(-5, 50), ylim = c(-5, 50), col = 'blue',
                  xlab = 'ERA5, deg C', ylab = 'observed, deg C',
                  main = 'daily minimum'))
        #legend(-2, 42, c('T_min', 'T_max'), pch = 16, col = c('blue', 'red'), bty = 'n')
        abline(0, 1, lwd = 2)
        lm.min <- lm(Minimum.temperature..Degree.C. ~ Tmin, data = obs_ERA_Tmin_sub)[[1]]
        abline(lm.min, lty = 2, lwd = 2)
        text(2, 44, paste0('r=', round(r.Tmin, 2)))
        text(2, 41, paste0('rmsd=', round(rmsd.Tmin, 1)))
        text(2, 38, paste0('bias=', round(bias.Tmin, 1)))
        mtext(paste0(sites[i], years[j]), outer = TRUE)
        
        with(obs_ERA_Tmax_sub,
             plot(Tmax, Maximum.temperature..Degree.C., pch = 16, cex = 0.6,
                  xlim = c(-5, 50), ylim = c(-5, 50), col = 'red',
                  xlab = 'ERA5, deg C', ylab = 'observed, deg C',
                  main = 'daily maximum'))
        lm.max <- lm(Maximum.temperature..Degree.C. ~ Tmax, data = obs_ERA_Tmax_sub)[[1]]
        abline(0, 1, lwd = 2)
        abline(lm.max, lty = 2, lwd = 2)
        text(2, 44, paste0('r=', round(r.Tmax, 2)))
        text(2, 41, paste0('rmsd=', round(rmsd.Tmax, 1)))
        text(2, 38, paste0('bias=', round(bias.Tmax, 1)))
        
        
        
        # par(mfrow = c(3, 1))
        
        
        # plot(dates.daily-3600*24, pred_ERA5_day$Tmax, type = 'l', col = 'black', ylim = c(10, 45), xlab = '', ylab = 'daily max T_air, deg C')
        # points(dates.daily, obs.Tmax.sub$Maximum.temperature..Degree.C., type = 'l', col = 'red')
        # plot(dates.daily-3600*24, pred_ERA5_day$Tmin, type = 'l', col = 'black', ylim = c(-5, 30), xlab = '', ylab = 'daily max T_air, deg C')
        # points(dates.daily, obs.Tmin.sub$Minimum.temperature..Degree.C., type = 'l', col = 'red')
      }}
  }
  allresults <- as.data.frame(allresults)
  write.csv(allresults, file = paste0('output/check ERA5/stats_', sites[i], '.csv'))
}
dev.off()

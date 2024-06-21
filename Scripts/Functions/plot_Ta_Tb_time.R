plot_Ta_Tb_time <- function(dstart, dfinish, environ, merged){
  par(oma = c(2, 1, 2, 2) + 0.1)
  par(mar = c(3, 3, 1.5, 1) + 0.1)
  par(mgp = c(2, 1, 0))
  par(mfrow = c(2, 1))
  if(!exists("Australia_Tb")){
    Australia_Tb <<- read.csv(file = "Raw_data/Australia_Tb.csv")# %>% 
  }
  times <- sprintf('%04d', Australia_Tb$time_orig)
  months <- sprintf('%02d', Australia_Tb$Month)
  days <- sprintf('%02d', Australia_Tb$Day)
  times <- paste0(substr(times, 1, 2),':',substr(times, 3, 4))
  dates2plot <- as.POSIXct(paste0(Australia_Tb$yr, '-', months, '-', days, ' ', times), format = "%Y-%m-%d %H:%M", tz = 'Etc/GMT-8')
  environ <- subset(environ, dates > dstart & dates < dfinish)
  plot(environ$dates, environ$TA1m, type = 'l', col = 'black', ylim = c(0, 47), ylab = 'T_air, deg C', xlab = "", main = paste0(genus, ' ', species, ' Site', site))
  points(dates2plot, as.numeric(dates2plot), type = 'h', col = 'light grey')
  points(environ$dates, environ$TA1m, type = 'l', col = 'black', ylim = c(0, 47), ylab = 'T_air, deg C', xlab = "", main = paste0(genus, ' ', species, ' Site', site))
  points(merged$datetime2, merged$AT, pch = 16, cex = 0.6, col = 2)
  if(do.legend){
    legend(environ$dates[1], 5, legend = c("Tbreg/Tair", "Tbmax", "Tsoil"), lty = c(1, 1, 2), col = c("black", "grey", "brown"), bty = 'n', horiz = TRUE)
    legend(environ$dates[1], 49, legend = c("inactive", "basking", "active", "obs"), pch = 16, pt.cex = c((c(0, 1, 2) + 1)/3, 1), col = c(rep("grey", 3), "red"), bty = 'n', horiz = FALSE)
  }
  plot(environ$dates, environ$TC, type = 'p', pch = 16, cex = (environ$ACT + 1)/3, col = 'grey', ylim = c(0, 47), ylab = 'T_b, deg C', xlab = "")
  points(environ$dates, environ$SHADE, type = 'h', lwd = 2, pch = 16, col = 'lightgreen', ylim = c(0, 47), ylab = 'T_b, deg C', xlab = "")
  points(environ$dates, environ$TC, type = 'p', pch = 16, cex = (environ$ACT + 1)/3, col = 'grey', ylim = c(0, 47), ylab = 'T_b, deg C', xlab = "")
  points(environ$dates, environ$TCMAX, type = 'l', col = 'grey', lty = 1)
  points(environ$dates, environ$TC, type = 'l', col = 'black', )
  points(merged$datetime2, merged$BT, pch = 16, cex = 0.6, col = 2)
  points(environ$dates, environ$Tsoil, type = 'l', col = 'brown', lty = 2)
  abline(h = T_pref, col = 'orange', lty = 2)
  abline(h = T_F_max, col = 'red', lty = 2)
  abline(h = T_F_min, col = 'blue', lty = 2)
}

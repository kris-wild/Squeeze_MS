library(NicheMapR)

species <- "Ctenotus quatt"

source(paste0('Scripts/parameters/', species, '/parameters_biophys.R'))

sites <- read.csv("Sites/Aus_KL_Sites.csv")
site_name <- "Australia_L"
location <- subset(sites, Site.name == site_name)
loc <- c(location$Long, location$Lat)

warm <- 1
Usrhyt <- 0.005
maxshade <- .050
micro <- micro_global(loc = loc,
                      warm = warm,
                      Usrhyt = Usrhyt,
                      maxshade = maxshade,
                      cap = 0)
# retrieve output
metout <- as.data.frame(micro$metout) # above ground microclimatic conditions, min shade
shadmet <- as.data.frame(micro$shadmet) # above ground microclimatic conditions, max shade
soil <- as.data.frame(micro$soil) # soil temperatures, minimum shade
shadsoil <- as.data.frame(micro$shadsoil) # soil temperatures, maximum shade

# append dates
dates <- micro$dates
metout <- cbind(dates, metout)
soil <- cbind(dates, soil)
shadmet <- cbind(dates, shadmet)
shadsoil <- cbind(dates, shadsoil)

ecto <- ectotherm(Ww_g = Ww_g,
                  shape = shape,
                  CT_min = CT_min, 
                  T_RB_min = T_RB_min,
                  T_B_min = T_B_min,
                  T_F_min = T_F_min,
                  T_pref = T_pref,
                  T_F_max = T_F_max,
                  CT_max = CT_max,
                  alpha_max = alpha_max,
                  alpha_min = alpha_min,
                  diurn = diurn,
                  nocturn = nocturn,
                  shade_seek = shade_seek,
                  burrow = burrow,
                  climb = climb,
                  custom_shape = c(10.4713, 0.688, 0.425, 0.85, 3.798, 0.683, 10.4713, 0.688),
                  minshades = micro$minshade,
                  maxshades = micro$maxshade,
                  shdburrow = shdburrow,
                  mindepth = mindepth,
                  maxdepth = maxdepth,
                  pct_wet = pct_wet,
                  pct_eyes = pct_eyes)


# retrieve output
environ <- as.data.frame(ecto$environ) # activity, Tb and environment
enbal <- as.data.frame(ecto$enbal) # energy balance values
masbal <- as.data.frame(ecto$masbal) # mass balance value (note most missing if DEB model not running)

# append dates
environ <- cbind(dates, environ)
masbal <- cbind(dates, masbal)
enbal <- cbind(dates, enbal)

############### plot results ######################

# Hourly Tb (black), activity (orange, 5 = bask, 10 = forage), depth (brown, m) and shade (green, %/10)
# with(environ, plot(TC ~ dates, ylab = "", xlab="month of year", col = 'black', xlim = c(-0.25, 12), ylim = c(-20, 45), type = "l", yaxt = 'n'))
# with(environ, points(ACT * 2 + 7 ~ dates, type = "p", pch = 16, col = "orange"))
# with(environ, points(SHADE / 10 - 6 ~ dates, type = "l", col = "dark green"))
# with(environ, points(DEP - 10 ~ dates, type = "l", col = "brown"))
# abline(ecto$T_F_min, 0, lty = 2, col = 'blue')
# abline(ecto$T_F_max, 0, lty = 2, col = 'red')
# ytick<-seq(15, 40, by=5)
# axis(side=2, at=ytick, labels = TRUE)
# mtext(text = c('A', 'B', 'I'), side = 2, line = 1, at = c(11, 9, 7))
# ytick<-seq(-6, 4, by=2)
# axis(side=2, at=ytick, labels = FALSE)
# mtext(text = seq(0, 100, 20), side = 2, line = 1, at = seq(-6, 4, 2), las = 2)
# ytick<-seq(-20, -10, by=2)
# axis(side=2, at=ytick, labels = FALSE)
# mtext(text = rev(seq(0, 100, 20)), side = 2, line = 1, at = seq(-20, -10, 2), las = 2)
# abline(h = -10, lty = 2, col = 'grey')
# mtext(text = c('body temperature (°C)', 'activity', 'shade (%)', 'depth (cm)'), side = 2, line = 2.5, at = c(30, 9, 0, -15))
# text(-0.2, c(ecto$T_F_max + 1, ecto$T_F_min + 1), c('T_F_max', 'T_F_min'), col = c('red', 'blue'), cex = 0.75)

# seasonal activity plot (dark blue = night, light blue = basking, orange = foraging)
forage <- subset(environ, ACT == 2)
bask <- subset(environ, ACT == 1)
night <- subset(metout, ZEN == 90)
day <- subset(metout, ZEN != 90)
with(night, plot(TIME / 60 ~ DOY, ylab = "Hour of Day", xlab = "Day of Year", pch = 15, cex = 2, col = 'dark blue', main = paste('warm =', warm, 'deg C')))
# nighttime hours
with(forage, points(TIME ~ DOY, pch = 15, cex = 2, col = 'orange')) # foraging Tbs
with(bask, points(TIME ~ DOY, pch = 15, cex = 2, col = 'light blue')) # basking Tbs

warming <- 0.1
doys <- unique(metout$DOY)
environ$toohot <- 0
environ$toohot[environ$ACT == 0 & environ$ZEN < 90 & soil$D0cm > T_F_min] <- 1
hr_actual <- aggregate(environ$toohot, by = list(metout$DOY), FUN = sum)[, 2]
Tmaxs <- aggregate(metout$TAREF, by = list(metout$DOY), FUN = max)[, 2]
hr1 <- 6.12 + 0.74 * (Tmaxs - T_pref)
hr1[hr1 < 0] <- 0
text(x = doys, y = 14,labels = round(hr1, 1), col = 'red')
text(x = doys, y = 12,labels = round(hr_actual, 0))
text(x = 25, y = 15, labels = 'Sinervo', col = 'red')
text(x = 25, y = 13, labels = 'NicheMapR', col = 'black')
#plot(hr1, hr_actual, pch = 16)
#abline(0, 1)





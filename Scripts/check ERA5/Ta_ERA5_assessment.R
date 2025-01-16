par(oma = c(2, 1, 2, 2) + 0.1)
par(mar = c(3, 3, 1.5, 1) + 0.1)
par(mgp = c(2, 1, 0))
par(mfrow = c(3, 2))

# Site A
region <- 'Kalahari'
site <- 'A'
ystart <- 1970
yfinish <- 1970
maxshade <- 50
warm <- 0
cap <- 0

offsets <- TRUE
offset.A <- -0.8
offset.B <- -2
offset.K <- 0.2
offset.L <- -1
offset.T <- -0.6
offset.X <- -2.6

genera <- c("Agama", "Meroles", "Pedioplanis", "Chondrodactylus")
species <- c("aculeata", "suborbitalis", "lineoocellata", "angulifer")
Usrhyts <- c(0.03, 0.005, 0.005, 0.005, 0.005)
for(i in 1:length(genera)){
  filenm <- paste0('output/merged comparisons/', genera[i], ' ', species[i], '/Ta_Tb_', genera[i], '_', species[i], '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyts[i], 'm_', cap, 'cap.csv')
  datain <- read.csv(filenm)
  if(i == 1){
    obspred_TA <- datain
  }else{
    obspred_TA <- rbind(obspred_TA, datain)
  }
}
obspred_TA <- obspred_TA[, -1]
colnames(obspred_TA) <- c('date1', 'date2', 'Ta_ERA5', 'Ta_obs', 'Tc_ERA5', 'Tc_obs')
obspred_TA <- as.data.frame(obspred_TA)
if(offsets){
  obspred_TA$Ta_ERA5 <- obspred_TA$Ta_ERA5 + offset.A
}
r.TA <- cor(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)
p.TA <- cor.test(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)$p.value
rmsd.TA <- sqrt(mean(((obspred_TA$Ta_obs - obspred_TA$Ta_ERA5) ^ 2), na.rm = TRUE))
bias.TA <- mean(obspred_TA$Ta_obs - obspred_TA$Ta_ERA5, na.rm = TRUE)

# 1to1 plot

plot(obspred_TA$Ta_ERA5, obspred_TA$Ta_obs, pch = 16, col = 1, ylab = 'obs', xlab = 'pred', main = paste('site', site), ylim = c(10, 45), xlim = c(10, 45))
abline(0, 1)
text(20, 43, paste0('r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))

obspred_all <- cbind(site, obspred_TA)

# site B
site <- 'B'

genera <- c("Trachylepis")
species <- c("sparsa")
Usrhyts <- c(0.005)
for(i in 1:length(genera)){
  filenm <- paste0('output/merged comparisons/', genera[i], ' ', species[i], '/Ta_Tb_', genera[i], '_', species[i], '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyts[i], 'm_', cap, 'cap.csv')
  datain <- read.csv(filenm)
  if(i == 1){
    obspred_TA <- datain
  }else{
    obspred_TA <- rbind(obspred_TA, datain)
  }
}
obspred_TA <- obspred_TA[, -1]
colnames(obspred_TA) <- c('date1', 'date2', 'Ta_ERA5', 'Ta_obs', 'Tc_ERA5', 'Tc_obs')
obspred_TA <- as.data.frame(obspred_TA)
if(offsets){
 obspred_TA$Ta_ERA5 <- obspred_TA$Ta_ERA5 + offset.B
}
r.TA <- cor(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)
p.TA <- cor.test(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)$p.value
rmsd.TA <- sqrt(mean(((obspred_TA$Ta_obs - obspred_TA$Ta_ERA5) ^ 2), na.rm = TRUE))
bias.TA <- mean(obspred_TA$Ta_obs - obspred_TA$Ta_ERA5, na.rm = TRUE)
# 1to1 plot

plot(obspred_TA$Ta_ERA5, obspred_TA$Ta_obs, pch = 16, col = 1, ylab = 'obs', xlab = 'pred', main = paste('site', site), ylim = c(10, 45), xlim = c(10, 45))
abline(0, 1)
text(20, 43, paste0('r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))

obspred_all <- rbind(obspred_all, cbind(site, obspred_TA))

# site K
site <- 'K'

genera <- c("Trachylepis")
species <- c("sparsa")
Usrhyts <- c(0.5)
for(i in 1:length(genera)){
  filenm <- paste0('output/merged comparisons/', genera[i], ' ', species[i], '/Ta_Tb_', genera[i], '_', species[i], '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyts[i], 'm_', cap, 'cap.csv')
  datain <- read.csv(filenm)
  if(i == 1){
    obspred_TA <- datain
  }else{
    obspred_TA <- rbind(obspred_TA, datain)
  }
}
obspred_TA <- obspred_TA[, -1]
colnames(obspred_TA) <- c('date1', 'date2', 'Ta_ERA5', 'Ta_obs', 'Tc_ERA5', 'Tc_obs')
obspred_TA <- as.data.frame(obspred_TA)
if(offsets){
  obspred_TA$Ta_ERA5 <- obspred_TA$Ta_ERA5 + offset.K
}
r.TA <- cor(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)
p.TA <- cor.test(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)$p.value
rmsd.TA <- sqrt(mean(((obspred_TA$Ta_obs - obspred_TA$Ta_ERA5) ^ 2), na.rm = TRUE))
bias.TA <- mean(obspred_TA$Ta_obs - obspred_TA$Ta_ERA5, na.rm = TRUE)
# 1to1 plot

plot(obspred_TA$Ta_ERA5, obspred_TA$Ta_obs, pch = 16, col = 1, ylab = 'obs', xlab = 'pred', main = paste('site', site), ylim = c(10, 45), xlim = c(10, 45))
abline(0, 1)
text(20, 43, paste0('r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))

obspred_all <- rbind(obspred_all, cbind(site, obspred_TA))

# site L
site <- 'L'

genera <- c("Meroles")
species <- c("suborbitalis")
Usrhyts <- c(0.005)
for(i in 1:length(genera)){
  filenm <- paste0('output/merged comparisons/', genera[i], ' ', species[i], '/Ta_Tb_', genera[i], '_', species[i], '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyts[i], 'm_', cap, 'cap.csv')
  datain <- read.csv(filenm)
  if(i == 1){
    obspred_TA <- datain
  }else{
    obspred_TA <- rbind(obspred_TA, datain)
  }
}
obspred_TA <- obspred_TA[, -1]
colnames(obspred_TA) <- c('date1', 'date2', 'Ta_ERA5', 'Ta_obs', 'Tc_ERA5', 'Tc_obs')
obspred_TA <- as.data.frame(obspred_TA)
if(offsets){
  obspred_TA$Ta_ERA5 <- obspred_TA$Ta_ERA5 + offset.L
}
r.TA <- cor(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)
p.TA <- cor.test(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)$p.value
rmsd.TA <- sqrt(mean(((obspred_TA$Ta_obs - obspred_TA$Ta_ERA5) ^ 2), na.rm = TRUE))
bias.TA <- mean(obspred_TA$Ta_obs - obspred_TA$Ta_ERA5, na.rm = TRUE)
# 1to1 plot

plot(obspred_TA$Ta_ERA5, obspred_TA$Ta_obs, pch = 16, col = 1, ylab = 'obs', xlab = 'pred', main = paste('site', site), ylim = c(10, 45), xlim = c(10, 45))
abline(0, 1)
text(20, 43, paste0('r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))

obspred_all <- rbind(obspred_all, cbind(site, obspred_TA))

# site T
site <- 'T'

genera <- c("Pedioplanis", "Chondrodactylus")
species <- c("lineoocellata", "angulifer")
Usrhyts <- c(0.005, 0.005)

for(i in 1:length(genera)){
  filenm <- paste0('output/merged comparisons/', genera[i], ' ', species[i], '/Ta_Tb_', genera[i], '_', species[i], '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyts[i], 'm_', cap, 'cap.csv')
  datain <- read.csv(filenm)
  if(i == 1){
    obspred_TA <- datain
  }else{
    obspred_TA <- rbind(obspred_TA, datain)
  }
}
obspred_TA <- obspred_TA[, -1]
colnames(obspred_TA) <- c('date1', 'date2', 'Ta_ERA5', 'Ta_obs', 'Tc_ERA5', 'Tc_obs')
obspred_TA <- as.data.frame(obspred_TA)
if(offsets){
  obspred_TA$Ta_ERA5 <- obspred_TA$Ta_ERA5 + offset.T
}
r.TA <- cor(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)
p.TA <- cor.test(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)$p.value
rmsd.TA <- sqrt(mean(((obspred_TA$Ta_obs - obspred_TA$Ta_ERA5) ^ 2), na.rm = TRUE))
bias.TA <- mean(obspred_TA$Ta_obs - obspred_TA$Ta_ERA5, na.rm = TRUE)
# 1to1 plot

plot(obspred_TA$Ta_ERA5, obspred_TA$Ta_obs, pch = 16, col = 1, ylab = 'obs', xlab = 'pred', main = paste('site', site), ylim = c(10, 45), xlim = c(10, 45))
abline(0, 1)
text(20, 43, paste0('r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))

obspred_all <- rbind(obspred_all, cbind(site, obspred_TA))

# site X
site <- 'X'

genera <- c("Agama")
species <- c("aculeata")
Usrhyts <- c(0.03)

for(i in 1:length(genera)){
  filenm <- paste0('output/merged comparisons/', genera[i], ' ', species[i], '/Ta_Tb_', genera[i], '_', species[i], '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyts[i], 'm_', cap, 'cap.csv')
  datain <- read.csv(filenm)
  if(i == 1){
    obspred_TA <- datain
  }else{
    obspred_TA <- rbind(obspred_TA, datain)
  }
}
obspred_TA <- obspred_TA[, -1]
colnames(obspred_TA) <- c('date1', 'date2', 'Ta_ERA5', 'Ta_obs', 'Tc_ERA5', 'Tc_obs')
obspred_TA <- as.data.frame(obspred_TA)
if(offsets){
  obspred_TA$Ta_ERA5 <- obspred_TA$Ta_ERA5 + offset.X
}
r.TA <- cor(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)
p.TA <- cor.test(obspred_TA$Ta_obs, obspred_TA$Ta_ERA5)$p.value
rmsd.TA <- sqrt(mean(((obspred_TA$Ta_obs - obspred_TA$Ta_ERA5) ^ 2), na.rm = TRUE))
bias.TA <- mean(obspred_TA$Ta_obs - obspred_TA$Ta_ERA5, na.rm = TRUE)

plot(obspred_TA$Ta_ERA5, obspred_TA$Ta_obs, pch = 16, col = 1, ylab = 'obs', xlab = 'pred', main = paste('site', site), ylim = c(10, 45), xlim = c(10, 45))
abline(0, 1)
text(20, 43, paste0('r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))
# 
# par(mfrow=(c(1, 1)))
# obspred_all <- rbind(obspred_all, cbind(site, obspred_TA))
# r.TA <- cor(obspred_all$Ta_obs, obspred_all$Ta_ERA5)
# p.TA <- cor.test(obspred_all$Ta_obs, obspred_all$Ta_ERA5)$p.value
# rmsd.TA <- sqrt(mean(((obspred_all$Ta_obs - obspred_all$Ta_ERA5) ^ 2), na.rm = TRUE))
# bias.TA <- mean(obspred_TA$Ta_obs - obspred_TA$Ta_ERA5, na.rm = TRUE)
# plot(obspred_all$Ta_ERA5, obspred_all$Ta_obs, pch = 16, col = 1, ylab = 'obs', xlab = 'pred', main = "all sites", ylim = c(10, 45), xlim = c(10, 45))
# abline(0, 1)
# text(20, 43, paste0('r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))

# interpolate predictions to exact times of observations
interp_TC <- approxfun(merged$dates, merged$TC)
interp_TA <- approxfun(merged$dates, merged$TA1m)
merged$TC_interp <- interp_TC(merged$datetime2)
merged$TA_interp <- interp_TA(merged$datetime2)

Ta_Tb <- na.omit(cbind(merged$dates, merged$datetime2, merged$TA_interp, merged$AT, merged$TC_interp, merged$BT))
outdir <- paste0('output/merged comparisons/', genus, ' ', species)
if(!dir.exists(outdir)){
  dir.create(outdir)
}
write.csv(Ta_Tb, file = paste0('output/merged comparisons/', genus, ' ', species, '/Ta_Tb_', genus, '_', species, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap.csv'))

merged$diff <- merged$TC_interp - merged$BT
merged$diff[is.na(merged$diff)] <- 0
if(burrow_bask > 0){
 merged$TC_interp[merged$diff < burrow_bask * -1] <- merged$Tsoil[merged$diff < burrow_bask * -1]
}

# TEST: observed vs predicted correlations - both sites
# TA - ambient temp
obspred_TA <- na.omit(cbind(merged$AT, merged$TA_interp))
r.TA <- cor(obspred_TA[, 1], obspred_TA[, 2])
p.TA <- cor.test(obspred_TA[, 1], obspred_TA[, 2])$p.value
rmsd.TA <- sqrt(mean(((obspred_TA[, 1] - obspred_TA[, 2]) ^ 2), na.rm = TRUE))
bias.TA <- mean((obspred_TA[, 1] - obspred_TA[, 2]), na.rm = TRUE)
# Tb - body temp
obspred_TB <- na.omit(cbind(merged$BT, merged$TC_interp))
r.TB <- cor(obspred_TB[, 1], obspred_TB[, 2])
p.TB <- cor.test(obspred_TB[, 1], obspred_TB[, 2])$p.value
rmsd.TB <- sqrt(mean(((obspred_TB[, 1] - obspred_TB[, 2]) ^ 2), na.rm = TRUE))
bias.TB <- mean((obspred_TB[, 1] - obspred_TB[, 2]), na.rm = TRUE)

# 1to1 plot
par(mfrow = c(1, 1))
plot(merged$AT, merged$TA_interp, pch = 16, ylim = c(12, 47), xlim = c(12, 47), col = 'lightblue', ylab = 'pred', xlab = 'obs', main = paste0('site ', site, ' ', genus, ' ' , species))
text(17, 46, paste0('TA r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), ' bias=', round(bias.TA, 1)))
points(merged$BT, merged$TC_interp, pch = 16, col = 'gold')#, cex = merged$SHADE/50)
text(17, 42, paste0('TB r=', round(r.TB, 2), ' rmsd=', round(rmsd.TB, 1), ' bias=', round(bias.TB, 1)))
#points(merged$Tsoil, merged$BT, pch = 16, col = 'brown')#, cex = merged$SHADE/50)
abline(0, 1)
abline(v = T_F_max, lty = 2, col = 2)
abline(h = T_F_max, lty = 2, col = 2)


check <- subset(merged, BT > 0)
if(region == 'Australia'){
  check <- check[, c("ID", "Site", "Genus.species", "datetime2", "DEP", "SHADE", "BT", "TC_interp", "TC", "AT", "TA_interp", "TA1m")]
 }else{
 check <- check[, c("number", "note_book", "Site", "Genus_species", "datetime2", "DEP", "BT", "TC_interp", "TC", "AT", "TA_interp", "TA1m", "wt", "svl")]
}
check$TA_interp <- round(check$TA_interp, 2)
check$TC_interp <- round(check$TC_interp, 2)
check$diff_TC <- check$BT - check$TC_interp
check$diff_TC <- round(check$diff_TC, 1)
check$diff_TA <- check$AT - check$TA_interp
check$diff_TA <- round(check$diff_TA, 1)
check <- check[order(check$diff_TC), ]
outdir <- paste0('output/checking Tbs/', genus, ' ', species)
if(!dir.exists(outdir)){
  dir.create(outdir)
}
write.csv(check, paste0('output/checking Tbs/', genus, ' ', species, '/diffs_', genus, '_', species, 'Site', site, '.csv'))

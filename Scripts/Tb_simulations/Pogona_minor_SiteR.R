source('scripts/Functions/run_sim.R')
source('scripts/Functions/plot_Ta_Tb_time.R')

plottime <- TRUE # plot time series?
burrow_bask <- 0 # Tb delta to put in shallow burrow

genus <- 'Pogona'
species <- 'minor'
loadlizard <- TRUE

# microclimate
region <- 'Australia'
site <- 'R'
ystart <- 1950
yfinish <- 1998 
maxshade <- 50
warm <- 0
Usrhyt <- 0.05
cap <- 0
loadmicro <- TRUE

out <- run_sim(genus = genus,
               species = species,
               region = region,
               site = site,
               ystart = ystart,
               yfinish = yfinish,
               maxshade = maxshade,
               warm = warm,
               Usrhyt = Usrhyt,
               cap = cap,
               loadmicro = loadmicro,
               loadlizard = loadlizard,
               soilnode = 2)

merged <- out$merged
environ <- out$environ
# windows where measurements were made
dstarts <- as.POSIXct(c('1978-08-20', '1978-12-09', '1990-09-08', '1979-03-10', '1996-01-04'))
dfinishs <-  as.POSIXct(c('1978-08-30', '1978-12-10', '1990-09-09','1979-03-11', '1996-01-18'))

if(plottime){
  do.legend <- FALSE
  for(i in 1:length(dstarts)){
    plot_Ta_Tb_time(dstarts[i],
                    dfinishs[i],
                    environ,
                    merged)
  }
  do.legend <- TRUE
  outdir <- paste0('output/checking Tbs/', genus, ' ', species)
  if(!dir.exists(outdir)){
    dir.create(outdir)
  }
  filenm <- paste0('output/checking Tbs/', genus, ' ', species, '/', genus, ' ', species, '_diffs_Site', site, '.pdf')
  pdf(filenm)
  for(i in 1:length(dstarts)){
    plot_Ta_Tb_time(dstarts[i],
                    dfinishs[i],
                    environ,
                    merged)
  }
  dev.off()
}
# run stats script
source('scripts/Functions/Tb_stats.R')
source('scripts/Functions/check.R')

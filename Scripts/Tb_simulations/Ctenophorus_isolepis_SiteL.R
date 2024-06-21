source('scripts/Functions/run_sim.R')
source('scripts/Functions/plot_Ta_Tb_time.R')

plottime <- TRUE # plot time series?
burrow_bask <- 0 # Tb delta to put in shallow burrow

genus <- 'Ctenophorus'
species <- 'isolepis'
loadlizard <- TRUE

# microclimate
region <- 'Australia'
site <- 'L'
ystart <- 1978
yfinish <- 1979
maxshade <- 50
windfac <- 0.25
warm <- 0.7
Usrhyt <- 0.005
cap <- 0
canopy_height <- 0
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
               canopy_height = canopy_height,
               cap = cap,
               windfac = windfac,
               loadmicro = loadmicro,
               loadlizard = loadlizard)

merged <- out$merged
environ <- out$environ


# windows where measurements were made
# windows where measurements were made
dstarts <- as.POSIXct(c('1978-10-15', '1978-10-25', '1979-02-11', '1979-03-03'))
dfinishs <- as.POSIXct(c('1978-10-16', '1978-10-26', '1979-02-12','1979-03-04'))

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


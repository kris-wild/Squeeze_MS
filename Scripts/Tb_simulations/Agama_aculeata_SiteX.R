source('scripts/Functions/run_sim.R')
source('scripts/Functions/plot_Ta_Tb_time.R')

plottime <- TRUE # plot time series?
burrow_bask <- 0 # Tb delta to put in shallow burrow

genus <- 'Agama'
species <- 'aculeata'
loadlizard <- TRUE

# microclimate
region <- 'Kalahari'
site <- 'X'
ystart <- 1970
yfinish <- 1970
maxshade <- 50
warm <- 0#-2.6
Usrhyt <- 0.03
cap <- 0
tree <- 0
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
               loadlizard = loadlizard)

merged <- out$merged
environ <- out$environ

# windows where measurements were made
dstarts <- as.POSIXct(c('1970-05-21', '1970-09-24', '1970-04-14'))
dfinishs <- as.POSIXct(c('1970-05-22','1970-09-25', '1970-04-15'))

if(plottime){
  do.legend <- FALSE
  for(i in 1:length(dstarts)){
    plot_Ta_Tb_time(dstarts[i],
                    dfinishs[i],
                    environ,
                    merged)
  }
  do.legend <- TRUE
  filenm <- paste0('output/checking Tbs/', genus, ' ', species, '/diffs_Site', site, '.pdf')
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

sim_lizard <- function(genus = 'Meroles',
                       species = 'suborbitalis',
                       region = region,
                       site = site,
                       loc = loc,
                       ystart = ystart,
                       yfinish = yfinish,
                       maxshade = maxshade,
                       maxshade.lizard = maxshade.lizard,
                       warm = warm,
                       scenario = scenario,
                       Usrhyt = Usrhyt,
                       cap = cap,
                       ERR = ERR,
                       wind.mult = wind.mult,
                       slope = slope,
                       aspect = aspect,
                       tree = tree,
                       loadlizard = FALSE){
  
  species1 <- paste0(genus, "_", species)
  species2 <- paste0(genus, " ", species) 
  source(paste0('Scripts/parameters/', species2, '/parameters_biophys.R'))
  if(slope == 0){
    microfile <- paste0('output/microclimates/micro_', region, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', scenario,'scenario_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap.Rda')
    ectofile <- paste0('output/Tb_sims/', species2, '/ecto_',region, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', scenario,'scenario_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap.Rda')
  }else{
    microfile <- paste0('output/microclimates/micro_', region, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', scenario,'scenario_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap_', slope, 'slp_', aspect, 'asp.Rda')
    ectofile <- paste0('output/Tb_sims/', species2, '/ecto_',region, '_', site, '_', ystart, '_', yfinish, '_', warm, 'offset_', scenario,'scenario_', maxshade, 'shd_', Usrhyt, 'm_', cap, 'cap_', slope, 'slp_', aspect, 'asp.Rda')
  }
  if(loadlizard){
    ecto <- readRDS(file = ectofile)
  }else{
    micro <<- readRDS(microfile)
    micro$metout[, 18] <- 0
    micro$shadmet[, 18] <- 0
    if(tree == 1){
      print('test')
      micro$metout[, 3] <<- micro$metout[, 4]
      micro$shadmet[, 3] <<- micro$shadmet[, 4]
      micro$metout[, 5] <<- micro$metout[, 6]
      micro$shadmet[, 5] <<- micro$shadmet[, 6]
      micro$metout[, 7] <<- micro$metout[, 8]
      micro$shadmet[, 7] <<- micro$shadmet[, 8]
    }
    micro$metout[, 7] <<- micro$metout[, 7] * wind.mult
    micro$shadmet[, 7] <<- micro$shadmet[, 7] * wind.mult
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
                      #maxshades = micro$maxshade * 0 + maxshade.lizard,
                      minshades = micro$minshade,
                      shdburrow = shdburrow,
                      mindepth = mindepth,
                      maxdepth = maxdepth,
                      pct_wet = pct_wet,
                      pct_eyes = pct_eyes)
    
    # simulate maximum possible temperature
    ecto2 <- ectotherm(Ww_g = Ww_g,
                       shape = shape,
                       CT_min = -103, 
                       T_RB_min = -102,
                       T_B_min = -101,
                       T_F_min = -100,
                       T_pref = 100,
                       T_F_max = 101,
                       CT_max = 110,
                       alpha_max = alpha_max,
                       alpha_min = alpha_min,
                       diurn = diurn,
                       nocturn = nocturn,
                       shade_seek = shade_seek,
                       burrow = burrow,
                       climb = climb,
                       maxshades = micro$maxshade,
                       shdburrow = shdburrow,
                       mindepth = mindepth,
                       maxdepth = maxdepth,
                       pct_wet = pct_wet,
                       pct_eyes = pct_eyes)
    # put result in ecto$environ and relabel column
    ecto$environ[, 20] <- ecto2$environ[, 5]
    colnames(ecto$environ)[20] <- "TCMAX"
    outdir <- paste0('output/Tb_sims/', genus, ' ', species)
    if(!dir.exists(outdir)){
      dir.create(outdir)
    }
    saveRDS(ecto, file = ectofile)
  }
  return(ecto)
}


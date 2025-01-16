# Function for feeding demand 1) ant calculation (ants/h) a
#' @title get_ants
#' @description Calculates hourly ant needed baised off the estimated metabolic rate. Data are data are from Withers & Dickman 1995 Table 1. Information form table used in function were the most common ant that was consumed (Iridomyrmex 2*). Values used in function from table of ant: ant mass (mg), water content (%), and Energy Content (kJg). Needed Animal values needed for function are mass (g), body temperature (C), and estimated metabolic rate (J/h). 
get_ants_hourly <- function(m_rate_J_h, # metabolic rate (J/h) 
                            digestion_efficiency = 0.85, # digestion efficiency; Wehrle & German, 2023 
                            J_per_g = 28100,  # amount of energy of ant (J/g)
                            food_water_pct = 0.62, # water content of ant
                            food_item_wet_weight_mg = 0.45) # wet weight of ant 
  {
  adjusted_m_rate_J_h = m_rate_J_h / digestion_efficiency # accounting for digestion efficiency
  food_g_h = adjusted_m_rate_J_h / J_per_g # dry food required, g/h
  food_item_dry_weight_g = food_item_wet_weight_mg / 1000 * (1 - food_water_pct) # prey dry weight (g)
  prey_items_h = food_g_h / food_item_dry_weight_g # number of prey items 
  
  # Return a named list
  return(prey_items_h)
}



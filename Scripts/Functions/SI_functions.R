############
# Functions used in results section

## Regression function: Tb vs Tb predicted
# tests relationship overall and then seperatly by species
get_regression <- function(data, interp_col = "BT", 
                           response_col = "TC_interp") {
  # Ensure columns are numeric
  data[[interp_col]] <- as.numeric(data[[interp_col]])
  data[[response_col]] <- as.numeric(data[[response_col]])
  
  # Perform correlation cof for the entire dataset (Overall)
  sample_size <- nrow(data)
  rmsd <- sqrt(mean(((data[[response_col]] - data[[interp_col]]) ^ 2), na.rm = TRUE))
  bias <- mean((data[[response_col]] - data[[interp_col]]), na.rm = TRUE)
  
  overall_result <- data.frame(
    Genus_species = "Overall",
    n = sample_size,
    Correlation_coff = round(cor(data[[interp_col]], data[[response_col]], use = "complete.obs"), 3),
    rmsd = round(rmsd, 3),
    bias = round(bias, 3)
  )
  
  # Group by species and perform linear regression
  species_results <- data %>%
    group_by(Genus_species) %>%
    do({
      if (nrow(.) > 10) {  # Ensure sufficient data points
        sample_size <- nrow(.)
        rmsd_species <- sqrt(mean((.[[response_col]] - .[[interp_col]]) ^ 2, na.rm = TRUE))
        bias_species <- mean((.[[response_col]] - .[[interp_col]]), na.rm = TRUE)
        
        data.frame(
          Genus_species = unique(.$Genus_species),
          n = sample_size,
          Correlation_coff = round(cor(.[[interp_col]], .[[response_col]], use = "complete.obs"), 3),
          rmsd = round(rmsd_species, 3),
          bias = round(bias_species, 3)
        )
      } else {
        return(data.frame(Genus_species = unique(.$Genus_species), 
                          n = NA,
                          Correlation_coff = NA, 
                          rmsd = NA, bias = NA))
      }
    })
  
  # Combine Overall results with Species results
  results <- rbind(overall_result, species_results)
  
  return(results)
}






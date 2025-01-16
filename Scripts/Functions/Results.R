############
# Functions used in results section

## Regression function: Tb vs Tb predicted
# tests relationship overall and then seperatly by species
get_regression <- function(data, interp_col = "TC_interp", response_col = "BT") {
  # Ensure columns are numeric
  data[[interp_col]] <- as.numeric(data[[interp_col]])
  data[[response_col]] <- as.numeric(data[[response_col]])
  
  # Perform regression for the entire dataset (Overall)
  formula <- as.formula(paste(interp_col, "~", response_col))
  overall_model <- lm(formula, data)
  overall_summary <- summary(overall_model)
  overall_p_value <- ifelse(overall_summary$fstatistic[1] > 0, 
                            pf(overall_summary$fstatistic[1], overall_summary$fstatistic[2], overall_summary$fstatistic[3], lower.tail = FALSE), 
                            NA)
  
  overall_result <- data.frame(
    Genus_species = "Overall",
    R2 = round(overall_summary$r.squared, 3),
    Correlation_coff = round(cor(data[[interp_col]], data[[response_col]], use = "complete.obs"), 3),
    Df = overall_summary$df[2],
    Fstatistic = round(overall_summary$fstatistic[1], 3),
    Pvalue = round(overall_p_value, 3)
  )
  
  # Group by species and perform linear regression
  species_results <- data %>%
    group_by(Genus_species) %>%
    do({
      if (nrow(.) > 10) {  # Ensure sufficient data points
        model <- lm(formula, data = .)
        summary_model <- summary(model)
        p_value <- ifelse(summary_model$fstatistic[1] > 0, 
                          pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE), 
                          NA)
        
        data.frame(
          Genus_species = unique(.$Genus_species),
          R2 = round(summary_model$r.squared, 3),
          Correlation_coff = round(cor(.[[interp_col]], .[[response_col]], use = "complete.obs"), 3),
          Df = summary_model$df[2],
          Fstatistic = round(summary_model$fstatistic[1], 3),
          Pvalue = round(p_value, 3)
        )
      } else {
        return(data.frame(Genus_species = unique(.$Genus_species), R2 = NA, Correlation_coff = NA, Df = NA, Fstatistic = NA, Pvalue = NA))
      }
    })
  
  # Combine Overall results with Species results
  results <- rbind(overall_result, species_results)
  
  return(results)
}



############
# Observed vs predicted: TA and TB
plot_obs_vs_pred <-function(data) {
  # Calculate statistics for TA
  obspred_TA <- na.omit(cbind(data$AT, data$TA_interp))
  r.TA <- cor(obspred_TA[, 1], obspred_TA[, 2])
  p.TA <- cor.test(obspred_TA[, 1], obspred_TA[, 2])$p.value
  rmsd.TA <- sqrt(mean(((obspred_TA[, 1] - obspred_TA[, 2]) ^ 2), na.rm = TRUE))
  bias.TA <- mean((obspred_TA[, 1] - obspred_TA[, 2]), na.rm = TRUE)
  
  # Calculate statistics for TB
  obspred_TB <- na.omit(cbind(data$BT, data$TC_interp))
  r.TB <- cor(obspred_TB[, 1], obspred_TB[, 2])
  p.TB <- cor.test(obspred_TB[, 1], obspred_TB[, 2])$p.value
  rmsd.TB <- sqrt(mean(((obspred_TB[, 1] - obspred_TB[, 2]) ^ 2), na.rm = TRUE))
  bias.TB <- mean((obspred_TB[, 1] - obspred_TB[, 2]), na.rm = TRUE)
  
  # Define species shapes
  unique_species <- unique(data$species)
  species_shapes <- setNames(1:length(unique_species), unique_species)
  region <- unique(data$region)
  
  # Plotting
  plot(data$TA_interp, data$BT, type = "n", ylim = c(12, 47), 
       xlim = c(12, 47), ylab = 'obs', xlab = 'pred', main = paste0(region),
       cex.main = 0.7, cex.lab = 0.6, cex.axis = 0.6)
  text(32, 46, paste0('TA r=', round(r.TA, 2), ' rmsd=', round(rmsd.TA, 1), 
                      ' bias=', round(bias.TA, 1)), cex = 0.5)
  
  for (sp in unique_species) {
    subset_data <- subset(data, species == sp)
    points(subset_data$TC_interp, subset_data$BT, pch = species_shapes[sp], col = 'gold')
    points(subset_data$TA_interp, subset_data$AT, pch = species_shapes[sp], col = 'lightblue')
  }
  
  text(32, 42, paste0('TB r=', round(r.TB, 2), ' rmsd=', round(rmsd.TB, 1), 
                      ' bias=', round(bias.TB, 1)), cex = 0.5)
  
  abline(0, 1)
  
  # Add legend
  legend("bottomright", legend = names(species_shapes), 
         pch = species_shapes, title = "Species", cex = 0.5)       
}



###################
# 70 year ERA 5 TA Functions for Africa and Australia
kal_ERA5_ta <- function(kal_environ, return_stats = FALSE) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Site = character(), Variable = character(), 
                         Correlation = numeric(), P_value = numeric(),
                         Slope = numeric())
  
  # Summarize Tb min, mean, max by day, year, and species
  summary_ta <- kal_environ %>%
    group_by(year, date, site) %>%
    summarise(
      mean_Ta = mean(TAREF, na.rm = TRUE),
      min_Ta = min(TAREF, na.rm = TRUE),
      max_Ta = max(TAREF, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, site) %>%
    summarise(
      mean_Ta = mean(mean_Ta, na.rm = TRUE),
      min_Ta = mean(min_Ta, na.rm = TRUE),
      max_Ta = mean(max_Ta, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get unique site names
  unique_site <- unique(summary_ta$site)
  
  # Create a color palette and shape vector
  colors <- c("coral2","bisque3", "darkgoldenrod3", "lightblue")
  shapes <- c(20, 20, 20, 20)
  
  # Initialize the plot layout
  par(mfrow = c(1, 3), mar = c(4, 2, 4, 2))
  
  # Create a function to plot the relationship
  plot_relationship <- function(y_var, y_label, add_map = FALSE, stats_df) {
    line_count <- -1  # Initialize line count for mtext
    ylim_range <- range(summary_ta[[y_var]], na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - .8, ylim_range[2] + .6)
    plot(NA, xlim = range(kal_environ$year), ylim = ylim_range_adjusted, 
         main = paste(y_label, "across years"), 
         xlab = "Year", ylab = paste(y_label, "(°C)"), type = "n")
    
    for (i in 1:length(unique_site)) {
      sp <- unique_site[i]
      subset_data <- subset(summary_ta, site == sp)
      points(subset_data$year, subset_data[[y_var]], col = colors[i], pch = shapes[i])
      
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        abline(lm(subset_data[[y_var]] ~ subset_data$year), col = colors[i])
        
        # Calculate and add correlation coefficients and p-values
        lm_fit <- lm(subset_data[[y_var]] ~ subset_data$year)
        slope <- coef(lm_fit)["subset_data$year"]  # Extracting the slope
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        p_value <- cor.test(subset_data$year, subset_data[[y_var]], 
                            use = "complete.obs")$p.value
        mtext(paste(sp, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[i], cex = 0.5)
        line_count <- line_count - 1  # Move the next line of text up
        
        # Add to stats data frame
        stats_df <- rbind(stats_df, data.frame(Site = sp, Variable = y_label, 
                                               Correlation = r, P_value = p_value, 
                                               Slope = slope))
      }
    }
    
    # Add black dashed line for overall relationship
    if (sum(!is.na(summary_ta[[y_var]])) > 1) {
      overall_lm <- lm(summary_ta[[y_var]] ~ summary_ta$year)
      overall_slope <- coef(overall_lm)["summary_ta$year"]
      abline(overall_lm, col = "black", lty = 2, lwd = 3)
      
      # Calculate and add overall correlation coefficients and p-values
      overall_r <- cor(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")
      overall_p_value <- cor.test(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")$p.value
      mtext(paste("Overall: r =", round(overall_r, 3), ", p =", round(overall_p_value, 3)), 
            side = 1, line = line_count, adj = 0, col = "black", cex = 0.5)
      line_count <- line_count - 1  # Move the next line of text up
      
      # Add to stats data frame
      stats_df <- rbind(stats_df, data.frame(Site = "Overall", Variable = y_label, 
                                             Correlation = overall_r, 
                                             P_value = overall_p_value, 
                                             Slope = overall_slope))
    }
    
    if (add_map) {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(-15, 130), ylim = c(-320, 25))
      map('world', xlim = c(-20, 60), ylim = c(-40, 36), fill = FALSE, add = TRUE)
      # Add a X at lat -28.5, lon 122.67 - The REAL L site
      points(20.75, -26.35, pch = 4, cex = .8, col = "black", lwd = 1.5)
      box()
      par(new = FALSE)
    }
    
    return(stats_df)
  }
  
  stats_df <- plot_relationship("min_Ta", "Min Ta", add_map = TRUE, stats_df)
  stats_df <- plot_relationship("mean_Ta", "Mean Ta", add_map = FALSE, stats_df)
  stats_df <- plot_relationship("max_Ta", "Max Ta", add_map = FALSE, stats_df)
  
  legend("topright", legend = unique_site, col = colors, pch = shapes, title = "Site")
  
  # Return stats_df if return_stats is TRUE
  if (return_stats) {
    return(stats_df)
  }
}

# australia
aus_ERA5_ta <- function(aus_environ, return_stats = FALSE) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Site = character(), Variable = character(), 
                         Correlation = numeric(), P_value = numeric(),
                         Slope = numeric())
  
  # Summarize Tb min, mean, max by day, year, and species
  summary_ta <- aus_environ %>%
    group_by(year, date, site) %>%
    summarise(
      mean_Ta = mean(TAREF, na.rm = TRUE),
      min_Ta = min(TAREF, na.rm = TRUE),
      max_Ta = max(TAREF, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, site) %>%
    summarise(
      mean_Ta = mean(mean_Ta, na.rm = TRUE),
      min_Ta = mean(min_Ta, na.rm = TRUE),
      max_Ta = mean(max_Ta, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get unique site names
  unique_site <- unique(summary_ta$site)
  
  # Create a color palette and shape vector
  colors <- c("coral2","bisque3", "darkgoldenrod3")
  shapes <- c(20, 20, 20)
  
  # Initialize the plot layout
  par(mfrow = c(1, 3), mar = c(4, 2, 4, 2))
  
  # Create a function to plot the relationship
  plot_relationship <- function(y_var, y_label, add_map = FALSE, stats_df) {
    line_count <- -1  # Initialize line count for mtext
    ylim_range <- range(summary_ta[[y_var]], na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - .8, ylim_range[2] + .6)
    plot(NA, xlim = range(aus_environ$year), ylim = ylim_range_adjusted, 
         main = paste(y_label, "across years"), 
         xlab = "Year", ylab = paste(y_label, "(°C)"), type = "n")
    
    for (i in 1:length(unique_site)) {
      sp <- unique_site[i]
      subset_data <- subset(summary_ta, site == sp)
      points(subset_data$year, subset_data[[y_var]], col = colors[i], pch = shapes[i])
      
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        abline(lm(subset_data[[y_var]] ~ subset_data$year), col = colors[i])
        
        # Calculate and add correlation coefficients and p-values
        lm_fit <- lm(subset_data[[y_var]] ~ subset_data$year)
        slope <- coef(lm_fit)["subset_data$year"]  # Extracting the slope
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        p_value <- cor.test(subset_data$year, subset_data[[y_var]], 
                            use = "complete.obs")$p.value
        mtext(paste(sp, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[i], cex = 0.5)
        line_count <- line_count - 1  # Move the next line of text up
        
        # Add to stats data frame
        stats_df <- rbind(stats_df, data.frame(Site = sp, Variable = y_label, 
                                               Correlation = r, P_value = p_value,
                                               Slope = slope))
      }
    }
    
    # Add black dashed line for overall relationship
    if (sum(!is.na(summary_ta[[y_var]])) > 1) {
      overall_lm <- lm(summary_ta[[y_var]] ~ summary_ta$year)
      overall_slope <- coef(overall_lm)["summary_ta$year"]
      abline(overall_lm, col = "black", lty = 2, lwd = 3)
      
      # Calculate and add overall correlation coefficients and p-values
      overall_r <- cor(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")
      overall_p_value <- cor.test(summary_ta$year, summary_ta[[y_var]], use = "complete.obs")$p.value
      mtext(paste("Overall: r =", round(overall_r, 3), ", p =", round(overall_p_value, 3)), 
            side = 1, line = line_count, adj = 0, col = "black", cex = 0.5)
      line_count <- line_count - 1  # Move the next line of text up
      
      # Add to stats data frame
      stats_df <- rbind(stats_df, data.frame(Site = "Overall", Variable = y_label, 
                                             Correlation = overall_r, 
                                             P_value = overall_p_value,
                                             Slope = overall_slope))
    }
    
    if (add_map) {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(110, 200), ylim = c(-200, -15))
      map('world', regions = 'Australia', fill = FALSE, add = TRUE)
      box()
      # Add a X at lat -28.5, lon 122.67 - L site
      points(122.67, -28.5, pch = 4, cex = .8, col = "black", lwd = 1.5)
      par(new = FALSE)
    }
    
    return(stats_df)
  }
  
  stats_df <- plot_relationship("min_Ta", "Min Ta", add_map = TRUE, stats_df)
  stats_df <- plot_relationship("mean_Ta", "Mean Ta", add_map = FALSE, stats_df)
  stats_df <- plot_relationship("max_Ta", "Max Ta", add_map = FALSE, stats_df)
  
  legend("topright", legend = unique_site, col = colors, pch = shapes, title = "Site")
  
  # Return stats_df if return_stats is TRUE
  if (return_stats) {
    return(stats_df)
  }
}

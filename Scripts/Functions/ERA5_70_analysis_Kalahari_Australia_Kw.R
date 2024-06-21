pacman::p_load(maps, dplyr)
##########
### Load environ files for Australian species
pomi_environ<-readRDS("output/environ/ERA5_70YR_Australia_Pogona_minor_A_environ.RDS")
ctqu_environ<-readRDS("output/environ/ERA5_70YR_Australia_Ctenotus_quatt_L_environ.RDS")
moho_environ<-readRDS("output/environ/ERA5_70YR_Australia_Moloch_horridus_R_environ.RDS")
geva_environ<-readRDS("output/environ/ERA5_70YR_Australia_Gehyra_variegata_L_environ.RDS")
ctis_environ<-readRDS("output/environ/ERA5_70YR_Australia_Ctenophorus_isolepis_L_environ.RDS")
# combine species
aus_environ <- rbind(pomi_environ, ctqu_environ, moho_environ, geva_environ, ctis_environ) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species))
### Load environ files for African species 
agac_environ<-readRDS("output/environ/ERA5_70YR_Kalahari_Agama_aculeata_X_environ.RDS")
chan_environ<-readRDS("output/environ/ERA5_70YR_Kalahari_Chondrodactylus_angulifer_A_environ.RDS")
mesu_environ<-readRDS("output/environ/ERA5_70YR_Kalahari_Meroles_suborbitalis_L_environ.RDS")
peli_environ<-readRDS("output/environ/ERA5_70YR_Kalahari_Pedioplanis_lineoocellata_A_environ.RDS")
trsp_environ<-readRDS("output/environ/ERA5_70YR_Kalahari_Trachylepis_sparsa_B_environ.RDS")
kal_environ <- rbind(agac_environ, chan_environ, mesu_environ, peli_environ, trsp_environ) %>% 
  mutate(species = paste0(substr(genus, 1, 1), ". ", species))


########
# Tables and plots
########
# AUS: Tb function with tbl output and figure
aus_ERA5_tb <- function(aus_environ) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), 
                         Correlation = numeric(), P_value = numeric())
  
  # Summarize Tb min, mean, max by day, year, and species
  summary_tb <- aus_environ %>%
    group_by(year, date, species) %>%
    summarise(
      mean_Tb = mean(TC, na.rm = TRUE),
      min_Tb = min(TC, na.rm = TRUE),
      max_Tb = max(TC, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, species) %>%
    summarise(
      mean_Tb = mean(mean_Tb, na.rm = TRUE),
      min_Tb = mean(min_Tb, na.rm = TRUE),
      max_Tb = mean(max_Tb, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get unique species names
  unique_species <- unique(summary_tb$species)
  
  # Create a color palette and shape vector
  colors <- c("coral2","bisque3", "grey40", "firebrick2", "darkgoldenrod3")
  shapes <- c(20, 20, 20, 20, 20)
  
  # Initialize the plot layout
  par(mfrow = c(1, 3))
  
  # Create a function to plot the relationship
  plot_relationship <- function(y_var, y_label, add_map = FALSE, stats_df) {
    line_count <- -1  # Initialize line count for mtext
    ylim_range <- range(summary_tb[[y_var]], na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - .8, ylim_range[2] + .6)
    plot(NA, xlim = range(aus_environ$year), ylim = ylim_range_adjusted, 
         main = paste(y_label, "across years"), 
         xlab = "Year", ylab = paste(y_label, "(°C)"), type = "n")
    
    for (i in 1:length(unique_species)) {
      sp <- unique_species[i]
      subset_data <- subset(summary_tb, species == sp)
      points(subset_data$year, subset_data[[y_var]], col = colors[i], pch = shapes[i])
      
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        abline(lm(subset_data[[y_var]] ~ subset_data$year), col = colors[i])
        
        # Calculate and add correlation coefficients and p-values
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        p_value <- cor.test(subset_data$year, subset_data[[y_var]], 
                            use = "complete.obs")$p.value
        mtext(paste(sp, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[i], cex = 0.7)
        line_count <- line_count - 1  # Move the next line of text up
        
        # Add to stats data frame
        stats_df <- rbind(stats_df, data.frame(Species = sp, Variable = y_label, 
                                               Correlation = r, P_value = p_value))
      }
    }
    
    # Add black dashed line for overall relationship
    if (sum(!is.na(summary_tb[[y_var]])) > 1) {
      overall_lm <- lm(summary_tb[[y_var]] ~ summary_tb$year)
      abline(overall_lm, col = "black", lty = 2, lwd = 3)
      
      # Calculate and add overall correlation coefficients and p-values
      overall_r <- cor(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")
      overall_p_value <- cor.test(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")$p.value
      mtext(paste("Overall: r =", round(overall_r, 3), ", p =", round(overall_p_value, 3)), 
            side = 1, line = line_count, adj = 0, col = "black", cex = 0.7)
      line_count <- line_count - 1  # Move the next line of text up
      
      # Add to stats data frame
      stats_df <- rbind(stats_df, data.frame(Species = "Overall", Variable = y_label, 
                                             Correlation = overall_r, P_value = overall_p_value))
    }
    
    if (add_map) {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(110, 200), ylim = c(-200, -15))
      map('world', regions = 'Australia', fill = FALSE, add = TRUE)
      box()
      # Add a X at lat -28.5, lon 122.67 - L site
      points(122.67, -28.5, pch = 4, cex = 1.5, col = "black", lwd = 3)
      par(new = FALSE)
    }
    
    return(stats_df)
  }
  
  stats_df <- plot_relationship("min_Tb", "Min Tb", add_map = TRUE, stats_df)
  stats_df <- plot_relationship("mean_Tb", "Mean Tb", add_map = FALSE, stats_df)
  stats_df <- plot_relationship("max_Tb", "Max Tb", add_map = FALSE, stats_df)
  
  legend("topright", legend = unique_species, col = colors, pch = shapes, title = "Species")
  
  return(stats_df)
}
aus_ERA5_tb_tbl <- aus_ERA5_tb(aus_environ)

# Kal: Tb function with tbl output and figure
kal_ERA5_tb <- function(kal_environ) {
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), 
                         Correlation = numeric(), P_value = numeric())
  
  # Summarize Tb min, mean, max by day, year, and species
  summary_tb <- kal_environ %>%
    group_by(year, date, species) %>%
    summarise(
      mean_Tb = mean(TC, na.rm = TRUE),
      min_Tb = min(TC, na.rm = TRUE),
      max_Tb = max(TC, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(year, species) %>%
    summarise(
      mean_Tb = mean(mean_Tb, na.rm = TRUE),
      min_Tb = mean(min_Tb, na.rm = TRUE),
      max_Tb = mean(max_Tb, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get unique species names
  unique_species <- unique(summary_tb$species)
  
  # Create a color palette and shape vector
  colors <- c("coral2","bisque3", "grey40", "firebrick2", "darkgoldenrod3")
  shapes <- c(20, 20, 20, 20, 20)
  
  # Initialize the plot layout
  par(mfrow = c(1, 3))
  
  # Create a function to plot the relationship
  plot_relationship <- function(y_var, y_label, add_map = FALSE, stats_df) {
    line_count <- -1  # Initialize line count for mtext
    ylim_range <- range(summary_tb[[y_var]], na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - .8, ylim_range[2] + 1.5)
    plot(NA, xlim = range(kal_environ$year), ylim = ylim_range_adjusted, 
         main = paste(y_label, "across years"), 
         xlab = "Year", ylab = paste(y_label, "(°C)"), type = "n")
    
    for (i in 1:length(unique_species)) {
      sp <- unique_species[i]
      subset_data <- subset(summary_tb, species == sp)
      points(subset_data$year, subset_data[[y_var]], col = colors[i], pch = shapes[i])
      
      if (sum(!is.na(subset_data[[y_var]])) > 1) {
        abline(lm(subset_data[[y_var]] ~ subset_data$year), col = colors[i])
        
        # Calculate and add correlation coefficients and p-values
        r <- cor(subset_data$year, subset_data[[y_var]], use = "complete.obs")
        p_value <- cor.test(subset_data$year, subset_data[[y_var]], 
                            use = "complete.obs")$p.value
        mtext(paste(sp, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[i], cex = 0.7)
        line_count <- line_count - 1  # Move the next line of text up
        
        # Add to stats data frame
        stats_df <- rbind(stats_df, data.frame(Species = sp, Variable = y_label, 
                                               Correlation = r, P_value = p_value))
      }
    }
    
    # Add black dashed line for overall relationship
    if (sum(!is.na(summary_tb[[y_var]])) > 1) {
      overall_lm <- lm(summary_tb[[y_var]] ~ summary_tb$year)
      abline(overall_lm, col = "black", lty = 2, lwd = 3)
      
      # Calculate and add overall correlation coefficients and p-values
      overall_r <- cor(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")
      overall_p_value <- cor.test(summary_tb$year, summary_tb[[y_var]], use = "complete.obs")$p.value
      mtext(paste("Overall: r =", round(overall_r, 3), ", p =", round(overall_p_value, 3)), 
            side = 1, line = line_count, adj = 0, col = "black", cex = 0.7)
      line_count <- line_count - 1  # Move the next line of text up
      
      # Add to stats data frame
      stats_df <- rbind(stats_df, data.frame(Species = "Overall", Variable = y_label, 
                                             Correlation = overall_r, P_value = overall_p_value))
    }
    
    if (add_map) {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(-15, 130), ylim = c(-320, 25))
      map('world', xlim = c(-20, 60), ylim = c(-40, 36), fill = FALSE, add = TRUE)
      # Add a X at lat -28.5, lon 122.67 - The REAL L site
      points(20.75, -26.35, pch = 4, cex = 1.5, col = "black", lwd = 3)
      box()
      par(new = FALSE)
    }
    
    return(stats_df)
  }
  
  stats_df <- plot_relationship("min_Tb", "Min Tb", add_map = TRUE, stats_df)
  stats_df <- plot_relationship("mean_Tb", "Mean Tb", add_map = FALSE, stats_df)
  stats_df <- plot_relationship("max_Tb", "Max Tb", add_map = FALSE, stats_df)
  
  legend("topright", legend = unique_species, col = colors, pch = shapes, title = "Species")
  
  return(stats_df)
}
kal_ERA5_tb_tbl <- kal_ERA5_tb(kal_environ)

# AUS: behaviour  - NOT including season - tbl and figure
aus_comb_beh <- function(aus_environ) {
  
  # Save original par settings
  opar <- par(no.readonly = TRUE)
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), 
                         Correlation = numeric(), P_value = numeric())
  
  # Summarizing the data for h/d for each day and then year
  total_annual_time <- aus_environ %>%
    mutate(
      behaviour = case_when(
        ACT %in% c(1, 2) ~ "active",
        ACT == 0 ~ "inactive",
        TRUE ~ as.character(ACT))) %>% 
    group_by(year, date, behaviour, species) %>%
    summarise(
      daily_hr = n()) %>% 
    group_by(year, behaviour, species) %>%
    summarise(annual_hr = mean(daily_hr))
  
  behaviors <- c("inactive", "active")
  
  # Create a color palette and shape vector
  colors <- c("coral2","bisque3", "grey40", "firebrick2", "darkgoldenrod3")
  shapes <- c(20, 20, 20, 20, 20)
  
  # Set up the layout for 1 row x 3 columns
  layout_matrix <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
  layout(layout_matrix, widths = c(1, 1, 0.2))
  
  # Loop through each behavior
  for(behavior in behaviors) {
    
    line_count <- -1  # Initialize line count for mtext
    
    # Extract behavior-specific data
    behavior_data <- total_annual_time %>% 
      filter(behaviour == behavior)
    
    ylim_range <- range(behavior_data$annual_hr, na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - 1.4, ylim_range[2] + .5)
    
    # Plot initialization
    plot(NA, xlim = range(kal_environ$year), ylim = ylim_range_adjusted, 
         main = paste(behavior, "across years"), 
         xlab = "Year", ylab = "Mean Activity (h/d)", 
         type = "n")
    
    # Add overall relationship (dashed black line)
    if (sum(!is.na(behavior_data$annual_hr)) > 1) {
      abline(lm(annual_hr ~ year, data = behavior_data), 
             col = 'black', lty = 2, lwd = 2.5)
      
      # Calculate and add correlation coefficients and p-values for overall relationship
      r_overall <- cor(behavior_data$year, behavior_data$annual_hr, use = "complete.obs")
      p_value_overall <- cor.test(behavior_data$year, behavior_data$annual_hr, 
                                  use = "complete.obs")$p.value
      mtext(paste("Overall: r =", round(r_overall, 3), ", p =", round(p_value_overall, 3)), 
            side = 1, line = line_count, adj = 0, col = "black", cex = 0.7)
      line_count <- line_count - 1  # Move the next line of text up
      
      # Add to stats data frame
      stats_df <- rbind(stats_df, data.frame(Species = "Overall", Variable = behavior, 
                                             Correlation = r_overall, P_value = p_value_overall))
    }
    
    # Add points and lines
    for (i in 1:length(unique(behavior_data$species))) {
      sp <- unique(behavior_data$species)[i]
      sp_data <- subset(behavior_data, species == sp)
      
      points(sp_data$year, sp_data$annual_hr, col = colors[i], pch = shapes[i])
      
      if (sum(!is.na(sp_data$annual_hr)) > 1) {
        abline(lm(annual_hr ~ year, data = sp_data), col = colors[i])
        
        # Calculate and add correlation coefficients and p-values
        r <- cor(sp_data$year, sp_data$annual_hr, use = "complete.obs")
        p_value <- cor.test(sp_data$year, sp_data$annual_hr, 
                            use = "complete.obs")$p.value
        mtext(paste(sp, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[i], cex = 0.7)
        line_count <- line_count - 1  # Move the next line of text up
        
        # Add to stats data frame
        stats_df <- rbind(stats_df, data.frame(Species = sp, Variable = behavior, 
                                               Correlation = r, P_value = p_value))
      }
    } 
    
    if (behavior == "inactive") {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(115, 320), ylim = c(-310, -20))
      map('world', regions = 'Australia', fill = FALSE, add = TRUE)
      box()
      par(new = FALSE)
    }
  }
  
  # Add the legend in the third "plot" area
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", legend = c("Overall", unique(behavior_data$species)), 
         col = c("black", colors), pch = shapes, title = "Species", cex = 1)
  
  # Restore original par settings
  par(opar)
  
  return(stats_df)
}
aus_comb_beh_tbl <- aus_comb_beh(aus_environ)

# KAL: behaviour - NOT including season - tbl and figure
kal_comb_beh <- function(kal_environ) {
  
  # Save original par settings
  opar <- par(no.readonly = TRUE)
  
  # Initialize an empty data frame to store statistics
  stats_df <- data.frame(Species = character(), Variable = character(), 
                         Correlation = numeric(), P_value = numeric())
  
  # Summarizing the data for h/d for each day and then year
  total_annual_time <- kal_environ %>%
    mutate(
      behaviour = case_when(
        ACT %in% c(1, 2) ~ "active",
        ACT == 0 ~ "inactive",
        TRUE ~ as.character(ACT))) %>% 
    group_by(year, date, behaviour, species) %>%
    summarise(
      daily_hr = n()) %>% 
    group_by(year, behaviour, species) %>%
    summarise(annual_hr = mean(daily_hr))
  
  behaviors <- c("inactive", "active")
  
  # Create a color palette and shape vector
  colors <- c("coral2","bisque3", "grey40", "firebrick2", "darkgoldenrod3")
  shapes <- c(20, 20, 20, 20, 20)
  
  # Set up the layout for 1 row x 3 columns
  layout_matrix <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
  layout(layout_matrix, widths = c(1, 1, 0.3))  # Adjusted the width for the legend
  
  # Loop through each behavior
  for(behavior in behaviors) {
    
    line_count <- -1  # Initialize line count for mtext
    
    # Extract behavior-specific data
    behavior_data <- total_annual_time %>% 
      filter(behaviour == behavior)
    
    ylim_range <- range(behavior_data$annual_hr, na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - 1, ylim_range[2] + .5)
    
    # Plot initialization
    plot(NA, xlim = range(kal_environ$year), ylim = ylim_range_adjusted, 
         main = paste(behavior, "across years"), 
         xlab = "Year", ylab = "Mean Activity (h/d)", 
         type = "n")
    
    # Add overall relationship (dashed black line)
    if (sum(!is.na(behavior_data$annual_hr)) > 1) {
      abline(lm(annual_hr ~ year, data = behavior_data), 
             col = 'black', lty = 2, lwd = 2.5)
      
      # Calculate and add correlation coefficients and p-values for overall relationship
      r_overall <- cor(behavior_data$year, behavior_data$annual_hr, use = "complete.obs")
      p_value_overall <- cor.test(behavior_data$year, behavior_data$annual_hr, 
                                  use = "complete.obs")$p.value
      mtext(paste("Overall: r =", round(r_overall, 3), ", p =", round(p_value_overall, 3)), 
            side = 1, line = line_count, adj = 0, col = "black", cex = 0.7)
      line_count <- line_count - 1  # Move the next line of text up
      
      # Add to stats data frame
      stats_df <- rbind(stats_df, data.frame(Species = "Overall", Variable = behavior, 
                                             Correlation = r_overall, P_value = p_value_overall))
    }
    
    # Add points and lines
    for (i in 1:length(unique(behavior_data$species))) {
      sp <- unique(behavior_data$species)[i]
      sp_data <- subset(behavior_data, species == sp)
      
      points(sp_data$year, sp_data$annual_hr, col = colors[i], pch = shapes[i])
      
      if (sum(!is.na(sp_data$annual_hr)) > 1) {
        abline(lm(annual_hr ~ year, data = sp_data), col = colors[i])
        
        # Calculate and add correlation coefficients and p-values
        r <- cor(sp_data$year, sp_data$annual_hr, use = "complete.obs")
        p_value <- cor.test(sp_data$year, sp_data$annual_hr, 
                            use = "complete.obs")$p.value
        mtext(paste(sp, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[i], cex = 0.7)
        line_count <- line_count - 1  # Move the next line of text up
        
        # Add to stats data frame
        stats_df <- rbind(stats_df, data.frame(Species = sp, Variable = behavior, 
                                               Correlation = r, P_value = p_value))
      }
    } 
    
    if (behavior == "inactive") {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(-15, 260), ylim = c(-420, 25))
      map('world', xlim = c(-20, 60), ylim = c(-40, 36), fill = FALSE, add = TRUE)
      box()
      par(new = FALSE)
    }
  }
  
  # Add the legend in the third "plot" area
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", legend = c("Overall", unique(behavior_data$species)), 
         col = c("black", colors), pch = shapes, title = "Species", cex = 1)
  
  # Restore original par settings
  par(opar)
  
  return(stats_df)
}
kal_comb_beh_tbl <- kal_comb_beh(kal_environ) 

# Aus seasonal activity grouped with species and output tbl by species
aus_seasonal_beh <- function(aus_environ) {
  
  # Save original par settings
  opar <- par(no.readonly = TRUE)
  
  # Initialize statistics DataFrame
  stats_df <- data.frame(Species = character(),
                         Behavior = character(),
                         Season = character(),
                         Slope = numeric(),
                         P_Value = numeric(),
                         Correlation = numeric())
  
  # Filter out winter and summer: Oct-Dec
  environ_season <- aus_environ %>%
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(1, 2) ~ "active",
      ACT == 0 ~ "inactive",
      TRUE ~ as.character(ACT)
    ))
  
  # Summarize the data for hours per day (h/d) and then each year - plot
  season_time <- environ_season %>%
    group_by(year, date, ACT_combined, season, species) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season, species) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season) %>% # drop species
    summarise(annual_hr = mean(annual_hr), .groups = 'drop')
  
  # Summarize the data for hours per day (h/d) and then each year - plot
  season_time_tbl <- environ_season %>%
    group_by(year, date, ACT_combined, season, species) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season, species) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop') 
  
  # Create a color palette
  colors <- c("winter" = "blue", "summer" = "red")
  
  # Set up the layout for 1 row x 3 columns
  layout_matrix <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
  layout(layout_matrix, widths = c(1, 1, 0.2))
  
  # Loop through each behavior in the order "inactive", "active"
  for(behavior in c("inactive", "active")) {
    
    # Extract behavior-specific data
    behavior_data <- season_time %>% 
      filter(ACT_combined == behavior)
    
    ylim_range <- range(behavior_data$annual_hr, na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - 1.4, ylim_range[2] + .5)
    
    # Plot initialization
    plot(NA, xlim = range(aus_environ$year), ylim = ylim_range_adjusted, 
         main = paste(behavior, "across years"), 
         xlab = "Year", ylab = "Mean Activity (h/d)", 
         type = "n")
    
    # Reset line_count for each behavior
    line_count <- -1
    
    # Add points and lines for each season
    for (s in unique(behavior_data$season)) {
      
      season_data <- subset(behavior_data, season == s)
      
      points(season_data$year, season_data$annual_hr, 
             col = colors[s], pch = 21)
      
      if (sum(!is.na(season_data$annual_hr)) > 1) {
        fit <- lm(annual_hr ~ year, data = season_data)
        abline(fit, col = colors[s])
        
        # Calculate and add correlation coefficients and p-values
        r <- cor(season_data$year, season_data$annual_hr, use = "complete.obs")
        p_value <- cor.test(season_data$year, season_data$annual_hr, 
                            use = "complete.obs")$p.value
        mtext(paste(s, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[s], cex = 0.7)
        line_count <- line_count - 1  # Move the next line of text up
      }
    }
    
    if (behavior == "inactive") {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(115, 320), ylim = c(-310, -20))
      map('world', regions = 'Australia', fill = FALSE, add = TRUE)
      box()
      par(new = FALSE)
    }
  }
  
  # Add the legend in the third "plot" area
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", legend = c("Winter", "Summer"), 
         col = c("blue", "red"), pch = 21, title = "Season", cex = 1)
  
  # Calculate statistics for each species, behavior, and season
  for (species in unique(season_time_tbl$species)) {
    for (behavior in unique(season_time_tbl$ACT_combined)) {
      for (s in unique(season_time_tbl$season)) {
        species_behavior_season_data <- filter(season_time_tbl, species == !!species & ACT_combined == !!behavior & season == !!s)
        
        if (nrow(species_behavior_season_data) > 1) {  # Check if there's enough data to fit a model
          fit <- lm(annual_hr ~ year, data = species_behavior_season_data)
          correlation <- cor(species_behavior_season_data$year, species_behavior_season_data$annual_hr, use = "complete.obs")
          stats_df <- rbind(stats_df, data.frame(
            Species = species,
            Behavior = behavior,
            Season = s,
            Slope = round(coef(fit)[2], 3),
            P_Value = round(summary(fit)$coefficients[2, 4], 3),
            Correlation = round(correlation, 3)
          ))
        }
      }
    }
  }
  # Remove the 'year' columm 
  rownames(stats_df) <- NULL
  
  # Restore original par settings
  par(opar)
  
  return(stats_df)
}
aus_seasonal_beh_tbl <- aus_seasonal_beh(aus_environ)

# KAL: seasonal activity grouped with and output tbl by species
kal_seasonal_beh <- function(kal_environ) {
  
  # Save original par settings
  opar <- par(no.readonly = TRUE)
  
  # Initialize statistics DataFrame
  stats_df <- data.frame(Species = character(),
                         Behavior = character(),
                         Season = character(),
                         Slope = numeric(),
                         P_Value = numeric(),
                         Correlation = numeric())
  
  # Filter out winter and summer: Oct-Dec
  environ_season <- kal_environ %>%
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(1, 2) ~ "active",
      ACT == 0 ~ "inactive",
      TRUE ~ as.character(ACT)
    ))
  
  # Summarize the data for hours per day (h/d) and then each year - plot
  season_time <- environ_season %>%
    group_by(year, date, ACT_combined, season, species) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season, species) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season) %>% # drop species
    summarise(annual_hr = mean(annual_hr), .groups = 'drop')
  
  # Summarize the data for hours per day (h/d) and then each year - plot
  season_time_tbl <- environ_season %>%
    group_by(year, date, ACT_combined, season, species) %>%
    summarise(daily_hr = n(), .groups = 'drop') %>% 
    group_by(year, ACT_combined, season, species) %>%
    summarise(annual_hr = mean(daily_hr), .groups = 'drop') 
  
  # Create a color palette
  colors <- c("winter" = "blue", "summer" = "red")
  
  # Set up the layout for 1 row x 3 columns
  layout_matrix <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
  layout(layout_matrix, widths = c(1, 1, 0.2))
  
  # Loop through each behavior in the order "inactive", "active"
  for(behavior in c("inactive", "active")) {
    
    # Extract behavior-specific data
    behavior_data <- season_time %>% 
      filter(ACT_combined == behavior)
    
    ylim_range <- range(behavior_data$annual_hr, na.rm = TRUE)
    ylim_range_adjusted <- c(ylim_range[1] - 1.4, ylim_range[2] + 1)
    
    # Plot initialization
    plot(NA, xlim = range(aus_environ$year), ylim = ylim_range_adjusted, 
         main = paste(behavior, "across years"), 
         xlab = "Year", ylab = "Mean Activity (h/d)", 
         type = "n")
    
    # Reset line_count for each behavior
    line_count <- -1
    
    # Add points and lines for each season
    for (s in unique(behavior_data$season)) {
      
      season_data <- subset(behavior_data, season == s)
      
      points(season_data$year, season_data$annual_hr, 
             col = colors[s], pch = 21)
      
      if (sum(!is.na(season_data$annual_hr)) > 1) {
        fit <- lm(annual_hr ~ year, data = season_data)
        abline(fit, col = colors[s])
        
        # Calculate and add correlation coefficients and p-values
        r <- cor(season_data$year, season_data$annual_hr, use = "complete.obs")
        p_value <- cor.test(season_data$year, season_data$annual_hr, 
                            use = "complete.obs")$p.value
        mtext(paste(s, ": r =", round(r, 3), ", p =", round(p_value, 3)), 
              side = 1, line = line_count, adj = 0, col = colors[s], cex = 0.7)
        line_count <- line_count - 1  # Move the next line of text up
      }
    }
    
    if (behavior == "inactive") {
      # Add inset map
      par(new = TRUE)
      plot.window(xlim = c(-15, 260), ylim = c(-420, 25))
      map('world', xlim = c(-20, 60), ylim = c(-40, 36), fill = FALSE, add = TRUE)
      box()
      par(new = FALSE)
    }
  }
  
  # Add the legend in the third "plot" area
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", legend = c("Winter", "Summer"), 
         col = c("blue", "red"), pch = 21, title = "Season", cex = 1)
  
  # Calculate statistics for each species, behavior, and season
  for (species in unique(season_time_tbl$species)) {
    for (behavior in unique(season_time_tbl$ACT_combined)) {
      for (s in unique(season_time_tbl$season)) {
        species_behavior_season_data <- filter(season_time_tbl, species == !!species & ACT_combined == !!behavior & season == !!s)
        
        if (nrow(species_behavior_season_data) > 1) {  # Check if there's enough data to fit a model
          fit <- lm(annual_hr ~ year, data = species_behavior_season_data)
          correlation <- cor(species_behavior_season_data$year, species_behavior_season_data$annual_hr, use = "complete.obs")
          stats_df <- rbind(stats_df, data.frame(
            Species = species,
            Behavior = behavior,
            Season = s,
            Slope = round(coef(fit)[2], 3),
            P_Value = round(summary(fit)$coefficients[2, 4], 3),
            Correlation = round(correlation, 3)
          ))
        }
      }
    }
  }
  # Remove the 'year' columm 
  rownames(stats_df) <- NULL
  
  # Restore original par settings
  par(opar)
  
  return(stats_df)
}
kal_seasonal_beh_tbl <- kal_seasonal_beh(kal_environ)

# AUS: seasonal activity as a function of degree of change in temp
aus_delta_temp_activity <- function(aus_environ) {
  
  # Function to get differences by species
  perform_analysis <- function(data) {
    filter <- data %>% 
      filter(year == "2020") 
    mean_current <- mean(filter$TAREF)
      
    data %>%
      group_by(year, date) %>%
      summarise(daily_hr = n(),
                daily_TAREF = mean(TAREF), .groups = 'drop') %>%
      group_by(year) %>%
      summarise(annual_hr = mean(daily_hr),
                annual_TAREF = mean(daily_TAREF), .groups = 'drop') %>%
      mutate(annual_diff_TAREF = mean_current - annual_TAREF) 
    
    
  }
  
  # Your existing code for data manipulation
  environ_season <- aus_environ %>%
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(1, 2) ~ "active",
      ACT == 0 ~ "inactive",
      TRUE ~ as.character(ACT)
    ))
  
  nested_data <- environ_season %>%
    group_by(species, ACT_combined, season) %>%
    tidyr::nest()
  
  nested_data <- nested_data %>%
    mutate(data = purrr::map(data, ~perform_analysis(.x)))
  
  final_data <- nested_data %>%
    tidyr::unnest(cols = c(data))
  
  spp_df <- final_data %>%
    rename(species = species,
           behaviour = ACT_combined,
           season = season,
           annual_hr = annual_hr,
           annual_diff_TAREF = annual_diff_TAREF)
  
  final_df <- spp_df %>% 
    group_by(year, season, behaviour) %>% 
    summarise(annual_hr = mean(annual_hr),
              annual_diff_TAREF = mean(annual_diff_TAREF))
  # linear relationships for each behaviour and season - filtering
  summer_active <- final_df %>% filter(season == "summer" & behaviour == "active")
  summer_inactive <- final_df %>% filter(season == "summer" & behaviour == "inactive")
  winter_active <- final_df %>% filter(season == "winter" & behaviour == "active")
  winter_inactive <- final_df %>% filter(season == "winter" & behaviour == "inactive")
  
  # linear relationships for each behaviour and season - linear relationships
  # Run linear models
  lm_summer_active <- lm(annual_hr ~ annual_diff_TAREF, data = summer_active)
  lm_summer_inactive <- lm(annual_hr ~ annual_diff_TAREF, data = summer_inactive)
  lm_winter_active <- lm(annual_hr ~ annual_diff_TAREF, data = winter_active)
  lm_winter_inactive <- lm(annual_hr ~ annual_diff_TAREF, data = winter_inactive)
  
  # Calculate p-values and correlation coefficients
  pval_summer_active <- summary(lm_summer_active)$coefficients[2, 4]
  r_summer_active <- cor(summer_active$annual_hr, summer_active$annual_diff_TAREF, use = "complete.obs")
  pval_winter_active <- summary(lm_winter_active)$coefficients[2, 4]
  r_winter_active <- cor(winter_active$annual_hr, winter_active$annual_diff_TAREF, use = "complete.obs")
  pval_summer_inactive <- summary(lm_summer_inactive)$coefficients[2, 4]
  r_summer_inactive <- cor(summer_inactive$annual_hr, summer_inactive$annual_diff_TAREF, use = "complete.obs")
  pval_winter_inactive <- summary(lm_winter_inactive)$coefficients[2, 4]
  r_winter_inactive <- cor(winter_inactive$annual_hr, winter_inactive$annual_diff_TAREF, use = "complete.obs")
  
  
  # Plot1
  par(mfrow = c(1, 2))
  # Adjust the margins (bottom, left, top, right)
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  # Plot for Inactive Behaviour
  plot(NULL, xlim = range(final_df$annual_diff_TAREF, na.rm = TRUE), 
       ylim = range(final_df$annual_hr, na.rm = TRUE),
       xlab = "Warming Δ°C", ylab = "annual_hr", 
       main = "Inactive Behaviour")
  # Add points and lines for summer and winter
  points(summer_inactive$annual_diff_TAREF, summer_inactive$annual_hr, col = "red", pch = 19)
  abline(lm_summer_inactive, col = "red")
  points(winter_inactive$annual_diff_TAREF, winter_inactive$annual_hr, col = "blue", pch = 19)
  abline(lm_winter_inactive, col = "blue")
  # Add p-values and correlation coefficients to the Inactive Behaviour plot
  mtext(paste("Summer: r =", round(r_summer_inactive, 3), ", p =", round(pval_summer_inactive, 3)), 
        side = 1, line = 2, adj = 0, col = "red", cex = 0.7)
  mtext(paste("Winter: r =", round(r_winter_inactive, 3), ", p =", round(pval_winter_inactive, 3)), 
        side = 1, line = 3, adj = 0, col = "blue", cex = 0.7)
  
  # Plot for Active Behaviour
  plot(NULL, xlim = range(final_df$annual_diff_TAREF, na.rm = TRUE), 
       ylim = range(final_df$annual_hr, na.rm = TRUE),
       xlab = "Warming Δ°C", ylab = "annual_hr", 
       main = "Active Behaviour")
  # Add points and lines for summer and winter
  points(summer_active$annual_diff_TAREF, summer_active$annual_hr, col = "red", pch = 19)
  abline(lm_summer_active, col = "red")
  points(winter_active$annual_diff_TAREF, winter_active$annual_hr, col = "blue", pch = 19)
  abline(lm_winter_active, col = "blue")
  # Add p-values and correlation coefficients to the Active Behaviour plot
  mtext(paste("Summer: r =", round(r_summer_active, 3), ", p =", round(pval_summer_active, 3)), 
        side = 1, line = 2, adj = 0, col = "red", cex = 0.7)
  mtext(paste("Winter: r =", round(r_winter_active, 3), ", p =", round(pval_winter_active, 3)), 
        side = 1, line = 3, adj = 0, col = "blue", cex = 0.7)
  
  # Add inset map
  par(new = TRUE)
  plot.window(xlim = c(115, 320), ylim = c(-310, -20))
  map('world', regions = 'Australia', fill = FALSE, add = TRUE)
  box()
  par(new = FALSE)
  # Add legend
  legend("topright", legend = c("Summer", "Winter"), col = c("red", "blue"), pch = 19)
  
  # Reset the layout
  par(mfrow = c(1, 1))
  
  return(final_df)
}
aus_delta_temp_activity_tbl <- aus_delta_temp_activity(aus_environ)

# KAL: seasonal activity as a function of degree of change in temp
kal_delta_temp_activity <- function(kal_environ) {
  
  # Function to get differences by species
  perform_analysis <- function(data) {
    filter <- data %>% 
      filter(year == "2020") 
    mean_current <- mean(filter$TAREF)
    
    data %>%
      group_by(year, date) %>%
      summarise(daily_hr = n(),
                daily_TAREF = mean(TAREF), .groups = 'drop') %>%
      group_by(year) %>%
      summarise(annual_hr = mean(daily_hr),
                annual_TAREF = mean(daily_TAREF), .groups = 'drop') %>%
      mutate(annual_diff_TAREF = mean_current - annual_TAREF) 
    
    
  }
  
  # Your existing code for data manipulation
  environ_season <- kal_environ %>%
    mutate(season = case_when(
      month %in% c(6, 7, 8) ~ "winter",
      month %in% c(12, 1, 2) ~ "summer",
      TRUE ~ "other")) %>%
    filter(season != "other") %>%
    mutate(ACT_combined = case_when(
      ACT %in% c(1, 2) ~ "active",
      ACT == 0 ~ "inactive",
      TRUE ~ as.character(ACT)
    ))
  
  nested_data <- environ_season %>%
    group_by(species, ACT_combined, season) %>%
    tidyr::nest()
  
  nested_data <- nested_data %>%
    mutate(data = purrr::map(data, ~perform_analysis(.x)))
  
  final_data <- nested_data %>%
    tidyr::unnest(cols = c(data))
  
  spp_df <- final_data %>%
    rename(species = species,
           behaviour = ACT_combined,
           season = season,
           annual_hr = annual_hr,
           annual_diff_TAREF = annual_diff_TAREF)
  
  final_df <- spp_df %>% 
    group_by(year, season, behaviour) %>% 
    summarise(annual_hr = mean(annual_hr),
              annual_diff_TAREF = mean(annual_diff_TAREF))
  # linear relationships for each behaviour and season - filtering
  summer_active <- final_df %>% filter(season == "summer" & behaviour == "active")
  summer_inactive <- final_df %>% filter(season == "summer" & behaviour == "inactive")
  winter_active <- final_df %>% filter(season == "winter" & behaviour == "active")
  winter_inactive <- final_df %>% filter(season == "winter" & behaviour == "inactive")
  
  # linear relationships for each behaviour and season - linear relationships
  # Run linear models
  lm_summer_active <- lm(annual_hr ~ annual_diff_TAREF, data = summer_active)
  lm_summer_inactive <- lm(annual_hr ~ annual_diff_TAREF, data = summer_inactive)
  lm_winter_active <- lm(annual_hr ~ annual_diff_TAREF, data = winter_active)
  lm_winter_inactive <- lm(annual_hr ~ annual_diff_TAREF, data = winter_inactive)
  
  # Calculate p-values and correlation coefficients
  pval_summer_active <- summary(lm_summer_active)$coefficients[2, 4]
  r_summer_active <- cor(summer_active$annual_hr, summer_active$annual_diff_TAREF, use = "complete.obs")
  pval_winter_active <- summary(lm_winter_active)$coefficients[2, 4]
  r_winter_active <- cor(winter_active$annual_hr, winter_active$annual_diff_TAREF, use = "complete.obs")
  pval_summer_inactive <- summary(lm_summer_inactive)$coefficients[2, 4]
  r_summer_inactive <- cor(summer_inactive$annual_hr, summer_inactive$annual_diff_TAREF, use = "complete.obs")
  pval_winter_inactive <- summary(lm_winter_inactive)$coefficients[2, 4]
  r_winter_inactive <- cor(winter_inactive$annual_hr, winter_inactive$annual_diff_TAREF, use = "complete.obs")
  
  
  # Plot1
  par(mfrow = c(1, 2))
  # Adjust the margins (bottom, left, top, right)
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  # Plot for Inactive Behaviour
  plot(NULL, xlim = range(final_df$annual_diff_TAREF, na.rm = TRUE), 
       ylim = range(final_df$annual_hr, na.rm = TRUE),
       xlab = "Warming Δ°C", ylab = "annual_hr", 
       main = "Inactive Behaviour")
  # Add points and lines for summer and winter
  points(summer_inactive$annual_diff_TAREF, summer_inactive$annual_hr, col = "red", pch = 19)
  abline(lm_summer_inactive, col = "red")
  points(winter_inactive$annual_diff_TAREF, winter_inactive$annual_hr, col = "blue", pch = 19)
  abline(lm_winter_inactive, col = "blue")
  # Add p-values and correlation coefficients to the Inactive Behaviour plot
  mtext(paste("Summer: r =", round(r_summer_inactive, 3), ", p =", round(pval_summer_inactive, 3)), 
        side = 1, line = 2, adj = 0, col = "red", cex = 0.7)
  mtext(paste("Winter: r =", round(r_winter_inactive, 3), ", p =", round(pval_winter_inactive, 3)), 
        side = 1, line = 3, adj = 0, col = "blue", cex = 0.7)
  
  # Plot for Active Behaviour
  plot(NULL, xlim = range(final_df$annual_diff_TAREF, na.rm = TRUE), 
       ylim = range(final_df$annual_hr, na.rm = TRUE),
       xlab = "Warming Δ°C", ylab = "annual_hr", 
       main = "Active Behaviour")
  # Add points and lines for summer and winter
  points(summer_active$annual_diff_TAREF, summer_active$annual_hr, col = "red", pch = 19)
  abline(lm_summer_active, col = "red")
  points(winter_active$annual_diff_TAREF, winter_active$annual_hr, col = "blue", pch = 19)
  abline(lm_winter_active, col = "blue")
  # Add p-values and correlation coefficients to the Active Behaviour plot
  mtext(paste("Summer: r =", round(r_summer_active, 3), ", p =", round(pval_summer_active, 3)), 
        side = 1, line = 2, adj = 0, col = "red", cex = 0.7)
  mtext(paste("Winter: r =", round(r_winter_active, 3), ", p =", round(pval_winter_active, 3)), 
        side = 1, line = 3, adj = 0, col = "blue", cex = 0.7)
  
  # Add inset map
  par(new = TRUE)
  plot.window(xlim = c(-15, 260), ylim = c(-420, 25))
  map('world', xlim = c(-20, 60), ylim = c(-40, 36), fill = FALSE, add = TRUE)
  box()
  par(new = FALSE)
  # Add legend
  legend("topright", legend = c("Summer", "Winter"), col = c("red", "blue"), pch = 19)
  
  # Reset the layout
  par(mfrow = c(1, 1))
  
  return(final_df)
}
kal_delta_temp_activity_tbl <- kal_delta_temp_activity(kal_environ) 

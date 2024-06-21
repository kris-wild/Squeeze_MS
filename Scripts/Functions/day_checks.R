############################################
############################################

############ Daily checks function
###### Function that returns predicted Ta, Tb,
###### Activity, Shade % and depth/height of 
###### ecotherm outputs used in MS.
###### User needs to supply data of ecotherm output
###### and the required date in YYYY-MM-DD format
###### e.g. daily_plot(trsp_contemp, '1958-10-08')

###########
# upload packages
pacman::p_load(ggplot2,dplyr, patchwork)

###########
# Function for depth (DEP), activity, shade, temperature
daily_checks_plot <- function(data, plot_date, title_suffix = NULL) {
  
  # Filter for plot
  metout <- data %>% 
    dplyr::filter(date == plot_date) %>% 
    select(genus, species, date, hr, DEP, SHADE, TC, ACT, TA) %>% 
    rename(shade = SHADE, behaviour = ACT, Ta = TA, Tb = TC,
           depth_height_cm = DEP) %>% 
    mutate(across(c(Ta, Tb), round, 0))
  
  # Check if depth_height_cm has values greater than 5
  if (any(metout$depth_height_cm > 5)) {
    # Use the original p1 plot
    p1 <- ggplot(metout, aes(x = hr, y = depth_height_cm)) +
      geom_line(color = "brown", size = .5) +
      scale_y_continuous(limits = c(-20, 150)) +
      labs(y = "depth/height (cm)", x = "hr") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title.x = element_text(),
        axis.text.x = element_text(),
        axis.ticks.x = element_line()
      ) +
      annotate("text", x = 20, y = 140, label = "depth/height (cm)", color = "brown", 
               fontface = "bold", size = 4) +
      annotate("segment", x = 17, xend = 18, y = 140, yend = 140, color = "brown", 
               size = 2.5)
  } else {
    # Use the modified p1 plot with y limits from -20 to 5
    p1 <- ggplot(metout, aes(x = hr, y = depth_height_cm)) +
      geom_line(color = "brown", size = .5) +
      scale_y_continuous(limits = c(-20, 5)) +
      labs(y = "depth/height (cm)", x = "hr") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title.x = element_text(),
        axis.text.x = element_text(),
        axis.ticks.x = element_line()
      ) +
      annotate("text", x = 20, y = 4, label = "depth/height (cm)", color = "brown", 
               fontface = "bold", size = 4) +
      annotate("segment", x = 16, xend = 17, y = 4, yend = 4, color = "brown", 
               size = 2.5)
  }
  
  # Shade
  p2 <- ggplot(metout, aes(x = hr, y = shade)) +
    geom_line(color = "green3", size = .5, linetype = 'dashed') +
    scale_y_continuous(limits = c(0, 60)) +
    labs(y = "% shade", color = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    annotate("text", x = 20, y = 55, label = "% shade", color = "green3", fontface = "bold", size = 4) +
    annotate("segment", x = 17, xend = 18, y = 55, yend = 55, color = "green3", linetype = "dashed", 
             size = 2.5)
  
  # behavior factor
  metout$behavior_factor <- factor(metout$behaviour, levels = c(2, 1, 0), labels = c("A", "B", "I"))
  p3 <- ggplot(metout, aes(x = hr, y = behavior_factor)) +
    geom_point(aes(shape = behavior_factor, fill = behavior_factor), size = 4, color = "black") +
    scale_y_discrete(limits = c("I", "B", "A")) +
    scale_shape_manual(values = c("I" = 22, "B" = 22, "A" = 22)) +  # Use squares for all
    scale_fill_manual(values = c("I" = "white", "B" = "black", "A" = "gold")) +
    labs(y = "activity") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    annotate("point", x = 4, y = 3.5, color = "gold", fill = "gold", shape = 22, size = 2) +
    annotate("text", x = 4.5, y = 3.5, label = "Active", hjust = 0, fontface = 'bold', size = 3) +
    annotate("point", x = 8, y = 3.5, color = "black", fill = "black", shape = 22, size = 2) +
    annotate("text", x = 8.5, y = 3.5, label = "Basking", hjust = 0, fontface = 'bold', size = 3) +
    annotate("point", x = 13, y = 3.5, color = "black", fill = "white", shape = 22, size = 2) +
    annotate("text", x = 13.5, y = 3.5, label = "Inactive", hjust = 0, fontface = 'bold', size = 3)
  
  # species details
  genus <- unique(metout$genus)
  species <- unique(metout$species)
  date <- unique(metout$date)
  species_date <- paste0(genus, ' ', species, ' ', date)
  
  # Temperature plot
  title <- species_date
  if (!is.null(title_suffix)) {
    title <- paste0(species_date, " - ", title_suffix)
  }
  
  p4 <- ggplot(metout, aes(x = hr)) +
    geom_point(aes(y = Ta), color = "dodgerblue2", size = 3) +
    geom_line(aes(y = Ta), color = "dodgerblue2", size = 1) +
    geom_point(aes(y = Tb), color = "black", size = 3) +
    geom_line(aes(y = Tb), color = "black", size = 1) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(y = "Temperature (°C)", color = NULL, x = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.75)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    annotate("point", x = 20, y = 48, color = "dodgerblue2", shape = 21, size = 4, 
             fill = "dodgerblue2") +
    annotate("text", x = 21, y = 48, label = "T[a]", parse = TRUE,
             color = "black", fontface = "bold", hjust = 0, size = 6) +
    annotate("point", x = 20, y = 40, color = "black", shape = 21, size = 4, 
             fill = "black") +
    annotate("text", x = 21, y = 40, label = "T[b]", parse = TRUE,
             color = "black", fontface = "bold", hjust = 0, size = 6) +
    annotate("text", x = .5, y = 48, label = title, color = "black",
             fontface = "bold", hjust = 0, size = 4)
  
  combined_plot <- p4 / p3 / p2 / p1
  return(combined_plot)
}


############
# annual TB differences
pacman::p_load(dplyr, ggplot2, scales, patchwork)
plot_temperature_summary <- function(data_norm, data_shade) {
  # Combine genus and species
  genus_species <- paste0(unique(data_norm$genus), "_", unique(data_norm$species))
  
  # Summary for normal simulation
  data_norm_summary <- data_norm %>%
    group_by(yr, date) %>%
    summarise(daily_mean = mean(TC), 
              daily_min = min(TC), 
              daily_max = max(TC), 
              .groups = 'drop') %>%
    group_by(yr) %>%
    summarise(annual_mean = mean(daily_mean),
              annual_min = mean(daily_min),
              annual_max = mean(daily_max),
              .groups = 'drop') %>%
    mutate(Test = "norm_sim") 
  
  # Summary for zero shade simulation
  zero_shade_summary <- data_shade %>%
    group_by(yr, date) %>%
    summarise(daily_mean = mean(TC), 
              daily_min = min(TC), 
              daily_max = max(TC), 
              .groups = 'drop') %>%
    group_by(yr) %>%
    summarise(annual_mean = mean(daily_mean),
              annual_min = mean(daily_min),
              annual_max = mean(daily_max),
              .groups = 'drop') %>%
    mutate(Test = "zero_shade_sim") 
  
  # Combine the summaries
  data_summary <- rbind(data_norm_summary, zero_shade_summary)  
  data_summary$color <- rescale(as.numeric(data_summary$yr), to = c(0, 1))
  
  # Determine y-axis limits
  y_limits_min <- range(data_summary$annual_min)
  y_limits_mean <- range(data_summary$annual_mean)
  y_limits_max <- range(data_summary$annual_max)
  
  # Plot for min temperatures
  p1 <- ggplot(data_summary, aes(x = Test, y = annual_min, color = color, shape = Test)) +
    geom_point(position = position_dodge(width = 0.25), alpha = 0.3, size = 3) +
    stat_summary(fun.data = "mean_se", 
                 geom = "errorbar", position = position_dodge(width = 0.25), width = 0.2) +
    stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.25), size = 4) +
    scale_color_gradient(name = "", 
                         breaks = rescale(c(1950, 1985, 2020), to = c(0, 1)),
                         labels = c(1950, 1985, 2015),
                         low = "tomato", high = "purple3") +
    scale_y_continuous(limits = y_limits_min,   
                       breaks = seq(floor(y_limits_min[1]), ceiling(y_limits_min[2]), by = 1),
                       minor_breaks = seq(floor(y_limits_min[1]), ceiling(y_limits_min[2]), by = 0.5)) +
    labs(title = NULL, y = "Temperature (°C)", x = "") +
    scale_shape_discrete() +
    theme_bw() +
    theme(legend.position = c(0.95, 0.05), 
          legend.justification = c(1, 0),   
          legend.background = element_blank(),  
          legend.box.background = element_blank(), 
          plot.title = element_text(face="bold", hjust=0.5, family="Times", size=18),
          legend.title = element_blank()) +  
    ggtitle("Min")
  
  # Plot for mean temperatures
  p2 <- ggplot(data_summary, aes(x = Test, y = annual_mean, color = color, shape = Test)) +
    geom_point(position = position_dodge(width = 0.25), alpha = 0.3, size = 3) +
    stat_summary(fun.data = "mean_se", 
                 geom = "errorbar", position = position_dodge(width = 0.25), width = 0.2) +
    stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.25), size = 4) +
    scale_color_gradient(name = "", 
                         breaks = rescale(c(1950, 1985, 2020), to = c(0, 1)),
                         labels = c(1950, 1985, 2015),
                         low = "tomato", high = "purple3") +
    scale_y_continuous(limits = y_limits_mean,   
                       breaks = seq(floor(y_limits_mean[1]), ceiling(y_limits_mean[2]), by = 1),
                       minor_breaks = seq(floor(y_limits_mean[1]), ceiling(y_limits_mean[2]), by = 0.5)) +
    labs(title = NULL, y = "Temperature (°C)", x = "") +
    scale_shape_discrete() +
    theme_bw() +
    theme(legend.position = c(0.95, 0.05), 
          legend.justification = c(1, 0), 
          legend.background = element_blank(), 
          legend.box.background = element_blank(), 
          plot.title = element_text(face="bold", hjust=0.5, family="Times", size=18),
          legend.title = element_blank()) +  
    ggtitle(paste0("Mean ", genus_species))
  
  # Plot for max temperatures
  p3 <- ggplot(data_summary, aes(x = Test, y = annual_max, color = color, shape = Test)) +
    geom_point(position = position_dodge(width = 0.25), alpha = 0.3, size = 3) +
    stat_summary(fun.data = "mean_se", 
                 geom = "errorbar", position = position_dodge(width = 0.25), width = 0.2) +
    stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.25), size = 4) +
    scale_color_gradient(name = "", 
                         breaks = rescale(c(1950, 1985, 2020), to = c(0, 1)),
                         labels = c(1950, 1985, 2015),
                         low = "tomato", high = "purple3") +
    scale_y_continuous(limits = y_limits_max,   
                       breaks = seq(floor(y_limits_max[1]), ceiling(y_limits_max[2]), by = 1),
                       minor_breaks = seq(floor(y_limits_max[1]), ceiling(y_limits_max[2]), by = 0.5)) +
    labs(title = NULL, y = "Temperature (°C)", x = "") +
    theme_bw() +
    theme(legend.position = c(0.95, 0.05), 
          legend.justification = c(1, 0), 
          legend.background = element_blank(), 
          legend.box.background = element_blank(), 
          plot.title = element_text(face="bold", hjust=0.5, family="Times", size=18),
          legend.title = element_blank()) +  
    ggtitle("Max")
  
  # Combine plots using patchwork
  combined_plot <- p1 + p2 + p3
  return(combined_plot)
}




plot_ACT_summary <- function(data_norm, data_shade) {
  # Combine genus and species
  genus_species <- paste0(unique(data_norm$genus), "_", unique(data_norm$species))
  
  # Summary for normal simulation
  data_norm_summary <- data_norm %>%
    mutate(ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, # renaming activity for sum cal
                                    ACT == 2 ~ 1, # foraging 
                                    TRUE ~ as.numeric(ACT))) %>% 
    group_by(yr) %>%
    summarise(annual_act = sum(ACT_combined),
              .groups = 'drop') %>%
    mutate(Test = "norm_sim") 
  
  # Summary for zero shade simulation
  zero_shade_summary <- data_shade %>%
    mutate(ACT_combined = case_when(ACT %in% c(0, 1) ~ 0, # renaming activity for sum cal
                                    ACT == 2 ~ 1, # foraging 
                                    TRUE ~ as.numeric(ACT))) %>% 
    group_by(yr) %>%
    summarise(annual_act = sum(ACT_combined),
              .groups = 'drop') %>%
    mutate(Test = "zero_shade_sim") 
  
  # Combine the summaries
  data_summary <- rbind(data_norm_summary, zero_shade_summary)  
  data_summary$color <- rescale(as.numeric(data_summary$yr), to = c(0, 1))
  
  # Determine y-axis limits
  y_limits <- range(data_summary$annual_act)
  
  # Plot for min temperatures
  p1 <- ggplot(data_summary, aes(x = Test, y = annual_act, color = color, shape = Test)) +
    geom_point(position = position_dodge(width = 0.25), alpha = 0.3, size = 3) +
    stat_summary(fun.data = "mean_se", 
                 geom = "errorbar", position = position_dodge(width = 0.25), width = 0.2) +
    stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.25), size = 4) +
    scale_color_gradient(name = "", 
                         breaks = rescale(c(1950, 1985, 2020), to = c(0, 1)),
                         labels = c(1950, 1985, 2015),
                         low = "tomato", high = "purple3") +
    scale_y_continuous(limits = y_limits,   
                       breaks = seq(floor(y_limits[1]), ceiling(y_limits[2]), by = 300),
                       minor_breaks = seq(floor(y_limits[1]), ceiling(y_limits[2]), by = 300)) +
    labs(title = NULL, y = "Mean Foraging Time (h)", x = "") +
    scale_shape_discrete() +
    theme_bw() +
    theme(legend.position = c(0.95, 0.05), 
          legend.justification = c(1, 0),   
          legend.background = element_blank(),  
          legend.box.background = element_blank(), 
          plot.title = element_text(face="bold", hjust=0.5, family="Times", size=18),
          legend.title = element_blank()) +  
    ggtitle(paste0("Annual Foraging ", genus_species))
  
  return(p1)
}






##########
# Date range daily example for M. horridus 1997 example in discussion: 
weekly_checks_plot <- function(data, start_date, end_date, title_suffix = NULL) {
  
  # Filter for the date range
  metout <- data %>% 
    dplyr::filter(date >= start_date & date <= end_date) %>% 
    select(genus, species, date, hr, DEP, SHADE, TC, ACT, TA) %>% 
    rename(shade = SHADE, behaviour = ACT, Ta = TA, Tb = TC,
           depth_height_cm = DEP) %>% 
    mutate(across(c(Ta, Tb), round, 0)) %>% 
    mutate(datetime = as.POSIXct(paste(date, hr), format="%Y-%m-%d %H"))
  
  
  # Check if depth_height_cm has values greater than 5
  if (any(metout$depth_height_cm > 5)) {
    # Use the original p1 plot
    p1 <- ggplot(metout, aes(x = datetime, y = depth_height_cm)) +
      geom_line(color = "brown", size = .5) +
      scale_y_continuous(limits = c(-20, 150)) +
      labs(y = "depth/height (cm)", x = "Datetime") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title.x = element_text(),
        axis.text.x = element_text(),
        axis.ticks.x = element_line()
      ) +
      annotate("text", x = max(metout$datetime), y = 140, label = "depth/height (cm)", color = "brown", 
               fontface = "bold", size = 4) +
      annotate("segment", x = max(metout$datetime) - 1*3600, xend = max(metout$datetime), y = 140, yend = 140, color = "brown", 
               size = 2.5)
  } else {
    # Use the modified p1 plot with y limits from -20 to 5
    p1 <- ggplot(metout, aes(x = datetime, y = depth_height_cm)) +
      geom_line(color = "brown", size = .5) +
      scale_y_continuous(limits = c(-20, 5)) +
      labs(y = "depth/height (cm)", x = "Datetime") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title.x = element_text(),
        axis.text.x = element_text(),
        axis.ticks.x = element_line()
      ) +
      annotate("text", x = max(metout$datetime), y = 4, label = "depth/height (cm)", color = "brown", 
               fontface = "bold", size = 4) +
      annotate("segment", x = max(metout$datetime) - 1*3600, xend = max(metout$datetime), y = 4, yend = 4, color = "brown", 
               size = 2.5)
  }
  
  # Shade
  p2 <- ggplot(metout, aes(x = datetime, y = shade)) +
    geom_line(color = "green3", size = .5, linetype = 'dashed') +
    scale_y_continuous(limits = c(0, 60)) +
    labs(y = "% shade", color = NULL, x = "Datetime") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    annotate("text", x = max(metout$datetime), y = 55, label = "% shade", color = "green3", fontface = "bold", size = 4) +
    annotate("segment", x = max(metout$datetime) - 1*3600, xend = max(metout$datetime), y = 55, yend = 55, color = "green3", linetype = "dashed", 
             size = 2.5)
  
  # Behavior factor
  metout$behavior_factor <- factor(metout$behaviour, levels = c(2, 1, 0), labels = c("A", "B", "I"))
  p3 <- ggplot(metout, aes(x = datetime, y = behavior_factor)) +
    geom_point(aes(shape = behavior_factor, fill = behavior_factor), size = 4, color = "black") +
    scale_y_discrete(limits = c("I", "B", "A")) +
    scale_shape_manual(values = c("I" = 22, "B" = 22, "A" = 22)) +  # Use squares for all
    scale_fill_manual(values = c("I" = "white", "B" = "black", "A" = "gold")) +
    labs(y = "activity", x = "Datetime") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    annotate("point", x = max(metout$datetime) - 16*3600, y = "A", color = "gold", fill = "gold", shape = 22, size = 2) +
    annotate("text", x = max(metout$datetime) - 15*3600, y = "A", label = "Active", hjust = 0, fontface = 'bold', size = 3) +
    annotate("point", x = max(metout$datetime) - 12*3600, y = "B", color = "black", fill = "black", shape = 22, size = 2) +
    annotate("text", x = max(metout$datetime) - 11*3600, y = "B", label = "Basking", hjust = 0, fontface = 'bold', size = 3) +
    annotate("point", x = max(metout$datetime) - 8*3600, y = "I", color = "black", fill = "white", shape = 22, size = 2) +
    annotate("text", x = max(metout$datetime) - 7*3600, y = "I", label = "Inactive", hjust = 0, fontface = 'bold', size = 3)
  
  # Species details
  genus <- unique(metout$genus)
  species <- unique(metout$species)
  date_range <- paste0(start_date, " to ", end_date)
  species_date <- paste0(genus, ' ', species, ' ', date_range)
  
  # Temperature plot
  title <- species_date
  if (!is.null(title_suffix)) {
    title <- paste0(species_date, " - ", title_suffix)
  }
  
  p4 <- ggplot(metout, aes(x = datetime)) +
    geom_point(aes(y = Ta), color = "dodgerblue2", size = 3) +
    geom_line(aes(y = Ta), color = "dodgerblue2", size = 1) +
    geom_point(aes(y = Tb), color = "black", size = 3) +
    geom_line(aes(y = Tb), color = "black", size = 1) +
    scale_y_continuous(limits = c(12, 58)) +
    labs(y = "Temperature (°C)", color = NULL, x = "Datetime") +
    theme_minimal() +
    theme(
      legend.position = "none",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.75)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    annotate("point", x = max(metout$datetime) - 1*3600, y = 48, color = "dodgerblue2", shape = 21, size = 4, 
             fill = "dodgerblue2") +
    annotate("text", x = max(metout$datetime), y = 48, label = "T[a]", parse = TRUE,
             color = "black", fontface = "bold", hjust = 0, size = 6) +
    annotate("point", x = max(metout$datetime) - 1*3600, y = 40, color = "black", shape = 21, size = 4, 
             fill = "black") +
    annotate("text", x = max(metout$datetime), y = 40, label = "T[b]", parse = TRUE,
             color = "black", fontface = "bold", hjust = 0, size = 6) +
    annotate("text", x = min(metout$datetime), y = 58, label = title, color = "black",
             fontface = "bold", hjust = 0, size = 4)
  
  combined_plot <- p4 / p3 / p2 / p1
  
  return(combined_plot)
}

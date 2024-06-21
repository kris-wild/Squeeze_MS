pacman::p_load(maps, dplyr, tidyr, tidyverse, flextable, knitr, here, ggplot2, purrr, rnaturalearth, rnaturalearthdata, grid, cowplot, simplecolors, ggnewscale, gridExtra, grid)

################################################
# INSET MAPS FOR PLOTS for each contenent 
################################################ 
### Australia map: 
world <- ne_countries(scale = "medium", returnclass = "sf")
australia <- world[world$admin == "Australia", ]
# Create a map grob
australia_map <- ggplot() +
  geom_sf(data = australia, fill = NA, color = "black") + 
  theme_void()
# Convert the map to a grob
australia_grob <- ggplotGrob(australia_map)

### Africa map:
# Filter for African continent by removing countries not in Africa
africa <- world[world$continent == "Africa", ]
# Create a map grob
africa_map <- ggplot() +
  geom_sf(data = africa, fill = NA, color = "black") +
  theme_void()

###  Function to add an inset map to a plot
add_inset_map <- function(plot, inset_map, x, y, width, height) {
  inset_grob <- ggplotGrob(inset_map)
  inset_grob <- gtable::gtable_filter(inset_grob, "panel")
  
  plot +
    annotation_custom(
      grob = inset_grob,
      xmin = x, xmax = x + width,
      ymin = y, ymax = y + height
    )
}
# Get the map data for Australia
australia_map <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
# Create a ggplot object for the map
australia_plot <- ggplot(data = australia_map) +
  geom_sf(fill = "transparent", color = "black") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))
# Get the map data for Africa
africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
# Create a ggplot object for the map
africa_plot <- ggplot(data = africa_map) +
  geom_sf(fill = "transparent", color = "black") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))
# Convert the ggplot map objects to grobs
australia_grob <- ggplotGrob(australia_plot)
africa_grob <- ggplotGrob(africa_plot)
# Define viewport for the inset maps
vp <- viewport(x = 0.1, y = 0.95, just = c("left", "top"), 
               width = unit(0.25, "npc"), height = unit(0.25, "npc"))



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by year  
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Figure TA and TB - Africa
contemp_ta_tb_africa <- readRDS("Raw_data/contemp_ta_tb_africa.rds")
# Define factor levels
contemp_ta_tb_africa$Combined <- factor(contemp_ta_tb_africa$Combined, 
                                        levels = c("Min Ta", "Mean Ta", "Max Ta", "Min Tb", "Mean Tb",  "Max Tb"))
species_order_AF = c("C. angulifer", "A. aculeata", "M. suborbitalis", "P. lineoocellata",
                     "T. sparsa", "Site_AFA", "Site_AFB", "Site_AFL", "Site_AFX")
colors <- c( "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2",
             "#9DB9F1", "#4479E4", "#16439C", "#0D2659")

###########
### Plotting - Africa
# Create the plot with custom legend
Ta_Tb_contemp_africa <- ggplot(contemp_ta_tb_africa, aes(x = Combined, y = Slope_10_Year, 
                                                         color = Species, shape = Significance)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_shape_manual(values = c("p < 0.05" = 16, "p ≥ 0.05" = 1), 
                     guide = FALSE) + 
  scale_color_manual(values =  colors, breaks = species_order_AF) + 
  theme_bw() +
  labs(y = "Temperature Change by Decade (°C)", color = "Species/Site", 
       x = NULL) +
  ylim(0, .5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.25, .77),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA))  + 
  guides(color = guide_legend(ncol = 2, title.position = "top", )) +  
  annotate("text",  x = "Mean Tb", y = .35, label = "● P-value < 0.05",  size = 4)+
  annotate("text", x = "Mean Tb", y = .32, label = "○ P-value > 0.05", size = 4) +
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 40, unit = "pt"))
Ta_Tb_contemp_africa
# add inset map
Ta_Tb_contemp_africa_final <- add_inset_map(Ta_Tb_contemp_africa, africa_plot, 
                                            x = 5, y = 0.16, 
                                            width = 1.2, height = .5)

Ta_Tb_contemp_africa_final


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Plotting - Australia
contemp_ta_tb_australia <- readRDS("Raw_data/contemp_ta_tb_australia.rds")
contemp_ta_tb_australia$Combined <- factor(contemp_ta_tb_australia$Combined, 
                                           levels = c("Min Ta", "Mean Ta", "Max Ta", "Min Tb", "Mean Tb",  "Max Tb"))
#levels = c("Min Ta", "Min Tb", "Mean Ta", "Mean Tb", "Max Ta", "Max Tb"))

species_order_AU = c("G. variegata", "P. minor", "C. isolepis", "C. quatt",
                     "M. horridus", "Site_AUA", "Site_AUL", "Site_AUR")
colors <- c( "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2",
             "#9DB9F1", "#4479E4", "#16439C", "#0D2659")
###########
### Plotting 
Ta_Tb_contemp_australia <- ggplot(contemp_ta_tb_australia, 
                                  aes(x = Combined, y = Slope_10_Year,
                                      color = Species, shape = Significance)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_shape_manual(values = c("p < 0.05" = 16, "p ≥ 0.05" = 1), 
                     guide = FALSE) + 
  scale_color_manual(values =  colors, breaks = species_order_AU) + 
  theme_bw() +
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  ylim(0, .5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.25, .78),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA))  + 
  guides(color = guide_legend(ncol = 2, title.position = "top", )) +  
  annotate("text",  x = "Mean Tb", y = .35, label = "● P-value < 0.05",  size = 4)+
  annotate("text", x = "Mean Tb", y = .32, label = "○ P-value > 0.05", size = 4) +
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 40, unit = "pt"))

Ta_Tb_contemp_australia

Ta_Tb_contemp_australia_final <- add_inset_map(Ta_Tb_contemp_australia, australia_plot, 
                                               x = 5, y = 0.16, 
                                               width = 1.2, height = .5)
Ta_Tb_contemp_australia_final

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Ta/Tb contemp year final plot
Ta_Tb_contemp_year_plot <- plot_grid(Ta_Tb_contemp_africa_final,
                                     Ta_Tb_contemp_australia_final,
                                     ncol = 2, nrow = 1, align = "h", 
                                     labels = c("A", "B"))
Ta_Tb_contemp_year_plot






####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by season and species  
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   

############ READ IN DATA
contemp_ta_tb_season_spp <- readRDS("Raw_data/contemp_ta_tb_season_spp.rds")

## final seasonal data - Africa
contemp_ta_tb_season_spp_africa <- contemp_ta_tb_season_spp %>% filter(Region == "Africa")
# set up data
contemp_ta_tb_season_spp_africa <- contemp_ta_tb_season_spp_africa %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max")) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
# Set up data - Convert 'Combined' and 'season' to factors with specified levels
contemp_ta_tb_season_spp_africa$Combined <- factor(contemp_ta_tb_season_spp_africa$Combined, levels = c("Min Ta", "Mean Ta", "Max Ta", "Min Tb", "Mean Tb",  "Max Tb"))
#levels = c("Min Ta", "Min Tb", "Mean Ta", "Mean Tb", "Max Ta", "Max Tb"))
contemp_ta_tb_season_spp_africa$Season <- factor(contemp_ta_tb_season_spp_africa$season, levels = c("winter", "summer"))
species_order <- c("C. angulifer", "A. aculeata", "M. suborbitalis", 
                   "P. lineoocellata", "T. sparsa", "Site_AFA", "Site_AFB", 
                   "Site_AFL", "Site_AFX")
contemp_ta_tb_season_spp_africa$Species <- factor(contemp_ta_tb_season_spp_africa$Species, 
                                                  levels = species_order)
# Determine significance
contemp_ta_tb_season_spp_africa$Significant <- contemp_ta_tb_season_spp_africa$P_value < 0.05

## final seasonal data - Australia
contemp_ta_tb_season_spp_australia <- contemp_ta_tb_season_spp  %>% filter(Region == "Australia")
## final seasonal data - Africa
contemp_ta_tb_season_spp_australia <- contemp_ta_tb_season_spp_australia %>%
  mutate(Type = case_when(
    grepl("Min", Variable) ~ "Min",
    grepl("Mean", Variable) ~ "Mean",
    grepl("Max", Variable) ~ "Max")) %>%
  mutate(TA_TB = ifelse(grepl("Ta", Variable), "Ta", "Tb"),
         Combined = paste(Type, TA_TB),
         Significance = ifelse(P_value < 0.05, "p < 0.05", "p ≥ 0.05"))
# Set up data - Convert 'Combined' and 'season' to factors with specified levels
contemp_ta_tb_season_spp_australia$Combined <- factor(contemp_ta_tb_season_spp_australia$Combined, levels = c("Min Ta", "Mean Ta", "Max Ta", "Min Tb", "Mean Tb",  "Max Tb"))
#levels = c("Min Ta", "Min Tb", "Mean Ta", "Mean Tb", "Max Ta", "Max Tb"))
# Species order
species_order <- c("G. variegata", "P. minor", "C. isolepis", "C. quatt", "M. horridus",
                   "Site_AUA", "Site_AUL", "Site_AUR")
contemp_ta_tb_season_spp_australia$Species <- factor(contemp_ta_tb_season_spp_australia$Species, 
                                                     levels = species_order)
# Season order
contemp_ta_tb_season_spp_australia$Season <- factor(contemp_ta_tb_season_spp_australia$season, 
                                                    levels = c("winter", "summer", "spring"))
# Determine significance
contemp_ta_tb_season_spp_australia$Significant <- contemp_ta_tb_season_spp_australia$P_value < 0.05

########
# filter season data for plots for each region
contemp_ta_tb_winter_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "winter")
contemp_ta_tb_summer_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "summer")
contemp_ta_tb_spring_africa <- contemp_ta_tb_season_spp_africa %>% filter(season == "spring")
contemp_ta_tb_winter_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "winter")
contemp_ta_tb_summer_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "summer")
contemp_ta_tb_spring_australia <- contemp_ta_tb_season_spp_australia %>% 
  filter(season == "spring")


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  CONTEMPORARY WARMING Ta and Tb by season and species  Plots
# Colors for plots
semi_transparent_blue <- "#0000FF15"
semi_transparent_red <- "#FF999915"
semi_transparent_green <- "#00FF080F"

####################   ####################   
########   Africa plots Winter and summer
Ta_Tb_contemp_winter_africa_plot <- ggplot(contemp_ta_tb_winter_africa, aes(x = Combined, 
                                                                            y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "summer" = 21)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", "T. sparsa" = "firebrick2", 
                                "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                                "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", "T. sparsa" = "firebrick2", 
                               "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                               "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .4)+
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "Winter", 
           size = 6,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

####################   ####################   ####################  
######## Africa SUMMER
Ta_Tb_contemp_summer_africa_plot <- ggplot(contemp_ta_tb_summer_africa, aes(x = Combined, 
                                                                            y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("summer" = 21, "summer" = 21)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", "T. sparsa" = "firebrick2", 
                                "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                                "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", "T. sparsa" = "firebrick2", 
                               "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                               "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .4)+
  labs(y = "Temperature Change by Decade (°C)", color = "Species/Site", 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "Summer", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))




####################   ####################   ####################  
######## Africa spring
Ta_Tb_contemp_spring_africa_plot <- ggplot(contemp_ta_tb_spring_africa, aes(x = Combined, 
                                                                            y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("spring" = 21, "spring" = 21)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", "T. sparsa" = "firebrick2", 
                                "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                                "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", "T. sparsa" = "firebrick2", 
                               "Site_AFA" = "#9DB9F1", "Site_AFB" = "#4479E4", 
                               "Site_AFL" = "#16439C", "Site_AFX" = "#0D2659"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .4)+
  labs(y = NULL, 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "Spring", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))


####################   ####################   ####################   ####################   
######### Africa Ta/Tb seasonal plot final
Tb_Ta_contemporary_season_africa <- plot_grid(Ta_Tb_contemp_summer_africa_plot,
                                              Ta_Tb_contemp_spring_africa_plot,
                                              Ta_Tb_contemp_winter_africa_plot,
                                              nrow = 1, ncol = 3)



####################   ####################   ####################   ####################      
########  Australia Contemporary warming  Plots - Winter and Summer
#### Australia Winter plot
Ta_Tb_contemp_winter_australia_plot <- ggplot(contemp_ta_tb_winter_australia, aes(x = Combined, 
                                                                                  y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "summer" = 21)) +
  scale_color_manual(values = c("G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", "M. horridus" = "firebrick2",
                                "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                                "Site_AUR" = "#16439C")) +
  scale_fill_manual(values = c("G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", "M. horridus" = "firebrick2",
                               "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                               "Site_AUR" = "#16439C"), na.value = NA) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .4)+
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "Winter", 
           size = 6,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))


####################   ####################   ####################   ####################   
########  Australia SUMMER
# plot
Ta_Tb_contemp_summer_australia_plot <- ggplot(contemp_ta_tb_summer_australia, aes(x = Combined, 
                                                                                  y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = Season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "summer" = 21)) +
  scale_color_manual(values = c("G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", "M. horridus" = "firebrick2",
                                "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                                "Site_AUR" = "#16439C")) +
  scale_fill_manual(values = c("G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", "M. horridus" = "firebrick2",
                               "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                               "Site_AUR" = "#16439C"), na.value = NA) + 
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "Summer", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

####################   ####################   ####################   ####################   
########  Australia spring
# plot
Ta_Tb_contemp_spring_australia_plot <- ggplot(contemp_ta_tb_spring_australia, aes(x = Combined, 
                                                                                  y = Slope_10_Year, color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 4, alpha = 0.5, 
             aes(shape = season, fill = ifelse(Significant, as.character(Species), NA)), stroke = 1.5) +
  scale_shape_manual(values = c("winter" = 21, "spring" = 21)) +
  scale_color_manual(values = c("G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", "M. horridus" = "firebrick2",
                                "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                                "Site_AUR" = "#16439C")) +
  scale_fill_manual(values = c("G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", "M. horridus" = "firebrick2",
                               "Site_AUA" = "#9DB9F1", "Site_AUL" = "#4479E4", 
                               "Site_AUR" = "#16439C"), na.value = NA) + 
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ylim(-.2, .4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  labs(y = NULL, color = "Species/Site", 
       x = NULL) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  annotate("text", x = 3.5, y = .4, label = "Spring", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

####################   ####################   ####################   ####################   
######## australia Ta/Tb seasonal plot
Tb_Ta_contemporary_season_australia <- plot_grid(Ta_Tb_contemp_summer_australia_plot,
                                                 Ta_Tb_contemp_spring_australia_plot,
                                                 Ta_Tb_contemp_winter_australia_plot,
                                                 nrow = 1, ncol =3)



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  ***FINAL PLOT CONTEMPORARY CLIMATE: Ta/Tb
Figure_2 <- grid.arrange(
  Ta_Tb_contemp_year_plot,
  Tb_Ta_contemporary_season_africa,
  Tb_Ta_contemporary_season_australia,
  ncol = 2,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)
)
Figure_2


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
### Contemporary sum annual activity, Mrate, Foraging (Figure 3)
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
############ READ IN DATA
ACT_MRate_Foraging_contemp <- readRDS("Raw_data/ACT_MRate_Foraging_contemp.rds")

#data prep
ACT_MRate_Foraging_contemp$Test <- factor(ACT_MRate_Foraging_contemp$Test, levels = c("ACT", "M_Rate", "Foraging"))
# Create a new column for significance
ACT_MRate_Foraging_contemp$Significant <- ACT_MRate_Foraging_contemp$P_value < 0.05

# data for plots
ACT_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "ACT")
MRate_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "M_Rate")
Foraging_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "Foraging")

# Define the order of the species for plots
species_order <- c("C. angulifer", "A. aculeata", "M. suborbitalis", 
                   "P. lineoocellata","T. sparsa", "G. variegata", 
                   "P. minor", "C. isolepis", "C. quatt",
                   "M. horridus")


####################   ####################   ####################   ####################   
########  ACT Plot
# Create the plot
ACT_contemp_plot <- ggplot(ACT_contemp_plot_dat, 
                           aes(x = Region, y = Slope_10_Year, 
                               color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5)  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(h/decade)", 
       color = "Species", 
       x = NULL) +
  ylim(-25,30)+
  annotate("text", x = 1.5, y = 28, label = expression(Delta * "Activity"), 
           fontface = "bold", size = 10.5)+
  #annotate("text", x = Inf, y = -Inf, label = "P-value: ○ > 0.05", 
  #hjust = 1.5, vjust = -0.5, size = 8) +
  #annotate("text", x = Inf, y = -Inf, label = "P-value: ● < 0.05", 
  #hjust = 1.5, vjust = -3, size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2), fill = "none")+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
ACT_contemp_plot

####################   ####################   ####################   ####################   
######## M Rate Plot
MRate_contemp_plot <- ggplot(MRate_contemp_plot_dat, 
                             aes(x = Region, y = Slope_10_Year, 
                                 color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5)  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(kJ/decade)", 
       color = "Species", 
       x = NULL) +
  ylim(-5,10)+
  annotate("text", x = 1.5, y = 9.5, label = expression(Delta * "Metabolic Rate"), 
           fontface = "bold", size = 10.5)+
  #annotate("text", x = Inf, y = -Inf, label = "P-value: ○ > 0.05", 
  #hjust = 1.5, vjust = -0.5, size = 8.5) +
  #annotate("text", x = Inf, y = -Inf, label = "P-value: ● < 0.05", 
  # hjust = 1.5, vjust = -3, size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2), fill = "none")+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
MRate_contemp_plot


####################   ####################   ####################   ####################   
######## Foraging Rate Plot
Foraging_contemp_plot <- ggplot(Foraging_contemp_plot_dat, 
                                aes(x = Region, y = Slope_10_Year, 
                                    color = Species, fill = Species)) + # Assuming 'fill' is mapped to 'Species' as well
  geom_point(position = position_dodge(width = .3), alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 6, stroke = 1.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA, limits = species_order) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA, limits = species_order) +
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3)))  +
  theme_bw() +
  labs(y = "(hr consumption ants/decade)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-3.5, 2.5), breaks = seq(-3.5, 2.5, by = 1)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 2.2, label = expression(Delta * "Feeding Rate"), 
           fontface = "bold", size = 10.5)+
  annotate("text", x = 1, y = 1.6, label = "P-value: ○ > 0.05", size = 6) +
  annotate("text", x = 1, y = 1.2, label = "P-value: ● < 0.05",  size = 6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.70, .20),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 11), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 11),
                              ncol = 2), fill = "none")+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
Foraging_contemp_plot


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Activity and Metabolic rate year final plot
ACT_MRate_Foraging_contemp_plot <- plot_grid(ACT_contemp_plot,
                                             MRate_contemp_plot,
                                             Foraging_contemp_plot,
                                             ncol = 3, nrow = 1, align = "h", 
                                             labels = c("A", "B", "C"))
ACT_MRate_Foraging_contemp_plot


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary sum seasonal activity  Metabolic rate, Feeding rate
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
############  READ IN DATA
contemp_ACT_MRate_Foraging_season <- readRDS("Raw_data/contemp_ACT_MRate_Foraging_season.rds")

### arrange data for plots
contemp_ACT_MRate_Foraging_season$Test <- factor(contemp_ACT_MRate_Foraging_season$Test, levels = c("ACT", "M_Rate", "Foraging"))
# Create a new column for significance
contemp_ACT_MRate_Foraging_season$Significant <- contemp_ACT_MRate_Foraging_season$P_value < 0.05

### Filter ACT and MRate data for plots
contemp_ACT_winter_plot_dat <- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "winter"& Test == "ACT")
contemp_ACT_summer_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "summer"& Test == "ACT")
contemp_ACT_spring_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "spring"& Test == "ACT")
contemp_MRate_winter_plot_dat <- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "winter"& Test == "M_Rate")
contemp_MRate_summer_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "summer" & Test == "M_Rate")
contemp_MRate_spring_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "spring" & Test == "M_Rate")
contemp_Foraging_winter_plot_dat <- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "winter"& Test == "Foraging")
contemp_Foraging_summer_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "summer" & Test == "Foraging")
contemp_Foraging_spring_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "spring" & Test == "Foraging")

# plotting colors
semi_transparent_blue<- "#0000FF15"
semi_transparent_red <- "#FF999915"
semi_transparent_green <- "#00FF080F"
# Define the order of the species for plots
species_order <- c("C. angulifer", "A. aculeata", 
                   "M. suborbitalis", "P. lineoocellata",
                   "T. sparsa", "G. variegata", 
                   "P. minor", "C. isolepis", 
                   "C. quatt", "M. horridus")


####################   ####################   ####################   ####################   
######## ACT Summer plot
contemp_ACT_summer_plot <- ggplot(contemp_ACT_summer_plot_dat, 
                                  aes(x = Region, y = Slope_10_Year, 
                                      color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =  "(h/decade)", 
       color = "Species", 
       x = NULL)  +
  ylim(-20, 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  annotate("text", x = 1.5, y = 7.5, label = "Summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

contemp_ACT_summer_plot


####################   ####################   ####################   ####################   
######## ACT Winter plots
contemp_ACT_winter_plot <- ggplot(contemp_ACT_winter_plot_dat, 
                                  aes(x = Region, y = Slope_10_Year, 
                                      color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-0,18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 16.5, label = "Winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") # 1.7 - too far left  
contemp_ACT_winter_plot


####################   ####################   ####################   ####################   
######## ACT spring plots
contemp_ACT_spring_plot <- ggplot(contemp_ACT_spring_plot_dat, 
                                  aes(x = Region, y = Slope_10_Year, 
                                      color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-12,18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 16.5, label = "Spring", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") # 1.7 - too far left  
contemp_ACT_spring_plot




####################   ####################   ####################   ####################   
######## MRate Summer plot
contemp_MRate_summer_plot <- ggplot(contemp_MRate_summer_plot_dat, 
                                    aes(x = Region, y = Slope_10_Year, 
                                        color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(kJ/decade)", 
       color = "Species", 
       x = NULL)  +
  ylim(-5, 5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x = 1.5, y = 4, label = "Summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")

contemp_MRate_summer_plot

####################   ####################   ####################   ####################   
######## MRate Winter plots
contemp_MRate_winter_plot <- ggplot(contemp_MRate_winter_plot_dat, 
                                    aes(x = Region, y = Slope_10_Year, 
                                        color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-0,10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 9, label = "Winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")

contemp_MRate_winter_plot


####################   ####################   ####################   ####################   
######## MRate spring plots
contemp_MRate_spring_plot <- ggplot(contemp_MRate_spring_plot_dat, 
                                    aes(x = Region, y = Slope_10_Year, 
                                        color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-3,3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 2.5, label = "Spring", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")

contemp_MRate_spring_plot


####################   ####################   ####################   ####################   
######## Foraging Summer plot
contemp_Foraging_summer_plot <- ggplot(contemp_Foraging_summer_plot_dat, 
                                       aes(x = Region, y = Slope_10_Year, 
                                           color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(hr consumption ants/decade)", 
       color = "Species", 
       x = NULL)  +
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x = 1.5, y = .45, label = "Summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")

contemp_Foraging_summer_plot

####################   ####################   ####################   ####################   
######## Foraging Winter plots
contemp_Foraging_winter_plot <- ggplot(contemp_Foraging_winter_plot_dat, 
                                       aes(x = Region, y = Slope_10_Year, 
                                           color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-15,4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x = 1.5, y = 2.3, label = "Winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")

contemp_Foraging_winter_plot



####################   ####################   ####################   ####################   
######## Foraging spring plots
contemp_Foraging_spring_plot <- ggplot(contemp_Foraging_spring_plot_dat, 
                                       aes(x = Region, y = Slope_10_Year, 
                                           color = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(fill = ifelse(Significant, as.character(Species), NA)), 
             shape = 21, size = 4, stroke = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-2,2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x = 1.5, y = 1.8, label = "Spring", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")

contemp_Foraging_spring_plot


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## ACT/Mrate seasonal plots
ACT_contemporary_season_plot <- plot_grid(contemp_ACT_summer_plot,
                                          contemp_ACT_spring_plot,
                                          contemp_ACT_winter_plot,
                                          nrow = 1, ncol = 3)

MRate_contemporary_season_plot <- plot_grid(contemp_MRate_summer_plot,
                                            contemp_MRate_spring_plot,
                                            contemp_MRate_winter_plot,
                                            nrow = 1, ncol = 3)

Foraging_contemporary_season_plot <- plot_grid(contemp_Foraging_summer_plot,
                                               contemp_Foraging_spring_plot,
                                               contemp_Foraging_winter_plot,
                                               nrow = 1, ncol = 3)


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  ***FINAL PLOT CONTEMPORARY CLIMATE: Activity and Metabolic rate
Figure_3 <-  grid.arrange(
  ACT_MRate_Foraging_contemp_plot,
  ACT_contemporary_season_plot,
  MRate_contemporary_season_plot,
  Foraging_contemporary_season_plot,
  ncol = 3,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 1, 2, 3, 4), 
                         nrow = 2, ncol = 3, byrow = TRUE)
)

# save: 1800 X 1100







####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################
####################   ####################   ####################   ####################   
####################     TERRA CLIMATE DATA and plots                ####################      
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   

############ READ IN DATA
Tb_Ta_change_spp_final <- readRDS("Raw_data/Tb_Ta_change_spp_final.rds")

# Australia - plotting data
Tb_Ta_change_spp_australia <- Tb_Ta_change_spp_final %>% filter(region == "Australia")
species_order_AU <- c("G. variegata", "P. minor", "C. isolepis", "C. quatt", 
                      "M. horridus", "Site_AUA", "Site_AUL", "Site_AUR")
Tb_Ta_change_spp_australia$species <- factor(Tb_Ta_change_spp_australia$species, 
                                             levels = species_order_AU)

# Africa - plotting data
Tb_Ta_change_spp_africa <- Tb_Ta_change_spp_final %>% filter(region == "Africa")
species_order_AF = c("C. angulifer", "A. aculeata", "M. suborbitalis", "P. lineoocellata",
                     "T. sparsa", "Site_AFA", "Site_AFB", "Site_AFL", "Site_AFX")
Tb_Ta_change_spp_africa$species <- factor(Tb_Ta_change_spp_africa$species,
                                          levels = species_order_AF)

####################   ####################   ####################   ####################   
######## Plotting
colors <- c( "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2",
             "#9DB9F1", "#4479E4", "#16439C", "#0D2659")
semi_transparent_blue <- "#0000FF15"
semi_transparent_red <- "#FF999915"

# Africa - Plot
Tb_Ta_change_spp_africa_plot <- ggplot(Tb_Ta_change_spp_africa, 
                                       aes(x = Variable, y = value, 
                                           color = species, 
                                           shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), 
             size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, 
                     breaks = species_order_AF) +
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", 
             color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", 
             color = "black") +  # Dashed line at y = 4
  labs(y = expression(Delta * " Temperature (" * degree * "C)"), 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.80, .83),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),  # Make legend background transparent
        legend.key = element_rect(fill = NA),  # Make background behind shapes in legend transparent
        legend.spacing.y = unit(-.1, 'cm'), # Adjust the spacing between legend boxes
        legend.box.spacing = unit(-54, 'cm')) +   
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 14), ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 14), ncol = 2))+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
Tb_Ta_change_spp_africa_plot

Tb_Ta_change_spp_africa_final <- add_inset_map(Tb_Ta_change_spp_africa_plot, africa_plot, 
                                               x = .30, y = 4.8, 
                                               width = 2, height = 1.5)
Tb_Ta_change_spp_africa_final

####################   ####################   ####################   ####################   
######## Australia - Plot
Tb_Ta_change_spp_australia_plot <- ggplot(Tb_Ta_change_spp_australia, aes(x = Variable, y = value, 
                                                                          color = species, shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AU)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y = NULL, 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.80, .84),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),  # Make legend background transparent
        legend.key = element_rect(fill = NA),  # Make background behind shapes in legend transparent
        legend.spacing.y = unit(-.1, 'cm'), # Adjust the spacing between legend boxes
        legend.box.spacing = unit(-54, 'cm')) +   
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 14), ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 14), ncol = 2))+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
Tb_Ta_change_spp_australia_plot

Tb_Ta_change_spp_australia_final <- add_inset_map(Tb_Ta_change_spp_australia_plot, australia_plot, 
                                                  x = .30, y = 4.8, 
                                                  width = 2, height = 1.5)
Tb_Ta_change_spp_australia_final

####################   ####################   ####################   ####################   
######## Final annual plot by region
Tb_Ta_change_spp_final_plot <- plot_grid(Tb_Ta_change_spp_africa_final,
                                         Tb_Ta_change_spp_australia_final,
                                         ncol = 2, nrow = 1, align = "h", 
                                         labels = c("A", "B"))
Tb_Ta_change_spp_final_plot



####################   ####################   ####################   ####################   
######## Tb/Ta seasonal
### READ IN DATA
Tb_Ta_change_spp_season_final <- readRDS("Raw_data/Tb_Ta_change_spp_season_final.rds")

# Africa - plotting data
Tb_Ta_change_spp_season_africa <- Tb_Ta_change_spp_season_final %>% filter(region == "Africa")
species_order_AF = c("C. angulifer", "A. aculeata", "M. suborbitalis", "P. lineoocellata",
                     "T. sparsa", "Site_AFA", "Site_AFB", "Site_AFL", "Site_AFX")
Tb_Ta_change_spp_season_africa$species <- factor(Tb_Ta_change_spp_season_africa$species, 
                                                 levels = species_order_AF)

# Australia - plotting data
Tb_Ta_change_spp_season_australia <- Tb_Ta_change_spp_season_final %>% filter(region == "Australia")
species_order_AU <- c("G. variegata", "P. minor", "C. isolepis", "C. quatt", 
                      "M. horridus", "Site_AUA", "Site_AUL", "Site_AUR")
Tb_Ta_change_spp_season_australia$species <- factor(Tb_Ta_change_spp_season_australia$species, 
                                                    levels = species_order_AU)


####################   ####################   ####################   ####################   
######## Plotting
colors <- c( "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2",
             "#9DB9F1", "#4479E4", "#16439C", "#0D2659")
semi_transparent_blue <- "#0000FF15"
semi_transparent_red <- "#FF999915"

####################   ####################   ####################   ####################   
######## Africa plots: summer and winter
# Africa - Plot: summer
Tb_Ta_change_season_summer_africa<- Tb_Ta_change_spp_season_africa %>% filter(season == "summer")
Tb_Ta_change_season_summer_africa_plot <- ggplot(Tb_Ta_change_season_summer_africa, 
                                                 aes(x = Variable, y = value, 
                                                     color = species, 
                                                     shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AF)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y = expression(Delta * " Temperature (" * degree * "C)"), 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 3.5, y = 5.8, label = "Summer", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")

Tb_Ta_change_season_summer_africa_plot

####################   ####################   ####################   ####################   
######## Africa plot: winter
Tb_Ta_change_season_winter_africa <- Tb_Ta_change_spp_season_africa %>% filter(season == "winter")
semi_transparent_blue <- "#0000FF15"
Tb_Ta_change_season_winter_africa_plot <- ggplot(Tb_Ta_change_season_winter_africa, 
                                                 aes(x = Variable, y = value, 
                                                     color = species, 
                                                     shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AF)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y= NULL, 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 3.5, y = 5.8, label = "Winter", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")
Tb_Ta_change_season_winter_africa_plot


####################   ####################   ####################   ####################   
######## Africa plot: spring
Tb_Ta_change_season_spring_africa <- Tb_Ta_change_spp_season_africa %>% filter(season == "spring")
Tb_Ta_change_season_spring_africa_plot <- ggplot(Tb_Ta_change_season_spring_africa, 
                                                 aes(x = Variable, y = value, 
                                                     color = species, 
                                                     shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AF)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y= NULL, 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 3.5, y = 5.8, label = "spring", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")
Tb_Ta_change_season_spring_africa_plot


####################   ####################   ####################   ####################   
######## Final Africa Ta/Tb seasonal plot
africa_Tb_Ta_change_season <- plot_grid(Tb_Ta_change_season_summer_africa_plot,
                                        Tb_Ta_change_season_spring_africa_plot,
                                        Tb_Ta_change_season_winter_africa_plot,
                                        nrow = 1, ncol = 3)
africa_Tb_Ta_change_season


####################   ####################   ####################   ####################   
######## australia plots: summer and winter
# australia - Plot: summer
Tb_Ta_change_season_summer_australia<- Tb_Ta_change_spp_season_australia %>% filter(season == "summer")
semi_transparent_red <- "#FF999915"
Tb_Ta_change_season_summer_australia_plot <- ggplot(Tb_Ta_change_season_summer_australia, 
                                                    aes(x = Variable, y = value, 
                                                        color = species, 
                                                        shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AU)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y = NULL, 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 3.5, y = 5.8, label = "Summer", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
Tb_Ta_change_season_summer_australia_plot

####################   ####################   ####################   ####################   
######## australia plot: winter
Tb_Ta_change_season_winter_australia<- Tb_Ta_change_spp_season_australia %>% filter(season == "winter")
semi_transparent_blue<- "#0000FF15"
Tb_Ta_change_season_winter_australia_plot <- ggplot(Tb_Ta_change_season_winter_australia, 
                                                    aes(x = Variable, y = value, 
                                                        color = species, 
                                                        shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AU)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y = NULL, 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 3.5, y = 5.8, label = "Winter", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
Tb_Ta_change_season_winter_australia_plot


####################   ####################   ####################   ####################   
######## australia plot: spring
Tb_Ta_change_season_spring_australia<- Tb_Ta_change_spp_season_australia %>% filter(season == "spring")
Tb_Ta_change_season_spring_australia_plot <- ggplot(Tb_Ta_change_season_spring_australia, 
                                                    aes(x = Variable, y = value, 
                                                        color = species, 
                                                        shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AU)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y = NULL, 
       color = "Species/Site", shape = "Climate Scenario", 
       x = NULL) +
  ylim(0,6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 9)),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 9))) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 3.5, y = 5.8, label = "spring", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
Tb_Ta_change_season_spring_australia_plot

####################   ####################   ####################   ####################   
######## Final australia Ta/Tb plot 
australia_Tb_Ta_change_season <- plot_grid(Tb_Ta_change_season_summer_australia_plot,
                                           Tb_Ta_change_season_spring_australia_plot,
                                           Tb_Ta_change_season_winter_australia_plot,
                                           nrow = 1, ncol = 3)

australia_Tb_Ta_change_season



############################################################
############################################################
######### ****Final Ta/Tb TerraClimate Plots
# Arrange the plots in a 2x2 grid
Figure_4 <- grid.arrange(
  Tb_Ta_change_spp_final_plot,
  africa_Tb_Ta_change_season,
  australia_Tb_Ta_change_season,
  ncol = 2,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)
)
#1500 X1300





####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
#################### TERRA delta change: Activity  Metabolic rates and foraging #########
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
### READ IN DATA: ANNIUAL AND SEASONAL
ACT_MR_Forage_change_yr_final <- readRDS("Raw_data/ACT_MR_Forage_change_yr_final.rds")


####################
## data prep for Mrate and ACT plots
MR_change_yr_terra_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "M_Rate_kJ")
ACT_change_yr_terra_ACT_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "ACT_hr")
Forage_change_yr_terra_ACT_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "Foraging")

####################   ####################   ####################   ####################   
#################### Plotting
# Define the order of the species
species_order <- c("C. angulifer", "A. aculeata", "M. suborbitalis", 
                   "P. lineoocellata","T. sparsa", "G. variegata", 
                   "P. minor", "C. isolepis", "C. quatt",
                   "M. horridus")

####################   ####################   ####################   ####################   
#################### ACT Plot
# Function to determine shape
# Create the plot
ACT_change_yr_terra_plot <- ggplot(ACT_change_yr_terra_ACT_plot_dat, 
                                   aes(x = Region, y = Value, 
                                       color = Species, fill = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    limits = species_order) +
  guides(color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =  "(hr/yr)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-420, 820), breaks = seq(-400, 800, by = 200)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 800, label = expression(Delta * "Activity"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
ACT_change_yr_terra_plot

####################   ####################   ####################   ####################   
#################### M Rate Plot
MR_change_yr_terra_plot_dat_plot <-  ggplot(MR_change_yr_terra_plot_dat, 
                                            aes(x = Region, y = Value, 
                                                color = Species, 
                                                fill = Species)) +
  geom_point(position = position_dodge(width = .3), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5)+
  scale_shape_identity() + 
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2",
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),  
                    limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(kJ/yr)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(0, 275), breaks = seq(0, 275, by = 50)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 270, label = expression(Delta * "Metabolic Rate"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
MR_change_yr_terra_plot_dat_plot

####################   ####################   ####################   ####################   
#################### Foraging rate change plot 
Forage_yr_terra_plot_dat_plot <-  ggplot(Forage_change_yr_terra_ACT_plot_dat, 
                                         aes(x = Region, y = Value, 
                                             color = Species,
                                             fill = Species)) +
  geom_point(position = position_dodge(width = .5), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5)+
  scale_shape_identity() + 
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2",
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order) +
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"), 
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(ants/hr)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-30, 20), breaks = seq(-40, 20, by = 10)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 19, label = expression(Delta * "Feeding Rate"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.70, .26),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 11),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 11),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend 
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
Forage_yr_terra_plot_dat_plot


####################   ####################   ####################   ####################   
#################### Final ACT/MR plot
ACT_M_Rate_Forage_change_yr <- plot_grid(ACT_change_yr_terra_plot,
                                         MR_change_yr_terra_plot_dat_plot,
                                         Forage_yr_terra_plot_dat_plot,
                                         nrow = 1, ncol = 3, 
                                         labels = c("A", "B", "C"))
ACT_M_Rate_Forage_change_yr


####################   ####################   ####################   ####################   
#################### SAVE AND READ IN DATA: ANNIUAL AND SEASONAL
ACT_MR_Foraging_change_season_final <- readRDS("Raw_data/ACT_MR_Foraging_change_season_final.rds")

## data prep for Mrate, ACT, Foraging rate plots
MR_change_season_terra_plot_dat <- ACT_MR_Foraging_change_season_final %>% 
  filter(Test == "M_Rate_kJ")
ACT_change_season_terra_ACT_plot_dat <- ACT_MR_Foraging_change_season_final %>% 
  filter(Test == "ACT_hr")
Forage_change_season_terra_ACT_plot_dat <- ACT_MR_Foraging_change_season_final %>% 
  filter(Test == "Foraging")

####################   ####################   ####################   ####################   
#################### Plotting 
# ACT plot: winter
ACT_terra_season_winter_plot_dat<- ACT_change_season_terra_ACT_plot_dat %>%
  filter(season == "winter")
semi_transparent_blue<- "#0000FF15"
ACT_terra_season_winter_plot <- ggplot(ACT_terra_season_winter_plot_dat, 
                                       aes(x = Region, y = Value, 
                                           color = Species,
                                           fill = Species)) +
  geom_point(position = position_dodge(width = .3), 
             size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-10,350)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 340, label = "Winter", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
ACT_terra_season_winter_plot

####################   ####################   ####################   ####################   
#################### ACT plot: summer
ACT_terra_season_summer_plot_dat<- ACT_change_season_terra_ACT_plot_dat %>% 
  filter(season == "summer")
semi_transparent_red <- "#FF999915"
ACT_terra_season_summer_plot <- ggplot(ACT_terra_season_summer_plot_dat, 
                                       aes(x = Region, y = Value, 
                                           color = Species,
                                           fill = Species)) +
  geom_point(position = position_dodge(width = .35), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3",
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(hr/season)", 
       color = "Species", 
       x = NULL) +
  ylim(-300,40)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 30, label = "Summer", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
ACT_terra_season_summer_plot



####################   ####################   ####################   ####################   
#################### ACT plot: spring
ACT_terra_season_spring_plot_dat<- ACT_change_season_terra_ACT_plot_dat %>% 
  filter(season == "spring")
ACT_terra_season_spring_plot <- ggplot(ACT_terra_season_spring_plot_dat, 
                                       aes(x = Region, y = Value, 
                                           color = Species,
                                           fill = Species)) +
  geom_point(position = position_dodge(width = .35), size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25))  +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3",
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
 ylim(-200,250)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 230, label = "Spring", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
ACT_terra_season_spring_plot

####################   ####################   ####################   ####################   
#################### Final plot: ACT Change Season
ACT_terra_change_season_plot <- plot_grid(ACT_terra_season_summer_plot,
                                          ACT_terra_season_spring_plot,
                                          ACT_terra_season_winter_plot,
                                          nrow = 1, ncol = 3)

ACT_terra_change_season_plot


####################   ####################   ####################   ####################   
#################### M_Rate plot: winter
M_Rate_terra_season_winter_plot_dat<- MR_change_season_terra_plot_dat%>% 
  filter(season == "winter")
semi_transparent_blue<- "#0000FF15"
M_Rate_terra_season_winter_plot <- ggplot(M_Rate_terra_season_winter_plot_dat, 
                                          aes(x = Region, y = Value, 
                                              color = Species,
                                              fill = Species)) +
  geom_point(position = position_dodge(width = .3), 
             size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-5,80)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 78, label = "Winter", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
M_Rate_terra_season_winter_plot

####################   ####################   ####################   ####################   
#################### M_Rate plot: summer
M_Rate_terra_season_summer_plot_dat<- MR_change_season_terra_plot_dat %>%
  filter(season == "summer")
semi_transparent_red <- "#FF999915"
M_Rate_terra_season_summer_plot <- ggplot(M_Rate_terra_season_summer_plot_dat, 
                                          aes(x = Region, y = Value, 
                                              color = Species,
                                              fill = Species)) +
  geom_point(position = position_dodge(width = .35), 
             size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(kJ/season)", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-20, 43), breaks = seq(-20, 40, by = 20)) + # Customize the y-axis with subscript
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_red, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 42, label = "Summer", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
M_Rate_terra_season_summer_plot


####################   ####################   ####################   ####################   
#################### M_Rate plot: spring
M_Rate_terra_season_spring_plot_dat<- MR_change_season_terra_plot_dat %>%
  filter(season == "spring")
M_Rate_terra_season_spring_plot <- ggplot(M_Rate_terra_season_spring_plot_dat, 
                                          aes(x = Region, y = Value, 
                                              color = Species,
                                              fill = Species)) +
  geom_point(position = position_dodge(width = .35), 
             size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3", 
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-20, 85), breaks = seq(-20, 80, by = 20)) + # Customize the y-axis with subscript
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_green, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  annotate("text", x = 1.5, y = 85, label = "Spring", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
M_Rate_terra_season_spring_plot

####################   ####################   ####################   ####################   
#################### MRate Final season plot 
MRate_terra_change_season_plot <- plot_grid(M_Rate_terra_season_summer_plot,
                                            M_Rate_terra_season_spring_plot,
                                            M_Rate_terra_season_winter_plot,
                                            nrow = 1, ncol = 3)



####################   ####################   ####################   ####################   
#################### Foraging rate (ants/hr): winter
Forage_terra_season_winter_plot_dat <- Forage_change_season_terra_ACT_plot_dat %>% 
  filter(season == "winter")
semi_transparent_blue<- "#0000FF15"
Forage_terra_season_winter_plot <- ggplot(Forage_terra_season_winter_plot_dat, 
                                          aes(x = Region, y = Value, 
                                              color = Species,
                                              fill = Species)) +
  geom_point(position = position_dodge(width = .4), 
             size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y =NULL, 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-100, 20), breaks = seq(-100, 20, by = 20)) + # Customize the y-axis with subscript
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_blue, colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x = 1.5, y = 17, label = "Winter", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
Forage_terra_season_winter_plot

####################   ####################   ####################   ####################   
#################### Foraging rate (ants/hr): summer
Forage_terra_season_summer_plot_dat <- Forage_change_season_terra_ACT_plot_dat %>% 
  filter(season == "summer")
semi_transparent_red <- "#FF999915"
Forage_terra_season_summer_plot <- ggplot(Forage_terra_season_summer_plot_dat, 
                                          aes(x = Region, y = Value, 
                                              color = Species,
                                              fill = Species)) +
  geom_point(position = position_dodge(width = .3), 
             size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "(ants/hr)", 
       color = "Species", 
       x = NULL) +
  ylim(0,35)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_red, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x =1.5, y = 34, label = "Summer", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
Forage_terra_season_summer_plot


####################   ####################   ####################   ####################   
#################### Foraging rate (ants/hr): spring
Forage_terra_season_spring_plot_dat <- Forage_change_season_terra_ACT_plot_dat %>% 
  filter(season == "spring")
Forage_terra_season_spring_plot <- ggplot(Forage_terra_season_spring_plot_dat, 
                                          aes(x = Region, y = Value, 
                                              color = Species,
                                              fill = Species)) +
  geom_point(position = position_dodge(width = .3), 
             size = 6, alpha = 0.5, 
             aes(shape = Climate_Scenario), stroke = 1.5) +
  scale_shape_manual(values = c("future_2" = 21, 
                                "future_4" = 25)) +
  scale_color_manual(values = c("C. angulifer" = "grey40",
                                "A. aculeata" = "darkgoldenrod3", 
                                "M. suborbitalis" = "coral2", 
                                "P. lineoocellata" = "bisque3", 
                                "T. sparsa" = "firebrick2",
                                "G. variegata" = "grey40",
                                "P. minor" = "darkgoldenrod3", 
                                "C. isolepis" = "coral2", 
                                "C. quatt" = "bisque3", 
                                "M. horridus" = "firebrick2"), 
                     na.value = NA,  limits = species_order)+
  scale_fill_manual(values = c("C. angulifer" = "grey40",
                               "A. aculeata" = "darkgoldenrod3",
                               "M. suborbitalis" = "coral2", 
                               "P. lineoocellata" = "bisque3", 
                               "T. sparsa" = "firebrick2",
                               "G. variegata" = "grey40",
                               "P. minor" = "darkgoldenrod3", 
                               "C. isolepis" = "coral2", 
                               "C. quatt" = "bisque3", 
                               "M. horridus" = "firebrick2"),
                    na.value = NA,  limits = species_order) +
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = NULL, 
       color = "Species", 
       x = NULL) +
  ylim(-10,20)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = semi_transparent_green, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),  
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2),
         fill = "none") +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x =1.5, y = 18, label = "Spring", 
           size = 7,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5)
Forage_terra_season_spring_plot

########
## Foraging final season plot 
Forage_terra_change_season_plot <- plot_grid(Forage_terra_season_summer_plot,
                                             Forage_terra_season_spring_plot,
                                             Forage_terra_season_winter_plot,
                                             nrow = 1, ncol = 3)

Forage_terra_change_season_plot


####################   ####################   ####################   ####################   
#################### ***FINAL PLOT TERRA CLIMATE: Activity and Metabolic rate
Figure_5 <- grid.arrange(
  ACT_M_Rate_Forage_change_yr,
  ACT_terra_change_season_plot,
  MRate_terra_change_season_plot,
  Forage_terra_change_season_plot,
  ncol = 3,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 1, 2, 3, 4), 
                         nrow = 2, ncol = 3, byrow = TRUE)
)



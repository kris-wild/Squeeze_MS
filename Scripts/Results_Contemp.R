pacman::p_load(maps, dplyr, tidyr, tidyverse, flextable, knitr, here, ggplot2, purrr, rnaturalearth, rnaturalearthdata, grid, cowplot, simplecolors, ggnewscale, gridExtra, grid, ggbreak, sf)

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
# Load world countries
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world[world$continent == "Africa", ]
# Clean the geometries
africa_clean <- africa %>% st_make_valid()
# Dissolve all internal borders by unifying all geometries into one
africa_unified <- africa_clean %>% 
  st_geometry() %>%
  st_union() %>% 
  st_collection_extract("POLYGON")
# Create a single feature for the continent of Africa
africa_sf <- st_sf(geometry = st_sfc(africa_unified))
# Create a map grob with only the outline
africa_map <- ggplot() +
  geom_sf(data = africa_sf, fill = "white", color = "black") +
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
# Create a ggplot object for the map
australia_plot <- ggplot(data = australia) +
  geom_sf(fill = "transparent", color = "black") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))
africa_plot <- ggplot(data = africa_sf) +
  geom_sf(fill = "white", color = "black") +
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
# Replace _AF with _Af
contemp_ta_tb_africa$Species <- gsub("_AF", "_Af", contemp_ta_tb_africa$Species)
# Add a "_" after every "f" in the Species column
contemp_ta_tb_africa$Species <- gsub("Site_Af", "Site_Af_", contemp_ta_tb_africa$Species)
# Remove "Site_" from the Species column
contemp_ta_tb_africa$Species <- gsub("Site_", "", contemp_ta_tb_africa$Species)
# Define factor levels
contemp_ta_tb_africa$Combined <- factor(contemp_ta_tb_africa$Combined, 
                                        levels = c("Min Ta", "Mean Ta", "Max Ta", 
                                                   "Min Tb", "Mean Tb",  "Max Tb"))
species_order_AF = c("Af_A", "Af_B", "Af_L", "Af_X",
                     "C. angulifer", "A. aculeata", "M. suborbitalis", "P. lineoocellata",
                     "T. sparsa")
colors <- c( "#9DB9F1", "#4479E4", "#16439C", "#0D2659",
             "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2")

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
  labs(y = "Temperature Change by Decade (°C)", color = "Site/Species", 
       x = NULL) +
  ylim(0, .5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.18, .70),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA, 
                                         color = "black", 
                                         size = 0.5), # Adjust color and size as needed
        legend.box.spacing = unit(0, 'lines'))  + 
  guides(color = guide_legend(ncol = 2, title.position = "top", )) +  
  annotate("text",  x = 1.4, y = .50, label = "● P-value < 0.05",  size = 5)+
  annotate("text", x = 1.4, y = .47, label = "○ P-value > 0.05", size = 5) +
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 40, unit = "pt"))
Ta_Tb_contemp_africa
# add inset map
Ta_Tb_contemp_africa_final <- add_inset_map(Ta_Tb_contemp_africa, africa_plot, 
                                            x = 5, y = 0.16, 
                                            width = 1.2, height = .5)

Ta_Tb_contemp_africa_final

#### Summary for paper
contemp_ta_tb_africa_summary <- contemp_ta_tb_africa %>% 
  group_by(TA_TB, Type) %>% 
  summarise(Slope_10_Year_mean = mean(Slope_10_Year))


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  Plotting - Australia
contemp_ta_tb_australia <- readRDS("Raw_data/contemp_ta_tb_australia.rds")
# Replace _AU with _Au
contemp_ta_tb_australia$Species <- gsub("_AU", "_Au", contemp_ta_tb_australia$Species)
# Add a "_" after every "f" in the Species column
contemp_ta_tb_australia$Species <- gsub("Site_Au", "Site_Au_", contemp_ta_tb_australia$Species)
# Remove "Site_" from the Species column
contemp_ta_tb_australia$Species <- gsub("Site_", "", contemp_ta_tb_australia$Species)
contemp_ta_tb_australia$Combined <- factor(contemp_ta_tb_australia$Combined, 
                                           levels = c("Min Ta", "Mean Ta", "Max Ta", 
                                                      "Min Tb", "Mean Tb",  "Max Tb"))

species_order_AU = c( "Au_A", "Au_L", "Au_R", 
                      "G. variegata", "P. minor", "C. isolepis", "C. quatt",
                     "M. horridus")
colors <- c( "#9DB9F1", "#4479E4", "#16439C","grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2")
###########
### Plotting - TA/TB australia 
Ta_Tb_contemp_australia <- ggplot(contemp_ta_tb_australia, 
                                  aes(x = Combined, y = Slope_10_Year,
                                      color = Species, shape = Significance)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_shape_manual(values = c("p < 0.05" = 16, "p ≥ 0.05" = 1), 
                     guide = FALSE) + 
  scale_color_manual(values =  colors, breaks = species_order_AU) + 
  theme_bw() +
  labs(y = NULL, color = "Site/Species", 
       x = NULL) +
  ylim(0, .5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.18, .7),  # Adjust these values to move the legend within the plot
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA, 
                                         color = "black", 
                                         size = 0.5), # Adjust color and size as needed
        legend.box.spacing = unit(0, 'lines'))  + 
  guides(color = guide_legend(ncol = 2, title.position = "top", 
                              nrow = 5)) +  
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 40, unit = "pt"))
Ta_Tb_contemp_australia

# add inset map
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
                                     labels = c("A", "B"),
                                     label_size = 24)
Ta_Tb_contemp_year_plot


##########################################
###### overall Ta/Tb change for paper
contemp_ta_tb_africa_summary <- contemp_ta_tb_africa %>% 
  group_by(TA_TB, Type) %>% 
  summarise(Slope_10_Year_mean = round(mean(Slope_10_Year), digits = 2))
contemp_ta_tb_australia_summary <- contemp_ta_tb_australia %>% 
  group_by(TA_TB, Type) %>% 
  summarise(Slope_10_Year_mean = round(mean(Slope_10_Year), digits = 2))


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
                                                      
# Species order for plotting
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
  labs(y = "Temperature Change by Decade (°C)", color = "Species/Site", 
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
  annotate("text", x = 3.5, y = .4, label = "winter", 
           size = 6,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))



####################   ####################   ####################  
######## Africa Spring
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
  annotate("text", x = 3.5, y = .4, label = "spring", 
           size = 6,  
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
  labs(y = NULL, color = "Species/Site", 
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
  annotate("text", x = 3.5, y = .4, label = "summer", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))


####################   ####################   ####################   ####################   
######### Africa Ta/Tb seasonal plot final
Tb_Ta_contemporary_season_africa <- plot_grid(Ta_Tb_contemp_winter_africa_plot,
                                              Ta_Tb_contemp_spring_africa_plot,
                                              Ta_Tb_contemp_summer_africa_plot,
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
  annotate("text", x = 3.5, y = .4, label = "winter", 
           size = 6,  # Size 8 in ggplot2 corresponds to 20pt font size
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
  annotate("text", x = 3.5, y = .4, label = "spring", 
           size = 6,  
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
  annotate("text", x = 3.5, y = .4, label = "summer", 
           size = 6,  
           fontface = "bold", hjust = 0.5) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))

####################   ####################   ####################   ####################   
######## australia Ta/Tb seasonal plot
Tb_Ta_contemporary_season_australia <- plot_grid(Ta_Tb_contemp_winter_australia_plot,
                                                 Ta_Tb_contemp_spring_australia_plot,
                                                 Ta_Tb_contemp_summer_australia_plot,
                                                 nrow = 1, ncol =3)




####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  ***FINAL PLOT CONTEMPORARY CLIMATE: Ta/Tb
# 1700 X1100 -png or eps
# 16 x 12 - pdf 
Figure_2 <- grid.arrange(
  Ta_Tb_contemp_year_plot,
  Tb_Ta_contemporary_season_africa,
  Tb_Ta_contemporary_season_australia,
  ncol = 2,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)
)
ggsave("../../Pianka_figs/Figure_2.pdf", Figure_2, device = "pdf", width = 25, height =16)



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
### Contemporary sum annual activity, Mrate, Foraging (Figure 3)
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
############ READ IN DATA
ACT_MRate_Foraging_contemp <- readRDS("Raw_data/ACT_MRate_Foraging_contemp.rds") %>% 
  filter(Species != "Overall")

# data prep
ACT_MRate_Foraging_contemp$Test <- factor(ACT_MRate_Foraging_contemp$Test, levels = c("ACT", "M_Rate", "Feeding_demand"))
# Create a new column for significance
ACT_MRate_Foraging_contemp$Significant <- ACT_MRate_Foraging_contemp$P_value < 0.05

# filter data by "Test" for plots
ACT_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "ACT")
MRate_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "M_Rate")
Foraging_contemp_plot_dat <- ACT_MRate_Foraging_contemp %>% 
  filter(Test == "Feeding_demand")

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
  guides(fill = FALSE, color = guide_legend(order = 1), 
         shape = guide_legend(order = 2)) +
  theme_bw() +
  labs(y = "h/decade", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-30, 35), breaks = seq(-30, 30, by = 10)) + 
  annotate("text", x = 1.5, y = 33, label = expression(Delta * "Activity time"), 
           fontface = "bold", size = 10.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  annotate("text",  x = 2.05, y = -3, label = "● P-value < 0.05",  size = 5)+
  annotate("text", x = 2.05, y = -6, label = "○ P-value > 0.05", size = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.78, .2),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA, 
                                         color = "black", 
                                         size = 0.5), # Adjust color and size as needed
        legend.box.spacing = unit(0, 'lines')) +
  guides(color = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", 
                              title.theme = element_text(size = 9),
                              ncol = 2), fill = "none")+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
ACT_contemp_plot


# diel Activity difference between diurnal and nocturnal change for 
overall_ACT_change_value <-  ACT_contemp_plot_dat%>% 
  mutate(diel = if_else(Species %in% c("G. variegata", "C. angulifer"), "nocturnal", # australian sites
                        "diurnal")) %>% 
  group_by(diel) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year))



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
  labs(y = "J/g/decade", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-100, 550), breaks = seq(-100, 500, by = 100)) + 
  annotate("text", x = 1.5, y = 540, label = expression(Delta * "Metabolic Rate"), 
           fontface = "bold", size = 10.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
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

overall_Mrate_change_value <-  ACT_contemp_plot_dat%>% 
  group_by(Region) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year))



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
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  labs(y = "J/g/h/decade", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-.05, .15), breaks = seq(-.05, .15, by = .05)) + 
  annotate("text", x = 1.5, y = .145, label = expression(Delta * "Feeding Demand"), 
           fontface = "bold", size = 10.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
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

overall_feeding_demand_change_value <-  Foraging_contemp_plot_dat%>% 
  filter(Species != "C. angulifer" & Species != "G. variegata" ) %>% 
  group_by(Region) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year))

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Activity and Metabolic rate year final plot
ACT_MRate_Foraging_contemp_plot <- plot_grid(ACT_contemp_plot,
                                             MRate_contemp_plot,
                                             Foraging_contemp_plot,
                                             ncol = 3, nrow = 1, align = "h", 
                                             labels = c("A", "B", "C"), 
                                             label_size = 24)
ACT_MRate_Foraging_contemp_plot


####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## Contemporary sum seasonal activity  Metabolic rate, Feeding rate
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
############  READ IN DATA
contemp_ACT_MRate_Foraging_season <- readRDS("Raw_data/contemp_ACT_MRate_Foraging_season.rds")

### arrange data for plots
contemp_ACT_MRate_Foraging_season$Test <- factor(contemp_ACT_MRate_Foraging_season$Test, levels = c("ACT", "M_Rate", "Feeding_demand"))
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
  filter(Season == "winter"& Test == "Feeding_demand")
contemp_Foraging_summer_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "summer" & Test == "Feeding_demand")
contemp_Foraging_spring_plot_dat<- contemp_ACT_MRate_Foraging_season %>% 
  filter(Season == "spring" & Test == "Feeding_demand")

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
  labs(y =  "h/decade", 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-0, 15), breaks = c(0, 5, 10, 15)) + 
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
  annotate("text", x = 1.5, y = 15, label = "winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") + # 1.7 - too far left  
  #annotate("text", x = 1.5, y = 10, 
           #label = "+", 
           #size = 12,
           #fontface = "bold") 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
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
  scale_y_continuous(limits = c(-12, 14), breaks = c(-10, -5, 0, 5, 10)) + 
  annotate("text", x = 1.5, y = 14, label = "spring", 
           size = 8,  
           fontface = "bold") +    
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
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
  #annotate("text", x = 1, y = 1.5, label = " -", 
           #size = 12,
           #fontface = "bold") +
  #annotate("text", x = 2, y = 5, label = " +", 
           #size = 12,
           #fontface = "bold") 
contemp_ACT_spring_plot

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
  labs(y =  NULL, 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-15, 5), breaks = c(-15,-10, -5, 0, 5)) + 
  annotate("text", x = 1.5, y = 5, label = "summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
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
        legend.box.spacing = unit(0, 'lines')) + # This line removes the fill legend
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  #annotate("text", x = 1.5, y = 0, label = "-", 
           #size = 12,
           #fontface = "bold") +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
contemp_ACT_summer_plot

# Seasonal Activity difference
ACT_change_season <-  contemp_ACT_MRate_Foraging_season%>% 
  filter(Test == "ACT") %>% 
  mutate(diel = if_else(Species %in% c("G. variegata", "C. angulifer"), "nocturnal", # australian sites
                        "diurnal")) %>% 
  group_by(Species, Season) %>% 
  summarise(ACT_mean_decade = mean(Slope_10_Year)) %>% 
  filter(Season == "spring" | Season == "summer")
range(ACT_change_season$ACT_mean_decade)



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
  labs(y = "J/g/decade", 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-155, 250), breaks = seq(-150, 250, by = 50)) +
  annotate("text", x = 1.5, y = 245, label = "winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")+
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
        legend.box.spacing = unit(0, 'lines')) +  # This line removes the fill legend
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
  #annotate("text", x = 1.5, y = 2.4, label = "+", 
           #size = 12,
           #fontface = "bold")
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
  scale_y_continuous(limits = c(-155, 250), breaks = seq(-150, 250, by = 50))  +
  annotate("text", x = 1.5, y = 245, label = "spring", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")+
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
contemp_MRate_spring_plot

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
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
  scale_y_continuous(limits = c(-155, 250), breaks = seq(-150, 250, by = 50))  +
  annotate("text", x = 1.5, y = 245, label = "summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold")+
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
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")   # Dashed line at y = 0
contemp_MRate_summer_plot



####################   ####################   ####################   ####################   
######## Foraging Winter plots
contemp_Foraging_winter_plot_1 <- ggplot(contemp_Foraging_winter_plot_dat, 
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
  scale_y_continuous(limits = c(-.12, .22), breaks = c( -.1, -.05, 0, 0.05, .1, .15, .2)) +
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
  annotate("text", x = 1.5, y = .22, label = "winter", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) +  # This line removes the fill legend
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 11.5, unit = "mm")) 
contemp_Foraging_winter_plot_1

contemp_Foraging_winter_plot_2 <- ggplot(contemp_Foraging_winter_plot_dat, 
                                         aes(x = Region, y = Slope_10_Year, 
                                             color = Species)) +
  geom_point(position = position_dodge(width = 0), size = 6, alpha = 0.5, 
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
  scale_y_continuous(limits = c(-1.99, -.8), breaks = seq(-1.8, -.8, by = .4))  +
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Hide the legend
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = semi_transparent_blue, 
                                        colour = NA),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) +  # This line removes the fill legend
  theme(plot.margin = margin(t = 0, r = 2, b = 5, l = 13.8, unit = "mm")) 
  #annotate("text", x = 1, y = -16.05, label = "-", 
           #size = 12,  # Size 8 in ggplot2 corresponds to 20pt font size
           #fontface = "bold")
contemp_Foraging_winter_plot_2

# Final
contemp_Foraging_winter_combined_plot <- plot_grid(
  contemp_Foraging_winter_plot_1, 
  contemp_Foraging_winter_plot_2,
  nrow = 2,  # Specify two rows
  rel_heights = c(2, 1)  # First plot has double the height of the second
) 

# adding y axis
contemp_Foraging_winter_plot_final <- ggdraw() +
  draw_plot(contemp_Foraging_winter_combined_plot) +
  draw_label("J/g/h/decade", 
             x = 0, 
             y = 0.6, 
             hjust = 0.5, 
             vjust = 2,
             angle = 90, # Rotate the label for y-axis
             size = 14) # Adjust size according to your preference
contemp_Foraging_winter_plot_final


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
  annotate("text", x = 1.5, y = .22, label = "spring", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  scale_y_continuous(limits = c(-.12, .22), breaks = c( -.1, -.05, 0, 0.05, .1, .15, .2)) +
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Dashed line at y = 0
  #annotate("text", x = 1, y = .60, label = "+", 
           #size = 12,
           #fontface = "bold") 
contemp_Foraging_spring_plot


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
  scale_y_continuous(limits = c(-.12, .22), breaks = c( -.1, -.05, 0, 0.05, .1, .15, .2)) +
  labs(y = NULL, 
       color = "Species", 
       x = NULL)  +
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
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  annotate("text", x = 1.5, y = .22, label = "summer", 
           size = 8,  # Size 8 in ggplot2 corresponds to 20pt font size
           fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  #annotate("text", x = 1, y = 0.5, label = "+", 
           #size = 12,
           #fontface = "bold") 
contemp_Foraging_summer_plot

####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
######## ACT/Mrate seasonal plots
ACT_contemporary_season_plot <- plot_grid(contemp_ACT_winter_plot,
                                          contemp_ACT_spring_plot,
                                          contemp_ACT_summer_plot,
                                          nrow = 1, ncol = 3)

MRate_contemporary_season_plot <- plot_grid(contemp_MRate_winter_plot,
                                            contemp_MRate_spring_plot,
                                            contemp_MRate_summer_plot,
                                            nrow = 1, ncol = 3)

Foraging_contemporary_season_plot <- plot_grid(contemp_Foraging_winter_plot_final,
                                               contemp_Foraging_spring_plot,
                                               contemp_Foraging_summer_plot,
                                               nrow = 1, ncol = 3)




####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
########  ***FINAL PLOT CONTEMPORARY CLIMATE: Activity and Metabolic rate
# save: 2200 X 1200
# 26X14 PDF
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
ggsave("../../Pianka_figs/Figure_3.pdf", Figure_3, device = "pdf", width = 25, height =16)



# Mrate averages by season - use this for results for quick overall pattern references
# ACT_MRate_Foraging_contemp
# contemp_ACT_MRate_Foraging_season
Mrate_season_value_diff <- contemp_ACT_MRate_Foraging_season %>%
  filter(Test == "ACT") %>% 
  filter(Species != "C. angulifer" & Species !="G. variegata") %>% 
  group_by(Region, Season) %>% 
  summarise(Mean_Mrate_season = mean(Slope_10_Year),
            SD_Mrate_season = sd(Slope_10_Year))

Mrate_season_value_diff



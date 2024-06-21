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
####################   ####################   ####################   ####################   
####################     TERRA CLIMATE TA/TB Figure 4                ####################      
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   

############ READ IN DATA
Tb_Ta_change_spp_final <- readRDS("Raw_data/Tb_Ta_change_spp_final.rds")%>% 
  mutate(species = gsub("Site_", "", species),
         species = ifelse(grepl("AF", species), 
                       paste0("Af_", substr(species, 3, nchar(species))), species),
         species = ifelse(grepl("AU", species), 
                       paste0("Au_", substr(species, 3, nchar(species))), species)) %>% 
  mutate(diel = case_when(
    species %in% c("C. angulifer", "G. variegata") ~ "nocturnal",
    species %in% c("A. aculeata ", "C. isolepis","C. quatt", "M. horridus", 
                   "M. suborbitalis", "P. minor", "T. sparsa") ~ "diurnal",
    species %in% "Af_A" ~ "Af_A",
    species %in% "Af_B" ~ "Af_B",
    species %in% "Af_L" ~ "Af_L",
    species %in% "Af_X" ~ "Af_X",
    species %in% "Au_A" ~ "Au_A",
    species %in% "Au_L" ~ "Au_L",
    species %in% "Au_R" ~ "Au_R")) %>%
  mutate(Variable = gsub("_", " ", Variable)) 
x_axis_order <- c("Min Ta","Mean Ta","Max Ta","Min Tb","Mean Tb","Max Tb")
Tb_Ta_change_spp_final$Variable <- factor(Tb_Ta_change_spp_final$Variable, 
                                             levels = x_axis_order)




# Australia - plotting data
Tb_Ta_change_spp_australia <- Tb_Ta_change_spp_final %>% filter(region == "Australia")
species_order_AU <- c("Au_A", "Au_L", "Au_R", "G. variegata", "P. minor", "C. isolepis", "C. quatt",
                      "M. horridus")
Tb_Ta_change_spp_australia$species <- factor(Tb_Ta_change_spp_australia$species, 
                                             levels = species_order_AU)

# Africa - plotting data
Tb_Ta_change_spp_africa <- Tb_Ta_change_spp_final %>% filter(region == "Africa")
species_order_AF = c("Af_A", "Af_B", "Af_L", "Af_X", "C. angulifer", "A. aculeata", "M. suborbitalis", "P. lineoocellata","T. sparsa")
Tb_Ta_change_spp_africa$species <- factor(Tb_Ta_change_spp_africa$species,
                                          levels = species_order_AF)

####################   ####################   ####################   ####################   
######## Plotting
colors <- c( "#9DB9F1", "#4479E4", "#16439C", "#0D2659", "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2")
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
       color = "Site/Species", shape = "Climate Scenario", 
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
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 14), 
                              ncol = 2),
         shape = guide_legend(title.position = "top", title.theme = element_text(size = 14), 
                              ncol = 2))+
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))
Tb_Ta_change_spp_africa_plot

Tb_Ta_change_spp_africa_final <- add_inset_map(Tb_Ta_change_spp_africa_plot, africa_plot, 
                                               x = .30, y = 4.8, 
                                               width = 2, height = 1.5)
Tb_Ta_change_spp_africa_final

####################   ####################   ####################   ####################   
######## Australia - Plot
colors <- c( "#9DB9F1", "#4479E4", "#16439C", "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2")
Tb_Ta_change_spp_australia_plot <- ggplot(Tb_Ta_change_spp_australia, aes(x = Variable, y = value, 
                                                                          color = species, shape = climate_scenario)) +
  geom_point(position = position_dodge(width = .3), size = 5, alpha = 0.5, stroke = 1.5) +
  scale_color_manual(values =  colors, breaks = species_order_AU)+
  theme_bw() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +  # Dashed line at y = 2
  geom_hline(yintercept = 4, linetype = "dashed", color = "black") +   # Dashed line at y = 4
  labs(y = NULL, 
       color = "Site/Species", shape = "Climate Scenario", 
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
  guides(color = guide_legend(title.position = "top", title.theme = element_text(size = 14), 
                              ncol = 2, nrow = 5),
         shape = FALSE)+
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
                                         labels = c("A", "B"),
                                         label_size = 24)
Tb_Ta_change_spp_final_plot



####################   ####################   ####################   ####################   
######## Tb/Ta seasonal
### READ IN DATA
Tb_Ta_change_spp_season_final <- readRDS("Raw_data/Tb_Ta_change_spp_season_final.rds") %>% 
  mutate(species = gsub("Site_", "", species),
         species = ifelse(grepl("AF", species), 
                          paste0("Af_", substr(species, 3, nchar(species))), species),
         species = ifelse(grepl("AU", species), 
                          paste0("Au_", substr(species, 3, nchar(species))), species)) %>% 
  mutate(Variable = gsub("_", " ", Variable))
Tb_Ta_change_spp_season_final$Variable <- factor(Tb_Ta_change_spp_season_final$Variable, 
                                          levels = x_axis_order)
# Africa - plotting data
Tb_Ta_change_spp_season_africa <- Tb_Ta_change_spp_season_final %>% filter(region == "Africa")
Tb_Ta_change_spp_season_africa$species <- factor(Tb_Ta_change_spp_season_africa$species, 
                                                 levels = species_order_AF)

# Australia - plotting data
Tb_Ta_change_spp_season_australia <- Tb_Ta_change_spp_season_final %>% filter(region == "Australia")
Tb_Ta_change_spp_season_australia$species <- factor(Tb_Ta_change_spp_season_australia$species, 
                                                    levels = species_order_AU)


####################   ####################   ####################   ####################   
######## Plotting
colors <- c(  "#9DB9F1", "#4479E4", "#16439C", "#0D2659","grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2")
# seasonal colors
semi_transparent_blue<- "#0000FF15"
semi_transparent_red <- "#FF999915"
semi_transparent_green <- "#00FF080F"

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
  annotate("text", x = 3.5, y = 5.8, label = "summer", 
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
  labs(y= expression(Delta * " Temperature (" * degree * "C)"), 
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
  annotate("text", x = 3.5, y = 5.8, label = "winter", 
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
africa_Tb_Ta_change_season <- plot_grid(Tb_Ta_change_season_winter_africa_plot,
                                        Tb_Ta_change_season_spring_africa_plot,
                                        Tb_Ta_change_season_summer_africa_plot,
                                        nrow = 1, ncol = 3)
africa_Tb_Ta_change_season


####################   ####################   ####################   ####################   
######## australia plots: summer and winter
# australia - Plot: summer
Tb_Ta_change_season_summer_australia<- Tb_Ta_change_spp_season_australia %>% filter(season == "summer")
colors <- c( "#9DB9F1", "#4479E4", "#16439C", "grey40","darkgoldenrod3", "coral2","bisque3", "firebrick2")
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
  annotate("text", x = 3.5, y = 5.8, label = "summer", 
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
  annotate("text", x = 3.5, y = 5.8, label = "winter", 
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
australia_Tb_Ta_change_season <- plot_grid(Tb_Ta_change_season_winter_australia_plot,
                                           Tb_Ta_change_season_spring_australia_plot,
                                           Tb_Ta_change_season_summer_australia_plot,
                                           nrow = 1, ncol = 3)

australia_Tb_Ta_change_season



############################################################
############################################################
######### ****Final Ta/Tb TerraClimate Plots
# Arrange the plots in a 2x2 grid
# 1500 X1300 - png or eps
# 18 x 14 - pdf 
Figure_4 <- grid.arrange(
  Tb_Ta_change_spp_final_plot,
  africa_Tb_Ta_change_season,
  australia_Tb_Ta_change_season,
  ncol = 2,
  nrow = 2,
  layout_matrix = matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)
)


ggsave("../../Pianka_figs/Figure_4.pdf", Figure_4, device = "pdf", width = 25, height =16)



####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
####################   ####################   ####################   ####################   
#################### FIGURE 5 TERRA delta change: Activity  Metabolic rates and foraging #########
####################   ####################   ####################   ####################   
####################   ####################   ####################   #################### 
### READ IN DATA: ANNIUAL AND SEASONAL
ACT_MR_Forage_change_yr_final <- readRDS("Raw_data/ACT_MR_Forage_change_yr_final.rds") %>% 
  mutate(diel = case_when(
    Species %in% c("C. angulifer", "G. variegata") ~ "nocturnal", 
    TRUE ~ "diurnal" # Else, diurnal
  )) 

####################
## data prep forACT plots
ACT_change_yr_terra_ACT_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "ACT_hr")%>% 
  group_by(Region, Climate_Scenario, diel) %>% 
  summarise(Value = mean(Value))

####################   ####################   ####################   ####################   
#################### Plotting
# Define the order of the species
species_order <- c("diurnal, nocturnal")



####################   ####################   ####################   ####################   
#################### ACT Plot
# Function to determine shape
# Add an interaction term for the line groups that includes Region
ACT_change_yr_terra_ACT_plot_dat <- ACT_change_yr_terra_ACT_plot_dat %>%
  mutate(diel_climate = interaction(diel, Climate_Scenario, sep = "_"),
         line_group = interaction(Region, diel))

# Now plot with the updated dataframe
ACT_change_yr_terra_plot <- ggplot(ACT_change_yr_terra_ACT_plot_dat, aes(x = Climate_Scenario, 
                                                                         y = Value, 
                                                                         group = line_group, 
                                                                         color = diel_climate,
                                                                         fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2), color = "black") +
  geom_point(position = position_dodge(width = 0.2), size = 6, aes(shape = Region), 
             stroke = 1.5) +
  scale_shape_manual(values = c("Africa" = 22, 
                                "Australia" = 25)) +
  scale_color_manual(values = c("diurnal_future_2" = "#FF9999",
                                "nocturnal_future_2" = "grey80",
                                "diurnal_future_4" = "#CC0000",
                                "nocturnal_future_4" = "grey35"),
                     guide = FALSE) + # Disable the color legend
  scale_fill_manual(values = c("diurnal_future_2" = "#FF9999",
                               "nocturnal_future_2" = "grey80",
                               "diurnal_future_4" = "#CC0000",
                               "nocturnal_future_4" = "grey35"),
                    guide = FALSE) + # Disable the fill legend
  guides(shape = guide_legend(title = "Region")) + # Enable and title the shape legend
  theme_bw() +
  labs(y =  "hr/yr", 
       x = NULL) +
  scale_y_continuous(limits = c(-200, 820), breaks = seq(-200, 800, by = 200)) +
  annotate("text", x = 1.5, y = 800, label = expression(Delta * "Activity time"), 
           fontface = "bold", size = 10.5)+
  annotate("text", x = .68, y = 620, label = "Nocturnal", 
           color = "grey35", fontface = "bold", size = 6)+
  annotate("text", x = .7, y = 580, label = "Diurnal", 
           color = "#CC0000", fontface = "bold", size = 6)+
  theme(legend.position = c(0.15, 0.88),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
ACT_change_yr_terra_plot


### READ IN DATA: ANNIUAL AND SEASONAL
ACT_MR_Foraging_change_season_final <- readRDS("Raw_data/ACT_MR_Foraging_change_season_final.rds") %>% 
  mutate(diel = case_when(
    Species %in% c("C. angulifer", "G. variegata") ~ "nocturnal", 
    TRUE ~ "diurnal")) 

## data prep for Mrate, ACT, Foraging rate plots
ACT_change_season_terra_ACT_plot_dat <- ACT_MR_Foraging_change_season_final %>% 
  filter(Test == "ACT_hr")%>% 
  group_by(Region, Climate_Scenario, season, diel) %>% 
  summarise(Value = mean(Value))

# ACT plot:AFRICA SEASON
ACT_terra_season_africa_dat<- ACT_change_season_terra_ACT_plot_dat %>%
  filter(Region == "Africa") %>% 
  mutate(diel_climate = interaction(diel, Climate_Scenario, season, sep = "_"),
         line_group = interaction(season, diel))
# Assuming ACT_terra_season_africa_dat is your dataframe as described.
ACT_terra_season_africa_dat <- ACT_terra_season_africa_dat %>%
  mutate(Climate_Season = interaction(Climate_Scenario, season, sep = "_"))

# Then, specify the order you want for Climate_Season
climate_season_order <- c("future_2_winter", "future_4_winter",
                          "future_2_spring", "future_4_spring", 
                          "future_2_summer", "future_4_summer")

# Now, factorize the Climate_Season column using the specified order
ACT_terra_season_africa_dat <- ACT_terra_season_africa_dat %>%
  mutate(Climate_Season = factor(Climate_Season, levels = climate_season_order))

# Now plot with the ordered factor
ACT_africa_season <- ggplot(ACT_terra_season_africa_dat, aes(x = Climate_Season, 
                                                             y = Value, 
                                                             group = line_group, 
                                                             shape = diel, 
                                                             color = diel_climate, 
                                                             fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 22) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y =  "hr/yr", 
       x = NULL) + 
  scale_y_continuous(limits = c(-250, 420), breaks = seq(-250, 400, by = 125)) +
  annotate("text", x = 3.5, y = 420, label = "Africa", 
           fontface = "bold", size = 10)+
  annotate("text", x = 1.5, y = 385, label = "winter", 
           color = "blue", fontface = "bold", size = 6)+
  annotate("text", x = 3.5, y = 385, label = "spring", 
           color = "#00A859", fontface = "bold", size = 6)+
  annotate("text", x = 5.5, y = 385, label = "summer", 
           color = "red", fontface = "bold", size = 6)+
  annotate("text", x = 1, y = 350, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 2, y = 350, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 3, y = 350, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 4, y = 350, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 5, y = 350, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 6, y = 350, label = "+4\u00B0C", size = 4)+
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
ACT_africa_season

# ACT plot:Australia SEASON
ACT_terra_season_Australia_dat<- ACT_change_season_terra_ACT_plot_dat %>%
  filter(Region == "Australia") %>% 
  mutate(diel_climate = interaction(diel, Climate_Scenario, season, sep = "_"),
         line_group = interaction(season, diel))
# Assuming ACT_terra_season_Australia_dat is your dataframe as described.
ACT_terra_season_Australia_dat <- ACT_terra_season_Australia_dat %>%
  mutate(Climate_Season = interaction(Climate_Scenario, season, sep = "_"))

# Now, factorize the Climate_Season column using the specified order
ACT_terra_season_Australia_dat <- ACT_terra_season_Australia_dat %>%
  mutate(Climate_Season = factor(Climate_Season, levels = climate_season_order))

# Then, specify the order you want for Climate_Season
climate_season_order <- c("future_2_winter", "future_4_winter",
                          "future_2_spring", "future_4_spring", 
                          "future_2_summer", "future_4_summer")

# Now plot with the ordered factor
ACT_australia_season <- ggplot(ACT_terra_season_Australia_dat, aes(x = Climate_Season, 
                                                                   y = Value, 
                                                                   group = line_group, 
                                                                   shape = diel, 
                                                                   color = diel_climate, 
                                                                   fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 25) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y =  NULL, 
       x = NULL) +
  scale_y_continuous(limits = c(-250, 420), breaks = seq(-250, 400, by = 125)) +
  annotate("text", x = 3.5, y = 420, label = "Australia", 
           fontface = "bold", size = 10)+
  annotate("text", x = 1.5, y = 385, label = "winter", 
           color = "blue", fontface = "bold", size = 6)+
  annotate("text", x = 3.5, y = 385, label = "spring", 
           color = "#00A859", fontface = "bold", size = 6)+
  annotate("text", x = 5.5, y = 385, label = "summer", 
           color = "red", fontface = "bold", size = 6)+
  annotate("text", x = 1, y = 350, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 2, y = 350, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 3, y = 350, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 4, y = 350, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 5, y = 350, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 6, y = 350, label = "+4\u00B0C", size = 4)+
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 


# Final plot
ACT_season_all <- plot_grid(ACT_africa_season, 
                            ACT_australia_season,
                            nrow = 1, ncol = 2)
final_example_activity <- plot_grid(ACT_change_yr_terra_plot,
                                    ACT_season_all,
                                    nrow = 2, ncol = 1,
                                    labels = c("A"),
                                    label_size = 24)

final_example_activity


############################################################
#################### #################### Mrate plot
####################
## data prep for Mrate and ACT plots
MR_change_yr_terra_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "M_Rate_J") %>% 
  group_by(Region, Climate_Scenario, diel) %>% 
  summarise(Value = mean(Value))
MRate_change_yr_terra_plot_dat <- MR_change_yr_terra_plot_dat %>%
  mutate(diel_climate = interaction(diel, Climate_Scenario, sep = "_"),
         line_group = interaction(Region, diel))

# Now plot with the updated dataframe
MRate_change_yr_terra_plot <- ggplot(MRate_change_yr_terra_plot_dat, 
                                     aes(x = Climate_Scenario, 
                                         y = Value, group = line_group, 
                                         color = diel_climate,
                                         fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2), color = "black") +
  geom_point(position = position_dodge(width = 0.2), size = 6, aes(shape = Region), 
             stroke = 1.5) +
  scale_shape_manual(values = c("Africa" = 22, 
                                "Australia" = 25)) +
  scale_color_manual(values = c("diurnal_future_2" = "#FF9999",
                                "nocturnal_future_2" = "grey80",
                                "diurnal_future_4" = "#CC0000",
                                "nocturnal_future_4" = "grey35"),
                     guide = FALSE) + # Disable the color legend
  scale_fill_manual(values = c("diurnal_future_2" = "#FF9999",
                               "nocturnal_future_2" = "grey80",
                               "diurnal_future_4" = "#CC0000",
                               "nocturnal_future_4" = "grey35"),
                    guide = FALSE) + # Disable the fill legend
  guides(shape = guide_legend(title = "Region")) + # Enable and title the shape legend
  theme_bw() +
  labs(y = "J/g/yr", 
       x = NULL) +
  scale_y_continuous(limits = c(0, 12)*1000, breaks = seq(0, 12, by = 2)*1000) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 12000, label = expression(Delta * "Metabolic Rate"), 
           fontface = "bold", size = 10.5)+
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
MRate_change_yr_terra_plot


####################
##### SEASON MRATE
## data prep for Mrate rate plots
MR_change_season_terra_plot_dat <- ACT_MR_Foraging_change_season_final %>% 
  filter(Test == "M_Rate_J") %>% 
  group_by(Region, Climate_Scenario, season, diel) %>% 
  summarise(Value = mean(Value))

##### Africa
MRate_terra_season_africa_dat<- MR_change_season_terra_plot_dat %>%
  filter(Region == "Africa") %>% 
  mutate(diel_climate = interaction(diel, Climate_Scenario, season, sep = "_"),
         line_group = interaction(season, diel))
# Assuming MRate_terra_season_africa_dat is your dataframe as described.
MRate_terra_season_africa_dat <- MRate_terra_season_africa_dat %>%
  mutate(Climate_Season = interaction(Climate_Scenario, season, sep = "_"))

# Now, factorize the Climate_Season column using the specified order
MRate_terra_season_africa_dat <- MRate_terra_season_africa_dat %>%
  mutate(Climate_Season = factor(Climate_Season, levels = climate_season_order))

# Now plot with the ordered factor
MRate_Africa_season <- ggplot(MRate_terra_season_africa_dat, aes(x = Climate_Season, 
                                                                 y = Value, 
                                                                 group = line_group, 
                                                                 shape = diel, 
                                                                 color = diel_climate, 
                                                                 fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 22) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y = "J/g/season", 
       x = NULL) + 
  scale_y_continuous(limits = c(-100, 4500), breaks = seq(-1000, 4000, by = 1000)) +
  annotate("text", x = 3.5, y = 4500, label = "Africa", 
           fontface = "bold", size = 10)+
  annotate("text", x = 1.5, y = 4200, label = "winter", 
           color = "blue", fontface = "bold", size = 6)+
  annotate("text", x = 3.5, y = 4200, label = "spring", 
           color = "#00A859", fontface = "bold", size = 6)+
  annotate("text", x = 5.5, y = 4200, label = "summer", 
           color = "red", fontface = "bold", size = 6)+
  annotate("text", x = 1, y = 4000, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 2, y = 4000, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 3, y = 4000, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 4, y = 4000, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 5, y = 4000, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 6, y = 4000, label = "+4\u00B0C", size = 4)+
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
MRate_Africa_season

##### Australia
MRate_terra_season_Australia_dat<- MR_change_season_terra_plot_dat %>%
  filter(Region == "Australia") %>% 
  mutate(diel_climate = interaction(diel, Climate_Scenario, season, sep = "_"),
         line_group = interaction(season, diel))
# Assuming MRate_terra_season_Australia_dat is your dataframe as described.
MRate_terra_season_Australia_dat <- MRate_terra_season_Australia_dat %>%
  mutate(Climate_Season = interaction(Climate_Scenario, season, sep = "_"))

# Now, factorize the Climate_Season column using the specified order
MRate_terra_season_Australia_dat <- MRate_terra_season_Australia_dat %>%
  mutate(Climate_Season = factor(Climate_Season, levels = climate_season_order))

# Now plot with the ordered factor
MRate_Australia_season <- ggplot(MRate_terra_season_Australia_dat, aes(x = Climate_Season, 
                                                                       y = Value, 
                                                                       group = line_group, 
                                                                       shape = diel, 
                                                                       color = diel_climate, 
                                                                       fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 25) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y = NULL, 
       x = NULL) + 
  scale_y_continuous(limits = c(-100, 4500), breaks = seq(-1000, 4000, by = 1000)) +
  annotate("text", x = 3.5, y = 4500, label = "Australia", 
           fontface = "bold", size = 10)+
  annotate("text", x = 1.5, y = 4200, label = "winter", 
           color = "blue", fontface = "bold", size = 6)+
  annotate("text", x = 3.5, y = 4200, label = "spring", 
           color = "#00A859", fontface = "bold", size = 6)+
  annotate("text", x = 5.5, y = 4200, label = "summer", 
           color = "red", fontface = "bold", size = 6)+
  annotate("text", x = 1, y = 4000, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 2, y = 4000, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 3, y = 4000, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 4, y = 4000, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 5, y = 4000, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 6, y = 4000, label = "+4\u00B0C", size = 4)+
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
MRate_Australia_season

# Final plot
MRate_season_all <- plot_grid(MRate_Africa_season, 
                              MRate_Australia_season,
                              nrow = 1, ncol = 2)

final_example_MRate <- plot_grid(MRate_change_yr_terra_plot,
                                 MRate_season_all,
                                 nrow = 2, ncol = 1, 
                                 labels = c("B"),
                                 label_size = 24)

final_example_MRate



####################   ####################   ####################   ####################   
#################### Forage Data
Forage_change_yr_terra_plot_dat <- ACT_MR_Forage_change_yr_final %>% 
  filter(Test == "Feeding_demand")%>% 
  group_by(Region, Climate_Scenario, diel) %>% 
  summarise(Value = mean(Value))



####################   ####################   ####################   ####################   
#################### Forage Plot
# Function to determine shape
# Add an interaction term for the line groups that includes Region
Foragechange_yr_terra_dat <- Forage_change_yr_terra_plot_dat %>%
  mutate(diel_climate = interaction(diel, Climate_Scenario, sep = "_"),
         line_group = interaction(Region, diel))

# Now plot with the updated dataframe
Foragechange_yr_terra_plot <- ggplot(Foragechange_yr_terra_dat, aes(x = Climate_Scenario, 
                                                                    y = Value, 
                                                                    group = line_group, 
                                                                    color = diel_climate,
                                                                    fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2), color = "black") +
  geom_point(position = position_dodge(width = 0.2), size = 6, aes(shape = Region), 
             stroke = 1.5) +
  scale_shape_manual(values = c("Africa" = 22, 
                                "Australia" = 25)) +
  scale_color_manual(values = c("diurnal_future_2" = "#FF9999",
                                "nocturnal_future_2" = "grey80",
                                "diurnal_future_4" = "#CC0000",
                                "nocturnal_future_4" = "grey35"),
                     guide = FALSE) + # Disable the color legend
  scale_fill_manual(values = c("diurnal_future_2" = "#FF9999",
                               "nocturnal_future_2" = "grey80",
                               "diurnal_future_4" = "#CC0000",
                               "nocturnal_future_4" = "grey35"),
                    guide = FALSE) + # Disable the fill legend
  guides(shape = guide_legend(title = "Region")) + # Enable and title the shape legend
  theme_bw() +
  labs(y = "J/g/hr", 
       color = "Species", 
       x = NULL) +
  scale_y_continuous(limits = c(-5.5, 3.5), breaks = seq(-5, 3, by = 1)) + # Customize the y-axis with subscript
  annotate("text", x = 1.5, y = 3.5, label = expression(Delta * "Feeding Demand"), 
           fontface = "bold", size = 10.5)+
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.spacing.y = unit(-.02, 'cm'),
        legend.box.spacing = unit(0, 'lines')) +
  theme(plot.margin = margin(t = 5, r = 2, b = 5, l = 5, unit = "mm")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
Foragechange_yr_terra_plot

############################################################################
###################################### FORAGE SEASON
## Foraging rate plots
Forage_change_season_terra_plot_dat <- ACT_MR_Foraging_change_season_final %>% 
  filter(Test == "Foraging")%>% 
  group_by(Region, Climate_Scenario, season, diel) %>% 
  summarise(Value = mean(Value))

# Foraging plot:AFRICA SEASON
Forage_terra_season_africa_dat<- Forage_change_season_terra_plot_dat %>%
  filter(Region == "Africa") %>% 
  mutate(diel_climate = interaction(diel, Climate_Scenario, season, sep = "_"),
         line_group = interaction(season, diel))
# Assuming Forage_terra_season_africa_dat is your dataframe as described.
Forage_terra_season_africa_dat <- Forage_terra_season_africa_dat %>%
  mutate(Climate_Season = interaction(Climate_Scenario, season, sep = "_"))

# Now, factorize the Climate_Season column using the specified order
Forage_terra_season_africa_dat <- Forage_terra_season_africa_dat %>%
  mutate(Climate_Season = factor(Climate_Season, levels = climate_season_order))

# Now plot with the ordered factor
Forage_Africa_season_1 <- ggplot(Forage_terra_season_africa_dat, aes(x = Climate_Season, 
                                                                     y = Value, 
                                                                     group = line_group, 
                                                                     shape = diel, 
                                                                     color = diel_climate, 
                                                                     fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 22) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y =  NULL, 
       x = NULL) + 
  scale_y_continuous(limits = c(-0.4, 6.2), breaks = seq(0, 5, by = 1)) +
  annotate("text", x = 3.5, y = 6.1, label = "Africa", 
           fontface = "bold", size = 10)+
  annotate("text", x = 1.5, y = 5.45, label = "winter", 
           color = "blue", fontface = "bold", size = 6)+
  annotate("text", x = 3.5, y = 5.45, label = "spring", 
           color = "#00A859", fontface = "bold", size = 6)+
  annotate("text", x = 5.5, y = 5.45, label = "summer", 
           color = "red", fontface = "bold", size = 6)+
  annotate("text", x = 1, y = 5, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 2, y = 5, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 3, y = 5, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 4, y = 5, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 5, y = 5, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 6, y = 5, label = "+4\u00B0C", size = 4)+
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 0, b = 1, l = 12, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
Forage_Africa_season_1


Forage_Africa_season_2 <- ggplot(Forage_terra_season_africa_dat, aes(x = Climate_Season, 
                                                                     y = Value, 
                                                                     group = line_group, 
                                                                     shape = diel, 
                                                                     color = diel_climate, 
                                                                     fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 22) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y =  NULL, 
       x = NULL) + 
  scale_y_continuous(limits = c(-24, -14), breaks = seq(-24, -14, by = 5)) +
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 2, r = 0, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
Forage_Africa_season_2


# Final
TERRA_Foraging_africa <- plot_grid(
  Forage_Africa_season_1, 
  Forage_Africa_season_2,
  nrow = 2,  # Specify two rows
  rel_heights = c(2, 1)  # First plot has double the height of the second
) 

# adding y axis
Final_season_terra_africa <- ggdraw() +
  draw_plot(TERRA_Foraging_africa) +
  draw_label("J/g/h", 
             x = 0, 
             y = 0.6, 
             hjust = 1, 
             vjust = 1.5,
             angle = 90, # Rotate the label for y-axis
             size = 22) # Adjust size according to your preference
Final_season_terra_africa


# feeding plot:australia SEASON
Forage_terra_season_australia_dat<- Forage_change_season_terra_plot_dat %>%
  filter(Region == "Australia") %>% 
  mutate(diel_climate = interaction(diel, Climate_Scenario, season, sep = "_"),
         line_group = interaction(season, diel))
# Assuming Forage_terra_season_australia_dat is your dataframe as described.
Forage_terra_season_australia_dat <- Forage_terra_season_australia_dat %>%
  mutate(Climate_Season = interaction(Climate_Scenario, season, sep = "_"))

# Now, factorize the Climate_Season column using the specified order
Forage_terra_season_australia_dat <- Forage_terra_season_australia_dat %>%
  mutate(Climate_Season = factor(Climate_Season, levels = climate_season_order))

# Now plot with the ordered factor
Forage_australia_season_1 <- ggplot(Forage_terra_season_australia_dat, aes(x = Climate_Season, 
                                                                     y = Value, 
                                                                     group = line_group, 
                                                                     shape = diel, 
                                                                     color = diel_climate, 
                                                                     fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 22) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y =  NULL, 
       x = NULL) + 
  scale_y_continuous(limits = c(-0.4, 6.2), breaks = seq(0, 5, by = 1)) +
  annotate("text", x = 3.5, y = 6.1, label = "Australia", 
           fontface = "bold", size = 10)+
  annotate("text", x = 1.5, y = 5.45, label = "winter", 
           color = "blue", fontface = "bold", size = 6)+
  annotate("text", x = 3.5, y = 5.45, label = "spring", 
           color = "#00A859", fontface = "bold", size = 6)+
  annotate("text", x = 5.5, y = 5.45, label = "summer", 
           color = "red", fontface = "bold", size = 6)+
  annotate("text", x = 1, y = 5, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 2, y = 5, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 3, y = 5, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 4, y = 5, label = "+4\u00B0C", size = 4)+
  annotate("text", x = 5, y = 5, label = "+2\u00B0C", size = 4)+
  annotate("text", x = 6, y = 5, label = "+4\u00B0C", size = 4)+
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 5, r = 0, b = 1, l = 12, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
Forage_australia_season_1


Forage_australia_season_2 <- ggplot(Forage_terra_season_australia_dat, aes(x = Climate_Season, 
                                                                     y = Value, 
                                                                     group = line_group, 
                                                                     shape = diel, 
                                                                     color = diel_climate, 
                                                                     fill = diel_climate)) +
  geom_line(aes(group = line_group), position = position_dodge(width = 0.2),
            color = "black") +
  geom_point(size = 6, position = position_dodge(width = 0.2), 
             stroke = 1.5, shape = 22) +
  scale_shape_manual(values = c(diurnal = 21, nocturnal = 21)) +
  scale_color_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                                "nocturnal_future_2_winter" = "grey80",
                                "diurnal_future_2_spring" = "#FF9999",
                                "nocturnal_future_2_spring" = "grey80" ,
                                "diurnal_future_2_summer" = "#FF9999",
                                "nocturnal_future_2_summer" = "grey80",
                                "diurnal_future_4_winter" = "#CC0000",
                                "nocturnal_future_4_spring" = "grey35",
                                "diurnal_future_4_spring" = "#CC0000",
                                "nocturnal_future_4_winter" = "grey35",
                                "diurnal_future_4_summer" = "#CC0000",
                                "nocturnal_future_4_summer" = "grey35"))+
  scale_fill_manual(values = c("diurnal_future_2_winter" = "#FF9999",
                               "nocturnal_future_2_winter" = "grey80",
                               "diurnal_future_2_spring" = "#FF9999",
                               "nocturnal_future_2_spring" = "grey80" ,
                               "diurnal_future_2_summer" = "#FF9999",
                               "nocturnal_future_2_summer" = "grey80",
                               "diurnal_future_4_winter" = "#CC0000",
                               "nocturnal_future_4_spring" = "grey35",
                               "diurnal_future_4_spring" = "#CC0000",
                               "nocturnal_future_4_winter" = "grey35",
                               "diurnal_future_4_summer" = "#CC0000",
                               "nocturnal_future_4_summer" = "grey35"))+
  theme_bw()+ 
  labs(y =  NULL, 
       x = NULL) + 
  scale_y_continuous(limits = c(-24, -14), breaks = seq(-24, -14, by = 5)) +
  theme(legend.position = "none",  # Hide the legend
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        legend.direction = "vertical", 
        legend.background = element_rect(fill = NA),
        legend.key = element_blank(),
        legend.spacing.y = unit(-.02, 'cm'), # Adjust the spacing between the legends
        legend.box.spacing = unit(0, 'lines')) + # Adjust the spacing between legend boxes
  theme(plot.margin = margin(t = 2, r = 0, b = 5, l = 5, unit = "mm"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
Forage_australia_season_2


# Final
Final_season_terra_australia <- plot_grid(
  Forage_australia_season_1, 
  Forage_australia_season_2,
  nrow = 2,  # Specify two rows
  rel_heights = c(2, 1)  # First plot has double the height of the second
) 




# Final plot
Forage_season_all <- plot_grid(Final_season_terra_africa, 
                               Final_season_terra_australia,
                               nrow = 1, ncol = 2)

final_example_Forage <- plot_grid(Foragechange_yr_terra_plot,
                                  Forage_season_all,
                                  nrow = 2, ncol = 1, 
                                  labels = c("C"),
                                  label_size = 24)

final_example_Forage




#########
# Figure 5
# save: 2200 X 1200 png or eps
#22x14 PDF
Figure_5 <-  plot_grid(final_example_activity,
                    final_example_MRate,
                    final_example_Forage,
                    nrow = 1, ncol = 3)
Figure_5

ggsave("../../Pianka_figs/Figure_5.pdf", Figure_5, device = "pdf", width = 25, height =16)


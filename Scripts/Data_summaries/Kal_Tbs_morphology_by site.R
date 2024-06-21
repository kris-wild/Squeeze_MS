# packages and data
pacman::p_load("dplyr", "ggplot2", "tidyr", "lubridate", "purrr")


################## Tb data
# import data and rename
Tb_data <- read.csv(file = "Raw_data/kalahari.csv") %>% 
  mutate(Genus.species = gsub(" ", "_", species)) %>% 
  arrange(Genus.species) %>%
  rename(Genus_species = Genus.species,
         BT = bt,
         AT = at,
         Site = site) %>% 
  mutate(dmy = dmy(date),
         year = year(dmy),
         year_cat = as.factor(year)) %>%
  filter(!is.na(BT)) %>% 
  filter(Genus_species == "Chondrodactylus_angulifer" |
           Genus_species == "Trachylepis_sparsa"|
           Genus_species == "Agama_aculeata" |
           Genus_species == "Meroles_suborbitalis"|
           Genus_species == "Pedioplanis_lineoocellata")

# overall TB site summary
Tb_Site_Summary <- Tb_data %>% 
  mutate(year = as.numeric(as.character(year_cat))) %>%
  group_by(Genus_species, Site, year) %>% 
  count()%>%
  rename(site_n= n) %>% 
  arrange(Genus_species, site_n, Site)

# getting year range for each species by site for  Genus_species 
Tb_Site_range <- Tb_data %>% 
  mutate(year = as.numeric(as.character(year_cat))) 

# sample size and year range for TB
year_range_data_bysite <- Tb_Site_range %>%
  group_by(Genus_species, Site) %>%
  summarize(year_range = paste(min(year), 
                               max(year), sep = " - "),
            Tb_sample_size = n()) %>% 
  arrange(desc(Tb_sample_size))

# Calculating max and mins BT's and AT's in the field
Tb_data_min_max <- Tb_data %>% 
  group_by(Genus_species) %>%
  summarise(min_BT = min(BT, na.rm = TRUE),
            mean_BT = mean(BT, na.rm = TRUE),
            max_BT = max(BT, na.rm = TRUE),
            min_AT = min(AT, na.rm = TRUE),
            max_AT = max(AT, na.rm = TRUE)) %>% 
  mutate(T_bask = min_BT -2,
         T_f_min = min_BT,
         T_f_max = max_BT)
Tb_data_min_max
check <- Tb_data %>% filter(Genus_species == "Pedioplanis_lineoocellata")
hist(check$BT)
table(check$BT)
################## Basic morphology: SVL MASS
Morph <- read.csv(file = "Raw_data/kalahari.csv") %>% 
  mutate(Genus_species = gsub(" ", "_", species)) %>% 
  dplyr::select(number, site, long,lat, Genus_species, 
                sex, date, time_orig, wt, svl) %>% 
  rename(Mass_g = wt,
         SVL_mm = svl) %>% 
  filter(Genus_species == "Chondrodactylus_angulifer" |
           Genus_species == "Trachylepis_sparsa"|
           Genus_species == "Agama_aculeata" |
           Genus_species == "Meroles_suborbitalis"|
           Genus_species == "Pedioplanis_lineoocellata")
Morph$Date <- dmy(Morph$date) #arrange dates
Morph$Year <- year(Morph$Date)#arrange dates
table(Morph$Year) 

# average mass for each species- used for "Aus_Thermal_beh_traits"
Morph_sum <- Morph %>%
  group_by(Genus_species) %>% 
  summarise(
    mean_Mass_g = mean(Mass_g, na.rm = TRUE),
    n_Mass_g = sum(!is.na(Mass_g)),
    mean_SVL_mm = mean(SVL_mm, na.rm = TRUE),
    n_SVL_mm = sum(!is.na(SVL_mm)))






sparsa <- Tb_data %>% filter(Genus_species == "Trachylepis_sparsa")



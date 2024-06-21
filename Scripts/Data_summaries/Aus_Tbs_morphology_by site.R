# Tb's and morph by site for ecotherm models for 5 species
# Ctenophorus_isolepis; Gehyra_variegata
# Ctenotus_quatt; Moloch_horridus; Pogona_minor

# packages and data
pacman::p_load("dplyr", "ggplot2", "tidyr", "lubridate", "purrr")
Tb_data <- read.csv(file = "Raw_data/OZ_TaTb_Clean.csv") %>% 
  arrange(Genus.species) %>%
  rename(Genus_species = Genus.species,
         BT = bt,
         AT = at,
         Site = site) %>% 
  mutate(dmy = dmy(date),
         year = year(dmy),
         year_cat = as.factor(year)) %>%
  filter(!is.na(BT)) %>% 
  filter(Genus_species == "Ctenotus quatt" |
           Genus_species == "Gehyra variegata"|
           Genus_species == "Moloch horridus" |
           Genus_species == "Pogona minor"|
           Genus_species == "Ctenophorus isolepis")

# overall site summary
Tb_Site_Summary <- Tb_data %>% 
  mutate(year = as.numeric(as.character(year_cat))) %>%
  group_by(Genus_species, Site, year) %>% 
  count()%>%
  rename(site_n= n) %>% 
  arrange(Genus_species, site_n, Site)

# picking top year by site
Top_Tb_Year_Site <- Tb_Site_Summary %>% 
  group_by(Genus_species) %>%
  top_n(4, site_n) 


# getting year range for each species by site for  Genus_species 
Tb_Site_range <- Tb_data %>% 
  mutate(year = as.numeric(as.character(year_cat))) 

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
check <- Tb_data %>% filter(Genus_species == "Pogona minor")
hist(check$BT)
table(check$BT)

################## Basic morphology: SVL MASS
Morph <- read.csv(file = "Raw_data/OZ-All.csv") %>% 
  filter(!is.na(Number)) %>% 
  filter(!is.na(Area)) %>% 
  filter(!is.na(SVL)) %>% 
  filter(!is.na(Wt)) %>% 
  dplyr::select(Number, Area, Date,Genus, Genus_species, 
                Sex, Age_est, SVL, Wt, Pit) %>% 
  rename(Mass_g = Wt) %>% 
  filter(Genus_species == "Ctenotus_quatt" |
           Genus_species == "Gehyra_variegata"|
           Genus_species == "Moloch_horridus" |
           Genus_species == "Pogona_minor"|
           Genus_species == "Ctenophorus_isolepis")
Morph$Date <- dmy(Morph$Date) #arrange dates
Morph$Year <- year(Morph$Date)#arrange dates
hist(Morph$Year) 

# average mass for each species- used for "Aus_Thermal_beh_traits"
mean_mass <- Morph %>%
  group_by(Genus_species) %>% 
  summarise(mass_g = mean(Mass_g), 
            svl_mm = mean(SVL))
  
  


#stomach vol 
guts <- read.csv(file = "Raw_data/OZ-All.csv") %>% 
  filter(Genus_species == "Ctenotus_quatt" |
           Genus_species == "Gehyra_variegata"|
           Genus_species == "Moloch_horridus" |
           Genus_species == "Pogona_minor"|
           Genus_species == "Ctenophorus_isolepis") %>% 
  filter(!is.na(TOTVOL)) %>% # this is the am
  group_by(Genus_species) %>% 
  summarise(mean_tot_vol = mean(TotTVol, na.rm = TRUE))
View(vol)


# food names
c("ParasitesP.N", 	"CentipedesN", 	"CentipedesV", 	"AranaeN", 	"AranaeV", 	"ScorpionN", 	"ScorpionV", 	"AntsN", 	"AntsV", 	"WaspN", 	"WaspV", 	"OrthoptN", 	"OrthoptV", 	"ThysanuraN", 	"ThysanuraV", 	"BlattariaN", 	"BlattariaV", 	"PhasmidN", 	"PhasmidV", 	"ColeopteraN", 	"ColeopteraV", 	"IsopteraWN", 	"IsopteraSN", 	"IsopteraRN", 	"IsopteraN", 	"IsopteraV", 	"HomopteraN", 	"HomopteraV", 	"HemipteraN", 	"HemipteraV", 	"DermapteraN", 	"DermapteraV", 	"DipteraN", 	"DipteraV", 	"LepidoptN", 	"LepidoptV", 	"EggsN", 	"EggsV", 	"LarvaeN", 	"LarvaeV", 	"OtherInsN", 	"OtherInsV", 	"NematodesN", 	"NematodesV", 	"VertebratesN", 	"VertebratesV", 	"VegN", 	"VegV", 	"UnIDN", 	"UnIDV")

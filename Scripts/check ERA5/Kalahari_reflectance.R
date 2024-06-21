library(readxl)
data_2017 <- read_excel('data/Reflectance_lizards_kalahari_RAY_MIKE.xlsx', sheet = 'lizards_2017')
data_2019 <- read_excel('data/Reflectance_lizards_kalahari_RAY_MIKE.xlsx', sheet = 'lizards_2019')
data_substrate <- read_excel('data/Reflectance_lizards_kalahari_RAY_MIKE.xlsx', sheet = 'sand_2017_2019')

substrate_agg <- aggregate(1 - data_substrate$reflectance, by = list(data_substrate$Location), FUN = 'mean')
colnames(substrate_agg) <- c('site', 'absorptivity')
substrate_agg
mean(substrate_agg$absorptivity)

data_all <- as.data.frame(rbind(as.matrix(data_2017), as.matrix(data_2019)))
data_all$region2 <- 'dorsal'
data_all$region2[substr(data_all$region, 1, 6) %in% c('ventra', 'Ventral')] <- 'ventral'
data_all$region2[substr(data_all$region, 1, 6) %in% c('top he', 'throat', 'green ')] <- 'other'
data_dorsvent <- subset(data_all, region2 != 'other')
data_dorsvent <- subset(data_dorsvent, 1-as.numeric(data_dorsvent$reflectance) > 0)
data_agg_mean <- aggregate(1 - as.numeric(data_dorsvent$reflectance) ~ data_dorsvent$Species * data_dorsvent$region2, FUN = 'mean')
data_agg_max <- aggregate(1 - as.numeric(data_dorsvent$reflectance) ~ data_dorsvent$Species * data_dorsvent$region2, FUN = 'max')
data_agg_min <- aggregate(1 - as.numeric(data_dorsvent$reflectance) ~ data_dorsvent$Species * data_dorsvent$region2, FUN = 'min')
data_agg_SD <- aggregate(1 - as.numeric(data_dorsvent$reflectance) ~ data_dorsvent$Species * data_dorsvent$region2, FUN = 'sd')
data_agg_n <- aggregate(1 - as.numeric(data_dorsvent$reflectance) ~ data_dorsvent$Species * data_dorsvent$region2, FUN = 'length')
data_agg <- cbind(data_agg_mean, data_agg_min$`1 - as.numeric(data_dorsvent$reflectance)`, data_agg_max$`1 - as.numeric(data_dorsvent$reflectance)`, data_agg_SD$`1 - as.numeric(data_dorsvent$reflectance)`, data_agg_n$`1 - as.numeric(data_dorsvent$reflectance)`) 
colnames(data_agg) <- c('species', 'region', 'abs_mean', 'abs_min', 'abs_max', 'sdev', 'n')
data_agg

write.csv(data_agg, file = 'output/kalhari_absorptivity.csv')

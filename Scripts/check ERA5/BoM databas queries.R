library(RMySQL)
host <- "115.146.93.180"
uid <- 'general'
pwd <- 'predecol'
channel <- RMySQL::dbConnect(MySQL(), user = uid, password = pwd, host = host, dbname = "BOM", port = 3306)
tables <- dbListTables(channel)
print(tables)


query<- "SELECT b.Station FROM All_Stations as b"
query<-paste0("SELECT a.latitude, a.longitude, a.Station, a.Name FROM All_Stations as a")
stationlist <- dbGetQuery(channel,query)
station2 <- subset(stationlist, latitude >= -29 & latitude <= -27 & longitude >= 119 & longitude <= 124)
# 
# latitude longitude Station           Name
# 146 -28.9700   119.569   12022 CASHMERE DOWNS
# 148 -28.6306   122.407   12045       LAVERTON
# 149 -28.8836   121.330   12046        LEONORA
# 154 -27.2842   120.093   12090      YEELIRRIE
# 155 -28.1664   123.656   12219        YAMARNA
station <- stationlist$Station[which(stationlist$Name == "PERTH REGIONAL OFFICE")]
station <- 12046
ystart <- 2010
yfinish <- 2010
nyears <- yfinish - ystart + 1
query<-paste0("SELECT 
        a.latitude, a.longitude, a.Station, a.Name, b.Year, b.Month, b.Day, b.Rainfall, b.Tmin, b.Tmax
             FROM All_Stations as a
             INNER JOIN All_soil as b
             ON (a.Station = b.Station) WHERE (a.Station='",station,"') and (b.Year between ",ystart," and ",ystart+nyears-1,")")
weather <- dbGetQuery(channel,query)
plot(weather$, type = 'l')

  
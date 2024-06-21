library(tidyverse)
library(lubridate)
library(here)
source('scripts/Functions/get_tz.R')
loc <- c(21.2670, -28.4)

# upington_RSA <- read.csv(here::here("Scripts, check ERA5"), upington_weather.csv"))

# upington_RSA <- read.csv("Scripts/check ERA5/upington_weather.csv")
upington_RSA <- read.csv("Scripts/check ERA5/upington_weather.csv")


# upington_RSA$date <- upington_RSA$DateT |>
#   mutate(date = ymd(DateT))
upington_RSA$date <- get_tz(as.POSIXct(upington_RSA$DateT), loc)
#upington_RSA$date <- get_tz(upington_RSA$date, loc = c(21.7, -28.4))

upington_RSA69_73 <- upington_RSA |>
  filter(date < "1974-01-01")
#
upington_ERA5 <- read_csv("Scripts/check ERA5/hourlydataUpington.csv")
upington_ERA5$date <-  get_tz(upington_ERA5$obs_time, loc)

# get max min by date for ERA5
# 
upington_ERA5_day <- upington_ERA5 |>
  mutate(date = as.Date(date)) |>
  group_by(date) |>
  summarise(Tmax = max(temperature),
            Tmin = min(temperature))

max <- aggregate(upington_ERA5$temperature, by = list(format(upington_ERA5$date, "%Y-%m-%d")), FUN = max)
min <- aggregate(upington_ERA5$temperature, by = list(format(upington_ERA5$date, "%Y-%m-%d")), FUN = min)
upington_ERA5_day <- cbind(min, max$x)
colnames(upington_ERA5_day) <- c('date', 'Tmin', 'Tmax')
upington_ERA5_day$date <- get_tz(as.POSIXct(upington_ERA5_day$date), loc)

upington_RSA_ERA <- full_join(
  upington_RSA69_73, upington_ERA5_day, by = "date")

# upington_ERA5$date <- as.Date(upington_ERA5$obs_time)
# max date at present "1973-12-31" 

upington_RSA_ERA$year <- year(upington_RSA_ERA$date)

ggplot(upington_RSA_ERA, aes(Tmax, Mx)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, 
              intercept = 0, col= "red")  +
  xlim(-5, 45) +
  ylim(-5, 45) +
  facet_wrap ( ~ year)


ggplot(upington_RSA_ERA, aes(Tmin, Mn)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, 
              intercept = 0, col= "red")  +
  xlim(-5, 45) +
ylim(-5, 45) +
  facet_wrap ( ~ year)


RMS <- function(num) sqrt(sum(num^2)/length(num))

minRMS <- (upington_RSA_ERA$Tmin- upington_RSA_ERA$Mn)
minRMS<- minRMS[complete.cases(minRMS)]
RMS(minRMS)


maxRMS <- (upington_RSA_ERA$Tmax- upington_RSA_ERA$Mx)
maxRMS <- maxRMS[complete.cases(maxRMS)]
RMS(maxRMS)

# # set up year vector
# upington_RSA_ERA$year <-
#   year(upington_RSA_ERA$date)
# upington_RSA_ERA$plus1 <- upington_RSA_ERA$plus1 
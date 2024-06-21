library(NicheMapR)
library(ecmwfr)
library(mcera5)
library(lubridate)
library(dplyr)
library(tidync)
setwd("~/Desktop/")
uid <- "233381"
ecmwfr::wf_set_key(user = uid, key = cds_api_key, service = "cds")

# KALAHARI bounding coordinates (in WGS84)
xmn <- 19.75
xmx <- 22.5
ymn <- -28.50
ymx <- -25.50

# temporal extent
st_time <- lubridate::ymd("2016:01:01")
en_time <- lubridate::ymd("2020:12:31")

file_prefix <- "ERA5"
op <- "Era5/"

# build a request (covering multiple years)
req <- build_era5_request(xmin = xmn, xmax = xmx,
                          ymin = ymn, ymax = ymx,
                          start_time = st_time,
                          end_time = en_time,
                          outfile_name = file_prefix)
str(req)
request_era5(request = req, uid = uid, out_path = op)



################################################################
###################### AUSTRALIA  SITES ###################### 
################################################################
library(NicheMapR)
library(ecmwfr)
library(mcera5)
library(lubridate)
library(dplyr)
library(tidync)
setwd("~/Desktop/")
uid <- "233381"
cds_api_key <- "b92ba7e9-f5db-48ce-bce7-62100e55b4a4"
ecmwfr::wf_set_key(user = uid, key = cds_api_key, service = "cds")

# AUSTRALIA Bounding coordinates (in WGS84)
xmn <- 118
xmx <- 125
ymn <- -29
ymx <- -25

# temporal extent 80-2021
st_time <- lubridate::ymd("2018:01:01")
en_time <- lubridate::ymd("2021:12:31")


file_prefix <- "ERA5"
op <- "Era5/"

# build a request (covering multiple years)
req <- build_era5_request(xmin = xmn, xmax = xmx,
                          ymin = ymn, ymax = ymx,
                          start_time = st_time,
                          end_time = en_time,
                          outfile_name = file_prefix)
str(req)
request_era5(request = req, uid = uid, out_path = op)

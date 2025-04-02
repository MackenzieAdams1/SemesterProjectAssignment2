# LOADING PACKAGES

#remotes::install_github("https://github.com/ropensci/rnoaa.git")
#install.packages("daymetr")

library(dataRetrieval)
library(tidyverse)
library(dplyr)
library(patchwork)
library(rnoaa) #add code make it reproduce
library(zoo)
library(daymetr)
library(leaflet)


# USGS GAGE INFO/DATA 

# With the "help" of Poole (he wrote it), this has been rewritten such that all the stream 
# data is in one df.
# Can talk through the implications of this later. But is very nice

# This efficiently automates the process of getting all the stream data desired into one df.
# This is allowed to be a df w/ minimal info bc it has to be user defined and im not gonna go 
# thru the effort of developing a "frontend" for this

gage_ids <- 
  tribble(
    ~site_name,        ~site_no,
    "Dungeness_Gage",  "12048000",
    "Elwha_Gage",      "12045500",
    "Skokomish_Gage",  "12061500")

# set all the parameters
start <- date("1987-10-01")
end <- date("2023-9-30")
para <-c("00060", "00065")

# if sites in gage_ids are changed, uncomment the code below and run it once. This stores the 
# data locally and eventually in the repo if its pushed. This is mostly to be polite to the  
# USGS hosting service.

# NWIS_data <- readNWISdv(gage_ids$site_no, para, start, end)
# saveRDS(NWIS_data, "NWIS_data.rdata")

# NWIS_meta <- readNWISsite(gage_ids$site_no)
# saveRDS(NWIS_meta, "NWIS_meta.rdata")

gage_ids <- 
  readRDS("NWIS_meta.rdata") %>% 
  select(site_no,
         dec_lat_va, 
         dec_long_va) %>%
  inner_join(gage_ids, .)

gage_data <-
  readRDS("NWIS_data.rdata") %>%
  inner_join(gage_ids, .) %>%
  renameNWISColumns() %>%
  group_by(site_name, site_no)


# Shows all unique grouped elements in gage_data. Verifies I got all the desired gages
summarize(gage_data)

# WEATHER DATA ACQUISITION 

# automate downloading climate data

# write_csv(x = select(gage_ids, -"site_no"), file = "gage_ids.csv")

# daymet_all <- download_daymet_batch(file_location = "./gage_ids.csv", start = year(start), end = year(end), internal = T, simplify = F)
# saveRDS(daymet_all, "DAYMET_all.rdata")


# Downloading data works. Now to append it...

daymet_unfucker_hum <-
  function(f) {
    bind_cols(data = f$data, site = f$site, .name_repair = "unique")
  } 

gage_data_all <- 
  lapply(readRDS("./DAYMET_all.rdata"), daymet_unfucker_hum) %>%
  bind_rows() %>%
  rename(doy = yday, 
         ann = year,
         prcp_daily_mm = prcp..mm.day.,
         t_max_C = tmax..deg.c.,
         t_min_C = tmin..deg.c.) %>%
  mutate(
    Date = make_datetime(year = ann, day = doy),
    t_avg_C = (t_max_C - t_min_C)/2) %>%
  select(site,
         Date,
         prcp_daily_mm,
         t_max_C,
         t_min_C,
         t_avg_C
  ) %>%
  left_join(ungroup(gage_data), ., join_by(site_name == site, Date == Date))

# ADD FISH DATA

temp <- read.csv("./Dungeness Salmon Data.csv")

temp <- 
  temp %>%
  filter(data_type == "TSAEJ")

temp_fish_all <-
  lapply("./Salmon_Data", read_csv) %>%
  bind_rows()










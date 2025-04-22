# LOADING PACKAGES

library(dataRetrieval)
library(tidyverse)
library(dplyr)
library(patchwork)
library(zoo)
library(daymetr) 
library(leaflet) 
library(fs)
library(ggpubr) # for adding R and P vals to graphs
library(DescTools) # for doing Mode statistical calc

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
    "Dungeness_River",  "12048000",
    "Elwha_River",      "12045500",
    "Skokomish_River",  "12061500")

# set all the parameters
start <- date("1987-10-01")
end <- date("2023-9-30")
para <- c("00060", "00065")

# if sites in gage_ids are changed, uncomment the code below and run it once. This stores the 
# data locally and eventually in the repo if its pushed. This is mostly to be polite to the  
# USGS hosting service.

# NWIS_data <- readNWISdv(gage_ids$site_no, para, start, end)
# saveRDS(NWIS_data, "NWIS_data.rdata")
# 
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
  drop_na(Flow) %>%
  group_by(site_name, site_no)


# Shows all unique grouped elements in gage_data. Verifies I got all the desired gages
summarize(gage_data)

# WEATHER DATA ACQUISITION 

# automate downloading climate data

# write_csv(x = select(gage_ids, -"site_no"), file = "gage_ids.csv")
# 
# daymet_all <- download_daymet_batch(file_location = "./gage_ids.csv", start = year(start), end = year(end), internal = T, simplify = F)
# saveRDS(daymet_all, "DAYMET_all.rdata")


# Downloading data works. Now to append it...

daymet_reorg <-
  function(f) {
    bind_cols(data = f$data, site = f$site, .name_repair = "unique")
  } 

gage_data_all <- 
  lapply(readRDS("./DAYMET_all.rdata"), daymet_reorg) %>%
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
         t_avg_C,
         ann
  ) %>%
  left_join(ungroup(gage_data), ., join_by(site_name == site, Date == Date)) %>%
  drop_na(t_min_C)

# ADD FISH DATA

# provide fish data directory. Maybe this can be automated, maybe not. If theirs not a package for it im not writing it tho.
fish_files <-
  dir_ls("./Salmon_Data/")

gage_fish_all <-
  fish_files %>%
  lapply(read_csv) %>%
  bind_rows()%>%
  filter(data_type == "TSAEJ") %>%
  mutate(site_name = paste0(str_extract(population_name, "^\\w+"), "_River"), .before = "stock_number") %>% #this line is can break
  select(abundance_qty,                                                                                     #if gage_ids changed. 
         year,                                                                                              #maybe should be fixed...
         site_name) %>%
  left_join(gage_data_all,., by = c(
    "ann" = "year",
    "site_name" = "site_name"
  )) %>%
  drop_na(abundance_qty)

# REVEL IN ITS GLORY. FISH SCRIPT REIGNS SUPREME

write.csv(slice(gage_fish_all, c(1:4)), "all_data_preview.csv", )


gage.labs <- c("Dungeness Stream", "Elwha Stream", "Skokomish Stream")

# Plot temperature vs salmon returns
temp_v_returns <-
  gage_fish_all %>%
  group_by(site_name, ann) %>%
  summarise(ann_temp = mean(t_avg_C), salmon_returns = mean(abundance_qty)) %>%
  ggplot(aes(x = ann_temp, y = salmon_returns, color = site_name)) +
  geom_point() +
  facet_wrap(~ gsub("_", " ", site_name), nrow = 3, scale = "free") + #Modify facet labels
  stat_smooth(method = "lm", se = F) +
  stat_regline_equation(label.x.npc = "middle", label.y.npc = "top") + #add regression line
  stat_cor(label.x.npc = "left", label.y.npc = "top") + #add p-val
  labs(y = "Salmon Returns" , x = "Avg Temperature (C)") +
  theme(legend.position = "none") #Hide legend


ggsave("temp_v_returns.png", temp_v_returns, width = 10, height = 5, dpi = "retina")  


gage_fish_all %>%
  ggplot(aes(x = Date, y = Flow)) +
  geom_line() +
  geom_line(aes(x = Date, y = prcp_daily_mm)) 
# +
#   facet_wrap(facets = "site_name", nrow = 3, scales = "free")
  




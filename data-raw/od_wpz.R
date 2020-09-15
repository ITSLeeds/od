# This script creates the od_data_wpz, _oacents and od_data_df2 datasets

library(sf)
library(tidyverse)
set.seed(2020)

# get centroid data from https://geoportal.statistics.gov.uk/

u_wpz = "https://opendata.arcgis.com/datasets/176661b9403a4c84ae6aedf8bb4127cf_0.kml?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
download.file("https://opendata.arcgis.com/datasets/176661b9403a4c84ae6aedf8bb4127cf_0.kml", "~/hd/data/raw/geo/wpz.kml")
wpz_uk_centroids = sf::read_sf("~/hd/data/raw/geo/wpz.kml")
nrow(wpz_uk_centroids)
# [1] 53578
oa_centroids = sf::read_sf("~/hd/data/uk/centroids/OA_2011_EW_PWC.shp")
oa_2011_ew_pwc_wgs_4326 = sf::st_transform(oa_centroids, 4326)
?od_data_csa_zones
od_data_destinations = wpz_uk_centroids[od_data_zones, ]
nrow(od_data_destinations) # 766
od_data_centroids2 = oa_2011_ew_pwc_wgs_4326[od_data_zones, ]
nrow(od_data_centroids2) # 2523
od_data_centroids2 = od_data_centroids2 %>%
  sample_n(size = 20)
od_data_destinations = od_data_destinations %>%
  select(wz11cd) %>%
  sample_n(10)

# get OD data from here: https://wicid.ukdataservice.ac.uk/
od_data_oa_wpz_all = readr::read_csv("~/hd/data/uk/wf01aew_oa_v1.csv", col_names = FALSE)
nrow(od_data_oa_wpz_all) / 1e6 # 16 million OD pairs
sum(od_data_oa_wpz_all$X3) # 21 million commutes
od_data_df2 = od_data_oa_wpz_all %>%
  rename(o = X1, d = X2, n = X3) %>%
  filter(o %in% od_data_centroids2$OA11CD) %>%
  filter(d %in% od_data_destinations$wz11cd)

usethis::use_data(od_data_df2)
usethis::use_data(od_data_centroids2)
usethis::use_data(od_data_destinations)

mapview::mapview(od_data_centroids2) +
  mapview::mapview(od_data_destinations, zcol = "red")

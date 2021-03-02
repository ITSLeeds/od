# Aim: test the od_disaggregate() function

library(tidyverse)
library(tmap)

u = "https://github.com/cyipt/actdev/raw/main/data-small/poundbury/desire-lines-many.geojson"
desire_lines_many = sf::read_sf(u)

zones_msoa_national = pct::get_pct(layer = "z", geography = "msoa", national = TRUE)
centroids_lsoa_national = pct::get_pct(layer = "c", national = TRUE)
zones_lsoa_national = pct::get_pct(layer = "z", national = TRUE)
zones_many = zones_msoa_national %>% filter(geo_code %in% desire_lines_many$geo_code2)
centroids_lsoa_many = centroids_lsoa_national[zones_many, ]
zones_lsoa_many = zones_lsoa_national %>% filter(geo_code %in% centroids_lsoa_many$geo_code)

qtm(zones_lsoa_many) +
  qtm(desire_lines_many)

desire_lines_many = od::od_disaggregate(od = desire_lines_many %>% select(geo_code1:all_base), z = zones_many, subzones = zones_lsoa_many)

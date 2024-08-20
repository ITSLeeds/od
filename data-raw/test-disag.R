# Aim: test the od_disaggregate() function

library(tidyverse)
library(tmap)

# working example


od = od_data_df[1:2, ]
zones = od_data_zones_min
subzones = od_data_zones_small
od_disag = od_disaggregate(od, zones, subzones)
ncol(od_disag) - 1 == ncol(od) # same number of columns (except disag data gained geometry)
sum(od_disag[[3]]) == sum(od[[3]])
sum(od_disag[[4]]) == sum(od[[4]])
od_sf = od_to_sf(od, zones)
plot(od_data_zones_small$geometry)
plot(od_data_zones_min$geometry, lwd = 3, col = NULL, add = TRUE)
plot(od_sf["all"], add = TRUE)
plot(od_disag["all"], add = TRUE)
od$bus[1] = 0
# still works
od_disag = od_disaggregate(od, zones, subzones)

od = od_data_df[1:9, ]
od_disag = od_disaggregate(od, zones, subzones) # fails
# Error in if (nrow(od_new) > max_n_od) { :
#     missing value where TRUE/FALSE needed

u = "https://github.com/cyipt/actdev/raw/main/data-small/poundbury/desire-lines-many.geojson"
desire_lines_many = sf::read_sf(u)

zones_msoa_national = pct::get_pct(layer = "z", geography = "msoa", national = TRUE)
centroids_lsoa_national = pct::get_pct(layer = "c", national = TRUE)
zones_lsoa_national = pct::get_pct(layer = "z", national = TRUE, geography = "lsoa")
zones_many = zones_msoa_national %>% filter(geo_code %in% desire_lines_many$geo_code2)
centroids_lsoa_many = centroids_lsoa_national[zones_many, ]
zones_lsoa_many = zones_lsoa_national %>% filter(geo_code %in% centroids_lsoa_many$geo_code)

qtm(zones_many, borders.lwd = 3) +
  qtm(desire_lines_many) +
  tm_shape(zones_lsoa_many) + tm_borders("red", lty = 3) +
  qtm(centroids_lsoa_many)

od_test = desire_lines_many %>%
  select(geo_code1, geo_code2, all_base) %>%
  sf::st_drop_geometry()

# fails:
desire_lines_disag = od_disaggregate(od = od_test, z = zones_many, subzones = zones_lsoa_many)
# run with debug:
desire_lines_disag = od_disaggregate(od = od_test, z = zones_many, subzones = zones_lsoa_many)


# check od data codes match...
summary(zones_many$geo_code %in% od_test$geo_code2)
summary(zones_many$geo_code %in% od_test$geo_code1)

# remove missing line
od_test = desire_lines_many %>%
  select(geo_code1, geo_code2, all_base) %>%
  sf::st_drop_geometry() %>%
  slice(-nrow(od_test))

desire_lines_disag = od_disaggregate(od = od_test, z = zones_many, subzones = zones_lsoa_many)

plot(desire_lines_many)
sum(od_test$all_base) == sum(desire_lines_disag$all_base)

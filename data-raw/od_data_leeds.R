## code to prepare `od_data_df` dataset goes here

library(sf)
library(tidyverse)
library(stplanr)

od = pct::get_od()
centroids_ew_all = pct::get_centroids_ew() # todo: add centroids dataset based on this

od_data_df_all = od %>%
  filter(la_1 == "Leeds") %>%
  filter(la_2 == "Leeds") %>%
  filter(geo_code1 != geo_code2) %>%
  select(1:14)

od_data_df = od_data_df_all %>%
  select(geo_code1, geo_code2, all, train, bus, taxi, car_driver, car_passenger, bicycle, foot) %>%
  top_n(n = 6, bicycle) %>%
  as.data.frame(stringsAsFactors = FALSE)

od_data_df

usethis::use_data(od_data_df)

zones_leeds = ukboundaries::msoa2011_lds
zones_leeds_cents = st_centroid(zones_leeds)
zones_uk = ukboundaries::msoa2011_vsimple
zones_leeds_simple = zones_uk[zones_leeds_cents, ]
zones_leeds_simple = zones_leeds_simple %>%
  select(msoa11cd, msoa11nm)
object.size(zones_leeds)
object.size(zones_leeds_simple) # 1/3rd size
object.size(zones_leeds_cents)

od_data_zones = zones_leeds_simple

od_data_centroids = zones_leeds_cents %>%
  select(geo_code)
class(od_data_centroids)
od_data_coordinates = cbind(
  sf::st_drop_geometry(od_data_centroids),
  sf::st_coordinates(od_data_centroids)
)

usethis::use_data(od_data_centroids)
usethis::use_data(od_data_coordinates)

l = stplanr::od2line(od_data_df, zones_leeds)
mapview::mapview(l)


usethis::use_data(od_data_zones, overwrite = TRUE)
l = stplanr::od2line(od_data_df_all, zones_leeds)
plot(l[1:1000, ]) # slow, messy
l

# Study region and centre -------------------------------------------------

od_data_region = ukboundaries::leeds
plot(od_data_region)
od_data_centroid = st_centroid(od_data_region)
central_zone = od_data_zones[od_data_centroid, ]
od_data_centre = st_centroid(central_zone)
od_data_centre_projected = st_transform(od_data_centre, 27700)
od_data_region_projected = st_transform(od_data_region, 27700)
z = zonebuilder::zb_zone(x = od_data_region_projected, point = od_data_centre_projected, n_circles = 5)
plot(z)
od_data_csa_zones = z %>% st_transform(4326)
usethis::use_data(od_data_csa_zones, overwrite = TRUE)

od_data_df = od_data_df %>%
  filter(geo_code1 %in% od_data_zones$msoa11cd)

l = od_data_df %>% od2line(od_data_zones)
plot(l)
mapview::mapview(l) + mapview::mapview(od_data_csa_zones) # interesting plot showing importance of region centre
od_agg = od_aggregate(flow = od_data_df, od_data_zones, od_data_csa_zones) # fail!
plot(od_agg)


# get od_aggregate working ------------------------------------------------



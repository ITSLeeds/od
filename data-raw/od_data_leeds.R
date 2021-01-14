## code to prepare `od_data_df` dataset goes here

library(sf)
library(tidyverse)
library(stplanr)

od = pct::get_od()
centroids_ew_all = pct::get_centroids_ew() # todo: add centroids dataset based on this

od_data_df_medium = od %>%
  filter(geo_code1 %in% od_data_zones$msoa11cd) %>%
  filter(geo_code2 %in% od_data_zones$msoa11cd) %>%
  filter(geo_code1 != geo_code2) %>%
  select(geo_code1, geo_code2, all, train, bus, taxi, car_driver, car_passenger, bicycle, foot)

od_data_df_medium
class(od_data_df_medium)
class(od_data_df_medium) = "data.frame"
class(od_data_df_medium)

usethis::use_data(od_data_df_medium)

od_data_df = od_data_df_medium %>%
  select(geo_code1, geo_code2, all, train, bus, taxi, car_driver, car_passenger, bicycle, foot) %>%
  top_n(n = 6, bicycle) %>%
  as.data.frame(stringsAsFactors = FALSE)

od_data_df

usethis::use_data(od_data_df)

zones_leeds = ukboundaries::msoa2011_lds
class(zones_leeds)
sf::st_crs(zones_leeds) = 4326
zones_leeds = zones_leeds %>%
  transmute(geo_code = as.character(geo_code))
summary(zones_leeds)
zones_leeds_cents = st_centroid(zones_leeds)
zones_uk = ukboundaries::msoa2011_vsimple
sf::st_crs(zones_uk)
sf::st_crs(zones_uk) = 4326
sf::st_crs(zones_uk)
zones_leeds_simple = zones_uk[zones_leeds_cents, ]
zones_leeds_simple = zones_leeds_simple %>%
  select(msoa11cd, msoa11nm) %>%
  transmute(geo_code = as.character(msoa11cd))
object.size(zones_leeds)
object.size(zones_leeds_simple) # 1/3rd size
object.size(zones_leeds_cents)

od_data_zones = zones_leeds_simple
class(od_data_zones)

od_data_centroids = zones_leeds_cents %>%
  select(geo_code)
class(od_data_centroids)
identical(sf::st_crs(od_data_centroids), sf::st_crs(od_data_zones))
od_data_coordinates = cbind(
  sf::st_drop_geometry(od_data_centroids),
  sf::st_coordinates(od_data_centroids)
)
class(od_data_centroids)

usethis::use_data(od_data_zones, overwrite = TRUE)
usethis::use_data(od_data_centroids, overwrite = TRUE)
usethis::use_data(od_data_coordinates, overwrite = TRUE)

l = stplanr::od2line(od_data_df, zones_leeds)
mapview::mapview(l)

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


# get network for leeds ---------------------------------------------------

library(geofabrik)
route_network_west_yorkshire = get_geofabrik("west yorkshire")
route_network_leeds = route_network_west_yorkshire[od_data_zones, ]
summary(as.factor(route_network_leeds$highway))

# could save this at some point:
od_data_zones_min = od_data_zones %>%
  filter(geo_code %in% c(od_data_df$geo_code1, od_data_df$geo_code2))

od_data_region_1km = stplanr::geo_buffer(od_data_zones_min, dist = 500)
route_network_min = route_network_west_yorkshire[od_data_region_1km, ]
mapview::mapview(route_network_min)
od_data_network = route_network_min %>%
  filter(str_detect(highway, "cycleway|primary|second|tert|trunk"))

mapview::mapview(od_data_network) +
  mapview::mapview(od_data_zones_min)

usethis::use_data(od_data_network)
usethis::use_data(od_data_zones_min)

# get od_aggregate working ------------------------------------------------

zones_lsoa_leeds = pct::get_pct_zones(region = "west-yorkshire")
centroids_lsoa_leeds = pct::get_pct_centroids(region = "west-yorkshire")
nrow(zones_lsoa_leeds)
nrow(centroids_lsoa_leeds)
centroids_in_zones_min = centroids_lsoa_leeds[od::od_data_zones_min, ]
mapview::mapview(centroids_in_zones_min)
od_data_zones_small = zones_lsoa_leeds %>%
  filter(geo_code %in% centroids_in_zones_min$geo_code) %>%
  select(geo_code, all, foot, bicycle)
mapview::mapview(od_data_zones_small)
usethis::use_data(od_data_zones_small)


# get buildings for Leeds -------------------------------------------------

library(dplyr)
library(od)
# od_data_buildings
building_types = c(
  "office",
  "industrial",
  "commercial",
  "retail",
  "warehouse",
  "civic",
  "public"
)
leeds_osm = osmextract::oe_get(place = "leeds", layer = "multipolygons")
leeds_osm_buildigs = leeds_osm %>%
  filter(building %in% building_types)

zones_of_interest = od_data_zones_min[od_data_zones_min$geo_code %in% c(od_data_df$geo_code1[1:2], od_data_df$geo_code2[1:2]), ]
mapview::mapview(zones_of_interest)
buildings_in_zones = leeds_osm_buildigs[zones_of_interest, , op = sf::st_within]
mapview::mapview(buildings_in_zones)
od_data_buildings = buildings_in_zones
usethis::use_data(od_data_buildings)
file.size("data/od_data_buildings.rda") / 1000
# 40 kB...

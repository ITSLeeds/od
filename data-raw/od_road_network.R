q = "SELECT * FROM 'lines' WHERE highway IN ('primary', 'secondary', 'tertiary')"
road_network_area = osmextract::oe_get(place = "West Yorkshire", query = q)
od_road_network = road_network_area[od::od_data_zones_min, ]
table(od_road_network$highway)
mapview::mapview(od_road_network) + od_data_zones_min
# saveRDS(od_road_network, "od_road_network.Rds")
usethis::use_data(od_road_network)

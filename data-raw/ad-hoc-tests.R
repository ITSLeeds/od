# The code here represents 1 off tests saved for posterity


# 2020-08-01 --------------------------------------------------------------

x = od_data_df
p = od_data_centroids

x[[1]][1] =  "404"
# Next line will error:
od_coordinates(x, p, silent = FALSE)[1:2, ]
# From original stplanr function:
od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
# od_coords("Hereford", "Leeds") # geocode locations
od_coords(flowlines[1:3, ])
od_coords(flowlines_sf[1:3, ])

# 2020-08-01 - for vignette -----------------------------------------------

x = od_data_df
z = od_data_zones_min
network = od_data_network
(lines_to_points_on_network = od_to_sf_network(x, z, network = network))
(lines_to_points = od_to_sf(x, z))
library(tmap)
tmap_mode("view")
tm_shape(lines_to_points_on_network) + tm_lines(lwd = 5) +
 tm_shape(lines_to_points) + tm_lines(col = "grey", lwd = 5) +
 tm_shape(od_data_zones_min) + tm_borders() +
 qtm(od_data_network, lines.col = "yellow")
plot(sf::st_geometry(lines_to_points_on_network))
plot(lines_to_points, col = "grey", add = TRUE)
plot(sf::st_geometry(z), add = TRUE)


#' Convert OD data into lines with start and end points sampled on a network
#'
#' @inheritParams od_to_sf
#' @param network An sf object representing a transport network
#' @export
#' @examples
#' x = od_data_df
#' z = od_data_zones_min
#' network = od_data_network
#' (lines_to_points_on_network = od_to_network(x, z, network = network))
#' (lines_to_points = od_to_sf(x, z))
od_to_network = function(x, z, zd = NULL, silent = TRUE, package = "sf", crs = 4326,
                         network = NULL) {
  # browser() # todo: remove and optimise
  # odc = od_coordinates(x, z, silent = silent) # we want the data in this format

  z_nm = names(z)[1]
  zones_o = z[z[[1]] %in% x[[1]], ]
  zones_d = z[z[[1]] %in% x[[2]], ]
  # suppressWarnings({
  network_points = sf::st_cast(network, "POINT")
  network_points_o = network_points[zones_o, ] # subset only points on network in the zones
  network_points_d = network_points[zones_d, ] # subset only points on network in the zones
  # })

  net_o = sf::st_join(network_points_o, z[1])
  net_d = sf::st_join(network_points_d, z[1])

  # unique_origin_ids = unique(x[[1]])
  # uoid = table(x[[1]])
  # udid = table(x[[2]])
  #
  # s_origin = split(net_o, net_o[[z_nm]])
  # l_origin = lapply(seq_along(uoid),
  #        function(i) {
  #          g = s_origin[[i]]
  #          g[sample(nrow(g), size = uoid[i]), ]
  #        })
  # i = 1
  l_origin = lapply(seq(nrow(x)),
                    function(i) {
                      g = net_o[net_o[[z_nm]] == x[[1]][i], ]
                      g[sample(nrow(g), size = 1), ]
                    })
  d_origin = do.call(rbind, l_origin)
  # d_origin$geo_code == x[[1]] TRUE
  odc_origin = sf::st_coordinates(d_origin)

  l_destination = lapply(seq(nrow(x)),
                         function(i) {
                           g = net_d[net_d[[z_nm]] == x[[2]][i], ]
                           g[sample(nrow(g), size = 1), ]
                         })
  d_destination = do.call(rbind, l_destination)
  odc_destination = sf::st_coordinates(d_destination)

  odc = cbind(
    ox = odc_origin[, 1],
    oy = odc_origin[, 2],
    dx = odc_destination[, 1],
    dy = odc_destination[, 2]
  )
  # od_sfc = odc_to_sfc(odc) # sfheaders way: todo add it
  od_sfc = odc_to_sfc_sf(odc)
  sf::st_sf(x, geometry = od_sfc)
}

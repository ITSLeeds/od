#' Move desire line end points within zone to avoid all trips going to a single centroid
#'
#' These functions tackle the problem associated with OD data representing movement to and from large zones.
#' Typically the associated desire lines start and end in one point per zone.
#' This function produces desire lines that can start and end anywhere (or at predefined points) within each zone.
#' See [issue #11](https://github.com/ITSLeeds/od/issues/11) for details.
#'
#' @inheritParams od_disaggregate
#'
#' @return An `sf` data frame
#' @export
#'
#' @examples
#' od = od_data_df
#' z = od_data_zones_min
#' desire_lines = od_to_sf(od, z)
#' desire_lines_rand = od_rand(od, z)
#' plot(z$geometry)
#' plot(desire_lines_rand$geometry, add = TRUE)
#' plot(desire_lines, add = TRUE)
#' subpoints = sf::st_sample(z, 100)
od_rand = function(od, z, subpoints = NULL) {
  if(is(od, "sf")) {
    od = sf::st_drop_geometry(od)
  }
  z_geo = sf::st_geometry(z)
  id = c(od[[1]], od[[2]])
  points_per_zone = data.frame(table(id))
  names(points_per_zone)[1] = names(z)[1]
  points_per_zone_joined = merge(sf::st_drop_geometry(z), points_per_zone)
  if(is.null(subpoints)) {
    suppressMessages({
      subpoints = sf::st_sample(z, size = points_per_zone_joined$Freq)
    })
  }
  # Remove data from subpoints and convert to an sf object
  subpoints = sf::st_sf(geometry = sf::st_geometry(subpoints))
  subpoints$id_internal = seq(nrow(subpoints))
  suppressMessages({
    # subpoints joined
    sj = sf::st_join(subpoints, z)
  })
  subpoints_matrix = sfheaders::sf_to_df(sj)
  id_o = sapply(od[[1]], function(x) sample(sj$id_internal[sj$geo_code == x], size = 1))
  id_d = sapply(od[[2]], function(x) sample(sj$id_internal[sj$geo_code == x], size = 1))
  # coordinates_o = sf::st_coordinates(sj$geometry[id_o])
  coordinates_o = sfheaders::sfc_to_df(sj$geometry[id_o])
  coordinates_d = sfheaders::sfc_to_df(sj$geometry[id_d])
  odc = cbind(coordinates_o[c("x", "y")], coordinates_d[c("x", "y")])
  sf::st_sf(od, geometry = odc_to_sfc_sf(as.matrix(odc), crs = sf::st_crs(z)))
}

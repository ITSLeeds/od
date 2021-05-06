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
#' # # Test interactively with
#' # mapview::mapview(desire_lines) + desire_lines_rand + z
#' subpoints = sf::st_sample(z, 100)
#' desire_lines_rand2 = od_rand(od, z, subpoints)
#' plot(z, reset = FALSE)
#' plot(subpoints, add = TRUE)
#' plot(desire_lines_rand2$geometry, add = TRUE)
od_rand = function(od, z, subpoints = NULL) {
  if(methods::is(od, "sf")) {
    od = sf::st_drop_geometry(od)
  }
  z_geo = sf::st_geometry(z)
  id = c(od[[1]], od[[2]])
  points_per_zone = data.frame(table(id))
  names(points_per_zone)[1] = names(z)[1]
  points_per_zone_joined = merge(sf::st_drop_geometry(z), points_per_zone)
  z = z[z[[1]] %in% points_per_zone[[1]], ]
  if(is.null(subpoints)) {
    suppressMessages({
      subpoints = sf::st_sample(z, size = points_per_zone_joined$Freq)
    })
  }
  # Remove data from subpoints and convert to an sf object
  subpoints = sf::st_sf(geometry = sf::st_geometry(subpoints))
  subpoints$id = seq(nrow(subpoints))
  suppressMessages({
    # subpoints joined
    sj = sf::st_join(subpoints, z)
  })
  # id_o = sapply(od[[1]], function(x) sample(sj$id[sj[[2]] == x], size = 1))
  # id_d = sapply(od[[2]], function(x) sample(sj$id[sj[[2]] == x], size = 1))
  # # coordinates_o = sf::st_coordinates(sj$geometry[id_o])

  # generate origin indices
  # browser()
  sj_id = sj$id
  sj_geo = sj[[2]]
  x = od[[1]][1]
  id_o = sample_one(sj_id[sj_geo == x], size = 1)
  sj_id = sj_id[-id_o]
  sj_geo = sj_geo[-id_o]
  for(i in 2:nrow(od)) {
    x = od[[1]][i]
    i_remove = which(sj_geo == x)[1]
    id_o_new = sample_one(sj_id[i_remove], size = 1)
    sj_id = sj_id[-i_remove]
    sj_geo = sj_geo[-i_remove]
    id_o = c(id_o, id_o_new)
  }

  # sj_id = sj$id
  # sj_geo = sj[[2]]
  x = od[[2]][1]
  i_remove = which(sj_geo == x)[1]
  id_d = sample_one(sj_id[i_remove], size = 1)
  sj_id = sj_id[-i_remove]
  sj_geo = sj_geo[-i_remove]
  for(i in 2:nrow(od)) {
    x = od[[2]][i]
    i_remove = which(sj_geo == x)[1]
    id_d_new = sample_one(sj_id[i_remove], size = 1)
    sj_id = sj_id[-i_remove]
    sj_geo = sj_geo[-i_remove]
    id_d = c(id_d, id_d_new)
  }
  # browser()

  # coordinates_o = sf::st_coordinates(sj$geometry[id_o])
  # coordinates_d = sfheaders::sfc_to_df(sj$geometry[id_d])
  coordinates_o = sfheaders::sfc_to_df(sj$geometry[id_o])
  coordinates_d = sfheaders::sfc_to_df(sj$geometry[id_d])
  odc = cbind(coordinates_o[c("x", "y")], coordinates_d[c("x", "y")])
  d = sf::st_sf(od, geometry = odc_to_sfc_sf(as.matrix(odc), crs = sf::st_crs(z)))
  d
}

# https://stackoverflow.com/questions/48505961
sample_one = function(x, size) ifelse(length(x) > 1, sample(x, size), x)

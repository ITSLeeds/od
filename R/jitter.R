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
#' desire_lines_rand = od_jitter(od, z)
#' desire_lines = od_to_sf(od, z)
#' plot(z$geometry)
#' plot(desire_lines_rand$geometry, add = TRUE)
#' plot(desire_lines, add = TRUE)
#' # # Test interactively with
#' # mapview::mapview(desire_lines) + desire_lines_rand + z
#' subpoints = sf::st_sample(z, 100)
#' desire_lines_rand2 = od_jitter(desire_lines, z, subpoints)
#' plot(z, reset = FALSE)
#' plot(subpoints, add = TRUE)
#' plot(desire_lines_rand2$geometry, add = TRUE)
#' # # larger example with only subset of matching zones
#' # od = od_data_df_medium
#' # od_sf = od_to_sf(od, od_data_zones)
#' # desire_lines_rand3 = od_jitter(od_sf, z)
#' # plot(od_sf$geometry[od$all > 200])
#' # plot(desire_lines_rand3$geometry[od$all > 200])
od_jitter = function(od, z, subpoints = NULL) {
  if(!methods::is(od, "sf")) {
    # the data structure to reproduce for matching OD pairs
  od = od::od_to_sf(od, z = z)
  }
  odc_new = odc_original = od::od_coordinates(od)
  od = sf::st_drop_geometry(od)
  odc_df = data.frame(o = od[[1]], d = od[[2]], odc_original)

  z_geo = sf::st_geometry(z)
  id = c(od[[1]], od[[2]])
  points_per_zone = data.frame(table(id))
  names(points_per_zone)[1] = names(z)[1]
  points_per_zone_joined = merge(sf::st_drop_geometry(z), points_per_zone)
  # unique_zone_codes = points_per_zone_joined[[1]]
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
  sj_coords = sf::st_coordinates(sj)
  sj_df = data.frame(geo_code = sj[[2]], x = sj_coords[, 1], y = sj_coords[, 2])

  # unique_zones = z[[1]] # same as:
  unique_zones = points_per_zone_joined[[1]]
  i = unique_zones[1]
  # browser()
  for(i in unique_zones) {
    # total number of origins and destinations
    # n = points_per_zone_joined$Freq[points_per_zone_joined[[1]] == i]
    n_origins = sum(od[[1]] == i)
    if(n_origins == 0) next()
    sel_sj = which(sj_df$geo_code == i)
    sel_sj_o = sel_sj[sample(length(sel_sj), size = n_origins, replace = TRUE)]
    odc_new[od[[1]] == i, "ox"] = sj_df$x[sel_sj_o]
    odc_new[od[[1]] == i, "oy"] = sj_df$y[sel_sj_o]
    # remove those random points from the list of options
    sj_df = sj_df[-sel_sj_o, ]
  }

  for(i in unique_zones) {
    n_destinations = sum(od[[2]] == i)
    if(n_destinations == 0) next()
    sel_sj = which(sj_df$geo_code == i)
    sel_sj_d = sel_sj[sample(length(sel_sj), size = n_destinations, replace = TRUE)]
    odc_new[od[[2]] == i, "dx"] = sj_df$x[sel_sj_d]
    odc_new[od[[2]] == i, "dy"] = sj_df$y[sel_sj_d]
    # remove those random points from the list of options
    sj_df = sj_df[-sel_sj_d, ]
  }

  sf::st_sf(od, geometry = odc_to_sfc_sf(odc_new, crs = sf::st_crs(z)))
}

# https://stackoverflow.com/questions/48505961
sample_one = function(x, size) ifelse(length(x) > 1, sample(x, size), x)

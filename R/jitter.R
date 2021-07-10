#' Move desire line end points within zone to avoid all trips going to a single centroid
#'
#' These functions tackle the problem associated with OD data representing movement to and from large zones.
#' Typically the associated desire lines start and end in one point per zone.
#' This function produces desire lines that can start and end anywhere (or at predefined points) within each zone.
#' See [issue #11](https://github.com/ITSLeeds/od/issues/11) for details.
#'
#' @param zd Zones with ids matching the destination codes in input OD data
#' @param subpoints_d Points withing destination zones representing possible destinations
#' @inheritParams od_disaggregate
#'
#' @return An `sf` data frame
#' @export
#'
#' @examples
#'
#' # Basic example
#' od = od_data_df
#' z = od_data_zones_min
#' desire_lines_rand = od_jitter(od, z)
#' desire_lines = od_to_sf(od, z)
#' plot(z$geometry)
#' plot(desire_lines_rand, add = TRUE, lwd = 3)
#' plot(desire_lines, add = TRUE, lwd = 5)
#'
#' # Example showing use of subpoints
#' subpoints = sf::st_sample(z, 200)
#' subpoints_d = sf::st_sample(z, 100)
#' desire_lines_rand_d = od_jitter(od, z, subpoints = subpoints, subpoints_d = subpoints_d)
#' plot(z$geometry)
#' plot(desire_lines_rand_d$geometry, add = TRUE)
#' plot(subpoints, add = TRUE)
#' plot(subpoints_d, col = "red", add = TRUE)
#' plot(desire_lines, add = TRUE)
#' # mapview::mapview(desire_lines) + desire_lines_rand + z # interactive map
#' subpoints = sf::st_sample(z, 100)
#' desire_lines_rand2 = od_jitter(desire_lines, z, subpoints = subpoints)
#' plot(z, reset = FALSE)
#' plot(subpoints, add = TRUE)
#' plot(desire_lines_rand2$geometry, add = TRUE)
#'
#' # Example showing jittering of origins and destinations
#' od = od_data_df2
#' z = sf::st_buffer(od_data_centroids2, dist = 1000)
#' zd = sf::st_buffer(od_data_destinations, dist = 300)
#' zd = zd[zd[[1]] %in% od[[2]], ]
#' desire_lines = od_to_sf(od, od_data_centroids2, od_data_destinations)
#' desire_lines_rand = od_jitter(od, z, zd)
#' plot(z$geometry)
#' plot(od_data_centroids2$geometry, add = TRUE)
#' plot(od_data_destinations$geometry, add = TRUE)
#' plot(zd$geometry, add = TRUE)
#' plot(desire_lines$geometry, add = TRUE)
#' plot(desire_lines_rand$geometry, add = TRUE, col = "red")
#'
#' # Larger example with only subset of matching zones
#' # od = od_data_df_medium
#' # od_sf = od_to_sf(od, od_data_zones)
#' # desire_lines_rand3 = od_jitter(od_sf, z)
#' # plot(od_sf$geometry[od$all > 200])
#' # plot(desire_lines_rand3$geometry[od$all > 200])
#' # mapview::mapview(od_sf$geometry[od$all > 200])
od_jitter = function(od,
                     z,
                     zd = NULL,
                     subpoints = NULL,
                     subpoints_d = NULL) {
  if (!methods::is(od, "sf")) {
    # the data structure to reproduce for matching OD pairs
    od = od::od_to_sf(od, z = z, zd = zd)
  }
  odc_new = odc_original = od::od_coordinates(od)
  od = sf::st_drop_geometry(od)
  odc_df = data.frame(o = od[[1]], d = od[[2]], odc_original)
  z_geo = sf::st_geometry(z)
  # browser()
  id_origins = od[[1]]
  points_per_zone = data.frame(table(id_origins))
  names(points_per_zone)[1] = names(z)[1]
  points_per_zone_joined = merge(sf::st_drop_geometry(z), points_per_zone)
  # unique_zone_codes = points_per_zone_joined[[1]]
  z = z[z[[1]] %in% points_per_zone[[1]],]
  if (is.null(subpoints)) {
      subpoints = sf::st_sample(z, size = points_per_zone_joined$Freq)
  }
  # Remove data from subpoints and convert to an sf object
  subpoints = sf::st_sf(geometry = sf::st_geometry(subpoints))
  subpoints$id = seq(nrow(subpoints))
  suppressMessages({
    sj = sf::st_join(subpoints, z) # subpoints joined
  })
  sjc = sf::st_coordinates(sj) # subpoint coordinates
  sj_df = data.frame(geo_code = sj[[2]], x = sjc[, 1], y = sjc[, 2])

  unique_zones = points_per_zone_joined[[1]]
  i = unique_zones[1]
  for (i in unique_zones) {
    # total number of origins and destinations
    n_origins = sum(od[[1]] == i)
    if (n_origins == 0)
      next()
    sel_sj = which(sj_df$geo_code == i)
    sel_sj_o = sel_sj[sample(length(sel_sj), size = n_origins)]
    odc_new[od[[1]] == i, "ox"] = sj_df$x[sel_sj_o]
    odc_new[od[[1]] == i, "oy"] = sj_df$y[sel_sj_o]
    # remove those random points from the list of options
    sj_df = sj_df[-sel_sj_o,]
  }

  if (is.null(zd)) {
    zd = z[z[[1]] %in% od[[2]], ]
  } else {
    zd = zd[zd[[1]] %in% od[[2]], ]
  }

  id_destinations = od[[2]]
  points_per_zone = data.frame(table(id_destinations))
  if (is.null(subpoints_d)) {
    names(points_per_zone)[1] = names(zd)[1]
    points_per_zone_joined_d = merge(sf::st_drop_geometry(zd), points_per_zone)
    subpoints_d = sf::st_sample(zd, size = points_per_zone_joined_d$Freq)
  }

  points_per_zone_joined_d = merge(sf::st_drop_geometry(zd), points_per_zone)
  subpoints_d = sf::st_sf(geometry = sf::st_geometry(subpoints_d))
  subpoints_d$id = seq(nrow(subpoints_d))
  # subpoints joined
  sj_d = sf::st_join(subpoints_d, zd)
  sjc_d = sf::st_coordinates(sj_d)
  # browser()
  sj_df_d = data.frame(geo_code = sj_d[[2]], x = sjc_d[, 1], y = sjc_d[, 2])
  unique_zones_d = points_per_zone_joined_d[[1]]
  i = unique_zones_d[1]
  for (i in unique_zones_d) {
    n_destinations = sum(od[[2]] == i)
    if (n_destinations == 0)
      next()
    # when there are subpoints
    sel_sj = which(sj_df_d$geo_code == i)
    sel_sj_d = sel_sj[sample(length(sel_sj), size = n_destinations)]
    odc_new[od[[2]] == i, "dx"] = sj_df_d$x[sel_sj_d]
    odc_new[od[[2]] == i, "dy"] = sj_df_d$y[sel_sj_d]
    # remove those random points from the list of options
    sj_df = sj_df[-sel_sj_d,]
  }
  sf::st_sf(od, geometry = odc_to_sfc_sf(odc_new, crs = sf::st_crs(z)))
}

# https://stackoverflow.com/questions/48505961
sample_one = function(x, size) ifelse(length(x) > 1, sample(x, size), x)

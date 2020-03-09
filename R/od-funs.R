#' Convert OD data into geographic 'desire line' objects
#'
#' @param x A data frame in which the first two columns are codes
#'   representing points/zones of origin and destination
#' @param z Zones representing origins and destinations
#' @param zd Zones representing destinations
#' @param verbose Print messages providing progress updates? FALSE by default
#' @param package Which package to use to create the sf object? `sfheaders` is the default.
#' @param crs The coordinate reference system of the output, 4326 by default
#' @export
#' @examples
#' desire_lines = od_to_sfc(od_data_df, od_data_zones)
#' desire_lines[1:3]
#' if(requireNamespace("stplanr") && requireNamespace("bench")) {
#' desire_lines2 = stplanr::od2line(od_data_df, od_data_zones)
#' plot(desire_lines)
#' plot(desire_lines2)
#' bench::mark(check = FALSE, max_iterations = 10,
#'   od = od_to_sfc(od_data_df, od_data_zones),
#'   od2 = od_to_sfc(od_data_df, od_data_zones, package = "sf"),
#'   od_sf = od_to_sf(od_data_df, od_data_zones),
#'   od_sf2 = od_to_sf(od_data_df, od_data_zones, package = "sf"),
#'   stplanr = stplanr::od2line(od_data_df, od_data_zones)
#' )
#' }
od_to_sf = function(x, z, zd = NULL, verbose = FALSE, package = "sfheaders", crs = 4326) {
  od_sfc = od_to_sfc(x, z, zd = zd, verbose = verbose, package = package, crs = crs)
  sf::st_sf(x, geometry = od_sfc)
}
#' @rdname od_to_sf
#' @export
od_to_sfc = function(x, z, zd = NULL, verbose = FALSE, package = "sfheaders", crs = 4326) {
  if(package == "sfheaders") {
    odc = od_coordinates(x, z, verbose = verbose) # todo: add support for p
    od_sfc = od_coordinates_to_sfc(odc)
  } else {
    odc = od_coordinates(x, z, verbose = verbose, sfnames = TRUE) # todo: add support for p
    od_sfc = od_coordinates_to_sfc_sf(odc, crs = crs)
  }
  od_sfc
}

#' Create matrices representing origin-destination coordinates
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (ox, oy) and destination (dx, dy) points.
#' @param p Points representing origins and destinations
#' @param sfnames Should output column names be compatible with the sf package?
#' @inheritParams od_to_sfc
#' @export
#' @examples
#' od_coordinates(od_data_df, p = od::od_data_zones)[1:2, ]
#' od_coordinates(od_data_df, p = od::od_data_zones, sfnames = TRUE)[1:2, ]
#' od_coordinates(od_data_df, p = od::od_data_zones, verbose = TRUE)[1:2, ]
#' # od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' # od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' # od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # # od_coords("Hereford", "Leeds") # geocode locations
#' # od_coords(flowlines[1:3, ])
#' # od_coords(flowlines_sf[1:3, ])
od_coordinates = function(x, p = NULL, verbose = FALSE, sfnames = FALSE) {
  o_code = x[[1]]
  d_code = x[[2]]
  p_code_original = p[[1]]
  od_code = unique(c(o_code, d_code))
  sel_p_in_x = p_code_original %in% od_code
  geometry_contains_polygons = geometry_contains_polygons(p)
  if(geometry_contains_polygons) {
    if(requireNamespace("sf", quietly = TRUE)) {
      suppressWarnings({
        p_in_x = sf::st_centroid(sf::st_geometry(p)[sel_p_in_x])
      })
    } else {
      stop("sf package required, to install it see https://github.com/r-spatial/sf#installing")
    }
  } else {
    p_in_x = sf::st_geometry(p)[sel_p_in_x]
  }
  if(verbose) message(nrow(p) - nrow(p_in_x), " points not in od data removed.")
  p_code = p_code_original[sel_p_in_x]
  stopifnot(all(o_code %in% p_code)) # todo: add error message
  o_matching_p = match(o_code, p_code)
  d_matching_p = match(d_code, p_code)

  # p_coordinates = sf::st_coordinates(p_in_x)
  p_coordinates = sfheaders::sfc_to_df(p_in_x)[c("x", "y")]
  o_coords = p_coordinates[o_matching_p, ]
  d_coords = p_coordinates[d_matching_p, ]
  odc = cbind(o_coords, d_coords)
  if(sfnames) return(as.matrix(odc)) # return without updating column names
  colnames(odc) = c("ox", "oy", "dx", "dy")
  odc
}
# example
# odc = od_coordinates(od_data_df, p = od::od_data_zones, sfnames = TRUE)
# od:::od_coordinates_to_sfc_sf(odc)
od_coordinates_to_sfc = function(odc) {
  odc_id = od_coordinates_ids(odc)
  sfheaders::sfc_linestring(obj = odc_id, x = "x", y = "y", linestring_id = "id")
}
od_coordinates_to_sfc_sf = function(odc, crs = 4326) {
  linestring_list = lapply(seq(nrow(odc)), function(i) {
    sf::st_linestring(rbind(odc[i, 1:2], odc[i, 3:4]))
    })
  sf::st_sfc(linestring_list, crs = crs)
}
od_coordinates_ids = function(odc) {
  res = data.frame(id = rep(1:nrow(odc), each = 2), x = NA, y = NA)
  ids_odd = seq(1, nrow(res), by = 2)
  ids_even = ids_odd + 1
  res[ids_odd, c("x", "y")] = odc[, 1:2]
  res[ids_even, c("x", "y")] = odc[, 3:4]
  res
}


od_to_odmatrix <- function(flow, attrib = 3, name_orig = 1, name_dest = 2) {
  # todo: add this function from stplanr
}

#' Convert origin-destination data from wide to long format
#'
#' This function takes a matrix representing travel between origins
#' (with origin codes in the `rownames` of the matrix)
#' and destinations
#' (with destination codes in the `colnames` of the matrix)
#' and returns a data frame representing origin-destination pairs.
#'
#' The function returns a data frame with rows ordered by origin and then destination
#' zone code values and with names `orig`, `dest` and `flow`.
#'
#' @param odmatrix A matrix with row and columns representing origin and destination zone codes
#' and cells representing the flow between these zones.
#' @family od
#' @export
#' @examples
#' # odmatrix <- od_to_odmatrix(flow)
#' # odmatrix_to_od(odmatrix)
#' # flow[1:9, 1:3]
#' # odmatrix_to_od(od_to_odmatrix(flow[1:9, 1:3]))
odmatrix_to_od <- function(odmatrix) {
  od <- as.data.frame(as.table(odmatrix))
  names(od) <- c("orig", "dest", "flow")
  od <- stats::na.omit(od)
  od[order(paste0(od$orig, od$dest)), ]
}

geometry_contains_polygons = function(z) {
  # The sf way:
  # if(!requireNamespace("sf", quietly = TRUE)) {
  #   stop("sf package required, to install it see https://github.com/r-spatial/sf#installing")
  # }
  # grepl(pattern = "POLY", unique(sf::st_geometry_type(z)))
  # without sf:
  grepl(pattern = "POLY", class(z$geometry)[1])
}

#' Convert OD data into lines with start and end points sampled on a network
#'
#' @inheritParams od_to_sf
#' @param network An sf object representing a transport network
#' @export
#' @examples
#' x = od_data_df
#' z = od_data_zones_min
#' network = od_data_network
#' (lines_to_points_on_network = od_to_sf_network(x, z, network = network))
#' (lines_to_points = od_to_sf(x, z))
#' # to put in vignette...
#' # library(tmap)
#' # tmap_mode("view")
#' # tm_shape(lines_to_points_on_network) + tm_lines() +
#' #  tm_shape(lines_to_points) + tm_lines(col = "grey") +
#' #  tm_shape(od_data_zones_min) + tm_borders() +
#' #  qtm(od_data_network, lines.col = "yellow")
#' plot(sf::st_geometry(lines_to_points_on_network))
#' plot(lines_to_points, col = "grey", add = TRUE)
#' plot(sf::st_geometry(z), add = TRUE)
od_to_sf_network = function(x, z, zd = NULL, verbose = FALSE, package = "sf", crs = 4326,
                    network = NULL) {
  # browser() # todo: remove and optimise
  # odc = od_coordinates(x, z, verbose = verbose) # we want the data in this format

  # suppressWarnings({
  network_points = sf::st_cast(network, "POINT")
  network_points = network_points[z, ] # subset only points on network in the zones
  # })

  network_points_joined = sf::st_join(network_points, z[1])

  # unique_origin_ids = unique(x[[1]])
  unique_origin_ids = table(x[[1]])
  unique_destination_ids = table(x[[2]])

  i = 1 # for testing
  odc_origin = NULL
  for(i in seq_along(unique_origin_ids)) {
    network_points_in_zone = network_points_joined[
      network_points_joined$geo_code == names(unique_origin_ids)[i],
    ]
    random_ids = sample(nrow(network_points_in_zone), size = unique_origin_ids[i])
    points_to_sample_sf = network_points_in_zone[random_ids, 1]
    odc_origin = rbind(odc_origin, sf::st_coordinates(points_to_sample_sf))
  }

  odc_destination = NULL
  for(i in seq_along(unique_destination_ids)) {
    network_points_in_zone = network_points_joined[
      network_points_joined$geo_code == names(unique_destination_ids)[i],
    ]
    random_ids = sample(nrow(network_points_in_zone), size = unique_destination_ids[i])
    points_to_sample_sf = network_points_in_zone[random_ids, 1]
    odc_destination = rbind(odc_destination, sf::st_coordinates(points_to_sample_sf))
  }

  # browser()
  odc = as.matrix(data.frame(
    ox = odc_origin[, 1],
    oy = odc_origin[, 2],
    dx = odc_destination[, 1],
    dy = odc_destination[, 2]
  ))


  # od_sfc = od_coordinates_to_sfc(odc) # sfheaders way: todo add it
  od_sfc = od_coordinates_to_sfc_sf(odc)


  sf::st_sf(x, geometry = od_sfc)

}

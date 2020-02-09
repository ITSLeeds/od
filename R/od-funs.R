#' Convert origin-destination coordinates into geographic 'desire line' objects
#'
#' @param x A data frame in which the first two columns are codes
#'   representing points/zones of origin and destination
#' @param z Zones representing origins and destinations
#' @param zd Zones representing destinations
#' @param verbose Print messages providing progress updates? FALSE by default
#' @export
#' @examples
#' desire_lines = od_to_sfc(od_data_leeds, od_data_zones)
#' desire_lines[1:3]
#' if(requireNamespace("stplanr") && requireNamespace("bench")) {
#' desire_lines2 = stplanr::od2line(od_data_leeds, od_data_zones)
#' plot(desire_lines)
#' plot(desire_lines2)
#' bench::mark(check = FALSE, max_iterations = 10,
#'   od = od_to_sfc(od_data_leeds, od_data_zones),
#'   stplanr = stplanr::od2line(od_data_leeds, od_data_zones)
#' )
#' }
od_to_sfc = function(x, z, zd = NULL, verbose = FALSE) {
  odc = od_coordinates(x, z, verbose = verbose) # todo: add support for p
  odc_to_sfc(odc)
  # the sf version (requires sf)
  # odc_to_sf(odc, df = x)
}

#' Create matrices representing origin-destination coordinates
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (ox, oy) and destination (dx, dy) points.
#' @param p Points representing origins and destinations
#' @inheritParams od_to_sfc
#' @export
#' @examples
#' od_coordinates(od_data_leeds, p = od::od_data_zones)
#' od_coordinates(od_data_leeds, p = od::od_data_zones, verbose = TRUE)
#' # od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' # od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' # od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # # od_coords("Hereford", "Leeds") # geocode locations
#' # od_coords(flowlines[1:3, ])
#' # od_coords(flowlines_sf[1:3, ])
od_coordinates = function(x, p = NULL, verbose = FALSE) {
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
    p_in_x = p[sel_p_in_x, ]
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
  od_coordinates = cbind(o_coords, d_coords)
  colnames(od_coordinates) = c("ox", "oy", "dx", "dy")
  od_coordinates
}
# #' @rdname od_coordinates
# od_coords <- function(from = NULL, to = NULL, l = NULL) {
#   # todo: re-implement this from stplanr
# }

# od_to_odc = function(from = NULL, to = NULL, l = NULL) {
#   # todo: decide its future
# }

#' Convert origin-data coordinates into sf object
#' @param odc A data frame or matrix representing the coordinates
#' of origin-destination data. The first two columns represent the
#' coordinates of the origin (typically longitude and latitude) points;
#' the third and fourth columns represent the coordinates of the destination
#' (in the same CRS). Each row represents travel from origin to destination.
#' Convert od coordinates to sf object
#' @param df Data frame with OD data attributes
#' @param package Which package should create the geographic object? `sfheaders` by default.
#' @param crs The coordinate reference system of the output, 4326 by default
#' @examples
#' # testing the result
#' # odlsf = od_coordinates_to_sfc(odc, "sf")
#' # odsf_stplanr = stplanr::od2line(od_data_leeds, od_data_zones)
#' # odl_stplanr = sf::st_geometry(odsf_stplanr)
#' # odl_stplanr[1]
#' # odl[1]
#' # odlsf[1]
#' # bench::mark(check = FALSE,
#' #   od_coordinates_to_sfc(odc),
#' #   od_coordinates_to_sfc(odc, package = "sf"),
#' #   stplanr::od2line(od_data_leeds, od_data_zones)
#' # )
#' # system.time({odl_stplanr = stplanr::od2line(od_data_leeds, od_data_zones)})
#' # plot(sf::st_geometry(odl_stplanr))
odc_to_sf = function(odc, df, package = "sfheaders", crs = 4326) {
  sfc = od_coordinates_to_sfc(odc, package = package, crs = crs)
  sf::st_sf(df, geometry = sfc)
}
odc_to_sfc = function(odc, df, package = "sfheaders", crs = 4326) {
  od_coordinates_to_sfc(odc, package = package, crs = crs)
}
od_coordinates_to_sfc = function(odc, package = "sfheaders", crs = 4326) {
  if(package == "sfheaders") {
    odc_id = od_coordinates_ids(odc)
    sfheaders::sfc_linestring(obj = odc_id, x = "x", y = "y", linestring_id = "id")
  } else {
    linestring_list = lapply(seq(nrow(odc)), function(i) {
      sf::st_linestring(rbind(odc[i, 1:2], odc[i, 3:4]))
    })
    sf::st_sfc(linestring_list, crs = crs)
    }
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


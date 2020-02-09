#' Convert origin-destination coordinates into desire lines
#'
#' @param x A data frame in which the first two columns are codes
#'   representing points/zones of origin and destination
#' @param z Zones representing origins and destinations
#' @param zd Zones representing destinations
#' @export
#' @examples
#' desire_lines = od_to_sf(od_data_leeds, od_data_zones)
#' plot(desire_lines)
#'
#' # desire_lines2 = stplanr::od2line(od_data_leeds, od_data_zones)
#' # plot(desire_lines2[-c(1:2)])
#' # bench::mark(check = FALSE,
#' #   desire_lines = od_to_sf(od_data_leeds, od_data_zones)
#' #   desire_lines2 = stplanr::od2line(od_data_leeds, od_data_zones)
#' # )
od_to_sf = function(x, z, zd = NULL) {
  odc = od_coordinates(x, z = z) # todo: add support for p
  odc_to_sf(
    odc,
    # df = odf[-c(1:2)]
    df = x[-c(1:2)]
    )
}

#' Create matrices representing origin-destination coordinates
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (ox, oy) and destination (dx, dy) points.
#' @param p Points representing origins and destinations
#' @inheritParams od_to_sf
#' @export
#' @examples
#' od_coordinates(od_data_leeds, z = od::od_data_zones)
#' # od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' # od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' # od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # # od_coords("Hereford", "Leeds") # geocode locations
#' # od_coords(flowlines[1:3, ])
#' # od_coords(flowlines_sf[1:3, ])
od_coordinates = function(x, p = NULL, z = NULL) {
  if(is.null(p) && !is.null(z)) {
    geometry_contains_polygons = geometry_contains_polygons(z)
    if(geometry_contains_polygons) { suppressWarnings({
      p = sf::st_centroid(z[1])
    })}
  }
  od_coordinates_from_points(x, p)
}
# #' @rdname od_coordinates
# od_coords <- function(from = NULL, to = NULL, l = NULL) {
#   # todo: re-implement this from stplanr
# }

# od_to_odc = function(from = NULL, to = NULL, l = NULL) {
#   # todo: decide its future
# }

#' @param odc A data frame or matrix representing the coordinates
#' of origin-destination data. The first two columns represent the
#' coordinates of the origin (typically longitude and latitude) points;
#' the third and fourth columns represent the coordinates of the destination
#' (in the same CRS). Each row represents travel from origin to destination.
#' Convert od coordinates to sf object
#' @examples
#' odc = od_coordinates(od_data_leeds, z = od_data_zones)
#' odc
#' # odl = od_coordinates_to_sfc(odc) # currently fails...
#' # class(odl)
#' # plot(odl)
#'
#' # odc_to_sf(odc, df = od_data_leeds)
#' # odsf = odc_to_sf()
#'
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


# @examples
# x = od_data_leeds
# p = sf::st_centroid(od::od_data_zones)
od_coordinates_from_points = function(x, p, p_destinations = NULL) {
  o_code = x[[1]]
  d_code = x[[2]]
  p_code = p[[1]]
  stopifnot(all(o_code %in% p_code)) # todo: add error message
  o_matching_p = match(o_code, p_code)
  d_matching_p = match(d_code, p_code)
  p_coordinates = sf::st_coordinates(p)
  o_coords = p_coordinates[o_matching_p, ]
  d_coords = p_coordinates[d_matching_p, ]
  od_coordinates = cbind(o_coords, d_coords)
  colnames(od_coordinates) = c("ox", "oy", "dx", "dy")
  od_coordinates
}

geometry_contains_polygons = function(z) {
  if(!requireNamespace("sf", quietly = TRUE)) {
    stop("sf package required, to install it see https://github.com/r-spatial/sf#installing")
  }
  grepl(pattern = "POLY", unique(sf::st_geometry_type(z)))
}


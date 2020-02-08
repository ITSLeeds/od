#' Convert origin-destination coordinates into desire lines
#'
#' @param odc A data frame or matrix representing the coordinates
#' of origin-destination data. The first two columns represent the
#' coordinates of the origin (typically longitude and latitude) points;
#' the third and fourth columns represent the coordinates of the destination
#' (in the same CRS). Each row represents travel from origin to destination.
#' @param crs A number representing the coordinate reference system
#' of the result, 4326 by default.
#' @examples
#' desire_lines = od_to_sf(od_data_leeds, od_data_zones)
#' plot(desire_lines)
#'
#' desire_lines2 = stplanr::od2line(od_data_leeds, od_data_zones)
#' plot(desire_lines2[-c(1:2)])
#' bench::mark(check = FALSE,
#'   desire_lines = od_to_sf(od_data_leeds, od_data_zones)
#'   desire_lines2 = stplanr::od2line(od_data_leeds, od_data_zones)
#' )
od_to_sf = function(odf, zones, destinations = NULL) {
  odc = od_coordinates(x = odf, z = zones)
  odc_to_sf(odc, df = odf[-c(1:2)])
}

#' Convert od coordinates to sf object
#' @examples
#' odc = od_coordinates(od_data_leeds, od_data_zones)
#' odl = od_coordinates_to_sfc(odc)
#' class(odl)
#' plot(odl)
#'
#' odc_to_sf(odc, df = od_data_leeds)
#' odsf = odc_to_sf()
#'
#' # testing the result
#' odlsf = od_coordinates_to_sfc(odc, "sf")
#' odsf_stplanr = stplanr::od2line(od_data_leeds, od_data_zones)
#' odl_stplanr = sf::st_geometry(odsf_stplanr)
#' odl_stplanr[1]
#' odl[1]
#' odlsf[1]
#' bench::mark(check = FALSE,
#'   od_coordinates_to_sfc(odc),
#'   od_coordinates_to_sfc(odc, package = "sf"),
#'   stplanr::od2line(od_data_leeds, od_data_zones)
#' )
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

x <- data.frame( id = 1:2, x = 1:2, y = 2:1 )
sfheaders::sfc_linestring( x )
sfheaders::sfc_linestring( x, linestring_id = "id", x = "x", y = "y")

#' Extract coordinates from OD data
#'
#' @details
#' Origin-destination (OD) data is often provided
#' in the form of 1 line per OD pair, with zone codes of the trip origin in the first
#' column and the zone codes of the destination in the second column
#' (see the [`vignette("stplanr-od")`](https://docs.ropensci.org/stplanr/articles/stplanr-od.html)) for details.
#' `od2odf()` creates an 'origin-destination data frame', based on a data frame containing
#' origin and destination cones (`flow`) that match the first column in a
#' a spatial (polygon or point) object (`zones`).
#'
#' The function returns a data frame with coordinates for the origin and destination.
#' @inheritParams od2line
#' @family od
#' @export
#' @examples
#' data(flow)
#' data(zones)
#' od2odf(flow[1:2, ], zones)
od2odf <- function(flow, zones) {
  coords <- dplyr::data_frame(
    code = as.character(zones[[1]]),
    fx = coordinates(zones)[, 1], fy = coordinates(zones)[, 2]
  )
  flowcode <- dplyr::data_frame(code_o = as.character(flow[[1]]), code_d = as.character(flow[[2]]))
  odf <- dplyr::left_join(flowcode, coords, by = c("code_o" = "code"))
  coords <- dplyr::rename_(coords, tx = quote(fx), ty = quote(fy))
  odf <- dplyr::left_join(odf, coords, by = c("code_d" = "code"))

  data.frame(odf) # return data.frame as more compatible with spatial data
}

#' Create matrices representing origin-destination coordinates
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (fx, fy) and destination (tx, ty) points.
#'
#' @param from An object representing origins
#' (if lines are provided as the first argument, from is assigned to `l`)
#' @param to An object representing destinations
#' @param l Only needed if from and to are empty, in which case this
#' should be a spatial object representing desire lines
#' @family od
#' @export
#' @examples
#' od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # od_coords("Hereford", "Leeds") # geocode locations
#' od_coords(flowlines[1:3, ])
#' od_coords(flowlines_sf[1:3, ])
od_coords <- function(from = NULL, to = NULL, l = NULL) {

  if (is(object = from, class2 = "sf")) {
    is_sf_line <- all(sf::st_geometry_type(from) == "LINESTRING")
  } else {
    is_sf_line <- FALSE
  }

  if (is_sf_line | any(grepl(pattern = "Line", x = class(from)))) {
    l <- from
  }

  if (!is.null(l)) {
    coord_matrix <- line2df(l) %>%
      dplyr::select("fx", "fy", "tx", "ty")
  }

  else {
    # Convert sp object to lat/lon vector
    if (is(object = from, "Spatial")) from <- sp::coordinates(from)
    if (is(object = to, "Spatial")) to <- sp::coordinates(to)

    # sf objects
    if (is(object = from, "sf") | is(object = from, "sfc")) from <- sf::st_coordinates(from)
    if (is(object = to, "sf") | is(object = to, "sfc")) to <- sf::st_coordinates(to)

    # Convert character strings to lon/lat if needs be
    if (is.character(from)) from <- matrix(geo_code(from), ncol = 2)
    if (is.character(to)) to <- matrix(geo_code(to), ncol = 2)
    if (is.vector(from) & is.vector(to)) {
      coord_matrix <- matrix(c(from, to), ncol = 4)
    } else {
      coord_matrix <- cbind(from, to)
    }
    colnames(coord_matrix) <- c("fx", "fy", "tx", "ty")
  }

  as.matrix(coord_matrix)

}

points2line <- function(p) {
  UseMethod("points2line")
}

#' Convert origin-destination data from long to wide format
#'
#' This function takes a data frame representing travel between origins
#' (with origin codes in `name_orig`, typically the 1st column)
#' and destinations
#' (with destination codes in `name_dest`, typically the second column) and returns a matrix
#' with cell values (from `attrib`, the third column by default) representing travel between
#' origins and destinations.
#'
#' @param flow A data frame representing flows between origin and destinations
#' @param attrib A number or character string representing the column containing the attribute data
#' of interest from the `flow` data frame
#' @param name_orig A number or character string representing the zone of origin
#' @param name_dest A number or character string representing the zone of destination
#' @family od
#' @export
#' @examples
#' od_to_odmatrix(flow)
#' od_to_odmatrix(flow[1:9, ])
#' od_to_odmatrix(flow[1:9, ], attrib = "Bicycle")
od_to_odmatrix <- function(flow, attrib = 3, name_orig = 1, name_dest = 2) {
  out <- matrix(
    nrow = length(unique(flow[[name_orig]])),
    ncol = length(unique(flow[[name_dest]])),
    dimnames = list(unique(flow[[name_orig]]), unique(flow[[name_dest]]))
  )
  out[cbind(flow[[name_orig]], flow[[name_dest]])] <- flow[[attrib]]
  out
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
#' odmatrix <- od_to_odmatrix(flow)
#' odmatrix_to_od(odmatrix)
#' flow[1:9, 1:3]
#' odmatrix_to_od(od_to_odmatrix(flow[1:9, 1:3]))
odmatrix_to_od <- function(odmatrix) {
  od <- as.data.frame(as.table(odmatrix))
  names(od) <- c("orig", "dest", "flow")
  od <- stats::na.omit(od)
  od[order(paste0(od$orig, od$dest)), ]
}


#' @examples
#' x = od_data_leeds
#' p = sf::st_centroid(od::od_data_zones)
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

#' @examples
#' od_coordinates(od_data_leeds, od::od_data_zones)
od_coordinates = function(x, z) {
  geometry_contains_polygons = geometry_contains_polygons(z)
  if(geometry_contains_polygons) { suppressWarnings({
    sf::st_geometry(z) = sf::st_centroid(sf::st_geometry(z))
  })}
  od_coordinates_from_points(x, z)
}

geometry_contains_polygons = function(z) {
  grepl(pattern = "POLY", unique(sf::st_geometry_type(z)))
}


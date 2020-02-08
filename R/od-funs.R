#' @examples
#' x = od_data_leeds
#' class(x)
#' x_od = od(x)
#' class(x_od)
od = function(x) {
  class(x) = c("od", class(x))
  return(x)
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

#' @examples
#' odm = od_coordinates(od_data_leeds, od_data_zones)
#' odm_id = od:::od_coordinates_ids(odm)
#' odl = od_coordinates_to_linstring(odm_id)
#' class(odl)
#' plot(odl)
#'
#' # testing the result
#' odsf_stplanr = stplanr::od2line(od_data_leeds, od_data_zones)
#' odl_stplanr = sf::st_geometry(odsf_stplanr)
#' odl_stplanr[1]
#' odl[1]
#' bench::mark(check = FALSE,
#' od_coordinates_to_linstring(odm_id),
#' stplanr::od2line(od_data_leeds, od_data_zones)
#' )
#' # system.time({odl_stplanr = stplanr::od2line(od_data_leeds, od_data_zones)})
#' # plot(sf::st_geometry(odl_stplanr))
od_coordinates_to_linstring = function(odm) {
  sfheaders::sfc_linestring(obj = odm_id, x = "x", y = "y", linestring_id = "id")
}

od_coordinates_ids = function(odm) {
  res = data.frame(id = rep(1:nrow(odm), each = 2), x = NA, y = NA)
  ids_odd = seq(1, nrow(res), by = 2)
  ids_even = ids_odd + 1
  res[ids_odd, c("x", "y")] = odm[, 1:2]
  res[ids_even, c("x", "y")] = odm[, 3:4]
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

#' Convert origin-destination coordinates into desire lines
#'
#' @param odc A data frame or matrix representing the coordinates
#' of origin-destination data. The first two columns represent the
#' coordinates of the origin (typically longitude and latitude) points;
#' the third and fourth columns represent the coordinates of the destination
#' (in the same CRS). Each row represents travel from origin to destination.
#' @param crs A number representing the coordinate reference system
#' of the result, 4326 by default.
#' @param remove_duplicates Should rows with duplicated rows be removed? `TRUE` by default.
#' @family od
#' @export
#' @examples
#' odf <- od_coords(l = flowlines_sf)
#' odlines <- od_coords2line(odf)
#' odlines <- od_coords2line(odf, crs = 4326)
#' plot(odlines)
#' x_coords = 1:3
#' n = 50
#' d = data.frame(lapply(1:4, function(x) sample(x_coords, n, replace = TRUE)))
#' names(d) = c("fx", "fy", "tx", "ty")
#' l = od_coords2line(d)
#' plot(l)
#' nrow(l)
#' l_with_duplicates = od_coords2line(d, remove_duplicates = FALSE)
#' plot(l_with_duplicates)
#' nrow(l_with_duplicates)
od_coords2line <- function(odc, crs = 4326, remove_duplicates = TRUE) {
  odc_unique <- odc[!duplicated(odc[, 1:4]), ]
  if(nrow(odc_unique) < nrow(odc) && remove_duplicates) {
    message("Duplicate OD pairs identified, removing ", nrow(odc) - nrow(odc_unique), " rows")
    odc <- odc_unique
    odc_unique$n = dplyr::group_size(dplyr::group_by_all(as.data.frame(odc[, 1:4])))
  }
  odm <- as.matrix(odc)
  linestring_list <- lapply(seq(nrow(odm)), function(i) {
    sf::st_linestring(rbind(odm[i, 1:2], odm[i, 3:4]))
  })
  sf::st_sf(odc, geometry = sf::st_sfc(linestring_list, crs = crs))
}
#' Convert origin-destination data to spatial lines
#'
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @details
#' Origin-destination (OD) data is often provided
#' in the form of 1 line per OD pair, with zone codes of the trip origin in the first
#' column and the zone codes of the destination in the second column
#' (see the [`vignette("stplanr-od")`](https://docs.ropensci.org/stplanr/articles/stplanr-od.html)) for details.
#' `od2line()` creates a spatial (linestring) object representing movement from the origin
#' to the destination for each OD pair.
#' It takes data frame containing
#' origin and destination cones (`flow`) that match the first column in a
#' a spatial (polygon or point) object (`zones`).
#'
#' @param flow A data frame representing origin-destination data.
#'  The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in [cents()],
#' the first column is geo_code. This corresponds to the first two columns
#' of [flow()].
#' @param zones A spatial object representing origins (and destinations
#' if no separate destinations object is provided) of travel.
#' @param destinations A spatial object
#' representing destinations of travel flows.
#' @param zone_code Name of the variable in `zones` containing the ids of the zone.
#' By default this is the first column names in the zones.
#' @param origin_code Name of the variable in `flow` containing the ids of the zone of origin.
#' By default this is the first column name in the flow input dataset.
#' @param dest_code Name of the variable in `flow` containing the ids of the zone of destination.
#' By default this is the second column name in the flow input dataset or the first column name in the
#' destinations if that is set.
#' @param zone_code_d Name of the variable in `destinations` containing the ids of the zone.
#' By default this is the first column names in the destinations.
#' @param silent TRUE by default, setting it to TRUE will show you the matching columns
#' @family od
#' @export
#' @examples
#' l <- od2line(flow = flow, zones = cents)
#' plot(cents)
#' plot(l, lwd = l$All / mean(l$All), add = TRUE)
#' # When destinations are different
#' head(flow_dests[1:5]) # check data
#' head(destinations[1:5])
#' flowlines_dests <- od2line(flow_dests, cents, destinations = destinations)
#' plot(flowlines_dests)
#' l <- od2line(flow, zones_sf)
#' plot(l["All"], lwd = l$All/mean(l$All))
#' @name od2line
NULL

#' @rdname od2line
#' @export
od2line <- function(flow, zones, destinations = NULL,
                    zone_code = names(zones)[1],
                    origin_code = names(flow)[1],
                    dest_code = names(flow)[2],
                    zone_code_d = NA, silent = FALSE) {
  UseMethod("od2line", object = zones)
}
#' @export
od2line.sf <- function(flow, zones, destinations = NULL,
                       zone_code = names(zones)[1],
                       origin_code = names(flow)[1],
                       dest_code = names(flow)[2],
                       zone_code_d = NA, silent = TRUE) {
  if (grepl(pattern = "POLYGON", x = unique(sf::st_geometry_type(zones)))) {
    message("Creating centroids representing desire line start and end points.")
    zones <- sf::st_centroid(zones)
  }

  coords_o <- sf::st_coordinates(zones)[, 1:2]
  origin_points <- coords_o[match(flow[[origin_code]], zones[[zone_code]]), ]

  if (is.null(destinations)) {
    if (!silent) {
      message(paste(
        "Matching", zone_code, "in the zones to", origin_code, "and", dest_code,
        "for origins and destinations respectively"
      ))
    }

    dest_points <- coords_o[match(flow[[dest_code]], zones[[zone_code]]), ]
  } else {
    dest_points <- coords_o[match(flow[[dest_code]], destinations[[zone_code_d]]), ]
  }

  odm = cbind(origin_points, dest_points)

  odsfc <- od_coords2line(odm, crs = sf::st_crs(zones), remove_duplicates = FALSE)
  sf::st_sf(flow, geometry = odsfc$geometry)

}
points2line <- function(p) {
  UseMethod("points2line")
}
#' @export
points2line.sf <- function(p) {
  points2flow(p = p)
}
#' @export
points2line.Spatial <- function(p) {
  if (is(p, "SpatialPoints")) {
    p_proj <- sp::proj4string(p)
    p <- sp::coordinates(p)
  } else {
    p_proj <- NA
  }
  l <- points2line(p)
  raster::crs(l) <- p_proj
  l
}
#' @export
points2line.matrix <- function(p) {
  l <- raster::spLines(p)
  l
}
#' Summary statistics of trips originating from zones in OD data
#'
#' This function takes a data frame of OD data and
#' returns a data frame reporting summary statistics for each unique zone of origin.
#'
#' It has some default settings: the default summary statistic is `sum()` and the
#' first column in the OD data is assumed to represent the zone of origin.
#' By default, if `attrib` is not set, it summarises all numeric columns.
#'
#' @inheritParams od2odf
#' @inheritParams overline
#' @param FUN A function to summarise OD data by
#' @param col The column that the OD dataset is grouped by
#' (1 by default, the first column usually represents the origin)
#' @param ... Additional arguments passed to `FUN`
#' @family od
#' @export
#' @examples
#' od_aggregate_from(flow)
od_aggregate_from <- function(flow, attrib = NULL, FUN = sum, ..., col = 1) {
  if(is.character(attrib)) {
    attrib_lgl <- grepl(pattern = attrib, x = names(flow))
    if(sum(attrib_lgl) == 0){
      stop("No columns match the attribute ", attrib)
    }
    attrib = which(attrib_lgl)
  }
  if(!is.null(attrib)) {
    flow <- flow[attrib]
  }
  flow_grouped <- dplyr::group_by_at(flow, col)
  dplyr::summarise_if(flow_grouped, is.numeric, .funs = FUN, ...)
}
#' Summary statistics of trips arriving at destination zones in OD data
#'
#' This function takes a data frame of OD data and
#' returns a data frame reporting summary statistics for each unique zone of destination.
#'
#' It has some default settings: it assumes the destination ID column is the 2nd
#' and the default summary statistic is `sum()`.
#' By default, if `attrib` is not set, it summarises all numeric columns.
#'
#' @inheritParams od_aggregate_from
#' @family od
#' @export
#' @examples
#' od_aggregate_to(flow)
od_aggregate_to <- function(flow, attrib = NULL, FUN = sum, ..., col = 2) {
  if(is.character(attrib)) {
    attrib_lgl <- grepl(pattern = attrib, x = names(flow))
    if(sum(attrib_lgl) == 0){
      stop("No columns match the attribute ", attrib)
    }
    attrib = which(attrib_lgl)
  }
  if(!is.null(attrib)) {
    flow <- flow[attrib]
  }
  flow_grouped <- dplyr::group_by_at(flow, col)
  dplyr::summarise_if(flow_grouped, is.numeric, .funs = FUN, ...)
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

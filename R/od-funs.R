#' Convert OD data into geographic 'desire line' objects
#'
#' @param x A data frame in which the first two columns are codes
#'   representing points/zones of origin and destination
#' @param z Zones representing origins and destinations
#' @param zd Zones representing destinations
#' @param silent Hide messages? `FALSE` by default.
#' @param filter Remove rows with no matches in `z`? `TRUE` by default
#' @param package Which package to use to create the sf object? `sfheaders` is the default.
#' @param crs The coordinate reference system of the output, if not known in `z`.
#' 4326 by default.
#' @inheritParams odc_to_sf
#' @export
#' @examples
#' x = od_data_df
#' z = od_data_zones
#' desire_lines = od_to_sf(x, z)
#' desire_lines[1:3]
#' plot(desire_lines)
#' desire_lines_d = od_to_sf(od_data_df2, od_data_centroids2, od_data_destinations)
#' o1 = od_data_centroids2[od_data_centroids2[[1]] == od_data_df2[[1]][1], ]
#' d1 = od_data_destinations[od_data_destinations[[1]] == od_data_df2[[2]][1], ]
#' plot(desire_lines_d$geometry)
#' plot(o1, add = TRUE)
#' plot(d1, add = TRUE)
#' plot(desire_lines_d$geometry[1], lwd = 3, add = TRUE)
#' n = 7
#' on = od_data_centroids2[od_data_centroids2[[1]] == od_data_df2[[1]][n], ]
#' dn = od_data_destinations[od_data_destinations[[1]] == od_data_df2[[2]][n], ]
#' plot(desire_lines_d$geometry)
#' plot(on, add = TRUE)
#' plot(dn, add = TRUE)
#' plot(desire_lines_d$geometry[n], lwd = 3, add = TRUE)
od_to_sf = function(x, z, zd = NULL, odc = NULL, silent = FALSE, filter = TRUE,
                    package = "sfheaders", crs = 4326) {

  if(!is.null(odc)) {
    return(odc_to_sf(odc = odc, crs = crs))
  }
  if (filter && is.null(zd)) {
    x = od_filter(x, codes = z[[1]], silent = silent)
  }
  if(filter && !is.null(zd)) {
    x = od_filter(x, codes = c(z[[1]], zd[[1]]))
  }
  od_sfc = od_to_sfc(x, z, zd, silent, package, crs, filter)
  sf::st_sf(x, geometry = od_sfc)
}
#' @rdname od_to_sf
#' @export
od_to_sfc = function(x,
                     z,
                     zd = NULL,
                     silent = TRUE,
                     package = "sfheaders",
                     crs = 4326,
                     filter = TRUE) {
  if(package == "sfheaders") {
    odc = od_coordinates(x, z, zd, silent = silent) # todo: add support for p
    od_sfc = odc_to_sfc(odc)
    if(requireNamespace("sf", quietly = TRUE)) {
      if(!is.na(sf::st_crs(z))) {
        crs = sf::st_crs(z)
      }
      sf::st_crs(od_sfc) = sf::st_crs(crs)
    } else {
      message("sf package not installed (install it to work with CRSs")
    }
  } else {
    odc = od_coordinates(x, z, zd, silent = silent, sfnames = TRUE) # todo: add support for p
    od_sfc = odc_to_sfc_sf(odc, crs = crs)
  }
  od_sfc
}

#' Create matrices representing origin-destination coordinates
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a data frame of coordinates representing origin (ox, oy) and destination (dx, dy) points.
#' @param p Points representing origins and destinations
#' @param pd Points representing destinations, if different from origin points
#' @param sfnames Should output column names be compatible with the sf package?
#' @return A data frame with origin and destination coordinates
#' @inheritParams od_to_sfc
#' @export
#' @examples
#' x = od_data_df
#' p = od_data_centroids
#' res = od_coordinates(x, p)[1:2, ]
#' class(res)
#' res
#' od_coordinates(x, p, sfnames = TRUE)[1:2, ]
#' od_coordinates(x, p, silent = FALSE)[1:2, ]
#' od_coordinates(x, p)
#' x = od_data_df2[1:3, ]
#' p = od_data_centroids2
#' pd = od_data_destinations
#' od_coordinates(x, p, pd)
od_coordinates = function(x, p = NULL, pd = NULL, silent = TRUE, sfnames = FALSE) {
  if(methods::is(x, "sf")) {
    return(od_coordinates_sf(x))
  }
  o_code = x[[1]]
  d_code = x[[2]]
  if(methods::is(o_code, "factor")) {
    message("Converting origin ID from factor to character")
    o_code = as.character(o_code)
  }
  if(methods::is(d_code, "factor")) {
    message("Converting destination ID from factor to character")
    d_code = as.character(d_code)
  }
  p_code_original = p[[1]]
  if(methods::is(p_code_original, "factor")) {
    message("Converting geometry ID from factor to character")
    p_code_original = as.character(p_code_original)
  }
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
  if(!silent) message(nrow(p) - nrow(p_in_x), " points not in od data removed.")
  p_code = p_code_original[sel_p_in_x]
  stopifnot(all(o_code %in% p_code)) # todo: add error message
  if(is.null(pd)) {
    stopifnot(all(d_code %in% p_code)) # todo: add error message
  } else {
    stopifnot(all(d_code %in% pd[[1]])) # todo: add error message
  }
  o_matching_p = match(o_code, p_code)
  if(is.null(pd)) {
    d_matching_p = match(d_code, p_code)
  } else {
    pcode_d = pd[[1]]
    d_matching_p = match(d_code, pcode_d)
    pd_coordinates = sfheaders::sfc_to_df(pd$geometry)[c("x", "y")]
    d_coords = pd_coordinates[d_matching_p, ]
  }
  p_coordinates = sfheaders::sfc_to_df(p_in_x)[c("x", "y")]
  o_coords = p_coordinates[o_matching_p, ]
  if(is.null(pd)) {
    d_coords = p_coordinates[d_matching_p, ]
  }
  odc = cbind(o_coords, d_coords)
  if(sfnames) return(as.matrix(odc)) # return without updating column names
  colnames(odc) = c("ox", "oy", "dx", "dy")
  odc
}
od_coordinates_sf = function(x) {
  coords_o = sf::st_coordinates(lwgeom::st_startpoint(x))
  coords_d = sf::st_coordinates(lwgeom::st_endpoint(x))
  odc = cbind(coords_o, coords_d)
  colnames(odc) = c("ox", "oy", "dx", "dy")
  odc
}
#' Convert origin-destination coordinates into geographic desire lines
#' @inheritParams od_to_sf
#' @param odc A matrix containing coordinates representing line start and end points
#' @param d An optional data frame to add to the geometry column
#' @export
#' @examples
#' (odc = od_coordinates(od_data_df, p = od_data_zones, sfnames = TRUE))
#' (l = odc_to_sf(odc))
#' plot(l)
#' lsfc = odc_to_sfc(odc)
odc_to_sf = function(odc, d = NULL, crs = 4326) {
  odc_id = od_coordinates_ids(odc)
  odc_sfc = sfheaders::sfc_linestring(obj = odc_id, x = "x", y = "y", linestring_id = "id")
  if(is.null(d)) {
    return(sf::st_sf(geometry = odc_sfc, crs = crs))
  }
  sf::st_sf(d, geometry = odc_sfc, crs = crs)
}
#' Convert origin-destination coordinates into geographic desire lines
#'
#' @param odc A matrix containing coordinates representing line start and end points
#' @export
#' @examples
#' (odc = od_coordinates(od_data_df, p = od::od_data_zones, sfnames = TRUE))
#' (l = odc_to_sfc(odc))
#' plot(l)
odc_to_sfc = function(odc) {
  odc_id = od_coordinates_ids(odc)
  sfheaders::sfc_linestring(obj = odc_id, x = "x", y = "y", linestring_id = "id")
}
odc_to_sfc_sf = function(odc, crs = 4326) {
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


#' Convert origin-destination data from long to wide format
#'
#' This function takes a data frame representing travel between origins
#' (with origin codes in `name_orig`, typically the 1st column)
#' and destinations
#' (with destination codes in `name_dest`, typically the second column) and returns a matrix
#' with cell values (from `attrib`, the third column by default) representing travel between
#' origins and destinations.
#'
#' @param x A data frame representing flows between origin and destinations
#' @param attrib A number or character string representing the column containing the attribute data
#' of interest from the `flow` data frame
#' @param name_orig A number or character string representing the zone of origin
#' @param name_dest A number or character string representing the zone of destination
#' @family od
#' @export
#' @examples
#' x = od_data_df[1:4, ]
#' x_matrix = od_to_odmatrix(x)
#' class(x_matrix)
#' od_to_odmatrix(x, attrib = "bicycle")
od_to_odmatrix = function(x, attrib = 3, name_orig = 1, name_dest = 2) {
  out = matrix(
    nrow = length(unique(x[[name_orig]])),
    ncol = length(unique(x[[name_dest]])),
    dimnames = list(unique(x[[name_orig]]), unique(x[[name_dest]]))
  )
  out[cbind(x[[name_orig]], x[[name_dest]])] = x[[attrib]]
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
#' x = od_data_df
#' x[1:3]
#' odmatrix = od_to_odmatrix(od_data_df)
#' odmatrix
#' odmatrix_to_od(odmatrix)
odmatrix_to_od = function(odmatrix) {
  od = as.data.frame(as.table(odmatrix))
  names(od) = c("orig", "dest", "flow")
  od = stats::na.omit(od)
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

#' Filter OD datasets
#'
#' This function takes and OD dataset and a character vector of codes
#' and returns an OD dataset with rows matching origin and destinations
#' zones present in the codes.
#'
#' @param codes The zone codes that must be in origins and destination
#' @inheritParams od_to_sf
#' @return A data frame
#' @export
#' @examples
#' x = od_data_df
#' z = od_data_zones
#' codes = z[[1]]
#' z_in_x_o = codes %in% x[[1]]
#' z_in_x_d = codes %in% x[[2]]
#' sum(z_in_x_d)
#' sum(z_in_x_o)
#' z = z[which(z_in_x_o | z_in_x_d)[-1], ]
#' z[[1]]
#' unique(c(x[[1]], x[[2]]))
#' try(od_to_sf(x, z)) # fails
#' nrow(x)
#' x = od_filter(x, z[[1]])
#' nrow(x)
#' od_to_sf(x, z)
od_filter = function(x, codes, silent = FALSE) {
  sel_o_in_codes = x[[1]] %in% codes
  sel_d_in_codes = x[[2]] %in% codes
  if(!silent) {
    message(sum(!sel_o_in_codes), " origins with no match in zone ids")
    message(sum(!sel_d_in_codes), " destinations with no match in zone ids")
  }
  x[sel_o_in_codes & sel_d_in_codes, ]
}
od_filter_origins = function(x, codes) {
  sel_o_in_codes = x[[1]] %in% codes
  x[sel_o_in_codes, ]
}
od_filter_destinations = function(x, codes) {
  sel_d_in_codes = x[[2]] %in% codes
  x[sel_d_in_codes, ]
}

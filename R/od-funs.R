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
#' @export
#' @examples
#' x = od_data_df
#' z = od_data_zones
#' desire_lines = od_to_sf(x, z)
#' desire_lines[1:3]
od_to_sf = function(x, z, zd = NULL, silent = FALSE, filter = TRUE,
                    package = "sfheaders", crs = 4326) {
  if (filter && is.null(zd)) {
    x = od_filter(x, codes = z[[1]], silent = silent)
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
    odc = od_coordinates(x, z, silent = silent) # todo: add support for p
    od_sfc = odc_to_sfc(odc)
    if(requireNamespace("sf")) {
      if(!is.na(sf::st_crs(z))) {
        crs = sf::st_crs(z)
      }
      sf::st_crs(od_sfc) = sf::st_crs(crs)
    }
  } else {
    odc = od_coordinates(x, z, silent = silent, sfnames = TRUE) # todo: add support for p
    od_sfc = odc_to_sfc_sf(odc, crs = crs)
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
#' x = od_data_df
#' p = od_data_centroids
#' od_coordinates(x, p)[1:2, ]
#' od_coordinates(x, p, sfnames = TRUE)[1:2, ]
#' od_coordinates(x, p, silent = FALSE)[1:2, ]
#' # x[[1]][1] =  "404"
#' # Next line will error:
#' # od_coordinates(x, p, silent = FALSE)[1:2, ]
#' # From original stplanr function:
#' # od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' # od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' # od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # # od_coords("Hereford", "Leeds") # geocode locations
#' # od_coords(flowlines[1:3, ])
#' # od_coords(flowlines_sf[1:3, ])
od_coordinates = function(x, p = NULL, silent = TRUE, sfnames = FALSE) {
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
  if(!silent) message(nrow(p) - nrow(p_in_x), " points not in od data removed.")
  p_code = p_code_original[sel_p_in_x]
  stopifnot(all(o_code %in% p_code)) # todo: add error message
  stopifnot(all(d_code %in% p_code)) # todo: add error message
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
#' # tm_shape(lines_to_points_on_network) + tm_lines(lwd = 5) +
#' #  tm_shape(lines_to_points) + tm_lines(col = "grey", lwd = 5) +
#' #  tm_shape(od_data_zones_min) + tm_borders() +
#' #  qtm(od_data_network, lines.col = "yellow")
#' plot(sf::st_geometry(lines_to_points_on_network))
#' plot(lines_to_points, col = "grey", add = TRUE)
#' plot(sf::st_geometry(z), add = TRUE)
od_to_sf_network = function(x, z, zd = NULL, silent = TRUE, package = "sf", crs = 4326,
                    network = NULL) {
  # browser() # todo: remove and optimise
  # odc = od_coordinates(x, z, silent = silent) # we want the data in this format

  z_nm = names(z)[1]
  zones_o = z[z[[1]] %in% x[[1]], ]
  zones_d = z[z[[1]] %in% x[[2]], ]
  # suppressWarnings({
  network_points = sf::st_cast(network, "POINT")
  network_points_o = network_points[zones_o, ] # subset only points on network in the zones
  network_points_d = network_points[zones_d, ] # subset only points on network in the zones
  # })

  net_o = sf::st_join(network_points_o, z[1])
  net_d = sf::st_join(network_points_d, z[1])

  # unique_origin_ids = unique(x[[1]])
  # uoid = table(x[[1]])
  # udid = table(x[[2]])
  #
  # s_origin = split(net_o, net_o[[z_nm]])
  # l_origin = lapply(seq_along(uoid),
  #        function(i) {
  #          g = s_origin[[i]]
  #          g[sample(nrow(g), size = uoid[i]), ]
  #        })
  i = 1
  l_origin = lapply(seq(nrow(x)),
         function(i) {
           g = net_o[net_o[[z_nm]] == x[[1]][i], ]
           g[sample(nrow(g), size = 1), ]
         })
  d_origin = do.call(rbind, l_origin)
  # d_origin$geo_code == x[[1]] TRUE
  odc_origin = sf::st_coordinates(d_origin)

  l_destination = lapply(seq(nrow(x)),
                    function(i) {
                      g = net_d[net_d[[z_nm]] == x[[2]][i], ]
                      g[sample(nrow(g), size = 1), ]
                    })
  d_destination = do.call(rbind, l_destination)
  odc_destination = sf::st_coordinates(d_destination)

  odc = cbind(
    ox = odc_origin[, 1],
    oy = odc_origin[, 2],
    dx = odc_destination[, 1],
    dy = odc_destination[, 2]
  )
  # od_sfc = odc_to_sfc(odc) # sfheaders way: todo add it
  od_sfc = odc_to_sfc_sf(odc)
  sf::st_sf(x, geometry = od_sfc)
}

#' Convert OD data into geographic 'desire line' objects
#' @param codes The zone codes that must be in origins and destination
#' @inheritParams od_to_sf
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

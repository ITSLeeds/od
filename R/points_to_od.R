#' Convert a series of points into a dataframe of origins and destinations
#'
#' Takes a series of geographical points and converts them into a data.frame
#' representing the potential flows, or 'spatial interaction', between every
#' combination of points.
#'
#' `points_to_odl()` generates the same output but returns a geographic object
#' representing desire lines in the class `sf`.
#'
#' @param p A spatial points object or a matrix of coordinates representing
#'   points
#' @param pd Optional spatial points object objects representing
#'   destinations.
#'   `pd` is ignored if `p` is a matrix.
#'    If `pd` is not provided, `p` is used as the destination points.
#' @param interzone_only Should the result only include interzonal OD pairs, in
#'   which the ID of the origin is different from the ID of the destination
#'   zone? `FALSE` by default
#' @param ids_only Should a data frame with only 2 columns (origin and
#'   destination IDs) be returned? The default is `FALSE`, meaning the result
#'   should also contain the coordinates of the start and end points of each OD
#'   pair.
#' @param max_dist Numeric, maximum distance to consider. Default Inf.
#'   Not applicable when `p` is a matrix.
#' @param max_dest The maximum number of destinations for each origin (numeric)
#'   sorted from closest to furthest. Default is Inf. Alternative to max_dist
#'   for limiting the number of ODs.
#'   Not applicable when `p` is a matrix.
#' @export
#' @examples
#' library(sf)
#' p = od_data_centroids[1:3, ]
#' points_to_od(p)
#' points_to_od(p, ids_only = TRUE)
#' (l = points_to_odl(p, interzone_only = TRUE))
#' plot(l)
#' points_to_od(od_data_centroids[1:2, ], od_data_centroids[3, ])
#' l = points_to_odl(od_data_centroids[1:2, ], od_data_centroids[3, ])
#' plot(l)
#' (od = points_to_od(p, interzone_only = TRUE))
#' l2 = od_to_sf(od, od_data_centroids)
#' l2$v = 1
#' (l2_oneway = od_oneway(l2))
#' sf::st_length(l2)
#' # With max_dist:
#' (l3 = points_to_odl(p, max_dist = 10000))
#' sf::st_length(l3)
points_to_od = function(p, pd = NULL, interzone_only = FALSE, ids_only = FALSE,
                        max_dist = Inf, max_dest = Inf) {
  # to work with other classes at some point, possibly, it's a generic:
  UseMethod("points_to_od")
}
#' @export
points_to_od.sf = function(p, pd = NULL, interzone_only = FALSE, ids_only = FALSE,
                           max_dist = Inf, max_dest = Inf) {

  single_geometry = is.null(pd)

  if(any(duplicated(p[[1]]))) {
    warning("Duplicated ids found in first column of origins")
  }

  if(any(sf::st_geometry_type(p) != "POINT")){
    message("Converting p to centroids")
    suppressWarnings(p <- sf::st_centroid(p))
  }

  if(!single_geometry){
    if(any(duplicated(pd[[1]]))) {
      warning("Duplicated ids found in first column of destinations")
    }
    if(any(sf::st_geometry_type(p) != "POINT")){
      message("Converting pd to centroids")
      suppressWarnings(p <- sf::st_centroid(p))
    }
  }

  if(single_geometry) {
    pd = p
  }

  # Use nngeo implementation if max_dist or max_dest is provided
  if(max_dist < Inf || max_dest < Inf) {
    if(max_dest > nrow(pd)){
      max_dest = nrow(pd)
    }
  
    nn <- nngeo::st_nn(p, pd, k = max_dest, maxdist = max_dist, returnDist = FALSE,
                       progress = FALSE)
    odf = data.frame(O = rep(p[[1]], lengths(nn)),
                     D = pd[[1]][unlist(nn, use.names = FALSE)])
  } else {
    odf = data.frame(expand.grid(p[[1]], pd[[1]], stringsAsFactors = FALSE))
  }

  if(interzone_only) {
    odf = od_interzone(odf)
  }
  if(ids_only) {
    return(odf)
  }
  if(single_geometry) {
    odc = od_coordinates(odf, p)
  } else {
    odc = od_coordinates(odf, p, pd = pd)
  }
  cbind(odf, odc)
}
#' @export
points_to_od.matrix =  function(p, pd = NULL, interzone_only = FALSE, ids_only = FALSE, max_dist = NULL, max_dest = NULL) {
  coords_to_od(p, interzone_only = interzone_only, ids_only = ids_only)
}
#' @rdname points_to_od
#' @inheritParams points_to_od
#' @inheritParams odc_to_sf
#' @param ... Additional arguments passed to `points_to_od)`
#' @export
points_to_odl = function(p, pd = NULL, crs = 4326, ...) {
  odf = points_to_od(p, pd, ...)
  odc_to_sf(odf[3:6], d = odf[1:2], crs = crs)
}
#' Convert coordinates into a data frame of origins and destinations
#'
#' Takes geographic coordinates and converts them into a data frame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @inheritParams points_to_od
#' @return A data frame object with O and D codes and origin and destination coordinates.
#' @export
#' @examples
#' p = sf::st_coordinates(od_data_centroids[1:3, ])
#' od = points_to_od(p)
#' (od = coords_to_od(p, interzone_only = TRUE))
#' l = odc_to_sf(od[3:6], d = od[1:2])
#' l$v = 1
#' (l_oneway = od_oneway(l))
#' plot(l_oneway)
#' @export
coords_to_od = function(p, interzone_only = FALSE, ids_only = FALSE) {
  id = seq(nrow(p))
  odf = data.frame(expand.grid(id, id, stringsAsFactors = FALSE)[2:1])
  if(interzone_only) {
    odf = od_interzone(odf)
  }
  names(odf) = c("O", "D")
  if(ids_only) {
    return(odf)
  }
  coords_o = p[odf$O, ]
  coords_d = p[odf$D, ]
  odf = cbind(odf, coords_o, coords_d)
  names(odf)[3:6] = c("ox", "oy", "dx", "dy")
  odf
}
#' Return only interzonal (io intrazonal) OD pairs
#'
#' This function takes an OD dataset and returns only the rows
#' corresponding to movements in which the origin is different than
#' the destination.
#'
#' @inheritParams od_to_sf
#' @export
#' @examples
#'
#' od_data = points_to_od(od_data_centroids)
#' nrow(od_data)
#' nrow(od_interzone(od_data))
#' nrow(od_intrazone(od_data))
od_interzone = function(x) {
  x[x[[1]] != x[[2]], ]
}
#' @rdname od_interzone
#' @export
od_intrazone = function(x) {
  x[x[[1]] == x[[2]], ]
}

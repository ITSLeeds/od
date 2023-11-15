#' Convert a series of points into a dataframe of origins and destinations
#'
#' Takes a series of geographical points and converts them into a data.frame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' `points_to_odl()` generates the same output but returns
#' a geographic object representing desire lines in the class `sf`.
#'
#' @param p A spatial points object or a matrix of coordinates representing points
#' @param pd Optional spatial points object or matrix objects representing destinations
#' @param interzone_only Should the result only include interzonal OD pairs, in which
#' the ID of the origin is different from the ID of the destination zone?
#' `FALSE` by default
#' @param ids_only Should a data frame with only 2 columns (origin and destination IDs)
#' be returned? The default is `FALSE`, meaning the result should also contain the
#' coordinates of the start and end points of each OD pair.
#' @export
#' @examples
#' library(sf)
#' p = od_data_centroids[1:3, ]
#' points_to_od(p)
#' points_to_od(p, ids_only = TRUE)
#' (l = points_to_odl(p, interzone_only = TRUE))
#' plot(l)
#' library(sf) # for subsetting sf objects:
#' points_to_od(od_data_centroids[1:2, ], od_data_centroids[3, ])
#' l = points_to_odl(od_data_centroids[1:2, ], od_data_centroids[3, ])
#' plot(l)
#' (od = points_to_od(p, interzone_only = TRUE))
#' l2 = od_to_sf(od, od_data_centroids)
#' l2$v = 1
#' (l2_oneway = od_oneway(l2))
#' plot(l2)
points_to_od = function(p, pd = NULL, interzone_only = FALSE, ids_only = FALSE) {
  # to work with other classes at some point, possibly, it's a generic:
  UseMethod("points_to_od")
}
#' @export
points_to_od.sf = function(p, pd = NULL, interzone_only = FALSE, ids_only = FALSE) {
  single_geometry = is.null(pd)
  if(single_geometry) {
    pd = p
    ids = p[[1]]
    if(any(duplicated(ids))) {
      warning("Duplicated ids found in first column of origins")
    }
    odf = data.frame(
      stringsAsFactors = FALSE,
      expand.grid(p[[1]], pd[[1]], stringsAsFactors = FALSE)[2:1]
    )
  } else {
    ids = p[[1]]
    if(any(duplicated(ids))) {
      warning("Duplicated ids found in first column of origins")
    }
    ids = pd[[1]]
    if(any(duplicated(ids))) {
      warning("Duplicated ids found in first column of destinations")
    }
    odf = data.frame(
      stringsAsFactors = FALSE,
      expand.grid(p[[1]], pd[[1]], stringsAsFactors = FALSE)
    )
  }

  names(odf) = c("O", "D")
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
points_to_od.matrix =  function(p, pd = NULL, interzone_only = FALSE, ids_only = FALSE) {
  coords_to_od(p, interzone_only = interzone_only, ids_only = ids_only)
}
#' @rdname points_to_od
#' @inheritParams points_to_od
#' @inheritParams odc_to_sf
#' @export
points_to_odl = function(p, pd = NULL, interzone_only = FALSE, ids_only = FALSE, crs = 4326) {
  odf = points_to_od(p, pd, interzone_only, ids_only)
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
  odf = data.frame(
    stringsAsFactors = FALSE,
    expand.grid(id, id, stringsAsFactors = FALSE)[2:1]
  )
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

#' Convert a series of points into a dataframe of origins and destinations
#'
#' Takes a series of geographical points and converts them into a data.frame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p A spatial points object
#' @param interzone_only Should the result only include interzonal OD pairs, in which
#' the ID of the origin is different from the ID of the destination zone?
#' `FALSE` by default
#' @export
#' @examples
#' p = od_data_centroids[1:3, ]
#' (od1 = points_to_od(p))
#' points_to_od(p, interzone_only = TRUE)
#' od1
#' od1$v = 1
#' unique(od1)
#' od_oneway(od1)
points_to_od <- function(p, interzone_only = FALSE) {
  # to work with other classes at some point, possibly, it's a generic:
  UseMethod("points_to_od")
}
#' @export
points_to_od.sf <- function(p, interzone_only = FALSE) {
  odf <- data.frame(
    stringsAsFactors = FALSE,
    expand.grid(p[[1]], p[[1]], stringsAsFactors = FALSE)[2:1]
  )
  names(odf) <- c("O", "D")
  if(interzone_only) {
    odf = od_interzone(odf)
  }
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

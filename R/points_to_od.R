#' Convert a series of points into a dataframe of origins and destinations
#'
#' Takes a series of geographical points and converts them into a data.frame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p A spatial points object
#' @export
#' @examples
#' p = od_data_centroids[1:3, ]
#' od1 <- points_to_od(p)
#' od1
#' od1$v = 1
#' unique(od1)
#' od_oneway(od1)
points_to_od <- function(p) {
  # to work with other classes at some point, possibly, it's a generic:
  UseMethod("points_to_od")
}
#' @export
points_to_od.sf <- function(p) {
  odf <- data.frame(
    stringsAsFactors = FALSE,
    expand.grid(p[[1]], p[[1]], stringsAsFactors = FALSE)[2:1]
  )
  names(odf) <- c("O", "D")
  odf
}

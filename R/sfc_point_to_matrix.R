#' Extract coordinates from sfc objects with point geometry
#'
#' This functions takes point geometries with class `sfc` from the `sf` package
#' and returns a matrix representing x and y (typically lon/lat) coordinates.
#'
#' See https://github.com/dcooley/sfheaders/issues/52 for details
#'
#' @author Dave Cooley
#'
#' @param x An `sfc` object
#' @export
#' @examples
#' sfc_point_to_matrix(od_data_centroids$geometry[1:6])
sfc_point_to_matrix = function(x) {
  matrix(
    unlist(x, use.names = FALSE),
    nrow = length(x),
    byrow = TRUE,
    dimnames = list(1:length(x))
  )
}

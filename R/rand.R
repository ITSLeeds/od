#' Move desire line end points within zone to avoid all trips going to a single centroid
#'
#' These functions tackle the problem associated with OD data representing movement to and from large zones.
#' Typically the associated desire lines start and end in one point per zone.
#' This function produces desire lines that can start and end anywhere (or at predefined points) within each zone.
#' See [issue #11](https://github.com/ITSLeeds/od/issues/11) for details.
#'
#' @inheritParams od_disaggregate
#'
#' @return An `sf` data frame
#' @export
#'
#' @examples
od_rand = function(od, z, subpoints = NULL) {

}

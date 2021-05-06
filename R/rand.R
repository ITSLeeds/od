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
#' od = od_data_df
#' z = od_data_zones_min
#' desire_lines = od_to_sf(od, z)
od_rand = function(od, z, subpoints = NULL) {
  z_geo = sf::st_geometry(z)
  if(is.null(subpoints)) {
    suppressMessages({
      subpoints = sf::st_sample(z, size = rep(1, length(z)))
    })
  }
  if(identical(class(subpoints), c("sfc_POINT", "sfc"))) {
    subpoints = sf::st_sf(geometry = subpoints)
  }
  suppressMessages({
    subpoints_joined = sf::st_join(subpoints, z)
  })

}

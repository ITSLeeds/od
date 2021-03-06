#' Split-up each OD pair into multiple OD pairs based on subpoints/subzones
#'
#' This function is for splitting-up OD pairs.
#' It increases the number of rows in an OD dataset, while aiming
#' to keep the amount of travel represented in the data the same.
#' To take an analogy from another package, it's roughly equivalent
#' to [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
#'
#' @inheritParams od_to_sf
#' @param od An origin-destination data frame
#' @param subpoints Points within the zones defining the OD data
#' @param subzones Sub-zones within the zones defining the OD data
#' @param code_append The name of the column containing aggregate zone names
#' @param population_column The column containing the total population (if it exists)
#' @param population_per_od Minimum number to assign per OD pair
#' @param keep_ids Should the origin and destination ids be kept?
#'   `TRUE` by default, meaning 2 extra columns are appended, with the
#'   names `o_agg` and `d_agg` containing IDs from the original OD data.
#'
#' @export
#' @examples
#' od = od_data_df[1:2, ]
#' zones = od::od_data_zones_min
#' subzones = od_data_zones_small
#' od_disag = od_disaggregate(od, zones, subzones)
#' ncol(od_disag) -1 == ncol(od) # same number of columns (except disag data gained geometry)
#' sum(od_disag[[3]]) == sum(od[[3]])
#' sum(od_disag[[4]]) == sum(od[[4]])
#' od_sf = od_to_sf(od, zones)
#' plot(od_data_zones_small$geometry)
#' plot(od_data_zones_min$geometry, lwd = 3, col = NULL, add = TRUE)
#' plot(od_sf["all"], add = TRUE)
#' plot(od_disag["all"], add = TRUE)
#'
#' # with buildings data
#' od_disag_buildings = od_disaggregate(od, zones, od_data_buildings)
#' summary(od_disag_buildings)
#' plot(od_disag_buildings)
od_disaggregate = function(od,
                           z,
                           subzones = NULL,
                           subpoints = NULL,
                           code_append = "_ag",
                           population_column = 3,
                           population_per_od = 5,
                           keep_ids = TRUE) {
  # browser()

  if (is.null(subpoints)) {
    suppressWarnings({
      subpoints = sf::st_centroid(subzones)
    })
  }

  o_in_zones = od[[1]] %in% z[[1]]
  d_in_zones = od[[2]] %in% z[[1]]
  if (!all(o_in_zones) || !all(d_in_zones)) {
    stop("No matching zones associated with ",
         which(!o_in_zones | !d_in_zones))
  }

  azn = paste0(names(z)[1], code_append)
  names(z)[1] = azn
  # subpoints_joined = sf::st_join(subpoints, z[1], largest = TRUE) # for zones
  n_subpoints = nrow(subpoints)
  subpoints = subpoints[z,]
  if (nrow(subpoints) < n_subpoints) {
    message(n_subpoints - nrow(subpoints),
            " locations outside zones removed")
  }
  subpoints_joined = sf::st_join(subpoints, z[1])

  # # test which points are in there:
  # plot(z$geometry)
# plot(subpoints_joined[subpoints_joined[[azn]]%in%od$geo_code1, ], add = TRUE)
# plot(subpoints_joined[subpoints_joined[[azn]]%in%od$geo_code2, ], add = TRUE)

  # todo: convert to lapply
  i = 1
  i_seq = seq(nrow(od))

  list_new = lapply(
    X = i_seq,
    FUN = function(i) {
      print(i)
      o_new = subpoints_joined[[1]][subpoints_joined[[azn]] == od[[1]][i]]
      d_new = subpoints_joined[[1]][subpoints_joined[[azn]] == od[[2]][i]]
      od_new = expand.grid(o_new, d_new, stringsAsFactors = FALSE)
      names(od_new) = c("o", "d")
      max_n_od = ceiling(od[[population_column]][i] / population_per_od)
      if (nrow(od_new) > max_n_od) {
        od_new = od_new[sample(nrow(od_new), size = max_n_od),]
      }
      od_new_attribute_list = lapply(od[i,-c(1, 2)], function(x)
        x / nrow(od_new))
      od_new_attributes = as.data.frame(od_new_attribute_list)[rep(1, nrow(od_new)), , drop = FALSE]
      # od_new_attributes[] = apply(od_new_attributes, 2, function(x) x + stats::runif(nrow(od_new), -0.4, 0.4))
      od_new_attributes[] = apply(od_new_attributes, 2, function(x)
        smart.round(x))
      od_new = cbind(od_new, od_new_attributes)
      if(keep_ids) {
        od_new$o_agg = od[[1]][i]
        od_new$d_agg = od[[2]][i]
      }
      od_new_sf = od::od_to_sf(od_new, subpoints_joined, silent = TRUE)

    }
  )

  # todo: could be sped-up
  od_new_sf = do.call(rbind, list_new)

  # output od data with same number of columns but more rows
  od_new_sf
}

smart.round <- function(x) {
  y <- floor(x)
  indices <- utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

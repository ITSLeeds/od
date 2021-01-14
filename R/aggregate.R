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
od_disaggregate = function(od, z, subzones = NULL, subpoints = NULL, code_append = "_ag", population_column = 3, population_per_od = 5) {

  if(is.null(subpoints)) {
    suppressWarnings({
      subpoints = sf::st_centroid(subzones)
    })
  }

  azn = paste0(names(z)[1], code_append)
  names(z)[1] = azn
  # subpoints_joined = sf::st_join(subpoints, z[1], largest = TRUE) # for zones
  subpoints_joined = sf::st_join(subpoints, z[1])

  # test which points are in there:
  # plot(z$geometry)
  # plot(subpoints_joined[ subpoints_joined$geo_code_ag == od$geo_code1, ], add = TRUE)
  # plot(subpoints_joined[ subpoints_joined$geo_code_ag == od$geo_code2, ], add = TRUE)

  # todo: convert to lapply
  i = 1
  i_seq = seq(nrow(od))
  # for(i in i_seq) {
  #   o_new = subpoints_joined[[1]][ subpoints_joined[[azn]] == od[[1]][i] ]
  #   d_new = subpoints_joined[[1]][ subpoints_joined[[azn]] == od[[2]][i] ]
  #   od_new = expand.grid(o_new, d_new)
  #   names(od_new) = c("o", "d")
  #   od_new_attribute_list = lapply(od[i, -c(1, 2)], function(x) x/nrow(od_new))
  #   od_new_attributes = as.data.frame(od_new_attribute_list)[rep(1, nrow(od_new)), ]
  #   od_new = cbind(od_new, od_new_attributes)
  #   # colSums(od_new[-c(1, 2)]) == colSums(od[i, -c(1, 2)]) # totals add up!
  #   od_new_sf = od_to_sf(od_new, subpoints)
  #   # mapview::mapview(od_new_sf) + mapview::mapview(od_to_sf(od, z)[1, ])
  # }

  list_new = lapply(X = i_seq, FUN = function(i) {
    browser()
    o_new = subpoints_joined[[1]][ subpoints_joined[[azn]] == od[[1]][i] ]
    d_new = subpoints_joined[[1]][ subpoints_joined[[azn]] == od[[2]][i] ]
    od_new = expand.grid(o_new, d_new, stringsAsFactors = FALSE)
    names(od_new) = c("o", "d")
    max_n_od = od[[3]][i] / population_per_od
    if(nrow(od_new) > max_n_od) {
      od_new = od_new[sample(nrow(od_new), size = max_n_od), ]
    }
    od_new_attribute_list = lapply(od[i, -c(1, 2)], function(x) x/nrow(od_new))
    od_new_attributes = as.data.frame(od_new_attribute_list)[rep(1, nrow(od_new)), ]
    od_new_attributes[] = lapply(od_new_attributes, function(x) x + runif(nrow(od_new), -0.4, 0.4))
    od_new_attributes[] = lapply(od_new_attributes, function(x) smart.round(x) )
    od_new = cbind(od_new, od_new_attributes)
    od_new_sf = od::od_to_sf(od_new, subpoints, silent = TRUE)
  })

  # todo: could be sped-up
  od_new_sf = do.call(rbind, list_new)

  # output od data with same number of columns but more rows
  od_new_sf
}

smart.round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

#' Split-up each OD pair into multiple OD pairs based on subpoints/subzones
#'
#' This function is for splitting-up OD pairs.
#' It increases the number of rows in an OD dataset, while aiming
#' to keep the amount of travel represented in the data the same.
#' To take an analogy from another package, it's roughly equivalent
#' to [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
#'
#' An alias for the function is `od_split()`.
#'
#' @inheritParams od_to_sf
#' @param od An origin-destination data frame
#' @param subpoints Points within the zones defining the OD data
#' @param subzones Sub-zones within the zones defining the OD data
#' @param code_append The name of the column containing aggregate zone names
#' @param population_column The column containing the total population (if it exists)
#' @param population_per_od Maximum flow in the population_column to assign per OD pair. This only comes into effect if there are enough subpoints to choose from.
#' @param keep_ids Should the origin and destination ids be kept?
#'   `TRUE` by default, meaning 2 extra columns are appended, with the
#'   names `o_agg` and `d_agg` containing IDs from the original OD data.
#' @param integer_outputs Should integer outputs be returned? `FALSE` by default.
#'   Note: there is a known issue when integer results are generated. See
#'   https://github.com/ITSLeeds/od/issues/31 for details.
#'
#' @export
#' @examples
#' od = od_data_df[1:2, ]
#' zones = od::od_data_zones_min
#' subzones = od_data_zones_small
#' od_disag = od_disaggregate(od, zones, subzones)
#' ncol(od_disag) -3 == ncol(od) # same number of columns
#' # (except disag data gained geometry and new agg ids)
#' sum(od_disag[[3]]) == sum(od[[3]])
#' sum(od_disag[[4]]) == sum(od[[4]])
#' # integer results
#' od_disag_integer = od_disaggregate(od, zones, subzones)
#' plot(rowSums(sf::st_drop_geometry(od_disag)[4:10]), od_disag[[3]])
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
                           keep_ids = TRUE,
                           integer_outputs = FALSE
                           ) {

  if (is.null(subpoints)) {
    message("Converting subzones to centroids")
    suppressWarnings({
      subpoints = sf::st_centroid(subzones)
    })
  }

  # is the input data an sf object? tell the user and convert to df if so
  if(methods::is(object = od, class2 = "sf")) {
    message("Input object is sf, attempting to convert to a data frame")
    od = sf::st_drop_geometry(od)
  }

  # detect and deal with non numeric inputs
  column_classes = sapply(X = od, FUN = function(x) class(x)[1])
  column_classes_data = column_classes[3:length(column_classes)]
  col_is_numeric = column_classes_data == "numeric"
  all_numeric = all(col_is_numeric)
  if(!all_numeric) {
    names_od = names(od)[3:length(column_classes)]
    names_readable = paste0(names_od[!col_is_numeric], collapse = ", ")
    message("Attempting to remove non-numeric columns: ", names_readable)
    od = od[setdiff(names(od), names_readable)]
  }

  o_in_zones = od[[1]] %in% z[[1]]
  d_in_zones = od[[2]] %in% z[[1]]
  if (!all(o_in_zones) || !all(d_in_zones)) {
    stop("No matching zones associated with ",
         which(!o_in_zones | !d_in_zones))
  }

  azn = paste0(names(z)[1], code_append)
  names(z)[1] = azn
  n_subpoints = nrow(subpoints)
  subpoints = subpoints[z,]
  if (nrow(subpoints) < n_subpoints) {
    message(n_subpoints - nrow(subpoints),
            " locations outside zones removed")
  }
  subpoints_joined = sf::st_join(subpoints, z[1])

  # i = 1 # for debugging
  i_seq = seq(nrow(od))
  list_new = lapply(
    X = i_seq,
    FUN = function(i) {
      o_new = subpoints_joined[[1]][subpoints_joined[[azn]] == od[[1]][i]]
      d_new = subpoints_joined[[1]][subpoints_joined[[azn]] == od[[2]][i]]
      od_new = expand.grid(o_new, d_new, stringsAsFactors = FALSE)
      names(od_new) = c("o", "d")
      max_n_od = ceiling(od[[population_column]][i] / population_per_od)
      if (nrow(od_new) > max_n_od) {
        od_new = od_new[sample(nrow(od_new), size = max_n_od),]
      }
      # new attributes
      odn_list = lapply(od[i, -c(1, 2)], function(x)
        x / nrow(od_new))
      odns = as.data.frame(odn_list)[rep(1, nrow(od_new)), , drop = FALSE]
      browser()
      if(integer_outputs) {
        odns[] = apply(odns, 2, function(x) smart.round(x))
      }
      od_new = cbind(od_new, odns)
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

#' Aggregate od pairs based on aggregating zones
#'
#' This function is for aggregating OD pairs.
#' It generally decreases the number of rows in an OD dataset, while aiming
#' to keep the amount of travel represented in the data the same.
#'
#' An alias for the function is `od_group()`.
#'
#' @param od An origin-destination data frame
#' @param aggzones Points within the zones defining the OD data
#' @param FUN The aggregating function to use
#'
#' @export
#' @examples
#' od_aggregated = od_data_df[1:2, ]
#' aggzones = od::od_data_zones_min
#' subzones = od_data_zones_small
#' plot(aggzones$geometry)
#' plot(subzones$geometry, add = TRUE)
#' od = od_disaggregate(od_aggregated, aggzones, subzones)
#' od_agg = od_aggregate(od, aggzones)
#' names(od_agg)[1:(ncol(od_agg) - 1)] = names(od_aggregated)
#' attr(od_aggregated, "spec") = NULL
#' identical(sf::st_drop_geometry(od_agg), od_aggregated)
od_aggregate = function(od,
                        aggzones = NULL,
                        FUN = sum) {
  requireNamespace("lwgeom", quietly = TRUE)

  o = sf::st_sf(geometry = lwgeom::st_startpoint(od))
  d = sf::st_sf(geometry = lwgeom::st_endpoint(od))
  odf = sf::st_drop_geometry(od)
  odf$o_disagg = od[[1]]
  odf$d_disagg = od[[2]]
  odf[[1]] = sf::st_join(o, aggzones[1])[[1]]
  odf[[2]] = sf::st_join(d, aggzones[1])[[1]]

  numeric_columns = sapply(odf, is.numeric)

  oda = stats::aggregate(odf[numeric_columns], list(odf[[1]], odf[[2]]), FUN)

  od_new_sf = od_to_sf(oda, z = aggzones)
}

#' @export
#' @rdname od_aggregate
od_group = od_aggregate


#' @export
#' @rdname od_disaggregate
od_split = od_disaggregate

smart.round = function(x) {
  y = floor(x)
  indices = utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y
}

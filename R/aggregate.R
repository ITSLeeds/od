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
#' @param subpoints Points or linestrings within the zones defining the OD data start/end points
#' @param subzones Sub-zones within the zones defining the OD data
#' @param code_append The name of the column containing aggregate zone names
#' @param population_column The column containing the total population (if it exists)
#' @param max_per_od Maximum flow in the population_column to assign per OD pair.
#'   This only comes into effect if there are enough subpoints to choose from.
#' @param keep_ids Should the origin and destination ids be kept?
#'   `TRUE` by default, meaning 2 extra columns are appended, with the
#'   names `o_agg` and `d_agg` containing IDs from the original OD data.
#' @param integer_outputs Should integer outputs be returned? `FALSE` by default.
#'   Note: there is a known issue when integer results are generated. See
#'   https://github.com/ITSLeeds/od/issues/31 for details.
#'
#' @export
#' @examples
#' od = od_data_df[1:2, c(1, 2, 9)]
#' od
#' zones = od::od_data_zones_min
#' od_sf = od_to_sf(od, zones)
#' set.seed(2021) # for reproducibility
#' od_disag = od_disaggregate(od, zones)
#' od_disag2 = od_disaggregate(od, zones, max_per_od = 11)
#' plot(zones$geometry)
#' plot(od_sf$geometry, lwd = 9, add = TRUE)
#' plot(od_disag$geometry, col = "grey", lwd = 1, add = TRUE)
#' plot(od_disag2$geometry, col = "green", lwd = 1, add = TRUE)
#' table(od_disag$o_agg, od_disag$d_agg)
#' # integer results
#' od_disaggregate(od, zones, integer_outputs = TRUE)
#'
#' # with more trips per disaggregated OD pair:
#' disag = od_disaggregate(od_data_df[1:2, ], z = zones, max_per_od = 50)
#' plot(disag[0])
#'
#' # with subpoints
#' subpoints = sf::st_sample(zones, 1000)
#' od_disag_subpoints = od_disaggregate(od, zones, subpoints = subpoints)
#'
#' # with buildings data
#' od_disag_buildings = od_disaggregate(od, zones, od_data_buildings)
#' summary(od_disag_buildings)
#' plot(od_disag_buildings)
#' # mapview::mapview(od_disag_buildings)
#'
#' od = od_data_df[1:2, 1:4]
#' subzones = od_data_zones_small
#' try(od_disaggregate(od, zones, subzones))
#' od_disag = od_disaggregate(od, zones, subzones, max_per_od = 500)
#' ncol(od_disag) -3 == ncol(od) # same number of columns, the same...
#' # Except disag data gained geometry and new agg ids:
#' sum(od_disag[[3]]) == sum(od[[3]])
#' sum(od_disag[[4]]) == sum(od[[4]])
#' plot(od_disag)
#' # test with road network dataset (don't run as time consuming):
#' \dontrun{
#' od_disag_net = od_disaggregate(od, zones, od_road_network, max_per_od = 500)
#' plot(zones$geometry)
#' plot(od_road_network$geometry, add = TRUE, col = "green")
#' plot(od_disag_net$geometry, add = TRUE)
#' mapview::mapview(zones) + od_disag_net
#' }
od_disaggregate = function(od,
                           z,
                           subzones = NULL,
                           subpoints = NULL,
                           code_append = "_ag",
                           population_column = 3,
                           max_per_od = 5,
                           keep_ids = TRUE,
                           integer_outputs = FALSE
                           ) {

  nrow_od_disag = od_nrows(od, population_column, max_per_od)
  nr = sum(nrow_od_disag)
  azn = paste0(names(z)[1], code_append)

  # is the input od data an sf object? tell the user and convert to df if so
  if(methods::is(object = od, class2 = "sf")) {
    message("Input object is sf, attempting to convert to a data frame")
    od = sf::st_drop_geometry(od)
  }

  if (is.null(subpoints) && is.null(subzones)) {
    message("Creating randomly sampled origin and destination points.")
    # from jitter.R
    # todo: consider splitting this out into new function
    od_disag_indices = rep(x = seq(nrow(od)), nrow_od_disag)
    od_disag_ids = od[od_disag_indices, c(1:2)]
    id_zones = c(od_disag_ids[[1]], od_disag_ids[[2]])
    points_per_zone = data.frame(table(id_zones))
    z = z[z[[1]] %in% points_per_zone[[1]], ]
    freq = points_per_zone$Freq
    subpoints_df = data.frame(id = as.character(seq(nr * 2)), azn = sort(id_zones))
    names(subpoints_df)[2] = azn
    subpoints = sf::st_sf(subpoints_df, geometry = sf::st_sample(z, freq))
  }

  if (is.null(subpoints) && !is.null(subzones)) {
    message("Converting subzones/lines to points")
    geomtype = as.character(unique(sf::st_geometry_type(subzones)))
    if (methods::is(subzones, "sf") && geomtype == "LINESTRING") {
      subpoints = od_sample_vertices(subzones)
      subzones = NULL
    } else {
      suppressWarnings({
        subpoints = sf::st_centroid(subzones)
      })
    }
  }

  # if subpoints is an sf object with class linestring
  geomtype = as.character(unique(sf::st_geometry_type(subpoints)))
  if (methods::is(subpoints, "sf") && geomtype == "POINT") {
    subpoints = od_sample_vertices(subpoints)
  }

  if (methods::is(subpoints, "sfc")) {
    if(length(subpoints) > nr * 2) {
      # from jitter.R
      # todo: consider splitting this out into new function
      od_disag_indices = rep(x = seq(nrow(od)), nrow_od_disag)
      od_disag_ids = od[od_disag_indices, c(1:2)]
      id_zones = c(od_disag_ids[[1]], od_disag_ids[[2]])
      points_per_zone = data.frame(table(id_zones))
      z = z[z[[1]] %in% points_per_zone[[1]], ]
      freq = points_per_zone$Freq
      subpoints_df = data.frame(id = as.character(seq(nr * 2)), azn = sort(id_zones))
      subpoints_joined = sf::st_join(sf::st_sf(subpoints), z[1])
      sel_list = lapply(1:nrow(points_per_zone), function(i) {
        sample(
          which(subpoints_joined[[1]] == points_per_zone[[1]][i]),
          size = points_per_zone[[2]][i]
          )
      })
      sel = unlist(sel_list)
      names(subpoints_df)[2] = azn
      subpoints = sf::st_sf(subpoints_df, geometry = subpoints[sel])
    } else {
      suppressMessages({
        subpoints = sf::st_sf(
          id = as.character(seq(nr * 2)),
          geometry = subpoints
        )
      })
    }
  }

  # detect and deal with non numeric inputs
  col_classes = sapply(X = od, FUN = function(x) class(x)[1])
  col_classes_data = col_classes[3:length(col_classes)]
  numeric_col_names = names(col_classes_data)[col_classes_data == "numeric"]
  if (length(numeric_col_names) < ncol(od) - 2) {
    message("Keeping only numeric columns: ", numeric_col_names)
    od = od[c(names(od)[1:2], numeric_col_names)]
  }

  o_in_zones = od[[1]] %in% z[[1]]
  d_in_zones = od[[2]] %in% z[[1]]
  if (!all(o_in_zones) || !all(d_in_zones)) {
    stop("No matching zones associated with ",
         which(!o_in_zones | !d_in_zones))
  }

  # If subpoints are not generated by the function:
  if (!azn %in% names(subpoints)) {
    suppressMessages({
      subpoints = subpoints[z,]
    })
    if (nrow(subpoints) < nrow(subpoints)) {
      message(nrow(subpoints) - nrow(subpoints),
              " locations outside zones removed")
    }
    names(z)[1] = azn
    suppressMessages({
      subpoints = sf::st_join(subpoints, z[1])
    })
  }

  # i = 1 # uncomment for debugging
  i_seq = seq(nrow(od))
  list_new = lapply(
    X = i_seq,
    FUN = function(i) {
      max_n_od = ceiling(od[[population_column]][i] / max_per_od)
      o_options = subpoints[[1]][subpoints[[azn]] == od[[1]][i]]
      d_options = subpoints[[1]][subpoints[[azn]] == od[[2]][i]]
      if(max_n_od > length(o_options) || max_n_od > length(d_options)) {
        warning("Insufficient subzones/points to prevent duplicate desire lines")
        message("Sampling may fail. Try again with larger max_per_od")
      }
      o = sample(o_options, size = max_n_od, replace = FALSE)
      d = sample(d_options, size = max_n_od, replace = FALSE)
      od_new = data.frame(o, d)
      # new attributes
      odn_list = lapply(od[i, -c(1, 2)], function(x) x / nrow(od_new))
      odns = as.data.frame(odn_list)[rep(1, nrow(od_new)), , drop = FALSE]
      names(odns) = numeric_col_names
      if(integer_outputs) {
        odns[] = apply(odns, 2, function(x) smart.round(x))
      }
      od_new = cbind(od_new, odns)
      if(keep_ids) {
        od_new$o_agg = od[[1]][i]
        od_new$d_agg = od[[2]][i]
      }
      od_new_sf = od::od_to_sf(od_new, subpoints, silent = TRUE)
      # Remove sampled points from 'universe' of available points
      if(i < nrow(od)) {
        subpoints <<- subpoints[!subpoints[[1]] %in% c(o, d), ]
      }
      od_new_sf
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
#' od_aggregated = od_data_df[1:2, c(1, 2, 9)]
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

#' Create a subsample of points from a route network for jittering
#'
#' Todo: export this at some point
#'
#' @param x An sf object representing a road network
#' @param fraction What percent of the network to sample?
#' @examples
#' \dontrun{
#' u = "https://github.com/ITSLeeds/od/releases/download/v0.3.1/road_network_min.Rds"
#' f = basename(u)
#' if(!file.exists(f)) download.file(u, f)
#' road_network_min = readRDS(f)
#' od_sample_vertices(road_network_min)
#' }
od_sample_vertices = function(x, fraction = 1) {
  suppressWarnings({
    x_point = sf::st_cast(x$geometry, "POINT")
  })
  if (fraction == 1) {
    sel = sample(length(x_point), size = length(x_point) * fraction)
    x_point = x_point[sel]
  }
  x_point
}

od_nrows = function(od, population_column, max_per_od) {
  ceiling(od[[population_column]] / max_per_od)
}

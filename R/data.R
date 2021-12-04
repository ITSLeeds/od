#' Example OD data
#'
#' Zone datasets for packages examples
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_zones
#' @aliases od_data_csa_zones od_data_zones_min
NULL
#' Origin-destination datasets
#'
#' Datasets representing top commuter desire lines
#' in Leeds based on the 2011 Census.
#' The first two variables of the data frame are the zone code of origin and destination, respectively.
#' The other columns record the number of people who travel by different modes, including
#' `all`, `train`, `bus`, `bicycle` and by `foot`.
#'
#' `od_data_df_medium` is a larger dataset with the same variables, with around 10k rows.
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_df
#' @aliases od_data_df_medium
#' @examples
#' od_data_df
NULL

#' Datasets reporesenting zone centroids
#'
#' These are provided as a geographic (sf) object and a simple
#' data frame with longitude (`X`) and latitude (`Y`) columns.
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_centroids
#' @aliases od_data_coordinates
#' @examples
#' head(od_data_coordinates)
NULL

#' Route network data for Leeds
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_network
#' @examples
#' head(od_data_network)
NULL

#' Origin-destination data with destinations in a different layer than origins
#'
#' This dataset represents commuter flows between Output Areas
#' and Workplace Zones, the most detailed open OD data in the UK.
#' See https://wicid.ukdataservice.ac.uk/ and the script
#' `data-raw/od_wpz.R` in the `od` package's GitHub repo.
#'
#' The dataset reports (in the 3rd column) the number of people travelling
#' between origins and destinations.
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_df2
#' @examples
#' head(od_data_df2)
NULL

#' Output area centroids
#'
#' This dataset represents geographic centroids of Output Areas in Leeds, UK.
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_centroids2
#' @examples
#' head(od_data_centroids2)
NULL

#' Residential (origin) 'Output Area' centroids
#'
#' This dataset represents geographic centroids of Output Areas in Leeds, UK.
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_centroids2
#' @examples
#' head(od_data_centroids2)
NULL

#' Workplace zone (destination) centroids
#'
#' This dataset represents geographic centroids of Output Areas in Leeds, UK.
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_destinations
#' @examples
#' nrow(od_data_destinations)
#' head(od_data_destinations)
NULL

#' Small zones dataset
#'
#' This dataset represents geographic zones of Lower Super Output Areas in Leeds, UK.
#' They fit completely within the `od_data_zones_min` dataset.
#'
#' @note The schema data can be (re-)generated using code in the
#' `data-raw` directory.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_zones_small
#' @examples
#' nrow(od_data_zones_small)
#' head(od_data_zones_small)
#' plot(od_data_zones_small$geometry)
#' plot(od_data_zones_min$geometry, lwd = 3, col = NULL, add = TRUE)
NULL

#' Simple buildings dataset
#'
#' Building data from OSM for testing od_disaggregate.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_buildings
#' @examples
#' nrow(od_data_buildings)
#' head(od_data_buildings)
#' plot(od_data_buildings$geometry)
#' plot(od_data_zones_min$geometry, lwd = 3, col = NULL, add = TRUE)
NULL

#' Simple road network dataset
#'
#' Road network data from OSM for testing od_disaggregate.
#'
#' @docType data
#' @keywords datasets
#' @name od_road_network
#' @examples
#' library(sf)
#' nrow(od_road_network)
#' head(od_road_network)
#' plot(od_road_network$geometry)
#' plot(od_data_zones_min$geometry, lwd = 3, col = NULL, add = TRUE)
NULL

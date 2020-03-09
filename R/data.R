#' Example od data
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
#' od_data_centroids
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
#' od_data_network
#' head(od_data_network)
NULL

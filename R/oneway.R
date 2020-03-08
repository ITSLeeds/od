#' Generate ordered ids of OD pairs so lowest is always first
#' This function is slow on large datasets, see szudzik_pairing for faster alternative
#'
#' @param x A data frame representing OD pairs
#' @param id1 Optional (it is assumed to be the first column)
#' text string referring to the name of the variable containing
#' the unique id of the origin
#' @param id2 Optional (it is assumed to be the second column)
#' text string referring to the name of the variable containing
#' the unique id of the destination
#' @examples
#' x = data.frame(id1 = c(1, 1, 2, 2, 3), id2 = c(1, 2, 3, 1, 4))
#' od_id_order(x) # 4th line switches id1 and id2 so oneway_key is in order
#' @export
od_id_order = function(x, id1 = names(x)[1], id2 = names(x)[2]) {
  data.frame(
    stringsAsFactors = FALSE,
    stplanr.id1 = x[[id1]],
    stplanr.id1 = x[[id2]],
    oneway_key = od_id_character(x[[id1]], x[[id2]])
  )
}
#' Combine two ID values to create a single ID number
#'
#' @details
#' In OD data it is common to have many 'oneway' flows from "A to B" and "B to A".
#' It can be useful to group these an have a single ID that represents pairs of IDs
#' with or without directionality, so they contain 'twoway' or bi-directional values.
#'
#' `od_id*` functions take two vectors of equal length and return a vector of IDs,
#' which are unique for each combination but the same for twoway flows.
#'
#' -  the Szudzik pairing function, on two vectors of equal
#' length. It returns a vector of ID numbers.
#'
#' This function superseeds od_id_order as it is faster on large datasets
#' @param x a vector of numeric, character, or factor values
#' @param y a vector of numeric, character, or factor values
#' @param ordermatters logical, does the order of values matter to pairing, default = FALSE
#' @family od
#' @seealso od_oneway
#' @name od_id
#' @examples
#' (d = od_data_df[2:9, 1:2])
#' (id = od_id_character(d[[1]], d[[2]]))
#' duplicated(id)
#' od_id_szudzik(d[[1]], d[[2]])
#' od_id_max_min(d[[1]], d[[2]])
#' n = 100
#' ids = as.character(runif(n, 1e4, 1e7 - 1))
#' # benchmark of methods:
#' x = data.frame(
#'   id1 = rep(ids, times = n),
#'   id2 = rep(ids, each = n),
#'   val = 1,
#'   stringsAsFactors = FALSE
#' )
#' bench::mark(
#'   check = FALSE, iterations = 10,
#'   od_id_order(x),
#'   od_id_character(x$id1, x$id2),
#'   od_id_szudzik(x$id1, x$id2),
#'   od_id_max_min(x$id1, x$id2)
#' )
NULL
#' @rdname od_id
#' @export
od_id_szudzik = function(x, y, ordermatters = FALSE) {
  if (length(x) != length(y)) {
    stop("x and y are not of equal length")
  }

  if (class(x) == "factor") {
    x = as.character(x)
  }
  if (class(y) == "factor") {
    y = as.character(y)
  }
  lvls = unique(c(x, y))
  x = as.integer(factor(x, levels = lvls))
  y = as.integer(factor(y, levels = lvls))
  if (ordermatters) {
    ismax = x > y
    oneway_key = (ismax * 1) * (x^2 + x + y) + ((!ismax) * 1) * (y^2 + x)
  } else {
    a = ifelse(x > y, y, x)
    b = ifelse(x > y, x, y)
    oneway_key = b^2 + a
  }
  return(oneway_key)
}
#' @export
#' @rdname od_id
od_id_max_min = function(x, y) {
  d = convert_to_numeric(x, y)
  a = pmax(d$x, d$y)
  b = pmin(d$x, d$y)
  a * (a + 1) / 2 + b
}

#' @export
#' @rdname od_id
od_id_character = function(x, y) {
  paste(
    pmin(x, y),
    pmax(x, y)
  )
}

convert_to_numeric = function(x, y) {
  if (length(x) != length(y)) stop("x and y are not of equal length")
  if (class(x) == "factor") x = as.character(x)
  if (class(y) == "factor") y = as.character(y)
  lvls = unique(c(x, y))
  x = as.integer(factor(x, levels = lvls))
  y = as.integer(factor(y, levels = lvls))
  list(x = x, y = y)
}

od_id_order_base = function(x, y) {
  d = convert_to_numeric(x, y)
  x = d$x
  y = d$y
  paste(pmin(x, y), pmax(x, y))
}

not_duplicated = function(x) {
  !duplicated(x)
}

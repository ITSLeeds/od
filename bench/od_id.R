n = 100
ids = as.character(runif(n, 1e4, 1e7 - 1))
# benchmark of methods:
x = data.frame(
  id1 = rep(ids, times = n),
  id2 = rep(ids, each = n),
  val = 1,
  stringsAsFactors = FALSE
)
bench::mark(
  check = FALSE, iterations = 10,
  od_id_order(x),
  od_id_character(x$id1, x$id2),
  od_id_szudzik(x$id1, x$id2),
  od_id_max_min(x$id1, x$id2)
)

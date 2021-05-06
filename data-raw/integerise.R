smart.round = function(x) {
  y = floor(x)
  indices = utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y
}

smart.round.df = function(d) {
  data.frame(apply(d, 2, function(x) smart.round(x)))
}

d = data.frame(
  all = 48.3, train = 0.7, bus = 7.65, taxi = 0.7,
  car_driver = 3.45, car_passenger = 0.9, bicycle = 0.65, foot = 33.95

)

res = smart.round.df(d)
rowSums(res[, 1])


library(od)
od = od_data_df[1:2, ]
zones = od::od_data_zones_min
subzones = od_data_zones_small
od_disag = od_disaggregate(od, zones, subzones)
#> Converting subzones to centroids
#> although coordinates are longitude/latitude, st_intersects assumes that they are planar
#> although coordinates are longitude/latitude, st_intersects assumes that they are planar
#> although coordinates are longitude/latitude, st_intersects assumes that they are planar
#> although coordinates are longitude/latitude, st_intersects assumes that they are planar
ncol(od_disag) -1 == ncol(od) # same number of columns (except disag data gained geometry)
#> [1] FALSE
sum(od_disag[[3]]) == sum(od[[3]])
#> [1] TRUE
sum(od_disag[[4]]) == sum(od[[4]])
#> [1] TRUE
plot(rowSums(sf::st_drop_geometry(od_disag)[4:10]), od_disag[[3]])

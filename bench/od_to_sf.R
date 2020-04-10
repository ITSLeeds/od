nrow(od_data_df)
bench::mark(check = FALSE, max_iterations = 100,
            stplanr = stplanr::od2line(od_data_df, od_data_zones),
            od = od_to_sfc(od_data_df, od_data_zones),
            od_sf1 = od_to_sf(od_data_df, od_data_zones),
            od_sf2 = od_to_sf(od_data_df, od_data_zones, package = "sf", crs = 4326)
)

bench::mark(check = FALSE, max_iterations = 100,
            stplanr_centroids = stplanr::od2line(od_data_df, od_data_centroids),
            od_sf3 = od_to_sf(od_data_df, od_data_centroids)
            # od_sf4 = od_to_sf(od_data_df, centroids, package = "sf", crs = 4326)
)

### Benchmark on medium-sized dataset

nrow(od_data_df_medium)
bench::mark(check = FALSE, max_iterations = 100,
            stplanr = stplanr::od2line(od_data_df_medium, od_data_zones),
            od = od_to_sfc(od_data_df_medium, od_data_zones),
            od_sf1 = od_to_sf(od_data_df_medium, od_data_zones),
            od_sf2 = od_to_sf(od_data_df_medium, od_data_zones, package = "sf", crs = 4326)
)

bench::mark(check = FALSE, max_iterations = 100,
            stplanr_centroids = stplanr::od2line(od_data_df_medium, od_data_centroids),
            od_sf3 = od_to_sf(od_data_df_medium, od_data_centroids)
            # od_sf4 = od_to_sf(od_data_df_medium, centroids, package = "sf", crs = 4326)
)

### Benchmark using low-level functions and made-up data

sf_internal = function(x) {
  matrix(
    unlist(x, use.names = FALSE),
    nrow = length(x),
    byrow = TRUE,
    dimnames = list(1:length(x))
  )
}

n = 1e5
df = data.frame(x = rnorm(n),
                y = rnorm(n))

pts = sfheaders::sf_point(obj = df)

sf = sf::st_coordinates(pts)
sfh = sfheaders::sf_to_df(pts)
sfi = sf_internal(pts$geometry)
head(sf)
head(sfh)
head(sfi)

all.equal(unname(as.matrix(sfh[, c("x", "y")])), unname(sf))
all.equal(unname(sfi), unname(sf))

res = bench::press(
  rows = 10 ^ (1:5),
  bench::mark(
    check = FALSE,
    sf = sf::st_coordinates(pts[1:rows, ]),
    sfh = sfheaders::sf_to_df(pts[1:rows, ]),
    sfi = sf_internal(pts[1:rows, ])
  )
)

res
# ggplot2::autoplot(res)


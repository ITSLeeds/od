
<!-- README.md is generated from README.Rmd. Please edit that file -->

# od

<!-- badges: start -->

<!-- badges: start -->

[![Travis build
status](https://img.shields.io/travis/robinlovelace/od/master?logo=travis&style=flat-square&label=Linux)](https://travis-ci.org/robinlovelace/od)
<!-- [![AppVeyor build status](https://img.shields.io/appveyor/ci/robinlovelace/od?label=Windows&logo=appveyor&style=flat-square)](https://ci.appveyor.com/project/robinlovelace/od) -->
<!-- [![CircleCI](https://img.shields.io/circleci/build/gh/robinlovelace/od/master?label=Linux&logo=circle&logoColor=green&style=flat-square)](https://circleci.com/gh/robinlovelace/od) -->
[![CRAN
status](https://www.r-pkg.org/badges/version/tic)](https://cran.r-project.org/package=tic)
<!-- [![codecov](https://codecov.io/gh/robinlovelace/od/branch/master/graph/badge.svg)](https://codecov.io/gh/robinlovelace/od) -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

<!-- badges: end -->

The goal of od is to provide tools and example datasets for working with
origin-destination (OD)
datasets.

## Installation

<!-- You can install the released version of od from [CRAN](https://CRAN.R-project.org) with: 

``` r
install.packages("od")
```

And the development version from [GitHub](https://github.com/) with:

-->

``` r
# install.packages("devtools")
devtools::install_github("robinlovelace/od")
```

## Motivation

The package originated as a set of functions in the package `stplanr`
for working with origin-destination data. The `od2line()` function, for
example, takes a data frame and a spatial object as inputs and outputs
geographic lines representing movement between origins and destinations:

``` r
library(od) # load example datasets
od_data_df # OD data as data frame
#>   geo_code1 geo_code2  all train bus taxi car_driver car_passenger bicycle foot
#> 1 E02002363 E02006875  922     5 356    7        375            76      43   53
#> 2 E02002373 E02006875 1037   111 424   20        155            30      73  214
#> 3 E02002385 E02006875  958   121 334   19        118            25      52  283
#> 4 E02006852 E02002392  437     1  96    3        142            26      61  108
#> 5 E02006852 E02006875 1221    14 509   13        401            50      99  118
#> 6 E02006861 E02006875 1177    43 400   30        123            28      56  492
od_data_centroids[1:2, ]
#>     geo_code             geometry
#> 22 E02002407 -1.609934, 53.790790
#> 27 E02002336   -1.62463, 53.88605
desire_lines_stplanr = stplanr::od2line(od_data_df, od_data_centroids)
desire_lines_stplanr[1:2, 1:9]
#> Simple feature collection with 2 features and 9 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.581773 ymin: 53.79593 xmax: -1.534957 ymax: 53.82859
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#>   geo_code1 geo_code2  all train bus taxi car_driver car_passenger bicycle
#> 1 E02002363 E02006875  922     5 356    7        375            76      43
#> 2 E02002373 E02006875 1037   111 424   20        155            30      73
#>                         geometry
#> 1 LINESTRING (-1.534957 53.82...
#> 2 LINESTRING (-1.581773 53.81...
```

It works great, and is plenty fast enough for most applications, but
there are some issues with `stplanr::od2line()` (which also affect the
other `od_*()` functions in `stplanr`):

  - The function is a commonly needed and low-level function, buried in
    a large package, reducing ‘findability’
  - To get the function you must install `stplanr` plus its numerous
    dependencies
  - The function has not been optimised
  - It has no class definition of ‘od’ data

The `od` package, as it currently stands, addresses the first three of
these issues (it may at some point define a class for `od` objects but
there are no immediate plans to do so).

The equivalent code in the `od` package is as follows:

``` r
desire_lines_od = od_to_sfc(od_data_df, od_data_centroids)
```

The result is an `sfc` object that has the same geometry as the output
from `od2line`:

``` r
desire_lines_od[1:2]
#> Geometry set for 2 features 
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.581773 ymin: 53.79593 xmax: -1.534957 ymax: 53.82859
#> epsg (SRID):    NA
#> proj4string:    NA
#> LINESTRING (-1.534957 53.82859, -1.545708 53.79...
#> LINESTRING (-1.581773 53.8186, -1.545708 53.79593)
desire_lines_stplanr$geometry[1:2]
#> Geometry set for 2 features 
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.581773 ymin: 53.79593 xmax: -1.534957 ymax: 53.82859
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#> LINESTRING (-1.534957 53.82859, -1.545708 53.79...
#> LINESTRING (-1.581773 53.8186, -1.545708 53.79593)
```

These are ‘desire lines’ representing the shortest (straight line) path
between two centoids and can plotted using geographic data and mapping
packages such as `sf`, `mapview`, `tmap` and `mapdeck`, e.g.:

``` r
plot(desire_lines_od)
plot(desire_lines_stplanr$geometry)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="50%" /><img src="man/figures/README-unnamed-chunk-5-2.png" width="50%" />

To make the results (almost) identical, we can specify `sf` outputs:

``` r
desire_lines_od_sf1 = od_to_sf(od_data_df, od_data_centroids)
desire_lines_od_sf1[1:2, 1:9]
#> Simple feature collection with 2 features and 9 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.581773 ymin: 53.79593 xmax: -1.534957 ymax: 53.82859
#> epsg (SRID):    NA
#> proj4string:    NA
#>   geo_code1 geo_code2  all train bus taxi car_driver car_passenger bicycle
#> 1 E02002363 E02006875  922     5 356    7        375            76      43
#> 2 E02002373 E02006875 1037   111 424   20        155            30      73
#>                         geometry
#> 1 LINESTRING (-1.534957 53.82...
#> 2 LINESTRING (-1.581773 53.81...
sf::st_crs(desire_lines_od_sf1)
#> Coordinate Reference System: NA
```

An additional option, that requires the `sf` package is to be installed,
is to use sf during the creation of the lines, allowing crss to be
provided:

``` r
desire_lines_od_sf2 = od_to_sf(od_data_df, od_data_centroids, package = "sf", crs = 4326)
sf::st_crs(desire_lines_od_sf2)
#> Coordinate Reference System:
#>   EPSG: 4326 
#>   proj4string: "+proj=longlat +datum=WGS84 +no_defs"
```

## Performance

The package is designed to be fast, with centroids only created when
needed and the use of `sfheaders`.

### Benchmark on a small dataset:

``` r
nrow(od_data_df)
#> [1] 6
bench::mark(check = FALSE, max_iterations = 100,
  stplanr = stplanr::od2line(od_data_df, od_data_zones),
  od = od_to_sfc(od_data_df, od_data_zones),
  od_sf1 = od_to_sf(od_data_df, od_data_zones),
  od_sf2 = od_to_sf(od_data_df, od_data_zones, package = "sf", crs = 4326)
)
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 stplanr      5.19ms   9.43ms      88.3    1.27MB     4.20
#> 2 od           1.96ms   3.34ms     202.    39.73KB     4.99
#> 3 od_sf1       2.62ms   4.76ms     146.    18.27KB     4.23
#> 4 od_sf2       2.69ms   4.12ms     156.    21.19KB     4.17
```

``` r
bench::mark(check = FALSE, max_iterations = 100,
  stplanr_centroids = stplanr::od2line(od_data_df, od_data_centroids),
  od_sf3 = od_to_sf(od_data_df, od_data_centroids)
  # od_sf4 = od_to_sf(od_data_df, centroids, package = "sf", crs = 4326)
)
#> # A tibble: 2 x 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 stplanr_centroids   1.67ms   2.22ms      302.      21KB     6.16
#> 2 od_sf3              1.35ms   1.51ms      430.    10.4KB     8.78
```

### Benchmark on medium-sized dataset

``` r
nrow(od_data_df_medium)
#> [1] 10245
bench::mark(check = FALSE, max_iterations = 100,
  stplanr = stplanr::od2line(od_data_df_medium, od_data_zones),
  od = od_to_sfc(od_data_df_medium, od_data_zones),
  od_sf1 = od_to_sf(od_data_df_medium, od_data_zones),
  od_sf2 = od_to_sf(od_data_df_medium, od_data_zones, package = "sf", crs = 4326)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 stplanr     662.3ms  662.3ms      1.51     9.1MB     4.53
#> 2 od           33.4ms   54.4ms     18.3     4.72MB     1.83
#> 3 od_sf1       44.2ms   55.2ms     17.4     5.42MB     3.87
#> 4 od_sf2      632.1ms  632.1ms      1.58    5.64MB     4.75
```

``` r
bench::mark(check = FALSE, max_iterations = 100,
  stplanr_centroids = stplanr::od2line(od_data_df_medium, od_data_centroids),
  od_sf3 = od_to_sf(od_data_df_medium, od_data_centroids)
  # od_sf4 = od_to_sf(od_data_df_medium, centroids, package = "sf", crs = 4326)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 2 x 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 stplanr_centroids  818.1ms  818.1ms      1.22    9.03MB     4.89
#> 2 od_sf3              46.3ms   65.9ms     12.8     5.36MB     1.60
```

### Benchmark using low-level functions and made-up data

``` r
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
#>            X            Y
#> 1  0.3511762 -0.092791070
#> 2 -0.6864000 -0.539929283
#> 3 -1.5311851 -0.108977646
#> 4 -0.7158086 -0.237707738
#> 5  1.1207717  0.007628652
#> 6 -1.2323664 -1.280546494
head(sfh)
#>   sfg_id point_id          x            y
#> 1      1        1  0.3511762 -0.092791070
#> 2      2        2 -0.6864000 -0.539929283
#> 3      3        3 -1.5311851 -0.108977646
#> 4      4        4 -0.7158086 -0.237707738
#> 5      5        5  1.1207717  0.007628652
#> 6      6        6 -1.2323664 -1.280546494
head(sfi)
#>         [,1]         [,2]
#> 1  0.3511762 -0.092791070
#> 2 -0.6864000 -0.539929283
#> 3 -1.5311851 -0.108977646
#> 4 -0.7158086 -0.237707738
#> 5  1.1207717  0.007628652
#> 6 -1.2323664 -1.280546494

all.equal(unname(as.matrix(sfh[, c("x", "y")])), unname(sf))
#> [1] TRUE
all.equal(unname(sfi), unname(sf))
#> [1] TRUE

res = bench::press(
  rows = 10 ^ (1:5),
  bench::mark(
    check = FALSE,
    sf = sf::st_coordinates(pts[1:rows, ]),
    sfh = sfheaders::sf_to_df(pts[1:rows, ]),
    sfi = sf_internal(pts[1:rows, ])
  )
)
#> Running with:
#>     rows
#> 1     10
#> 2    100
#> 3   1000
#> 4  10000
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> 5 100000
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.

res
#> # A tibble: 15 x 7
#>    expression   rows      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>  <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 sf             10 961.25µs   1.11ms   574.       6.36KB     2.06
#>  2 sfh            10 934.15µs   1.05ms   607.       8.24KB     4.21
#>  3 sfi            10 901.24µs 975.46µs   660.      39.08KB     2.07
#>  4 sf            100   3.15ms   3.53ms   200.      56.44KB     4.26
#>  5 sfh           100   3.19ms   3.97ms   187.      66.91KB     2.01
#>  6 sfi           100   3.09ms    3.5ms   194.      54.83KB     4.26
#>  7 sf           1000  25.78ms  43.41ms    24.4    474.55KB     4.43
#>  8 sfh          1000  26.29ms  40.09ms    25.1    551.82KB     5.03
#>  9 sfi          1000   25.4ms  41.58ms    25.6    458.88KB     2.13
#> 10 sf          10000 416.72ms 427.67ms     2.34     4.94MB     2.34
#> 11 sfh         10000 426.63ms 438.26ms     2.28     5.67MB     3.42
#> 12 sfi         10000 406.44ms 430.72ms     2.32     4.79MB     2.32
#> 13 sf         100000    4.79s    4.79s     0.209    47.3MB     2.50
#> 14 sfh        100000    5.06s    5.06s     0.197   54.55MB     2.57
#> 15 sfi        100000    5.16s    5.16s     0.194   45.77MB     2.32
# ggplot2::autoplot(res)
```

## Related open source projects

  - [stplanr](https://github.com/ropensci/stplanr/) is an R package
    package designed to support transport planning, with a focus on
    geographic transport datasets and many functions for working with OD
    data in the [od function
    family](https://docs.ropensci.org/stplanr/reference/index.html#section-work-with-od-data).
  - [cartography](http://riatelab.github.io/cartography) is an R package
    with functions for working with OD data, including
    [`getLinkLayer()`](http://riatelab.github.io/cartography/docs/reference/getLinkLayer.html)
  - [gravity](https://pacha.dev/gravity/) is an R package for developing
    ‘gravity models’ to estimate flow between zones
  - [flowmap.gl](https://github.com/teralytics/flowmap.gl), a JavaScript
    package for visualising OD data
  - [Arabesque](http://arabesque.ifsttar.fr/) is another JavaScript
    project for working with OD data

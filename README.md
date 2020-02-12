
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
needed and the use of `sfheaders`. The benchmark shows this performance.

``` r
bench::mark(check = FALSE, max_iterations = 100,
  stplanr = stplanr::od2line(od_data_df, od_data_zones),
  od = od_to_sfc(od_data_df, od_data_zones),
  od_sf1 = od_to_sf(od_data_df, od_data_zones),
  od_sf2 = od_to_sf(od_data_df, od_data_zones, package = "sf", crs = 4326)
)
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 stplanr      5.18ms  11.65ms      78.3    1.27MB     4.23
#> 2 od           1.98ms   3.21ms     195.    39.73KB     4.69
#> 3 od_sf1       2.34ms   3.49ms     180.    18.27KB     4.34
#> 4 od_sf2       2.38ms   4.28ms     161.    21.19KB     4.23
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
#> 1 stplanr_centroids    1.7ms   2.97ms      202.      21KB     4.12
#> 2 od_sf3              1.24ms   1.71ms      302.    10.4KB     6.16
```

##

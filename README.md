
<!-- README.md is generated from README.Rmd. Please edit that file -->

# od

<!-- badges: start -->

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

The package came out of conversations about the OD functions in the
package `stplanr`, particularly `od2line` which works as follows:

``` r
library(od) # load example datasets
od_data_leeds[1:2, 1:9] # OD data as data frame
#>   geo_code1 geo_code2 all from_home light_rail train bus taxi motorbike
#> 1 E02002330 E02002331 742         0          0     1   5    1         1
#> 2 E02002331 E02002331 849         0          0     0  34    1         6
od_data_zones[1:2, ]
#>       msoa11cd  msoa11nm
#> 2270 E02002330 Leeds 001
#> 2271 E02002331 Leeds 002
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              geometry
#> 2270                                                                                                                                                                                                                                                                                                                                           -1.392046, -1.398989, -1.410523, -1.417764, -1.420581, -1.415028, -1.412073, -1.408357, -1.406710, -1.410629, -1.409197, -1.409140, -1.402347, -1.400684, -1.397198, -1.383674, -1.386428, -1.384679, -1.383307, -1.392046, 53.929899, 53.915569, 53.917471, 53.915215, 53.919331, 53.921274, 53.919534, 53.927881, 53.927850, 53.930156, 53.935466, 53.935475, 53.936831, 53.938446, 53.942529, 53.941456, 53.938680, 53.936829, 53.932811, 53.929899
#> 2271 -1.340405, -1.344712, -1.339539, -1.307191, -1.308992, -1.300420, -1.294313, -1.307311, -1.297203, -1.300588, -1.313723, -1.321054, -1.322272, -1.328700, -1.345094, -1.350501, -1.357661, -1.364050, -1.359934, -1.362844, -1.370039, -1.370925, -1.386029, -1.378174, -1.390462, -1.386554, -1.398211, -1.398989, -1.392046, -1.383307, -1.384679, -1.386428, -1.383674, -1.381683, -1.340405, 53.945888, 53.939502, 53.940810, 53.934543, 53.924098, 53.929417, 53.927217, 53.921494, 53.921682, 53.907486, 53.904702, 53.903480, 53.900457, 53.901750, 53.906925, 53.909079, 53.907272, 53.908818, 53.911667, 53.914355, 53.915530, 53.907548, 53.909903, 53.906165, 53.908110, 53.912444, 53.914786, 53.915569, 53.929899, 53.932811, 53.936829, 53.938680, 53.941456, 53.940487, 53.945888
desire_lines_stplanr = stplanr::od2line(od_data_leeds, od_data_zones)
#> Creating centroids representing desire line start and end points.
desire_lines_stplanr[1:2, 1:9]
#> Simple feature collection with 2 features and 9 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.400106 ymin: 53.92305 xmax: -1.346499 ymax: 53.92906
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#>   geo_code1 geo_code2 all from_home light_rail train bus taxi motorbike
#> 1 E02002330 E02002331 742         0          0     1   5    1         1
#> 2 E02002331 E02002331 849         0          0     0  34    1         6
#>                         geometry
#> 1 LINESTRING (-1.400106 53.92...
#> 2 LINESTRING (-1.346499 53.92...
```

It works great, and is plenty fast enough for most applications, but
there are some issues with `stplanr::od2line()`:

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
desire_lines_od = od_to_sfc(od_data_leeds, od_data_zones)
```

The result is an `sfc` object that has the same geometry as the output
from `od2line`:

``` r
desire_lines_od[1:2]
#> Geometry set for 2 features 
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.400106 ymin: 53.92305 xmax: -1.346499 ymax: 53.92906
#> epsg (SRID):    NA
#> proj4string:    NA
#> LINESTRING (-1.400106 53.92906, -1.346499 53.92...
#> LINESTRING (-1.346499 53.92305, -1.346499 53.92...
desire_lines_stplanr$geometry[1:2]
#> Geometry set for 2 features 
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.400106 ymin: 53.92305 xmax: -1.346499 ymax: 53.92906
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#> LINESTRING (-1.400106 53.92906, -1.346499 53.92...
#> LINESTRING (-1.346499 53.92305, -1.346499 53.92...
```

To make the results (almost) identical, we can specify `sf` outputs:

``` r
desire_lines_od_sf1 = od_to_sf(od_data_leeds, od_data_zones)
desire_lines_od_sf1[1:2, 1:9]
#> Simple feature collection with 2 features and 9 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.400106 ymin: 53.92305 xmax: -1.346499 ymax: 53.92906
#> epsg (SRID):    NA
#> proj4string:    NA
#>   geo_code1 geo_code2 all from_home light_rail train bus taxi motorbike
#> 1 E02002330 E02002331 742         0          0     1   5    1         1
#> 2 E02002331 E02002331 849         0          0     0  34    1         6
#>                         geometry
#> 1 LINESTRING (-1.400106 53.92...
#> 2 LINESTRING (-1.346499 53.92...
sf::st_crs(desire_lines_od_sf1)
#> Coordinate Reference System: NA
```

An additional option, that requires the `sf` package is to be installed,
is to use sf during the creation of the lines, allowing crss to be
provided:

``` r
desire_lines_od_sf2 = od_to_sf(od_data_leeds, od_data_zones, package = "sf", crs = 4326)
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
  stplanr = stplanr::od2line(od_data_leeds, od_data_zones),
  od = od_to_sfc(od_data_leeds, od_data_zones),
  od_sf1 = od_to_sf(od_data_leeds, od_data_zones),
  od_sf2 = od_to_sf(od_data_leeds, od_data_zones, package = "sf", crs = 4326)
)
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 stplanr      4.91ms    5.5ms      138.   105.2KB     7.25
#> 2 od              2ms   2.27ms      330.    43.7KB    10.2 
#> 3 od_sf1       2.37ms   2.82ms      243.    45.1KB     7.52
#> 4 od_sf2       2.86ms   3.28ms      218.      48KB     9.09
```

``` r
centroids = sf::st_centroid(od_data_zones)
#> Warning in st_centroid.sf(od_data_zones): st_centroid assumes attributes are
#> constant over geometries of x
#> Warning in st_centroid.sfc(st_geometry(x), of_largest_polygon =
#> of_largest_polygon): st_centroid does not give correct centroids for longitude/
#> latitude data
bench::mark(check = FALSE, max_iterations = 100,
  stplanr_centroids = stplanr::od2line(od_data_leeds, centroids),
  od_sf3 = od_to_sf(od_data_leeds, centroids)
  # od_sf4 = od_to_sf(od_data_leeds, centroids, package = "sf", crs = 4326)
)
#> # A tibble: 2 x 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 stplanr_centroids   2.25ms   3.57ms      187.    36.2KB     6.52
#> 2 od_sf3               1.4ms   2.29ms      321.    29.2KB     6.54
```

## Testing the similarity between stplanr and sf objects

Due to the way `od2line()` works and the outputs `sfheaders` functions,
there are slight differences in the outputs of `od2line()` and
`od_to_sf()`. These have no impact on real-world applications, as far as
we are aware but are outlined below

``` r
desire_lines_od[1:2]
#> Geometry set for 2 features 
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -1.400106 ymin: 53.92305 xmax: -1.346499 ymax: 53.92906
#> epsg (SRID):    NA
#> proj4string:    NA
#> LINESTRING (-1.400106 53.92906, -1.346499 53.92...
#> LINESTRING (-1.346499 53.92305, -1.346499 53.92...
desire_lines_od_sf = sf::st_as_sf(od_data_leeds, geometry = desire_lines_od, crs = 4326)
identical(desire_lines_od_sf, desire_lines_stplanr)
#> [1] FALSE
identical(sf::st_drop_geometry(desire_lines_stplanr), sf::st_drop_geometry(desire_lines_od_sf))
#> [1] TRUE
attributes(desire_lines_od_sf$geometry) = attributes(desire_lines_stplanr$geometry)
identical(attributes(desire_lines_od_sf$geometry), attributes(desire_lines_stplanr$geometry))
#> [1] TRUE
attributes(desire_lines_od_sf) = attributes(desire_lines_stplanr)
identical(attributes(desire_lines_od_sf), attributes(desire_lines_stplanr))
#> [1] TRUE
identical(desire_lines_od_sf, desire_lines_stplanr)
#> [1] FALSE
identical(desire_lines_od_sf$geometry[1], desire_lines_stplanr$geometry[1])
#> [1] FALSE
identical(
  unclass(desire_lines_stplanr$geometry[1][[1]]),
  unclass(desire_lines_od_sf$geometry[1][[1]])
)
#> [1] FALSE
identical(
  unclass(desire_lines_stplanr$geometry[1][[1]]),
  unclass(desire_lines_od_sf$geometry[1][[1]])
)
#> [1] FALSE
attributes((desire_lines_od_sf$geometry[1]))
#> $class
#> [1] "sfc_LINESTRING" "sfc"           
#> 
#> $precision
#> [1] 0
#> 
#> $bbox
#>      xmin      ymin      xmax      ymax 
#> -1.400106 53.923049 -1.346499 53.929062 
#> 
#> $crs
#> Coordinate Reference System:
#>   EPSG: 4326 
#>   proj4string: "+proj=longlat +datum=WGS84 +no_defs"
#> 
#> $n_empty
#> [1] 0
attributes((desire_lines_od_sf$geometry[1][[1]]))
#> $dim
#> [1] 2 2
#> 
#> $class
#> [1] "XY"         "LINESTRING" "sfg"
attributes((desire_lines_stplanr$geometry[1][[1]]))
#> $dim
#> [1] 2 2
#> 
#> $dimnames
#> $dimnames[[1]]
#> NULL
#> 
#> $dimnames[[2]]
#> [1] "X" "Y"
#> 
#> 
#> $class
#> [1] "XY"         "LINESTRING" "sfg"
identical(
  as.numeric(unclass(desire_lines_stplanr$geometry[1][[1]])),
  as.numeric(unclass(desire_lines_od_sf$geometry[1][[1]]))
)
#> [1] TRUE

all.equal(
  as.numeric(unclass(desire_lines_stplanr$geometry[1][[1]])),
  as.numeric(unclass(desire_lines_od_sf$geometry[1][[1]]))
)
#> [1] TRUE
```

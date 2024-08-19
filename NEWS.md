# od 0.5.0

# od 0.4.4 (2024-03)

* Fix minor issue with geometry checking, result of upstream changes

# od 0.4.3 (2023-11)

* Check for duplicate ids

# od 0.4.2 (2023-09)

* `od_coordinates_ids()` exported
* Fixes for CRAN checks

# od 0.4.1 (2023-09)

* `od_coordinates_ids()` faster, uses `vctrs::vec_interleave()` (#45)

# od 0.4.0 (2022-04)

* `od_disaggregate()` can now take route networks as inputs
* `od_disaggregate()` is now used as a 'back-end' for `od_jitter()` where possible by default
* New `pd` argument in `points_to_od()` allows calculation of OD pairs associated
  with destinations that have a different geometry than the origins (#41)
* Remove 'old style CRSs', preventing warnings emitted by the `sf` package

# od 0.3.1 (2021-07)

* `od_jitter()` succeeds with wider range of input datasets, and can work with fewer subpoints than OD pairs thanks to the `replace = TRUE` setting in the base R function `sample()`

# od 0.3.0 (2021-07)

* New function `od_jitter()` allows offsetting of origin and destination points of desire lines (#11)
* Add new argument to `od_disaggregate()` to allow non-integer outputs

# od 0.2.1

* Updated `od_disaggregate()` so it's less likely to fail with unfathomable message (#30)

# od 0.2.0

* New `od_split()` function, a copy of `od_disaggregate()`
* New `od_aggregate()` and `od_group()` functions (#24)
* Updated datasets for improved documentation and examples (#28)
* Updated introductory vignette building on the stplanr `od` vignette (#16)

# od 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Lots of changes have been made since the package was first release on CRAN
  * See https://github.com/ITSLeeds/od/compare/0.0.1...master for details
* Key changes include:
  * Lots of new functionality, includine `od_disaggregate()`
  * Much of the functionality from `stplanr` had been ported


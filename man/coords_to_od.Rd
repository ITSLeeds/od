% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/points_to_od.R
\name{coords_to_od}
\alias{coords_to_od}
\title{Convert coordinates into a data frame of origins and destinations}
\usage{
coords_to_od(p, interzone_only = FALSE, ids_only = FALSE)
}
\arguments{
\item{p}{A spatial points object or a matrix of coordinates representing
points}

\item{interzone_only}{Should the result only include interzonal OD pairs, in
which the ID of the origin is different from the ID of the destination
zone? \code{FALSE} by default}

\item{ids_only}{Should a data frame with only 2 columns (origin and
destination IDs) be returned? The default is \code{FALSE}, meaning the result
should also contain the coordinates of the start and end points of each OD
pair.}
}
\value{
A data frame object with O and D codes and origin and destination coordinates.
}
\description{
Takes geographic coordinates and converts them into a data frame
representing the potential flows, or 'spatial interaction', between every combination
of points.
}
\examples{
p = sf::st_coordinates(od_data_centroids[1:3, ])
od = points_to_od(p)
(od = coords_to_od(p, interzone_only = TRUE))
l = odc_to_sf(od[3:6], d = od[1:2])
l$v = 1
(l_oneway = od_oneway(l))
plot(l_oneway)
}

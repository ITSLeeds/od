% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{od_data_df}
\alias{od_data_df}
\alias{od_data_df_medium}
\title{Origin-destination datasets}
\description{
Datasets representing top commuter desire lines
in Leeds based on the 2011 Census.
The first two variables of the data frame are the zone code of origin and destination, respectively.
The other columns record the number of people who travel by different modes, including
\code{all}, \code{train}, \code{bus}, \code{bicycle} and by \code{foot}.
}
\details{
\code{od_data_df_medium} is a larger dataset with the same variables, with around 10k rows.
}
\note{
The schema data can be (re-)generated using code in the
\code{data-raw} directory.
}
\examples{
od_data_df
}
\keyword{datasets}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cut_point_df}
\alias{cut_point_df}
\title{Cut Points}
\format{
A data frame with 3567 rows and 9 variables:
\describe{
  \item{star_year}{The star rating year}
  \item{cut_point_type}{One of "Part C", "Part D MA-PD" or "Part D PDP"}
  \item{measure_id}{The ID code used by CMS to identify in the given `star_year`}
  \item{cut_point}{Cut point}
  \item{low_star}{}
  \item{high_star}{}
  \item{higher_is_better}{TRUE if higher measure values mean higher star ratings. Otherwise, FALSE}
  \item{low_original_text}{`low_star` text from the source CSV file}
  \item{high_original_text}{`high_star` text from the source CSV file}
}
}
\usage{
cut_point_df
}
\description{
One row per measure, star year, and cut point. Each record gives the
star ratings that this cut point falls between. This table contains the
same data as `measure_star_range_df`, but rearranged.
}
\keyword{datasets}

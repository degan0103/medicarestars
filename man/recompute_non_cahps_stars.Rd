% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_points.R
\name{recompute_non_cahps_stars}
\alias{recompute_non_cahps_stars}
\title{Compute 2022 non-CAHPS measure-level star ratings}
\usage{
recompute_non_cahps_stars(df22, df21, cutting_df = NULL, common_seed = NULL)
}
\arguments{
\item{df22}{A dataframe of 2022 measure data, including the columns
`measure_id`, `measure_code`, `contract_id`, `stars` and `value`}

\item{df21}{A dataframe of 2022 measure data, including the columns
`measure_id`, `measure_code`, `contract_id` and `stars`}

\item{cutting_df}{A dataframe of the form output by `make_cutting_df()`}

\item{common_seed}{The integer to pass to `set.seed()` before starting each
mean resampling computation}
}
\value{

}
\description{
Compute 2022 non-CAHPS measure-level star ratings
}

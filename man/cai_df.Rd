% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cai_df}
\alias{cai_df}
\title{Categorical Adjustment Index (CAI)}
\format{
A data frame with 8821 rows and 5 variables:
\describe{
  \item{star_year}{The star rating year}
  \item{contract_id}{The health plan's Medicare contract ID}
  \item{summary_type}{The level of the star rating: "Part C", "Part D" or "Overall"}
  \item{fac}{The final adjustment category}
  \item{cai}{The categorical adjustment index}
}
}
\usage{
cai_df
}
\description{
One row per contract, star year (2017 or later), and summary level (Part C,
Part D or Overall). The CAI value is used to calculate summary star
ratings. See the Technical Notes for details.
}
\keyword{datasets}

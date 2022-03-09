

#' Medicare Part C&D Contracts
#'
#' One record per Medicare contract ID and star year.
#'
#' @format A data frame with 10,696 rows and 8 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{contract_id}{The health plan's Medicare contract ID}
#'   \item{org_type}{The type of Medicare plan}
#'   \item{contract_name}{Contract name}
#'   \item{marketing_name}{Marketing name}
#'   \item{parent_name}{Parent company name}
#'   \item{part_d_cut_point_type}{Either "Part D MA-PD" for Part C&D plans or "Part D PDP" for Part D only}
#'   \item{contract_type}{A categorization of contracts used to compute summary star ratings}
#' }
"contract_df"


#' Measures
#'
#' One record per measure and star year. The table includes display measures,
#' which are
#'
#' @format A data frame with 734 rows and 15 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{measure_id}{The ID code used by CMS to identify the measure in the
#'     given `star_year`}
#'   \item{measure_name}{The measure's name}
#'   \item{part}{"C" or "D"}
#'   \item{domain_id}{The domain's ID code for the given year}
#'   \item{domain_name}{Domain name}
#'   \item{measure_code}{All records with a given `measure_code` represent the
#'     same measure, over multiple years. This code was created for the R package
#'     and is not in the CMS data.}
#'   \item{measure_standard_name}{Name taken from the Star Ratings Measure
#'     History attachment of the Star Ratings Technical Notes PDF}
#'   \item{data_source}{}
#'   \item{data_start_date}{Start date of the period during which the measure
#'     data was collected}
#'   \item{data_end_date}{End date of the period during which the measure
#'     data was collected}
#'   \item{weight_category}{Measures in the same `weight_category` generally have the same weight}
#'   \item{part_weight}{The weight assigned to this measure when computing Part C/D summary star ratings}
#'   \item{overall_weight}{The weight assigned to this measure when computing overall star ratings}
#'   \item{display_measure}{TRUE if this is a display measure; otherwise, FALSE}
#'   \item{higher_is_better}{TRUE if higher measure values mean higher star ratings. Otherwise, FALSE}
#' }
"measure_df"


#' Measure-Level Values and Star Ratings
#'
#' One row per contract, measure, and star year. Each record contains the
#' measure value and star rating the contract received for that measure in the
#' given year, or a note explaining why the value/rating is NULL.
#'
#' @format A data frame with 791,087 rows and 11 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{measure_id}{The ID used by CMS to identify a measure in the given `star_year`}
#'   \item{measure_code}{A measure ID created for this dataset to represent one measure}
#'   \item{contract_id}{The health plan's Medicare contract ID}
#'   \item{value}{The measure value}
#'   \item{stars}{The star rating}
#'   \item{cut_point_type}{One of "Part C", "Part D MA-PD" or "Part D PDP"}
#'   \item{value_note}{If `value` is null, this field gives an explanation}
#'   \item{stars_note}{If `stars` is null, this field gives an explanation}
#'   \item{value_original_text}{Text from the source CSV file from CMS}
#'   \item{stars_original_text}{Text from the source CSV file from CMS}
#' }
"measure_score_df"


#' Measure-Level Star Ranges
#'
#' One row per measure, star year, and star rating. Each record gives the
#' measure value's lower and upper bounds. This table contains the same data
#' as `cut_point_df`, but rearranged.
#'
#' @format A data frame with 4509 rows and 8 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{cut_point_type}{One of "Part C", "Part D MA-PD" or "Part D PDP"}
#'   \item{measure_id}{The ID code used by CMS to identify in the given `star_year`}
#'   \item{stars}{Star rating}
#'   \item{lower_bound}{The lower bound for measure values with this star rating}
#'   \item{upper_bound}{The upper bound for measure values with this star rating}
#'   \item{higher_is_better}{TRUE if higher measure values mean higher star ratings. Otherwise, FALSE}
#'   \item{original_text}{Text from the source CSV file from CMS}
#' }
"measure_star_range_df"


#' Cut Points
#'
#' One row per measure, star year, and cut point. Each record gives the
#' star ratings that this cut point falls between. This table contains the
#' same data as `measure_star_range_df`, but rearranged.
#'
#' @format A data frame with 3567 rows and 9 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{cut_point_type}{One of "Part C", "Part D MA-PD" or "Part D PDP"}
#'   \item{measure_id}{The ID code used by CMS to identify in the given `star_year`}
#'   \item{cut_point}{Cut point}
#'   \item{low_star}{}
#'   \item{high_star}{}
#'   \item{higher_is_better}{TRUE if higher measure values mean higher star ratings. Otherwise, FALSE}
#'   \item{low_original_text}{`low_star` text from the source CSV file}
#'   \item{high_original_text}{`high_star` text from the source CSV file}
#' }
"cut_point_df"


#' Domain-Level Star Ratings
#'
#' One row per contract, measure domain, and star year. This gives each
#' contract's domain-level star ratings.
#'
#' @format A data frame with 89,263 rows and 6 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{domain_id}{The domain's ID code for the given year}
#'   \item{domain_name}{Domain name}
#'   \item{contract_id}{The health plan's Medicare contract ID}
#'   \item{stars}{Domain-level star rating}
#'   \item{stars_note}{If `stars` is null, this field gives an explanation}
#' }
"domain_stars_df"


#' Summary Star Ratings
#'
#' One row per contract, star year, and summary level (Part C, Part D or
#' Overall). This table gives contracts' highest-level star ratings.
#'
#' @format A data frame with 27,150 rows and 5 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{summary_type}{The level of the star rating: "Part C", "Part D" or "Overall"}
#'   \item{contract_id}{The health plan's Medicare contract ID}
#'   \item{stars}{Star score}
#'   \item{stars_note}{If `stars` is null, this field gives an explanation}
#' }
"summary_stars_df"


#' Categorical Adjustment Index (CAI)
#'
#' One row per contract, star year (2017 or later), and summary level (Part C,
#' Part D or Overall). The CAI value is used to calculate summary star
#' ratings. See the Technical Notes for details.
#'
#' @format A data frame with 8821 rows and 5 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{contract_id}{The health plan's Medicare contract ID}
#'   \item{summary_type}{The level of the star rating: "Part C", "Part D" or "Overall"}
#'   \item{fac}{The final adjustment category}
#'   \item{cai}{The categorical adjustment index}
#' }
"cai_df"


#' Disenrollment Data
#'
#' One row per contract, star year (2020 or later), and disenrollment reason.
#'
#' @format A data frame with 11,850 rows and 5 variables:
#' \describe{
#'   \item{star_year}{The star rating year}
#'   \item{contract_id}{The health plan's Medicare contract ID}
#'   \item{disenrollment_reason}{Reason for disenrolling from the Medicare plan}
#'   \item{disenrollment_pcg}{Percentage of members who disenrolled for the given reason}
#'   \item{disenrollment_note}{If `disenrollment_pcg` is null, this field gives an explanation}
#' }
"disenrollment_df"




#> usethis::use_data(df)
#> usethis::use_data(sysdata, internal = TRUE, overwrite = TRUE)



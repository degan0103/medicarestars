

#' Which contracts have enough measure star ratings for a summary star rating?
#'
#' Depending on the `contract_type`, there are different requirements for how
#' many measure-level star ratings are required for a health plan to receive a
#' summary star rating (Part C, Part D or Overall). See the 2022 Technical
#' Notes for details.
#'
#' @param df22 Dataframe of 2022 data, including the columns `measure_id`,
#' `contract_type` and `stars`
#'
#' @return
#' @export
#'
#' @examples
contracts_with_2022_summary_scores <- function(df22) {

  df22_req_cols <- c("measure_id", "contract_id", "contract_type", "stars")
  check_columns_are_present(df22_req_cols, df22, "df22 is missing columns:")

  part_cd_df <-
    df22 %>%
    dplyr::inner_join(sysdata$measure_requirements_df,
                      by = c("measure_id", "contract_type")) %>%
    dplyr::filter(required) %>%
    dplyr::group_by(contract_id, contract_type, summary_type) %>%
    dplyr::summarize(
      n_stars = sum(!is.na(stars)),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(sysdata$count_requirements_df,
                      by = c("contract_type", "summary_type")) %>%
    dplyr::filter(n_stars >= needed) %>%
    dplyr::select(contract_id, summary_type)

  overall_df <-
    df22 %>%
    dplyr::inner_join(part_cd_df %>% count(contract_id) %>%
                        filter(n == 2) %>% select(contract_id),
                      by = "contract_id") %>%
    dplyr::inner_join(sysdata$measure_requirements_df,
                      by = c("measure_id", "contract_type")) %>%
    dplyr::filter(required, !(measure_id %in% c("D02","D03"))) %>%
    dplyr::group_by(contract_id, contract_type) %>%
    dplyr::summarize(
      n_stars = sum(!is.na(stars)),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(
      sysdata$count_requirements_df %>%
        dplyr::filter(summary_type == "Overall"),
      by = c("contract_type")
    ) %>%
    dplyr::filter(n_stars >= needed) %>%
    dplyr::transmute(contract_id, summary_type = "Overall")

  rbind(part_cd_df, overall_df)
}




my_percentile_func <- function(x, pcg) {
  tibble(y=x) %>%
    group_by(y) %>%
    summarize(n=n(), .groups = "drop") %>%
    arrange(y) %>%
    mutate(rank = cumsum(n), percentile = rank / length(x)) %>%
    filter(percentile >= pcg) %>%
    summarize(min_y = min(y)) %>%
    pull(min_y)
}


#' Compute Part C&D summary star ratings
#'
#' @param df22 A dataframe with 2022 measure data, including the columns
#' `measure_id`, `contract_id`, `cut_point_type` and `stars`
#'
#' @return
#' @export
#'
#' @examples
part_cd_summary_stars_2022 <- function(df22) {

  df22_req_cols <- c("measure_id", "contract_id", "cut_point_type", "stars")
  check_columns_are_present(df22_req_cols, df22, "df is missing columns:")


  include_exclude_id_df <-
    measure_df %>%
    dplyr::filter(star_year == 2022) %>%
    dplyr::transmute(
      measure_id,
      summary_type = ifelse(part == "C", "Part C", "Part D")
    ) %>%
    dplyr::mutate(
      improvement = purrr::map(
        measure_id,
        ~if(.x %in% c("C25","D04")) {"With"} else {c("With", "Without")}
      ),
      mpf = purrr::map(
        measure_id,
        ~if(.x == "D07") {"With"} else {c("With", "Without")}
      )
    ) %>%
    tidyr::unnest(improvement) %>%
    tidyr::unnest(mpf)

  temp_cai_df <- cai_df %>%
    dplyr::filter(star_year == 2022) %>%
    dplyr::select(contract_id, cut_point_type = summary_type, cai)

  temp_measure_df <- measure_df %>%
    dplyr::filter(star_year == 2022) %>%
    dplyr::select(measure_id, higher_is_better, part_weight)

  df22[, df22_req_cols] %>%
    dplyr::filter(!is.na(stars)) %>%
    dplyr::transmute(
      measure_id,
      contract_id,
      cut_point_type,
      summary_type = stringr::str_sub(cut_point_type, end = 6),
      stars
    ) %>%
    dplyr::inner_join(temp_measure_df, by = "measure_id") %>%
    dplyr::inner_join(include_exclude_id_df,
                      by = c("measure_id", "summary_type")) %>%
    dplyr::group_by(contract_id, summary_type,
                    cut_point_type, improvement, mpf) %>%
    dplyr::summarize(
      avg = stats::weighted.mean(stars, part_weight),
      var = stats::weighted.mean((stars - avg)^2, part_weight) * n() / (n()-1),
      .groups = "drop"
    ) %>%
    tidyr::nest(data = c(contract_id, cut_point_type, avg, var)) %>%
    dplyr::mutate(
      n_total = purrr::map_int(data, ~nrow(.x)),
      avg_65 = purrr::map_dbl(data, ~my_percentile_func(.x$avg, 0.65)),
      avg_85 = purrr::map_dbl(data, ~my_percentile_func(.x$avg, 0.85)),
      var_30 = purrr::map_dbl(data, ~my_percentile_func(.x$var, 0.3)),
      var_70 = purrr::map_dbl(data, ~my_percentile_func(.x$var, 0.7))
    ) %>%
    dplyr::arrange(summary_type, dplyr::desc(improvement), dplyr::desc(mpf)) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(
      mean_cat = dplyr::case_when(avg >= avg_85 ~ "high",
                                  avg >= avg_65 ~ "medium",
                                  TRUE          ~ "low"),
      var_cat = dplyr::case_when(var >= var_70 ~ "high",
                                 var >= var_30 ~ "medium",
                                 TRUE            ~ "low"),
      r_factor = dplyr::case_when(
        var_cat == "low"    & mean_cat == "high"   ~ 0.4,
        var_cat == "medium" & mean_cat == "high"   ~ 0.3,
        var_cat == "low"    & mean_cat == "medium" ~ 0.2,
        var_cat == "medium" & mean_cat == "medium" ~ 0.1,
        TRUE                                       ~ 0.0
      ),
      stars_w_reward = avg + r_factor
    ) %>%
    tidyr::nest(
      data = c(improvement, mpf, avg, var, n_total, avg_65, avg_85,
               var_30, var_70, mean_cat, var_cat, r_factor, stars_w_reward)
    ) %>%
    dplyr::mutate(
      stars_w_reward = purrr::map_dbl(data, ~max(.x$stars_w_reward))
    ) %>%
    dplyr::inner_join(temp_cai_df, by = c("contract_id", "cut_point_type")) %>%
    dplyr::mutate(
      .stars = janitor::round_half_up(2 * (stars_w_reward + cai)) / 2,
      .stars = ifelse(.stars > 5, 5, .stars)
    ) %>%
    dplyr::select(contract_id, summary_type, .stars, cut_point_type)
}


#' Compute overall summary star ratings
#'
#' @param df22 A dataframe with 2022 measure data, including the columns
#' `measure_id`, `contract_id`, and `stars`
#'
#' @return
#' @export
#'
#' @examples
overall_stars_2022 <- function(df22) {

  df22_req_cols <- c("measure_id", "contract_id", "stars")
  check_columns_are_present(df22_req_cols, df22, "df is missing columns:")

  include_exclude_id_df <-
    measure_df %>%
    dplyr::filter(star_year == 2022) %>%
    dplyr::filter(!(measure_id %in% c("D02","D03"))) %>%
    dplyr::mutate(
      improvement = purrr::map(
        measure_id,
        ~if(.x %in% c("C25","D04")) {"With"} else {c("With", "Without")}
      ),
      mpf = purrr::map(
        measure_id,
        ~if(.x == "D07") {"With"} else {c("With", "Without")}
      )
    ) %>%
    tidyr::unnest(improvement) %>%
    tidyr::unnest(mpf)

  temp_cai_df <- cai_df %>%
    dplyr::filter(star_year == 2022, summary_type == "Overall") %>%
    dplyr::select(contract_id, cai)

  temp_measure_df <- measure_df %>%
    dplyr::filter(star_year == 2022) %>%
    dplyr::select(measure_id, higher_is_better, weight = overall_weight)

  df22[, df22_req_cols] %>%
    dplyr::filter(!is.na(stars)) %>%
    dplyr::transmute(
      measure_id,
      contract_id,
      stars
    ) %>%
    dplyr::inner_join(temp_measure_df,       by = "measure_id") %>%
    dplyr::inner_join(include_exclude_id_df, by = "measure_id") %>%
    dplyr::group_by(contract_id, improvement, mpf) %>%
    dplyr::summarize(
      n   = dplyr::n(),
      avg = stats::weighted.mean(stars, weight),
      var = stats::weighted.mean((stars - avg)^2, weight) * n / (n-1),
      .groups = "drop"
    ) %>%
    dplyr::select(-n) %>%
    tidyr::nest(data = c(contract_id, avg, var)) %>%
    dplyr::mutate(
      n_total = purrr::map_int(data, ~nrow(.x)),
      avg_65 = purrr::map_dbl(data, ~my_percentile_func(.x$avg, 0.65)),
      avg_85 = purrr::map_dbl(data, ~my_percentile_func(.x$avg, 0.85)),
      var_30 = purrr::map_dbl(data, ~my_percentile_func(.x$var, 0.3)),
      var_70 = purrr::map_dbl(data, ~my_percentile_func(.x$var, 0.7))
    ) %>%
    dplyr::arrange(dplyr::desc(improvement), dplyr::desc(mpf)) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(
      mean_cat = dplyr::case_when(avg >= avg_85 ~ "high",
                                  avg >= avg_65 ~ "medium",
                                  TRUE          ~ "low"),
      var_cat = dplyr::case_when(var >= var_70 ~ "high",
                                 var >= var_30 ~ "medium",
                                 TRUE          ~ "low"),
      r_factor = dplyr::case_when(
        var_cat == "low"    & mean_cat == "high"   ~ 0.4,
        var_cat == "medium" & mean_cat == "high"   ~ 0.3,
        var_cat == "low"    & mean_cat == "medium" ~ 0.2,
        var_cat == "medium" & mean_cat == "medium" ~ 0.1,
        TRUE                                       ~ 0.0
      ),
      stars_w_reward = avg + r_factor
    ) %>%
    tidyr::nest(
      data = c(improvement, mpf, avg, var, n_total, avg_65, avg_85,
               var_30, var_70, mean_cat, var_cat, r_factor, stars_w_reward)
    ) %>%
    mutate(stars_w_reward = purrr::map_dbl(data, ~max(.x$stars_w_reward))) %>%
    dplyr::inner_join(temp_cai_df, by = "contract_id") %>%
    dplyr::mutate(
      .stars = janitor::round_half_up(2 * (stars_w_reward + cai)) / 2,
      .stars = ifelse(.stars > 5, 5, .stars)
    ) %>%
    dplyr::transmute(contract_id, summary_type = "Overall", .stars)
}




check_columns_are_present <- function(required_cols, df, msg) {
  if (is.list(required_cols)) {
    x <- unlist(lapply(required_cols, function(x) any(x %in% colnames(df))))
    if (!all(x)) {
      missing_cols <- unlist(lapply(
        required_cols[!x],
        function(cols) {
          ifelse(length(cols) == 1, cols,
                 sprintf("{%s}", paste(cols, collapse = " or ")))
        }
      ))
      stop(paste(msg, paste(missing_cols, collapse = ", ")))
    }
  } else {
    if (!all(required_cols %in% colnames(df))) {
      missing_cols <- setdiff(required_cols, colnames(df))
      stop(paste(msg, paste(missing_cols, collapse = ", ")))
    }
  }
}



#' Compute cut points for a single non-CAHPS, pre-2022 measure with
#' Hierarchical Clustering
#'
#' @param value_vec A vector of all health plans' measure values for a given
#' measure and year
#' @param higher_is_better TRUE if higher measure values mean higher star
#' ratings, and FALSE otherwise
#'
#' @return A dataframe with two columns: an integer star rating in `stars`, and
#' a float cut-off measure value for that star rating in `cut_point`
#' @export
#'
#' @examples
hierarchical_clustering_cut_points <- function(value_vec, higher_is_better) {

  dist_mat <- stats::dist(value_vec, method = "euclidean")
  hc_avg   <- stats::hclust(dist_mat, method = "ward.D2")
  cut_avg  <- stats::cutree(hc_avg, k = 5)

  # TODO: Re-read rules for how some measures get <5 clusters
  df <-
    tibble::tibble(
      value = value_vec,
      cluster_id = cut_avg
    ) %>%
    dplyr::group_by(cluster_id)

  if (higher_is_better) {
    df %>%
      dplyr::summarize(cut_point = min(value), .groups = "drop") %>%
      dplyr::arrange(cut_point) %>%
      dplyr::mutate(stars = 1L:dplyr::n()) %>%
      dplyr::transmute(stars, cut_point) %>%
      dplyr::filter(stars > 1L)
  } else {
    df %>%
      dplyr::summarize(cut_point = max(value), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(cut_point)) %>%
      dplyr::mutate(stars = 1L:dplyr::n()) %>%
      dplyr::transmute(stars, cut_point) %>%
      dplyr::filter(stars > 1L)
  }
}



non_cahps_cut_points_without_fold <-
  function(fold_id, value_vec, fold_list, higher_is_better) {

  sample_vec <- value_vec[-fold_list[[fold_id]]]
  hierarchical_clustering_cut_points(sample_vec, higher_is_better)
}


#' Compute cut points for a single non-CAHPS measure for star year 2022
#'
#' @param value_vec A vector of all health plans' measure values for a given
#' measure and year
#' @param higher_is_better TRUE if higher measure values mean higher star
#' ratings, and FALSE otherwise
#' @param seed An optional integer passed to `set.seed()` before creating the
#' 10 folds partitioning `value_vec`
#'
#' @return A dataframe with two columns: an integer star rating in `stars`, and
#' a float cut-off measure value for that star rating in `cut_point`
#' @export
#'
#' @examples
mean_resampling_cut_points <-
  function(value_vec, higher_is_better, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }
  fold_list <- caret::createFolds(value_vec, k = 10, list = TRUE)
  intial_cut_points_df <-
    do.call(
      "rbind",
      lapply(1:length(fold_list), non_cahps_cut_points_without_fold,
             value_vec, fold_list, higher_is_better)
    ) %>%
    dplyr::group_by(stars) %>%
    dplyr::summarize(cut_point = mean(cut_point), .groups = "drop") %>%
    dplyr::arrange(stars)

  intial_cut_points_df %>%
    dplyr::mutate(value = rep(list(value_vec), nrow(intial_cut_points_df))) %>%
    tidyr::unnest(value) %>%
    dplyr::filter(value >= cut_point) %>%
    dplyr::group_by(stars) %>%
    dplyr::summarize(cut_point = min(value), .groups = "drop") %>%
    dplyr::arrange(stars)
}



#' Compute cut points for multiple non-CAHPS 2022 measures
#'
#' @param df A dataframe with columns `measure_id`, `measure_code`,
#' `cut_point_type`, `higher_is_better` and `value`
#' @param common_seed A integer used as the seed for each measure's cut point
#' computation
#'
#' @return
#' @export
#'
#' @examples
non_cahps_2022_cut_points <- function(df, common_seed = NULL) {

  required_cols <- list(c("measure_id", "measure_code"),
                        "cut_point_type", "higher_is_better", "value")
  check_columns_are_present(required_cols, df, "df is missing columns:")

  kept_cols <- intersect(unlist(required_cols), colnames(df))

  df[, kept_cols] %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::nest(data = c(value)) %>%
    dplyr::mutate(
      cut_point_df = purrr::map2(
        data, higher_is_better,
        ~mean_resampling_cut_points(.x$value, .y, common_seed)
      )
    ) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cut_point_df)
}




make_factor_to_star_func <- function(df, higher_is_better) {
  if (higher_is_better) {
    function(factor_int)
      c(min(df$stars)-1L, df$stars)[factor_int]
  } else {
    function(factor_int)
      c(df$stars, min(df$stars)-1L)[factor_int]
  }
}

make_cut_function <- function(breaks, right, factor_to_star_func) {
  function(value) {
    cut(value, breaks = breaks, right = right) %>%
      as.integer() %>%
      factor_to_star_func()
  }
}


#' Make a dataframe of functions to convert measure values to star ratings
#'
#' @param cut_point_df Dataframe of the form output by
#' `mean_resampling_cut_points()`
#'
#' @return
#' @export
#'
#' @examples
make_cutting_df <- function(cut_point_df) {

  required_cols <- c("measure_id", "cut_point_type", "cut_point",
                     "stars", "higher_is_better")
  check_columns_are_present(required_cols, cut_point_df,
                            "cut_point_df is missing columns:")

  cut_point_df[, required_cols] %>%
    dplyr::distinct() %>%
    dplyr::arrange(measure_id, cut_point_type, cut_point) %>%
    tidyr::nest(data = c(cut_point, stars)) %>%
    dplyr::mutate(
      breaks = purrr::map(data, ~c(-Inf, .x$cut_point, Inf)),
      right  = !higher_is_better,
      factor_to_star_func = purrr::map2(data, higher_is_better,
                                        make_factor_to_star_func)
    ) %>%
    dplyr::mutate(
      cut_function = purrr::pmap(list(breaks, right, factor_to_star_func),
                                 make_cut_function)
    ) %>%
    dplyr::select(measure_id, cut_point_type, cut_function)
}




apply_cut_function <- function(value, cut_function = NULL) {
  if (is.null(cut_function)) {
    NA_integer_
  } else{
    cut_function(value)
  }
}


#' Compute measure-level star ratings
#'
#' @param df A dataframe of measure values, including the columns `measure_id`,
#' `cut_point_type` and `value`
#' @param cutting_df A dataframe of the form output by `make_cutting_df()`
#'
#' @return
#' @export
#'
#' @examples
cut_measure_stars <- function(df, cutting_df) {

  df_req_cols <- c("measure_id", "cut_point_type", "value")
  check_columns_are_present(df_req_cols, df, "df is missing columns:")
  cut_req_cols <- c("measure_id", "cut_point_type", "cut_function")
  check_columns_are_present(cut_req_cols, cutting_df,
                            "cutting_df is missing columns:")

  df %>%
    dplyr::left_join(cutting_df, by = c("measure_id", "cut_point_type")) %>%
    dplyr::mutate(
      .stars = purrr::map2_int(value, cut_function, apply_cut_function)
    ) %>%
    dplyr::select(-cut_function)
}



non_cahps_measure_stars_2022 <-
  function(df22, df21, cutting_df = NULL, common_seed = NULL) {

  if (is.null(cutting_df) & !("stars" %in% colnames(df22))) {
    stop(stringr::str_c("Need a `cutting_df` dataframe, or else the `df22` ",
                        "dataframe must already have a 'stars' column"))
  }
  df22_req_cols <- c(
    "measure_id", "measure_code", "contract_id", "cut_point_type",
    ifelse(is.null(cutting_df), "value", "stars")
  )
  df21_req_cols <- c("measure_code", "contract_id", "stars")
  check_columns_are_present(df22_req_cols, df22, "df22 is missing columns:")
  check_columns_are_present(df21_req_cols, df21, "df21 is missing columns:")

  non_disaster_ids <- c('D07','C04','C13','C14','C25','D04','D01','C28')

  if (is.null(cutting_df)) {
    cut_point_df <- non_cahps_2022_cut_points(df22, common_seed = common_seed)
    cutting_df <- make_cutting_df(cut_point_df)
  }

  cut_measure_stars(df22, cutting_df) %>%
    dplyr::left_join(
      df21 %>%
        dplyr::select(measure_code, contract_id, .prev_stars = stars),
      by = c("measure_code", "contract_id")
    ) %>%
    dplyr::mutate(
      .disaster_measure = !(measure_id %in% non_disaster_ids),
      .prev_stars = tidyr::replace_na(.prev_stars, 0),
      .stars = ifelse(.disaster_measure, pmax(.stars, .prev_stars), .stars)
    ) %>%
    select(-.disaster_measure, -.prev_stars)
}


#' Compute 2022 non-CAHPS measure-level star ratings
#'
#' @param df22 A dataframe of 2022 measure data, including the columns
#' `measure_id`, `measure_code`, `contract_id`, `stars` and `value`
#' @param df21 A dataframe of 2022 measure data, including the columns
#' `measure_id`, `measure_code`, `contract_id` and `stars`
#' @param cutting_df A dataframe of the form output by `make_cutting_df()`
#' @param common_seed The integer to pass to `set.seed()` before starting each
#' mean resampling computation
#'
#' @return
#' @export
#'
#' @examples
recompute_non_cahps_stars <-
  function(df22, df21, cutting_df = NULL, common_seed = NULL) {

  df22_req_cols <- c("measure_id", "measure_code", "contract_id", "stars",
                     "value")
  df21_req_cols <- c("measure_id", "measure_code", "contract_id", "stars")
  check_columns_are_present(df22_req_cols, df22, "df22 is missing columns:")
  check_columns_are_present(df21_req_cols, df21, "df21 is missing columns:")

  cahps_measure_ids <- c("C03","C17","C18","C19","C20","C21","C22","D05","D06")
  improvement_ids <- c("C25","D04")

  non_cahps22_df <-
    df22 %>%
    dplyr::filter(!(measure_id %in% cahps_measure_ids)) %>%
    dplyr::filter(!(measure_id %in% improvement_ids)) %>%
    non_cahps_measure_stars_2022(df21, cutting_df = cutting_df,
                                 common_seed = common_seed)

  rbind(
    non_cahps22_df %>%
      dplyr::select(measure_code, measure_id, contract_id, stars, .stars),
    df22 %>%
      dplyr::filter(measure_id %in% c(cahps_measure_ids, improvement_ids)) %>%
      dplyr::mutate(.stars = stars) %>%
      dplyr::select(measure_code, measure_id, contract_id, stars, .stars)
  )
}





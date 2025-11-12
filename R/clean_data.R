#' Get clean variant data from raw data
#'
#' @param raw_variant_data Data.frame of latest data extracted directly from
#'    next strain
#' @param clade_list Vector of character strings of the clade names
#' @param location_data Data.frame of location information
#' @param nowcast_date Character string indicate the date range of
#'    the data.
#' @param seq_col_name Character string indicating the name of the column for
#'   number of sequences of that clade
#' @param type Character string indicating data is as of the nowcast date or
#'   evaluation data
#'
#' @returns Data.frame of counts of sequences of each clade we nowcasted during
#'   the season
#' @importFrom lubridate ymd days
#' @importFrom dplyr case_when rename left_join
#' @autoglobal
get_clean_variant_data <- function(raw_variant_data,
                                   clade_list,
                                   location_data,
                                   nowcast_date,
                                   seq_col_name,
                                   type) {
  loc_data_renamed <- rename(location_data,
    location_code = location,
    location = abbreviation
  )
  clean_latest_data <- raw_variant_data |>
    mutate(
      clades_modeled = ifelse(clade %in% clade_list, clade, "other")
    ) |>
    rename(
      sequences = {{ seq_col_name }},
      date = target_date
    ) |>
    dplyr::filter(
      location %in% location_data$abbreviation
    ) |>
    left_join(
      loc_data_renamed,
      by = "location"
    ) |>
    mutate(type = !!type) |>
    group_by(
      clades_modeled, location_name, location, location_code, population,
      type, date
    ) |>
    summarise(sequences = sum(sequences)) |>
    ungroup() |>
    mutate(nowcast_date = nowcast_date)
  return(clean_latest_data)
}
#' Get clean variant data from raw data
#'
#' @param raw_variant_data Data.frame of latest data extracted directly from
#'    next strain
#' @param clade_list Vector of character strings of the clade names
#' @param location_data Data.frame of location information
#' @param nowcast_dates Vector of character strings indicate the date range of
#'    the data.
#' @param type Character string indicating data is as of the nowcast date or
#'   evaluation data
#' @param nowcast_days Number of days we nowcast, default is `31`.
#' @param forecast_days Number of days we forecast, default is `10`.
#'
#' @returns Data.frame of counts of sequences of each clade we nowcasted during
#'   the season
#' @importFrom lubridate ymd days
#' @importFrom dplyr case_when rename left_join
#' @autoglobal
get_clean_variant_data_ns <- function(raw_variant_data,
                                      clade_list,
                                      location_data,
                                      nowcast_dates,
                                      type,
                                      nowcast_days = 31,
                                      forecast_days = 10) {
  loc_data_renamed <- rename(location_data,
    location_code = location,
    location = abbreviation
  )
  clean_latest_data <- raw_variant_data |>
    dplyr::mutate(
      location_name =
        case_when(
          location == "Washington DC" ~ "District of Columbia",
          location == "Deleware" ~ "Delaware",
          location == "Louisana" ~ "Louisiana",
          TRUE ~ location
        ),
      clades_modeled = ifelse(clade %in% clade_list, clade, "other")
    ) |>
    dplyr::select(-location) |>
    dplyr::filter(
      location_name %in% location_data$location_name,
      date <= ymd(max(nowcast_dates)) + days(forecast_days),
      date >= ymd(min(nowcast_dates)) - days(nowcast_days)
    ) |>
    left_join(
      loc_data_renamed,
      by = "location_name"
    ) |>
    mutate(type = !!type) |>
    group_by(
      clades_modeled, location_name, location, location_code, population,
      type, date
    ) |>
    summarise(sequences = sum(sequences)) |>
    ungroup()
  return(clean_latest_data)
}

#' Convert the scores to a scoringutils object
#'
#' @param scores_data Data.frame from Variant Nowcast Hub GitHub
#' @param brier_to_use Character string indicating
#' @importFrom data.table setattr as.data.table
#' @importFrom rlang arg_match
#' @importFrom dplyr rename select
#' @returns scoringutils object
convert_to_su_object <- function(scores_data,
                                 brier_to_use = c(
                                   "brier_point",
                                   "brier_dist"
                                 )) {
  brier_to_use <- rlang::arg_match(brier_to_use)
  scores2 <- scores_data |>
    rename(
      energy_score = energy,
      brier_score = {{ brier_to_use }},
      model = model_id
    ) |>
    select(
      location, target_date, nowcast_date, model, energy_score,
      brier_score, scored, status
    ) |>
    data.table::as.data.table()
  class(scores2) <- c("scores", class(scores2))
  scores_su <- data.table::setattr(
    scores2,
    "metrics",
    c("brier_score", "energy_score")
  )
  return(scores_su)
}

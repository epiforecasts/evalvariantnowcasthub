#' Get clean variant data from raw data
#'
#' @param raw_variant_data Data.frame of latest data extracted directly from
#'    next strain
#' @param clade_list Vector of character strings of the clade names
#' @param location_data Data.frame of location information
#' @param nowcast_dates Vector of character strings indicate the date range of
#'    the data.
#' @param nowcast_days Number of days we nowcast, default is `31`.
#'
#' @returns Data.frame of counts of sequences of each clade we nowcasted during
#'   the season
#' @importFrom lubridate ymd days
#' @importFrom dplyr case_when
#' @autoglobal
get_clean_variant_data <- function(raw_variant_data,
                                   clade_list,
                                   location_data,
                                   nowcast_dates,
                                   nowcast_days = 31) {
  clean_latest_data <- raw_variant_data |>
    dplyr::mutate(
      location =
        case_when(
          location == "Washington DC" ~ "District of Columbia",
          location == "Deleware" ~ "Delaware",
          location == "Louisana" ~ "Louisiana",
          TRUE ~ location
        ),
      clades_modeled = ifelse(clade %in% clade_list, clade, "other")
    ) |>
    dplyr::filter(
      location %in% location_data$location_name,
      date <= ymd(max(nowcast_dates)),
      date >= ymd(min(nowcast_dates)) - days(nowcast_days)
    ) |>
    left_join(location_data, by = c("location" = "location_name")) |>
    rename(
      location = abbreviation,
      location_code = location
    )

  return(clean_latest_data)
}

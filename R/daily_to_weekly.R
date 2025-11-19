#' Convert sequence data from daily to weekly
#'
#' @param daily_data Data.frame of sequence counts by location and clade
#'
#' @returns Data aggregated by epiweek.
#' @importFrom lubridate epiweek epiyear
daily_to_weekly <- function(daily_data) {
  date_spine <- tibble(
    date = seq(
      from = min(daily_data$date),
      to = max(daily_data$date), by = "day"
    )
  ) |>
    mutate(
      epi_week = epiweek(date),
      epi_year = epiyear(date),
      wday = wday(date, label = TRUE)
    ) |>
    filter(wday == "Sat")

  metadata <- daily_data |>
    select(location, location_name, location_code, population, type) |>
    unique()
  weekly_data <- daily_data |>
    mutate(
      epi_week = epiweek(date),
      epi_year = epiyear(date)
    ) |>
    group_by(
      epi_year, epi_week,
      location, clades_modeled
    ) |>
    summarise(
      sequences = sum(sequences),
      .groups = "drop"
    ) |>
    left_join(date_spine, by = c("epi_week", "epi_year")) |>
    left_join(metadata, by = "location")
  return(weekly_data)
}

#' Convert scores data from daily to weekly
#'
#' @param daily_data Data.frame of scores by location, model, and nowcast date
#'
#' @returns Data aggregated by epiweek.
#' @importFrom lubridate epiweek epiyear
daily_to_weekly_scores <- function(daily_data) {
  date_spine <- tibble(
    date = seq(
      from = min(daily_data$target_date),
      to = max(daily_data$target_date), by = "day"
    )
  ) |>
    mutate(
      epi_week = epiweek(date),
      epi_year = epiyear(date),
      wday = wday(date, label = TRUE)
    ) |>
    filter(wday == "Sat")

  weekly_data <- daily_data |>
    mutate(
      epi_week = epiweek(target_date),
      epi_year = epiyear(target_date)
    ) |>
    group_by(
      epi_year, epi_week,
      location, model, nowcast_date
    ) |>
    summarise(
      energy_score = mean(energy_score, na.rm = TRUE),
      scored = any(scored),
      .groups = "drop"
    ) |>
    left_join(date_spine, by = c("epi_week", "epi_year"))
  return(weekly_data)
}

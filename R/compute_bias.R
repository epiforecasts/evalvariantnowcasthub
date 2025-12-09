#' Prepare data for scoringutils bias computation
#'
#' @param df_mult_nowcasts Data frame with quantile predictions and observations
#' @param clade Character string indicating the clade to analyze
#' @param horizon_range Numeric vector of length 2 with min and max horizons
#'
#' @returns Data frame in long format ready for scoringutils
#' @importFrom dplyr filter mutate
#' @importFrom tidyr pivot_longer
#' @autoglobal
prepare_data_for_scoring_25A <- function(df_mult_nowcasts,
                                         clade = "25A",
                                         horizon_range = c(-31, 10)) {
  # Filter for specified clade and horizon range
  df_filtered <- df_mult_nowcasts |>
    filter(clade == !!clade) |>
    mutate(horizon = as.integer(target_date - nowcast_date)) |>
    filter(horizon >= min(horizon_range), horizon <= max(horizon_range))

  # Reshape from wide to long format for scoringutils
  # Current: q_0.5, q_0.025, q_0.975, q_0.25, q_0.75
  # Needed: quantile_level, predicted columns
  df_long <- df_filtered |>
    pivot_longer(
      cols = starts_with("q_"),
      names_to = "quantile_level",
      values_to = "predicted",
      names_prefix = "q_"
    ) |>
    mutate(
      quantile_level = as.numeric(quantile_level),
      observed = sequences / n_seq
    )

  return(df_long)
}

#' Compute bias using scoringutils
#'
#' @param df_prepared Prepared data in long format
#' @param locs Vector of location codes to include
#' @param nowcast_dates Vector of nowcast dates to include
#'
#' @returns Data frame with bias scores by model, location, and nowcast_date
#' @importFrom dplyr filter group_by summarise
#' @importFrom scoringutils as_forecast_quantile score bias_quantile
#' @autoglobal
compute_bias_25A <- function(df_prepared, locs, nowcast_dates) {
  # Filter to specific locations and nowcast dates
  df_to_score <- filter(
    df_prepared,
    location %in% locs,
    nowcast_date %in% nowcast_dates
  )

  # Convert to scoringutils forecast object
  forecast_obj <- scoringutils::as_forecast_quantile(
    df_to_score,
    forecast_unit = c(
      "model_id", "location", "nowcast_date",
      "target_date", "clade"
    ),
    observed = "observed",
    predicted = "predicted",
    quantile_level = "quantile_level"
  )

  # Score the forecasts - compute bias using scoringutils
  scores <- scoringutils::score(
    forecast_obj,
    metrics = list(bias = scoringutils::bias_quantile)
  )

  # Aggregate bias by model, location, and nowcast_date
  bias_summary <- scores |>
    group_by(model = model_id, location, nowcast_date) |>
    summarise(
      bias = mean(bias, na.rm = TRUE),
      .groups = "drop"
    )

  return(bias_summary)
}

#' Compute prediction interval coverage using scoringutils
#'
#' @param df_prepared Prepared data in long format
#' @param locs Vector of location codes to include
#' @param nowcast_dates Vector of nowcast dates to include
#'
#' @returns Data frame with coverage for 50% and 95% intervals by model,
#'   location, and nowcast_date
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom scoringutils as_forecast_quantile score interval_coverage
#' @autoglobal
compute_coverage_25A <- function(df_prepared, locs, nowcast_dates,
                                 intervals = c(50, 95)) {
  # Filter to specific locations and nowcast dates
  df_to_score <- filter(
    df_prepared,
    location %in% locs,
    nowcast_date %in% nowcast_dates
  )

  # Convert to scoringutils forecast object
  forecast_obj <- scoringutils::as_forecast_quantile(
    df_to_score,
    forecast_unit = c(
      "model_id", "location", "nowcast_date",
      "target_date", "clade"
    ),
    observed = "observed",
    predicted = "predicted",
    quantile_level = "quantile_level"
  )
  all_coverage <- scoringutils::get_coverage(
    forecast_obj,
    by = c(
      "location", "nowcast_date", "target_date",
      "model_id", "clade"
    )
  )
  coverage <- filter(
    all_coverage,
    interval_range %in% c(intervals)
  )

  coverage_summarised <- coverage |>
    group_by(model_id, location, interval_range) |>
    summarise(empirical_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = empirical_coverage
    ) |>
    mutate(`95` = `95` - `50`) |>
    pivot_longer(
      cols = c(`50`, `95`),
      names_to = "interval_range",
      values_to = "empirical_coverage"
    ) |>
    mutate(
      interval_label = paste0(interval_range, "%"),
      interval_range = factor(interval_range, levels = c("95", "50"))
    )

  return(coverage_summarised)
}

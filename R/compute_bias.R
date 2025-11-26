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

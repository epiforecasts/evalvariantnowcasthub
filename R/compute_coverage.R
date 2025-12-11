#' Compute prediction interval coverage using scoringutils
#'
#' @param df_prepared Prepared data in long format
#' @param locs Vector of location codes to include
#' @param nowcast_dates Vector of nowcast dates to include
#' @param intervals Numeric vector of interval ranges to include
#'   (default: c(50, 95)
#'
#' @returns Data frame with coverage for 50% and 95% intervals by model,
#'   location, and nowcast_date
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom scoringutils as_forecast_quantile score get_coverage
#' @autoglobal
compute_coverage <- function(df_prepared, locs, nowcast_dates,
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

  return(coverage)
}

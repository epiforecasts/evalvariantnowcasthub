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
#' @importFrom dplyr filter group_by summarise mutate bind_rows select
#' @importFrom scoringutils as_forecast_quantile score get_coverage
#' @autoglobal
compute_coverage <- function(df_prepared, locs, nowcast_dates,
                             intervals = c(50, 95)) {
  # Process by location to reduce memory usage
  coverage_list <- lapply(locs, function(loc) {
    # Filter to specific location and nowcast dates
    df_to_score <- filter(
      df_prepared,
      location == loc,
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

    # Get coverage at target_date level (needed for accurate computation)
    all_coverage <- scoringutils::get_coverage(
      forecast_obj,
      by = c(
        "location", "nowcast_date", "target_date",
        "model_id", "clade"
      )
    )

    # Immediately filter to desired intervals and select only needed columns
    coverage_loc <- all_coverage |>
      filter(interval_range %in% c(intervals)) |>
      select(location, nowcast_date, model_id, clade, interval_range,
             interval_coverage, target_date)

    return(coverage_loc)
  })

  # Combine all locations
  coverage <- bind_rows(coverage_list)

  return(coverage)
}

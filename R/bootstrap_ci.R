#' Draw moving block bootstrap resamples of nowcast dates
#'
#' Draws `n_boot` moving block bootstrap resamples of the nowcast dates
#' (Kunsch 1989), as used in Pohle, Zahn & Lerch (2026). Each resample
#' concatenates `ceiling(N / block_length)` blocks of consecutive nowcast
#' dates, with block starts drawn uniformly with replacement, and is
#' trimmed to the original length `N`.
#'
#' Each resample (`boot_id`) is a synthetic evaluation period of `N` slots,
#' numbered by `position` from 1 to `N`. Positions `1:block_length` hold the
#' first block, the next `block_length` positions the second block, and so
#' on, with the final block trimmed so there are exactly `N` positions. For
#' example, with `block_length = 6` a resample might hold weeks 18-23 at
#' positions 1-6, weeks 18-23 again at positions 7-12 and weeks 15-20 at
#' positions 13-18.
#'
#' Because block starts are drawn with replacement and blocks overlap, the
#' same nowcast date usually appears at several positions within a
#' resample (week 18 at positions 1, 7 and 16 above). When the scores are
#' joined to a resample by `nowcast_date` (see [get_resampled_scores()]),
#' every score row for that date is copied once per position, and the
#' `position` column becomes part of the forecast unit. Each copy is
#' therefore a distinct forecast to scoringutils rather than a duplicate.
#' The order of positions carries no other meaning: the means and pairwise
#' ratios computed from a resample do not depend on it.
#'
#' The resamples are drawn once and shared across all metrics, score types
#' and regions so that every bootstrap estimate uses the same resampled
#' nowcast dates.
#'
#' @param nowcast_dates Vector of nowcast dates in the evaluation period.
#'   Must be evenly spaced so that a block of positions is a block of time.
#' @param n_boot Integer number of bootstrap resamples.
#' @param block_length Integer block length, in number of nowcast dates.
#' @returns Data frame with columns `boot_id` (resample), `position` (slot
#'   within the resample, `1:N`) and `nowcast_date` (the nowcast date drawn
#'   into that slot), with `n_boot * length(nowcast_dates)` rows.
#' @autoglobal
get_block_bootstrap_dates <- function(nowcast_dates,
                                      n_boot,
                                      block_length) {
  nowcast_dates <- sort(unique(as.Date(nowcast_dates)))
  n_dates <- length(nowcast_dates)
  if (block_length < 1 || block_length > n_dates) {
    cli::cli_abort(
      "{.arg block_length} must be between 1 and {n_dates}, not {block_length}."
    )
  }
  if (length(unique(diff(nowcast_dates))) > 1) {
    cli::cli_abort("{.arg nowcast_dates} must be evenly spaced.")
  }

  n_blocks <- ceiling(n_dates / block_length)
  boot_ids <- lapply(seq_len(n_boot), function(b) {
    starts <- sample.int(n_dates - block_length + 1, n_blocks, replace = TRUE)
    idx <- as.vector(outer(seq_len(block_length) - 1, starts, "+"))
    idx[seq_len(n_dates)]
  })

  data.frame(
    boot_id = rep(seq_len(n_boot), each = n_dates),
    position = rep(seq_len(n_dates), times = n_boot),
    nowcast_date = nowcast_dates[unlist(boot_ids)]
  )
}

#' Build one bootstrap resample of the scores
#'
#' Joins the scores to the nowcast dates drawn for resample `b` (see
#' [get_block_bootstrap_dates()]). Every score row for a nowcast date is
#' repeated once for each position that date was drawn into, and dates not
#' drawn are dropped. The added `position` column becomes part of the
#' scoringutils forecast unit, so repeated dates are treated as distinct
#' forecasts.
#'
#' @param scores_obj Scoringutils scores object with a `nowcast_date`
#'   column.
#' @param boot_dates Data frame of resampled nowcast dates, as returned by
#'   [get_block_bootstrap_dates()].
#' @param b Integer index of the resample (`boot_id`) to build.
#' @importFrom dplyr inner_join
#' @returns Scores object with the rows for resample `b` and an extra
#'   `position` column.
#' @autoglobal
get_resampled_scores <- function(scores_obj, boot_dates, b) {
  dates_b <- boot_dates[boot_dates$boot_id == b, c("position", "nowcast_date")]
  inner_join(
    scores_obj,
    dates_b,
    by = "nowcast_date",
    relationship = "many-to-many"
  )
}

#' Mean score by model over all rows
#'
#' Averages a score over every scored row (location, target date and
#' nowcast date) for each model, as in the absolute score bar charts of
#' [get_plot_overall()]. Rows where the score is missing are dropped; in
#' our data a model's score is either missing for all rows or none.
#'
#' @param scores Scoringutils scores object, or a resample of one from
#'   [get_resampled_scores()].
#' @param score_type Character string indicating which score metric to use.
#' @importFrom dplyr filter group_by summarise n_distinct
#' @importFrom rlang sym
#' @returns Data frame with columns `model`, `estimate` (the mean score)
#'   and `n_nowcast_dates` (number of distinct nowcast dates scored).
#' @autoglobal
get_mean_score_by_model <- function(scores,
                                    score_type = c(
                                      "brier_score",
                                      "energy_score"
                                    )) {
  score_type <- rlang::arg_match(score_type)
  as.data.frame(scores) |>
    filter(!is.na(!!sym(score_type))) |>
    group_by(model) |>
    summarise(
      estimate = mean(!!sym(score_type)),
      n_nowcast_dates = n_distinct(nowcast_date)
    )
}

#' Bootstrap the mean score by model
#'
#' Computes [get_mean_score_by_model()] on every resample in `boot_dates`.
#'
#' @inheritParams get_resampled_scores
#' @inheritParams get_mean_score_by_model
#' @importFrom dplyr bind_rows mutate select
#' @returns Data frame with columns `boot_id`, `model` and `estimate`, one
#'   row per resample and model.
#' @autoglobal
get_bootstrap_mean_scores <- function(scores_obj,
                                      boot_dates,
                                      score_type) {
  boot_ids <- unique(boot_dates$boot_id)
  boot_estimates <- lapply(boot_ids, function(b) {
    get_resampled_scores(scores_obj, boot_dates, b) |>
      get_mean_score_by_model(score_type) |>
      mutate(boot_id = b)
  })
  bind_rows(boot_estimates) |>
    select(boot_id, model, estimate)
}

#' Bonferroni simultaneous confidence bands from bootstrap estimates
#'
#' Constructs simultaneous confidence bands following Pohle, Zahn & Lerch
#' (2026): `estimate +/- c * sd`, where `sd` is the standard deviation of
#' the bootstrap estimates and `c = qnorm(1 - alpha / (2P))` is the
#' Bonferroni critical value for the `P` rows of `estimates`. The bands
#' therefore hold simultaneously across all rows of `estimates`, so pass
#' one table (e.g. one region and score type) at a time.
#'
#' With `log_scale = TRUE` the bands are built for `log(estimate)` and
#' back-transformed, `exp(log(estimate) +/- c * sd(log(bootstrap)))`, so
#' they are symmetric about the estimate on a log axis and positive. Use
#' this for ratios such as the scaled relative skill. Coverage is unchanged
#' by the back-transformation because `exp()` is monotone.
#'
#' @param estimates Data frame of point estimates from the original data,
#'   with columns `by` and `estimate`. Each row is one comparison.
#' @param boot_estimates Data frame of bootstrap estimates with columns
#'   `boot_id`, `by` and `estimate`.
#' @param alpha Significance level of the simultaneous bands.
#' @param by Character vector of columns identifying a row of `estimates`,
#'   e.g. `"model"` or `c("model", "horizon")`.
#' @param log_scale Logical; build the bands on the log scale. Default
#'   `FALSE`.
#' @importFrom dplyr group_by summarise left_join mutate n across all_of
#' @importFrom stats qnorm sd
#' @returns `estimates` with added columns `boot_sd` (on the scale the band
#'   is built on), `n_boot` (resamples with an estimate), `lower`, `upper`,
#'   `n_comparisons` (`P`), `crit_value` (`c`), `alpha` and `log_scale`.
#' @autoglobal
get_bonferroni_ci <- function(estimates,
                              boot_estimates,
                              alpha,
                              by = "model",
                              log_scale = FALSE) {
  trans <- if (log_scale) log else identity
  inv_trans <- if (log_scale) exp else identity
  n_comparisons <- nrow(estimates)
  crit_value <- qnorm(1 - alpha / (2 * n_comparisons))
  boot_sd <- boot_estimates |>
    filter(!is.na(estimate)) |>
    group_by(across(all_of(by))) |>
    summarise(boot_sd = sd(trans(estimate)), n_boot = n(), .groups = "drop")
  estimates |>
    left_join(boot_sd, by = by) |>
    mutate(
      lower = inv_trans(trans(estimate) - crit_value * boot_sd),
      upper = inv_trans(trans(estimate) + crit_value * boot_sd),
      n_comparisons = n_comparisons,
      crit_value = crit_value,
      alpha = alpha,
      log_scale = log_scale
    )
}

#' Scaled relative skill against the baseline, overall
#'
#' Runs the full scoringutils pairwise tournament across all models, as in
#' the relative skill panels of [get_plot_overall()], and returns each
#' model's scaled relative skill (SRSS) against `baseline`. Each pair of
#' models is compared on the forecasts they share, so models need not
#' cover the same nowcast dates or locations.
#'
#' @param scores Scoringutils scores object, or a resample of one from
#'   [get_resampled_scores()].
#' @param score_type Character string indicating which score metric to use.
#' @param baseline Character string naming the baseline model.
#' @importFrom dplyr filter select
#' @importFrom rlang sym
#' @returns Data frame with columns `model` and `estimate` (the SRSS), one
#'   row per model other than the baseline.
#' @autoglobal
get_srss <- function(scores,
                     score_type = c("brier_score", "energy_score"),
                     baseline = "Hub-baseline") {
  score_type <- rlang::arg_match(score_type)
  srss_col <- glue::glue("{score_type}_scaled_relative_skill")
  scores |>
    filter(!is.na(!!sym(score_type))) |>
    scoringutils::get_pairwise_comparisons(
      baseline = baseline,
      metric = score_type
    ) |>
    as.data.frame() |>
    filter(model != baseline, compare_against == baseline) |>
    select(model, estimate = !!sym(srss_col))
}

#' Bootstrap the overall scaled relative skill
#'
#' Re-runs the full pairwise tournament ([get_srss()]) on every resample in
#' `boot_dates`.
#'
#' @inheritParams get_resampled_scores
#' @inheritParams get_srss
#' @importFrom dplyr bind_rows mutate select
#' @returns Data frame with columns `boot_id`, `model` and `estimate`.
#' @autoglobal
get_bootstrap_srss <- function(scores_obj,
                               boot_dates,
                               score_type,
                               baseline = "Hub-baseline") {
  boot_ids <- unique(boot_dates$boot_id)
  boot_estimates <- lapply(boot_ids, function(b) {
    get_resampled_scores(scores_obj, boot_dates, b) |>
      get_srss(score_type, baseline) |>
      mutate(boot_id = b)
  })
  bind_rows(boot_estimates) |>
    select(boot_id, model, estimate)
}

#' Daily scaled relative skill for each forecast
#'
#' Computes the scaled relative skill within each target date and nowcast
#' date, as in the relative skill by horizon panels of
#' [get_plot_horizon()], and adds the horizon in days.
#'
#' @inheritParams get_srss
#' @param scores_obj Scoringutils scores object.
#' @importFrom dplyr mutate
#' @returns Scores object with the added relative skill columns and
#'   `horizon`.
#' @autoglobal
get_daily_srss <- function(scores_obj, score_type) {
  get_relative_skill(
    scores_obj,
    score_type = score_type,
    by = c("target_date", "nowcast_date")
  ) |>
    mutate(horizon = as.integer(target_date - nowcast_date))
}

#' Scaled relative skill by horizon
#'
#' Averages the daily scaled relative skill from [get_daily_srss()] over
#' all rows for each model and horizon, matching
#' `summarise_scores(by = c("model", "horizon"))` in [get_plot_horizon()].
#'
#' @param daily_srss Output of [get_daily_srss()], or a resample of it from
#'   [get_resampled_scores()].
#' @inheritParams get_srss
#' @importFrom dplyr filter group_by summarise
#' @importFrom rlang sym
#' @returns Data frame with columns `model`, `horizon` and `estimate`.
#' @autoglobal
get_srss_by_horizon <- function(daily_srss,
                                score_type = c("brier_score", "energy_score"),
                                baseline = "Hub-baseline") {
  score_type <- rlang::arg_match(score_type)
  srss_col <- glue::glue("{score_type}_scaled_relative_skill")
  as.data.frame(daily_srss) |>
    filter(model != baseline) |>
    group_by(model, horizon) |>
    summarise(estimate = mean(!!sym(srss_col)), .groups = "drop")
}

#' Bootstrap the scaled relative skill by horizon
#'
#' Each daily tournament in [get_daily_srss()] only uses forecasts from one
#' nowcast date, and the bootstrap resamples whole nowcast dates, so the
#' daily scaled relative skill of a forecast is the same in every resample
#' that contains it (copies of a nowcast date copy every model, leaving the
#' ratios unchanged). The daily values are therefore computed once and
#' re-averaged over the rows of each resample, which is equivalent to, and
#' much faster than, recomputing the tournaments.
#'
#' @inheritParams get_resampled_scores
#' @inheritParams get_srss
#' @importFrom dplyr bind_rows mutate select
#' @returns Data frame with columns `boot_id`, `model`, `horizon` and
#'   `estimate`.
#' @autoglobal
get_bootstrap_srss_by_horizon <- function(scores_obj,
                                          boot_dates,
                                          score_type,
                                          baseline = "Hub-baseline") {
  daily_srss <- get_daily_srss(scores_obj, score_type)
  boot_ids <- unique(boot_dates$boot_id)
  boot_estimates <- lapply(boot_ids, function(b) {
    get_resampled_scores(daily_srss, boot_dates, b) |>
      get_srss_by_horizon(score_type, baseline) |>
      mutate(boot_id = b)
  })
  bind_rows(boot_estimates) |>
    select(boot_id, model, horizon, estimate)
}

#' Number of nowcast dates each model shares with the baseline
#'
#' Counts the nowcast dates on which both a model and the baseline have
#' scores, i.e. the dates contributing to their pairwise comparison.
#'
#' @inheritParams get_srss
#' @param scores_obj Scoringutils scores object.
#' @importFrom dplyr filter distinct inner_join count select
#' @importFrom rlang sym
#' @returns Data frame with columns `model` and `n_overlapping_dates`.
#' @autoglobal
get_n_overlapping_dates <- function(scores_obj,
                                    score_type = c(
                                      "brier_score",
                                      "energy_score"
                                    ),
                                    baseline = "Hub-baseline") {
  score_type <- rlang::arg_match(score_type)
  dates <- as.data.frame(scores_obj) |>
    filter(!is.na(!!sym(score_type))) |>
    distinct(model, nowcast_date)
  baseline_dates <- filter(dates, model == baseline)
  dates |>
    filter(model != baseline) |>
    inner_join(
      select(baseline_dates, nowcast_date),
      by = "nowcast_date"
    ) |>
    count(model, name = "n_overlapping_dates")
}

#' Format confidence bands as a results table
#'
#' Labels a table of Bonferroni bands from [get_bonferroni_ci()] with its
#' region, score type and metric, and puts the columns in a standard order
#' for saving.
#'
#' @param ci Confidence bands, as returned by [get_bonferroni_ci()].
#' @param region Character string naming the region, e.g.
#'   `"US excluding CA"`.
#' @param score_type Character string naming the score.
#' @param metric Character string naming the metric, e.g.
#'   `"mean score"` or `"SRSS"`.
#' @param n_dates Optional data frame with column `model` and a count of
#'   nowcast dates to add, e.g. from [get_n_overlapping_dates()].
#' @importFrom dplyr mutate rename select any_of left_join
#' @returns Data frame with columns `region`, `score_type`, `metric`,
#'   `model`, `horizon` (if present), `estimate`, `ci_lower`, `ci_upper`,
#'   the nowcast date count, `n_comparisons`, `crit_value`, `alpha`,
#'   `scale`, `boot_sd` and `n_boot`.
#' @autoglobal
format_ci_table <- function(ci, region, score_type, metric, n_dates = NULL) {
  if (!is.null(n_dates)) {
    ci <- left_join(ci, n_dates, by = "model")
  }
  ci |>
    mutate(
      region = region,
      score_type = score_type,
      metric = metric,
      scale = ifelse(log_scale, "log", "raw")
    ) |>
    rename(ci_lower = lower, ci_upper = upper) |>
    select(any_of(c(
      "region", "score_type", "metric", "model", "horizon", "estimate",
      "ci_lower", "ci_upper", "n_nowcast_dates", "n_overlapping_dates",
      "n_comparisons", "crit_value", "alpha", "scale", "boot_sd", "n_boot"
    )))
}

#' Save a table as a CSV file
#'
#' @param table Data frame to save.
#' @param file_name Character string, file name without extension.
#' @param output_fp Character string indicating directory to save file.
#' @importFrom fs dir_create
#' @importFrom readr write_csv
#' @returns Character string with the path of the saved file.
#' @autoglobal
save_table_csv <- function(table,
                           file_name,
                           output_fp = file.path("output", "tables")) {
  dir_create(output_fp, recurse = TRUE)
  path <- file.path(output_fp, glue::glue("{file_name}.csv"))
  write_csv(table, path)
  return(path)
}

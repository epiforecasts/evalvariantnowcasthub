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
#' @param estimates Data frame of point estimates from the original data,
#'   with columns `model` and `estimate`. Each row is one comparison.
#' @param boot_estimates Data frame of bootstrap estimates with columns
#'   `boot_id`, `model` and `estimate`.
#' @param alpha Significance level of the simultaneous bands.
#' @importFrom dplyr group_by summarise left_join mutate n
#' @importFrom stats qnorm sd
#' @returns `estimates` with added columns `boot_sd`, `n_boot` (resamples
#'   in which the model was scored), `lower`, `upper`, `n_comparisons`
#'   (`P`), `crit_value` (`c`) and `alpha`.
#' @autoglobal
get_bonferroni_ci <- function(estimates, boot_estimates, alpha) {
  n_comparisons <- nrow(estimates)
  crit_value <- qnorm(1 - alpha / (2 * n_comparisons))
  boot_sd <- boot_estimates |>
    filter(!is.na(estimate)) |>
    group_by(model) |>
    summarise(boot_sd = sd(estimate), n_boot = n())
  estimates |>
    left_join(boot_sd, by = "model") |>
    mutate(
      lower = estimate - crit_value * boot_sd,
      upper = estimate + crit_value * boot_sd,
      n_comparisons = n_comparisons,
      crit_value = crit_value,
      alpha = alpha
    )
}

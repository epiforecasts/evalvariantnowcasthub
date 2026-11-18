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
#' [get_plot_overall()], or within groups such as location as in
#' [get_plot_by_location()]. Rows where the score is missing are dropped; in
#' our data a model's score is either missing for all rows or none.
#'
#' @param scores Scoringutils scores object, or a resample of one from
#'   [get_resampled_scores()].
#' @param score_type Character string indicating which score metric to use.
#' @param by Optional character vector of columns to compute the mean
#'   within, in addition to model, e.g. `"location"`. Default `NULL` gives
#'   the overall mean per model.
#' @importFrom dplyr filter group_by summarise n_distinct across all_of
#' @importFrom rlang sym
#' @returns Data frame with columns `by` (if given), `model`, `estimate`
#'   (the mean score) and `n_nowcast_dates` (number of distinct nowcast
#'   dates scored).
#' @autoglobal
get_mean_score_by_model <- function(scores,
                                    score_type = c(
                                      "brier_score",
                                      "energy_score"
                                    ),
                                    by = NULL) {
  score_type <- rlang::arg_match(score_type)
  as.data.frame(scores) |>
    filter(!is.na(!!sym(score_type))) |>
    group_by(across(all_of(c(by, "model")))) |>
    summarise(
      estimate = mean(!!sym(score_type)),
      n_nowcast_dates = n_distinct(nowcast_date),
      .groups = "drop"
    )
}

#' Bootstrap the mean score by model
#'
#' Computes [get_mean_score_by_model()] on every resample in `boot_dates`.
#'
#' @inheritParams get_resampled_scores
#' @inheritParams get_mean_score_by_model
#' @importFrom dplyr bind_rows mutate select all_of
#' @returns Data frame with columns `boot_id`, `by` (if given), `model` and
#'   `estimate`, one row per resample, group and model.
#' @autoglobal
get_bootstrap_mean_scores <- function(scores_obj,
                                      boot_dates,
                                      score_type,
                                      by = NULL) {
  boot_ids <- unique(boot_dates$boot_id)
  boot_estimates <- lapply(boot_ids, function(b) {
    get_resampled_scores(scores_obj, boot_dates, b) |>
      get_mean_score_by_model(score_type, by = by) |>
      mutate(boot_id = b)
  })
  bind_rows(boot_estimates) |>
    select(boot_id, all_of(by), model, estimate)
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
#' by the back-transformation because `exp()` is monotone. Bootstrap
#' estimates that are not positive and finite (e.g. a scaled relative skill
#' of 0 when a model's only overlapping scores in a sparse resample are 0)
#' have no log and are dropped; `n_boot` reports how many were used.
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
  if (log_scale) {
    boot_estimates <- filter(
      boot_estimates,
      is.finite(estimate), estimate > 0
    )
  }
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
#' cover the same nowcast dates or locations. With `by`, a separate
#' tournament is run within each group, e.g. `by = "location"` as in
#' [get_plot_by_location()].
#'
#' @param scores Scoringutils scores object, or a resample of one from
#'   [get_resampled_scores()].
#' @param score_type Character string indicating which score metric to use.
#' @param baseline Character string naming the baseline model.
#' @param by Optional character vector of columns to run separate
#'   tournaments within, passed to
#'   [scoringutils::get_pairwise_comparisons()]. Default `NULL` gives one
#'   overall tournament.
#' @importFrom dplyr filter select all_of
#' @importFrom rlang sym
#' @returns Data frame with columns `by` (if given), `model` and `estimate`
#'   (the SRSS), one row per group and model other than the baseline.
#' @autoglobal
get_srss <- function(scores,
                     score_type = c("brier_score", "energy_score"),
                     baseline = "Hub-baseline",
                     by = NULL) {
  score_type <- rlang::arg_match(score_type)
  srss_col <- glue::glue("{score_type}_scaled_relative_skill")
  scores |>
    filter(!is.na(!!sym(score_type))) |>
    scoringutils::get_pairwise_comparisons(
      baseline = baseline,
      metric = score_type,
      by = by
    ) |>
    as.data.frame() |>
    filter(model != baseline, compare_against == baseline) |>
    select(all_of(by), model, estimate = !!sym(srss_col))
}

#' Bootstrap the scaled relative skill
#'
#' Re-runs the full pairwise tournament ([get_srss()]) on every resample in
#' `boot_dates`, overall or within the groups in `by`.
#'
#' @inheritParams get_resampled_scores
#' @inheritParams get_srss
#' @importFrom dplyr bind_rows mutate select all_of
#' @returns Data frame with columns `boot_id`, `by` (if given), `model` and
#'   `estimate`.
#' @autoglobal
get_bootstrap_srss <- function(scores_obj,
                               boot_dates,
                               score_type,
                               baseline = "Hub-baseline",
                               by = NULL) {
  boot_ids <- unique(boot_dates$boot_id)
  boot_estimates <- lapply(boot_ids, function(b) {
    get_resampled_scores(scores_obj, boot_dates, b) |>
      get_srss(score_type, baseline, by = by) |>
      mutate(boot_id = b)
  })
  bind_rows(boot_estimates) |>
    select(boot_id, all_of(by), model, estimate)
}

#' Keep bootstrap draws with the full model roster
#'
#' The scaled relative skill of a model depends on every model in the
#' pairwise tournament, not only on the model and the baseline. A resample
#' that contains none of a sparsely submitting model's dates drops that model
#' from the tournament, so the other models' SRSS in that draw describe a
#' different tournament from the reported estimate (#135). This keeps only
#' the draws (within each `by` group) in which every model of the original
#' tournament is present, so the bootstrap describes the same tournament as
#' the point estimate.
#'
#' @param boot_estimates Bootstrap estimates with columns `boot_id`, `by`,
#'   `model` and `estimate`, e.g. from [get_bootstrap_srss()].
#' @param estimates Estimates from the original data for the full
#'   tournament (before removing any models from the reported table), with
#'   columns `by` and `model`, e.g. from [get_srss()].
#' @param by Optional character vector of grouping columns, e.g.
#'   `"horizon"`, within which the roster is checked.
#' @importFrom dplyr semi_join group_by summarise filter n_distinct across
#'   all_of inner_join
#' @returns `boot_estimates` restricted to full-roster draws.
#' @autoglobal
filter_full_roster <- function(boot_estimates, estimates, by = NULL) {
  roster <- estimates |>
    group_by(across(all_of(by))) |>
    summarise(n_models = n_distinct(model), .groups = "drop")
  present <- boot_estimates |>
    filter(is.finite(estimate)) |>
    semi_join(estimates, by = c(by, "model")) |>
    group_by(across(all_of(c("boot_id", by)))) |>
    summarise(n_present = n_distinct(model), .groups = "drop")
  full <- if (length(by) == 0) {
    filter(present, n_present == roster$n_models)
  } else {
    present |>
      inner_join(roster, by = by) |>
      filter(n_present == n_models)
  }
  semi_join(boot_estimates, full, by = c("boot_id", by))
}

#' Add the horizon to a scores object
#'
#' Adds `horizon`, the target date minus the nowcast date in days, so that
#' scores can be summarised by horizon, e.g. with
#' `get_srss(by = "horizon")` as in [get_plot_horizon()].
#'
#' @param scores_obj Scoringutils scores object with `target_date` and
#'   `nowcast_date` columns.
#' @importFrom dplyr mutate
#' @returns `scores_obj` with an added integer `horizon` column.
#' @autoglobal
add_horizon <- function(scores_obj) {
  mutate(scores_obj, horizon = as.integer(target_date - nowcast_date))
}

#' Number of nowcast dates each model shares with the baseline
#'
#' Counts the nowcast dates on which both a model and the baseline have
#' scores, i.e. the dates contributing to their pairwise comparison,
#' overall or within the groups in `by` (e.g. per location).
#'
#' @inheritParams get_srss
#' @param scores_obj Scoringutils scores object.
#' @importFrom dplyr filter distinct inner_join count select all_of across
#' @importFrom rlang sym
#' @returns Data frame with columns `by` (if given), `model` and
#'   `n_overlapping_dates`.
#' @autoglobal
get_n_overlapping_dates <- function(scores_obj,
                                    score_type = c(
                                      "brier_score",
                                      "energy_score"
                                    ),
                                    baseline = "Hub-baseline",
                                    by = NULL) {
  score_type <- rlang::arg_match(score_type)
  dates <- as.data.frame(scores_obj) |>
    filter(!is.na(!!sym(score_type))) |>
    distinct(across(all_of(c(by, "model", "nowcast_date"))))
  baseline_dates <- filter(dates, model == baseline)
  dates |>
    filter(model != baseline) |>
    inner_join(
      select(baseline_dates, all_of(c(by, "nowcast_date"))),
      by = c(by, "nowcast_date")
    ) |>
    count(across(all_of(c(by, "model"))), name = "n_overlapping_dates")
}

#' Keep comparisons with enough overlapping nowcast dates for a band
#'
#' Adds the number of nowcast dates each model shares with the baseline
#' (from [get_n_overlapping_dates()]) to a table of estimates and keeps the
#' rows with at least `min_dates`. With very few dates the bootstrap can
#' only reweight those few weeks, so its band would understate the
#' uncertainty.
#'
#' @param estimates Data frame of estimates with column `model` and any
#'   grouping columns shared with `n_dates`.
#' @param n_dates Data frame from [get_n_overlapping_dates()].
#' @param min_dates Minimum number of overlapping nowcast dates.
#' @importFrom dplyr inner_join filter
#' @returns `estimates` with the added `n_overlapping_dates` column, rows
#'   with fewer than `min_dates` dropped.
#' @autoglobal
filter_min_overlap <- function(estimates, n_dates, min_dates) {
  estimates |>
    inner_join(n_dates, by = intersect(names(estimates), names(n_dates))) |>
    filter(n_overlapping_dates >= min_dates)
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
#' @param n_dates Optional data frame with column `model` (and any grouping
#'   columns shared with `ci`, e.g. `location`) and a count of nowcast dates
#'   to add, e.g. from [get_n_overlapping_dates()].
#' @importFrom dplyr mutate rename select any_of left_join
#' @returns Data frame with columns `region`, `score_type`, `metric`,
#'   `location` and `model`, `horizon` (if present), `estimate`, `ci_lower`,
#'   `ci_upper`,
#'   the nowcast date count, `n_comparisons`, `crit_value`, `alpha`,
#'   `scale`, `boot_sd` and `n_boot`.
#' @autoglobal
format_ci_table <- function(ci, region, score_type, metric, n_dates = NULL) {
  if (!is.null(n_dates)) {
    ci <- left_join(ci, n_dates, by = intersect(names(ci), names(n_dates)))
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
      "region", "score_type", "metric", "location", "model", "horizon",
      "estimate",
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

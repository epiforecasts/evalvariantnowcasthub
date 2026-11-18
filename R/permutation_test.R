#' Total scores of a model and the baseline by nowcast date
#'
#' Restricts to the forecasts (location, target date and nowcast date)
#' scored for both `model` and `baseline`, and sums each model's scores within
#' each nowcast date. These per-date totals are the units swapped in
#' [get_permutation_test()].
#'
#' @param scores_obj Scoringutils scores object.
#' @param score_type Character string indicating which score metric to use.
#' @param model Character string naming the model to compare.
#' @param baseline Character string naming the baseline model.
#' @importFrom dplyr filter select inner_join group_by summarise
#' @importFrom rlang sym
#' @returns Data frame with columns `nowcast_date`, `model_total`,
#'   `baseline_total` and `n_forecasts`, one row per nowcast date with at
#'   least one shared forecast.
#' @autoglobal
get_paired_date_totals <- function(scores_obj,
                                   score_type = c(
                                     "brier_score",
                                     "energy_score"
                                   ),
                                   model,
                                   baseline = "Hub-baseline") {
  score_type <- rlang::arg_match(score_type)
  keys <- c("location", "target_date", "nowcast_date")
  scores <- as.data.frame(scores_obj) |>
    filter(!is.na(!!sym(score_type)))
  pick <- function(m) {
    scores[scores$model == m, c(keys, score_type)] |>
      setNames(c(keys, "score"))
  }
  inner_join(pick(model), pick(baseline),
    by = keys, suffix = c("_model", "_baseline")
  ) |>
    group_by(nowcast_date) |>
    summarise(
      model_total = sum(score_model),
      baseline_total = sum(score_baseline),
      n_forecasts = dplyr::n(),
      .groups = "drop"
    )
}

#' Permutation test of a model against the baseline
#'
#' Tests whether a model's scores differ from the baseline's, using the
#' ratio of their mean scores on the forecasts both submitted. Under the
#' null hypothesis the two models are exchangeable, so within each nowcast
#' date their scores can be swapped. Each permutation swaps the model's and
#' baseline's totals on a random subset of nowcast dates, keeping all
#' locations and target dates of a nowcast date together (preserving
#' correlation across locations and horizons within a date). The
#' two-sided p-value is the proportion of permutations whose absolute log
#' ratio is at least as large as the observed one, computed as
#' `(1 + count) / (1 + n_perm)` so it is never zero.
#'
#' @param date_totals Data frame from [get_paired_date_totals()].
#' @param n_perm Integer number of random permutations.
#' @returns Data frame with one row and columns `n_dates` (nowcast dates
#'   with shared forecasts), `ratio` (model mean score / baseline mean
#'   score on the shared forecasts), `p_value` and `n_perm`.
#' @autoglobal
get_permutation_test <- function(date_totals, n_perm = 1e5) {
  m <- date_totals$model_total
  b <- date_totals$baseline_total
  observed <- abs(log(sum(m) / sum(b)))
  # Each row of `swap` flips the model and baseline totals on the nowcast
  # dates where it is 1; the swapped totals are sum(m) + swap %*% (b - m)
  # and sum(b) - swap %*% (b - m)
  swap <- matrix(
    stats::rbinom(n_perm * length(m), 1, 0.5),
    nrow = n_perm
  )
  shift <- as.vector(swap %*% (b - m))
  permuted <- abs(log((sum(m) + shift) / (sum(b) - shift)))
  n_extreme <- sum(permuted >= observed - 1e-12)
  data.frame(
    n_dates = length(m),
    ratio = sum(m) / sum(b),
    p_value = (1 + n_extreme) / (1 + n_perm),
    n_perm = n_perm
  )
}

#' Permutation tests of each model against the baseline
#'
#' Runs [get_permutation_test()] for every model other than the baseline,
#' and adds a Bonferroni adjustment across the models tested (one family per
#' region and score type, as for the simultaneous confidence bands).
#'
#' @inheritParams get_paired_date_totals
#' @inheritParams get_permutation_test
#' @param exclude Character vector of models not to test (e.g. models with
#'   too few nowcast dates for a meaningful test).
#' @importFrom dplyr bind_rows mutate
#' @returns Data frame with columns `model`, `n_dates`, `ratio`,
#'   `p_value`, `p_bonferroni`, `n_tests` and `n_perm`.
#' @autoglobal
get_permutation_tests <- function(scores_obj,
                                  score_type = c(
                                    "brier_score",
                                    "energy_score"
                                  ),
                                  baseline = "Hub-baseline",
                                  exclude = NULL,
                                  n_perm = 1e5) {
  score_type <- rlang::arg_match(score_type)
  scored <- as.data.frame(scores_obj)
  scored <- scored[!is.na(scored[[score_type]]), ]
  models <- setdiff(unique(scored$model), c(baseline, exclude))
  tests <- lapply(sort(models), function(m) {
    totals <- get_paired_date_totals(scores_obj, score_type, m, baseline)
    cbind(model = m, get_permutation_test(totals, n_perm))
  })
  bind_rows(tests) |>
    mutate(
      p_bonferroni = pmin(1, p_value * length(models)),
      n_tests = length(models),
      .after = p_value
    )
}

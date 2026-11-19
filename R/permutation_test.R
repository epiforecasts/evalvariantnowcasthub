#' Pair a model's and the baseline's scores for permutation
#'
#' Finds the forecasts (location, target date and nowcast date) scored for
#' both `model` and `baseline`, and labels each with its exchange unit (by
#' default a location and nowcast date). These are the scores swapped
#' between the two models in [get_permutation_srss()].
#'
#' @param scores_obj Scoringutils scores object.
#' @param score_type Character string indicating which score metric to use.
#' @param model Character string naming the model to compare.
#' @param baseline Character string naming the baseline model.
#' @param unit Character vector of columns defining the exchange unit: all
#'   shared forecasts within a unit are swapped together.
#' @importFrom dplyr inner_join group_indices group_by across all_of
#' @returns Data frame with one row per shared forecast and columns
#'   `row_model` and `row_baseline` (row indices into `scores_obj`) and
#'   `unit_id` (integer exchange unit).
#' @autoglobal
get_swap_pairs <- function(scores_obj,
                           score_type = c("brier_score", "energy_score"),
                           model,
                           baseline = "Hub-baseline",
                           unit = c("location", "nowcast_date")) {
  score_type <- rlang::arg_match(score_type)
  keys <- c("location", "target_date", "nowcast_date")
  scores <- as.data.frame(scores_obj)[, c("model", keys, score_type)]
  scores$row <- seq_len(nrow(scores))
  scores <- scores[!is.na(scores[[score_type]]), ]
  pick <- function(m) scores[scores$model == m, c(keys, "row")]
  paired <- inner_join(pick(model), pick(baseline),
    by = keys, suffix = c("_model", "_baseline")
  )
  paired$unit_id <- group_by(paired, across(all_of(unit))) |>
    group_indices()
  paired[, c("row_model", "row_baseline", "unit_id")]
}

#' Permutation distribution of a model's scaled relative skill
#'
#' Under the null hypothesis that a model and the baseline are exchangeable,
#' their scores can be swapped within any exchange unit. Each permutation
#' swaps the two models' scores on every shared forecast in a random subset
#' of units (each unit swapped with probability 0.5), then re-runs the full
#' pairwise tournament ([get_srss()]) including all other models, and
#' records the model's scaled relative skill.
#'
#' @inheritParams get_swap_pairs
#' @param n_perm Integer number of random permutations.
#' @returns Data frame with columns `perm_id`, `model` and `estimate`.
#' @autoglobal
get_permutation_srss <- function(scores_obj,
                                 score_type = c(
                                   "brier_score",
                                   "energy_score"
                                 ),
                                 model,
                                 baseline = "Hub-baseline",
                                 unit = c("location", "nowcast_date"),
                                 n_perm = 2000) {
  score_type <- rlang::arg_match(score_type)
  paired <- get_swap_pairs(scores_obj, score_type, model, baseline, unit)
  original <- scores_obj[[score_type]]
  n_units <- max(paired$unit_id)
  target_model <- model
  estimates <- vapply(seq_len(n_perm), function(i) {
    swapped_units <- which(stats::rbinom(n_units, 1, 0.5) == 1)
    swap <- paired[paired$unit_id %in% swapped_units, ]
    permuted <- original
    permuted[swap$row_model] <- original[swap$row_baseline]
    permuted[swap$row_baseline] <- original[swap$row_model]
    perm_scores <- scores_obj
    perm_scores[[score_type]] <- permuted
    srss <- get_srss(perm_scores, score_type, baseline)
    srss$estimate[srss$model == target_model]
  }, numeric(1))
  data.frame(
    perm_id = seq_len(n_perm),
    model = model,
    estimate = estimates,
    stringsAsFactors = FALSE
  )
}

#' Permutation distributions for every model against the baseline
#'
#' Runs [get_permutation_srss()] for every model other than the baseline and
#' `exclude`. Excluded models stay in the tournament (so the other models'
#' SRSS match the reported estimates) but are not tested.
#'
#' @inheritParams get_permutation_srss
#' @param exclude Character vector of models not to test (e.g. models with
#'   too few nowcast dates for a meaningful test).
#' @importFrom dplyr bind_rows
#' @returns Data frame with columns `perm_id`, `model` and `estimate`.
#' @autoglobal
get_permutation_srss_all <- function(scores_obj,
                                     score_type = c(
                                       "brier_score",
                                       "energy_score"
                                     ),
                                     baseline = "Hub-baseline",
                                     exclude = NULL,
                                     unit = c("location", "nowcast_date"),
                                     n_perm = 2000) {
  score_type <- rlang::arg_match(score_type)
  scored <- as.data.frame(scores_obj)
  scored <- scored[!is.na(scored[[score_type]]), ]
  models <- sort(setdiff(unique(scored$model), c(baseline, exclude)))
  lapply(models, function(m) {
    get_permutation_srss(
      scores_obj, score_type, m, baseline, unit, n_perm
    )
  }) |>
    bind_rows()
}

#' Permutation p-values for the scaled relative skill
#'
#' Two-sided p-value for each model: the proportion of permutations whose
#' absolute log SRSS is at least as large as the observed one, computed as
#' `(1 + count) / (1 + n_perm)` so it is never zero. Also gives a Bonferroni
#' adjustment across the models tested (one family per region and score
#' type, as for the simultaneous confidence bands).
#'
#' @param estimates Observed SRSS with columns `model` and `estimate`, e.g.
#'   from [get_srss()]. Models absent from `perm_estimates` are dropped.
#' @param perm_estimates Permutation estimates from
#'   [get_permutation_srss_all()].
#' @importFrom dplyr inner_join group_by summarise mutate n select
#' @returns Data frame with columns `model`, `estimate`, `p_value`,
#'   `p_bonferroni`, `n_tests` and `n_perm`.
#' @autoglobal
get_permutation_pvalues <- function(estimates, perm_estimates) {
  pvals <- perm_estimates |>
    inner_join(
      select(estimates, model, observed = estimate),
      by = "model"
    ) |>
    group_by(model) |>
    summarise(
      n_extreme = sum(
        abs(log(estimate)) >= abs(log(unique(observed))) - 1e-12
      ),
      n_perm = n(),
      estimate = unique(observed),
      .groups = "drop"
    ) |>
    mutate(p_value = (1 + n_extreme) / (1 + n_perm))
  n_tests <- nrow(pvals)
  pvals |>
    mutate(
      p_bonferroni = pmin(1, p_value * n_tests),
      n_tests = n_tests
    ) |>
    select(model, estimate, p_value, p_bonferroni, n_tests, n_perm) |>
    as.data.frame()
}

#' Format permutation p-values as a results table
#'
#' Computes the p-values for one region and score type with
#' [get_permutation_pvalues()] and labels them for combining into one table.
#'
#' @inheritParams get_permutation_pvalues
#' @param region Character string naming the region, e.g.
#'   `"US excluding CA"`.
#' @param score_type Character string naming the score.
#' @param unit Character vector of columns defining the exchange unit.
#' @importFrom dplyr mutate
#' @returns Data frame with columns `region`, `score_type`, the columns of
#'   [get_permutation_pvalues()] and `exchange_unit`.
#' @autoglobal
format_perm_table <- function(estimates, perm_estimates, region, score_type,
                              unit) {
  get_permutation_pvalues(estimates, perm_estimates) |>
    mutate(
      region = region,
      score_type = score_type,
      exchange_unit = paste(unit, collapse = " x "),
      .before = 1
    ) |>
    dplyr::relocate(exchange_unit, .after = dplyr::last_col())
}

perm_dates <- seq(as.Date("2024-10-09"), by = "week", length.out = 12)

# Brier-type scores for the baseline and other models over 12 nowcast
# dates, 2 locations and 2 target dates; `multipliers` scales each model's
# scores relative to the baseline's
perm_scores <- function(multipliers = c(model_a = 1.5)) {
  set.seed(5)
  forecast_grid <- expand.grid(
    nowcast_date = perm_dates,
    location = c("CA", "NY"),
    horizon = c(-1, 0),
    stringsAsFactors = FALSE
  )
  forecast_grid$target_date <- forecast_grid$nowcast_date +
    forecast_grid$horizon
  forecast_grid$horizon <- NULL
  base <- stats::runif(nrow(forecast_grid), 0.2, 0.4)
  rows <- lapply(c("Hub-baseline", names(multipliers)), function(m) {
    mult <- if (m == "Hub-baseline") 1 else multipliers[[m]]
    fc <- forecast_grid
    fc$model <- m
    fc$observed <- 0
    fc$predicted <- base * mult
    fc
  })
  scoringutils::score(
    scoringutils::as_forecast_point(do.call(rbind, rows)),
    metrics = list(
      brier_score = function(observed, predicted) abs(observed - predicted)
    )
  )
}

# scoringutils' pairwise Wilcoxon tests warn about ties and zero
# differences on toy data
quiet_ties <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("exact p-value", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

test_that("get_swap_pairs pairs shared forecasts within exchange units", {
  scores <- perm_scores()
  # model_a misses NY on the first nowcast date
  df <- as.data.frame(scores)
  drop <- df$model == "model_a" & df$nowcast_date == perm_dates[1] &
    df$location == "NY"
  scores <- scores[!drop, ]
  df <- as.data.frame(scores)
  paired <- get_swap_pairs(scores, "brier_score", "model_a")

  expect_named(paired, c("row_model", "row_baseline", "unit_id"))
  # 12 dates x 2 locations x 2 target dates, minus the 2 missing forecasts
  expect_identical(nrow(paired), 46L)
  expect_true(all(df$model[paired$row_model] == "model_a"))
  expect_true(all(df$model[paired$row_baseline] == "Hub-baseline"))
  for (col in c("location", "target_date", "nowcast_date")) {
    expect_identical(
      df[[col]][paired$row_model], df[[col]][paired$row_baseline]
    )
  }
  # one unit per location x nowcast date with a shared forecast
  expect_identical(max(paired$unit_id), 23L)
  units_per_id <- tapply(
    paste(
      df$location[paired$row_model], df$nowcast_date[paired$row_model]
    ),
    paired$unit_id, function(x) length(unique(x))
  )
  expect_true(all(units_per_id == 1))
})

test_that("swapping all units of two models swaps their SRSS", {
  scores <- perm_scores(c(model_a = 1.5, model_b = 0.8))
  paired <- get_swap_pairs(scores, "brier_score", "model_a")
  swapped <- scores
  original <- scores$brier_score
  swapped$brier_score[paired$row_model] <- original[paired$row_baseline]
  swapped$brier_score[paired$row_baseline] <- original[paired$row_model]
  srss <- quiet_ties(get_srss(swapped, "brier_score"))

  # model_a now scores as the baseline did, so its SRSS is 1 / 1.5
  expect_equal(srss$estimate[srss$model == "model_a"], 1 / 1.5,
    tolerance = 1e-12
  )
  # model_b's ratio to the (new) baseline is 0.8 / 1.5
  expect_equal(srss$estimate[srss$model == "model_b"], 0.8 / 1.5,
    tolerance = 1e-12
  )
})

test_that("a model identical to the baseline gets SRSS 1 and p = 1", {
  scores <- perm_scores(c(model_a = 1, model_b = 1.2))
  set.seed(1)
  perm <- quiet_ties(get_permutation_srss(
    scores, "brier_score", "model_a",
    n_perm = 20
  ))

  expect_named(perm, c("perm_id", "model", "estimate"))
  expect_identical(nrow(perm), 20L)
  expect_equal(perm$estimate, rep(1, 20), tolerance = 1e-12)
  observed <- quiet_ties(get_srss(scores, "brier_score"))
  pvals <- get_permutation_pvalues(observed, perm)
  expect_identical(pvals$p_value, 1)
})

test_that("a consistent difference gives the smallest possible p-value", {
  scores <- perm_scores(c(model_a = 1.5, model_b = 1.2))
  set.seed(1)
  perm <- quiet_ties(get_permutation_srss_all(
    scores, "brier_score",
    exclude = "model_b", n_perm = 50
  ))
  observed <- quiet_ties(get_srss(scores, "brier_score"))
  pvals <- get_permutation_pvalues(observed, perm)

  expect_identical(unique(perm$model), "model_a")
  expect_equal(pvals$estimate, 1.5, tolerance = 1e-12)
  # every permutation swaps only some of the 24 units, so |log SRSS| is
  # always below the observed log(1.5)
  expect_identical(pvals$p_value, 1 / 51)
  expect_identical(pvals$n_perm, 50L)
})

test_that("get_permutation_pvalues is two-sided and adjusts for models", {
  observed <- data.frame(
    model = c("a", "b", "baseline_only"), estimate = c(1.2, 1 / 1.2, 1),
    stringsAsFactors = FALSE
  )
  perm <- data.frame(
    perm_id = rep(1:4, 2),
    model = rep(c("a", "b"), each = 4),
    estimate = c(1.3, 1 / 1.3, 1, 1, 1.3, 1 / 1.3, 1, 1),
    stringsAsFactors = FALSE
  )
  pvals <- get_permutation_pvalues(observed, perm)

  expect_identical(pvals$model, c("a", "b"))
  # 2 of 4 permutations are at least as far from 1 on the log scale
  expect_equal(pvals$p_value, c(3 / 5, 3 / 5), tolerance = 1e-12)
  expect_identical(pvals$p_bonferroni, c(1, 1))
  expect_identical(pvals$n_tests, c(2L, 2L))
})

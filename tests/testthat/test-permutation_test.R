perm_dates <- seq(as.Date("2024-10-09"), by = "week", length.out = 20)

# Scores for the baseline and other models over 20 nowcast dates, 2
# locations and 2 target dates; `multiplier` scales a model's scores
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
  base <- runif(nrow(forecast_grid), 0.2, 0.4)
  rows <- lapply(c("Hub-baseline", names(multipliers)), function(m) {
    mult <- if (m == "Hub-baseline") 1 else multipliers[[m]]
    data.frame(forecast_grid[, c("location", "target_date", "nowcast_date")],
      model = m, brier_score = base * mult, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

test_that("get_paired_date_totals keeps only forecasts both models scored", {
  scores <- perm_scores()
  # model_a misses one location on the first nowcast date
  scores <- scores[!(scores$model == "model_a" &
    scores$nowcast_date == perm_dates[1] & scores$location == "NY"), ]
  totals <- get_paired_date_totals(scores, "brier_score", "model_a")

  expect_named(totals, c(
    "nowcast_date", "model_total", "baseline_total", "n_forecasts"
  ))
  expect_identical(nrow(totals), 20L)
  expect_identical(totals$n_forecasts[1], 2L)
  first <- scores$nowcast_date == perm_dates[1] & scores$location == "CA"
  expect_equal(
    totals$baseline_total[1],
    sum(scores$brier_score[first & scores$model == "Hub-baseline"]),
    tolerance = 1e-12
  )
})

test_that("identical models give a ratio of 1 and a p-value of 1", {
  scores <- perm_scores(c(model_a = 1))
  totals <- get_paired_date_totals(scores, "brier_score", "model_a")
  set.seed(1)
  test <- get_permutation_test(totals, n_perm = 1000)

  expect_equal(test$ratio, 1, tolerance = 1e-12)
  expect_identical(test$p_value, 1)
})

test_that("a consistent difference gives a small p-value", {
  scores <- perm_scores(c(model_a = 1.5))
  totals <- get_paired_date_totals(scores, "brier_score", "model_a")
  set.seed(1)
  test <- get_permutation_test(totals, n_perm = 2000)

  expect_equal(test$ratio, 1.5, tolerance = 1e-12)
  expect_lt(test$p_value, 0.01)
  expect_gte(test$p_value, 1 / 2001)
})

test_that("the test is two-sided: swapping the roles gives the same p", {
  scores <- perm_scores(c(model_a = 1.1))
  totals <- get_paired_date_totals(scores, "brier_score", "model_a")
  swapped <- totals
  swapped$model_total <- totals$baseline_total
  swapped$baseline_total <- totals$model_total
  set.seed(2)
  p1 <- get_permutation_test(totals, n_perm = 2000)$p_value
  set.seed(2)
  p2 <- get_permutation_test(swapped, n_perm = 2000)$p_value

  expect_equal(p1, p2, tolerance = 1e-12)
})

test_that("get_permutation_tests excludes models and adjusts p-values", {
  scores <- perm_scores(c(model_a = 1.02, model_b = 1.5, model_c = 1))
  set.seed(3)
  tests <- get_permutation_tests(scores, "brier_score",
    exclude = "model_c", n_perm = 1000
  )

  expect_identical(tests$model, c("model_a", "model_b"))
  expect_identical(tests$n_tests, c(2L, 2L))
  expect_equal(tests$p_bonferroni, pmin(1, 2 * tests$p_value),
    tolerance = 1e-12
  )
})

weekly_dates <- seq(as.Date("2024-10-09"), by = "week", length.out = 35)

test_that("get_block_bootstrap_dates returns N dates per resample", {
  set.seed(1)
  boot <- get_block_bootstrap_dates(weekly_dates, n_boot = 20, block_length = 6)

  expect_named(boot, c("boot_id", "position", "nowcast_date"))
  expect_identical(nrow(boot), 20L * 35L)
  expect_true(all(table(boot$boot_id) == 35))
  expect_true(all(boot$nowcast_date %in% weekly_dates))
  # position uniquely identifies each draw within a resample
  expect_false(anyDuplicated(boot[, c("boot_id", "position")]) > 0)
})

test_that("get_block_bootstrap_dates resamples blocks of consecutive dates", {
  set.seed(1)
  boot <- get_block_bootstrap_dates(weekly_dates, n_boot = 20, block_length = 6)

  # Positions 1-6, 7-12, ... each come from one block of consecutive weeks;
  # the final block of 6 is trimmed to 5 so each resample has 35 dates.
  boot$block <- (boot$position - 1) %/% 6
  steps <- tapply(
    boot$nowcast_date,
    list(boot$boot_id, boot$block),
    function(d) unique(diff(d))
  )
  expect_true(all(unlist(steps) == 7))
})

test_that("get_block_bootstrap_dates is reproducible given a seed", {
  set.seed(42)
  boot1 <- get_block_bootstrap_dates(weekly_dates, n_boot = 5, block_length = 6)
  set.seed(42)
  boot2 <- get_block_bootstrap_dates(weekly_dates, n_boot = 5, block_length = 6)
  expect_identical(boot1, boot2)
})

test_that("get_block_bootstrap_dates validates its inputs", {
  expect_error(
    get_block_bootstrap_dates(weekly_dates, n_boot = 5, block_length = 36),
    "block_length"
  )
  expect_error(
    get_block_bootstrap_dates(weekly_dates[-3], n_boot = 5, block_length = 6),
    "evenly spaced"
  )
})

# Small scores object: 2 models x 3 weekly nowcast dates x 2 locations
toy_scores <- function() {
  fc <- expand.grid(
    model = c("model_a", "Hub-baseline"),
    nowcast_date = weekly_dates[1:3],
    location = c("CA", "NY"),
    stringsAsFactors = FALSE
  )
  fc$observed <- 1
  fc$predicted <- seq_len(nrow(fc)) / 10
  scoringutils::score(scoringutils::as_forecast_point(fc))
}

test_that("get_resampled_scores copies rows once per position drawn", {
  scores <- toy_scores()
  # Date 1 drawn twice, date 2 once, date 3 not at all
  boot_dates <- data.frame(
    boot_id = 1L,
    position = 1:3,
    nowcast_date = weekly_dates[c(1, 1, 2)]
  )
  resample <- get_resampled_scores(scores, boot_dates, b = 1)

  # 4 rows per date (2 models x 2 locations) x 3 positions
  expect_identical(nrow(resample), 12L)
  expect_identical(
    as.vector(table(resample$nowcast_date)),
    c(8L, 4L)
  )
  expect_s3_class(resample, "scores")
  expect_true("position" %in% scoringutils::get_forecast_unit(resample))
  # Copies of date 1 are distinct forecasts, not duplicates
  expect_false(anyDuplicated(
    as.data.frame(resample)[, scoringutils::get_forecast_unit(resample)]
  ) > 0)
})

test_that("get_mean_score_by_model averages over all rows per model", {
  scores <- toy_scores()
  scores$ae_point[scores$model == "model_a"][1] <- NA
  # Pretend ae_point is brier_score to use the function's score types
  scores$brier_score <- scores$ae_point
  means <- get_mean_score_by_model(scores, "brier_score")

  df <- as.data.frame(scores)
  expect_equal(
    means$estimate[means$model == "model_a"],
    mean(df$brier_score[df$model == "model_a"], na.rm = TRUE),
    tolerance = 1e-12
  )
  expect_identical(means$n_nowcast_dates, c(3L, 3L))
})

test_that("an identity resample reproduces the original estimate", {
  scores <- toy_scores()
  scores$brier_score <- scores$ae_point
  boot_dates <- data.frame(
    boot_id = 1L,
    position = 1:3,
    nowcast_date = weekly_dates[1:3]
  )
  boot <- get_bootstrap_mean_scores(scores, boot_dates, "brier_score")
  orig <- get_mean_score_by_model(scores, "brier_score")

  expect_named(boot, c("boot_id", "model", "estimate"))
  expect_equal(boot$estimate, orig$estimate, tolerance = 1e-12)
})

test_that("get_bonferroni_ci uses estimate +/- qnorm(1 - alpha/2P) * sd", {
  estimates <- data.frame(
    model = c("a", "b", "c"),
    estimate = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  boot_estimates <- data.frame(
    boot_id = rep(1:4, times = 3),
    model = rep(c("a", "b", "c"), each = 4),
    estimate = c(0.9, 1.1, 1.0, 1.0, 1.8, 2.2, 2.0, 2.0, 3, 3, 3, NA),
    stringsAsFactors = FALSE
  )
  ci <- get_bonferroni_ci(estimates, boot_estimates, alpha = 0.05)

  crit <- qnorm(1 - 0.05 / (2 * 3))
  expect_identical(ci$n_comparisons, rep(3L, 3))
  expect_equal(ci$crit_value, rep(crit, 3), tolerance = 1e-12)
  expect_equal(
    ci$lower,
    c(1, 2, 3) - crit * c(sd(c(0.9, 1.1, 1, 1)), sd(c(1.8, 2.2, 2, 2)), 0),
    tolerance = 1e-12
  )
  expect_equal(ci$upper - ci$estimate, ci$estimate - ci$lower,
    tolerance = 1e-12
  )
  # Resamples where a model has no estimate are not counted
  expect_identical(ci$n_boot, c(4L, 4L, 3L))
})

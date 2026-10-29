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

test_that("log-scale bands are symmetric about the estimate on a log axis", {
  estimates <- data.frame(
    model = c("a", "b"),
    estimate = c(0.8, 1.25),
    stringsAsFactors = FALSE
  )
  boot_estimates <- data.frame(
    boot_id = rep(1:4, times = 2),
    model = rep(c("a", "b"), each = 4),
    estimate = c(0.7, 0.9, 0.8, 0.85, 1.1, 1.4, 1.2, 1.3),
    stringsAsFactors = FALSE
  )
  ci <- get_bonferroni_ci(estimates, boot_estimates,
    alpha = 0.05, log_scale = TRUE
  )

  crit <- qnorm(1 - 0.05 / 4)
  log_sd_a <- sd(log(c(0.7, 0.9, 0.8, 0.85)))
  expect_equal(ci$lower[1], 0.8 * exp(-crit * log_sd_a), tolerance = 1e-12)
  # Geometric symmetry: lower * upper == estimate^2
  expect_equal(ci$lower * ci$upper, ci$estimate^2, tolerance = 1e-12)
  expect_true(all(ci$lower > 0))
  expect_true(all(ci$log_scale))
})

test_that("get_bonferroni_ci counts every row of a by-horizon table in P", {
  estimates <- expand.grid(
    model = c("a", "b"), horizon = 0:2, stringsAsFactors = FALSE
  )
  estimates$estimate <- 1
  boot_estimates <- merge(
    data.frame(boot_id = 1:3), estimates[, c("model", "horizon")]
  )
  boot_estimates$estimate <- rep_len(c(0.9, 1, 1.1), 18)
  ci <- get_bonferroni_ci(estimates, boot_estimates,
    alpha = 0.05, by = c("model", "horizon"), log_scale = TRUE
  )

  expect_identical(nrow(ci), 6L)
  expect_identical(ci$n_comparisons, rep(6L, 6))
  expect_false(anyNA(ci$boot_sd))
})

# Small Brier-type scores object with target dates, for SRSS tests:
# 2 models x 3 weekly nowcast dates x 2 target dates x 2 locations
toy_brier_scores <- function() {
  fc <- expand.grid(
    model = c("model_a", "Hub-baseline"),
    nowcast_date = weekly_dates[1:3],
    horizon = c(-1, 0),
    location = c("CA", "NY"),
    stringsAsFactors = FALSE
  )
  fc$target_date <- fc$nowcast_date + fc$horizon
  fc$horizon <- NULL
  fc$observed <- 1
  set.seed(3)
  fc$predicted <- runif(nrow(fc), 0.2, 0.8)
  scores <- scoringutils::score(
    scoringutils::as_forecast_point(fc),
    metrics = list(
      brier_score = function(observed, predicted) abs(observed - predicted)
    )
  )
  scores
}

test_that("with two models SRSS is the ratio of mean scores", {
  scores <- toy_brier_scores()
  srss <- get_srss(scores, "brier_score")

  df <- as.data.frame(scores)
  expect_identical(srss$model, "model_a")
  expect_equal(
    srss$estimate,
    mean(df$brier_score[df$model == "model_a"]) /
      mean(df$brier_score[df$model == "Hub-baseline"]),
    tolerance = 1e-12
  )
})

test_that("an identity resample reproduces the original SRSS", {
  scores <- toy_brier_scores()
  boot_dates <- data.frame(
    boot_id = 1L, position = 1:3, nowcast_date = weekly_dates[1:3]
  )
  boot <- get_bootstrap_srss(scores, boot_dates, "brier_score")

  expect_named(boot, c("boot_id", "model", "estimate"))
  expect_equal(
    boot$estimate, get_srss(scores, "brier_score")$estimate,
    tolerance = 1e-12
  )
})

test_that("resampling daily SRSS equals recomputing it on the resample", {
  scores <- toy_brier_scores()
  # Date 1 drawn twice, date 3 once, date 2 not at all
  boot_dates <- data.frame(
    boot_id = 1L, position = 1:3, nowcast_date = weekly_dates[c(1, 1, 3)]
  )
  shortcut <- get_bootstrap_srss_by_horizon(
    scores, boot_dates, "brier_score"
  )
  # Duplicated forecasts give tied pairs in scoringutils' (unused) Wilcoxon
  # p-values; silence only that warning
  full <- withCallingHandlers(
    get_resampled_scores(scores, boot_dates, 1) |>
      get_daily_srss("brier_score") |>
      get_srss_by_horizon("brier_score"),
    warning = function(w) {
      if (grepl("exact p-value with ties", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_named(shortcut, c("boot_id", "model", "horizon", "estimate"))
  expect_identical(shortcut$horizon, full$horizon)
  expect_equal(shortcut$estimate, full$estimate, tolerance = 1e-12)
})

test_that("get_n_overlapping_dates counts dates shared with the baseline", {
  scores <- toy_brier_scores()
  # model_a misses nowcast date 1 entirely
  scores <- scores[!(scores$model == "model_a" &
    scores$nowcast_date == weekly_dates[1]), ]
  n_dates <- get_n_overlapping_dates(scores, "brier_score")

  expect_identical(n_dates$model, "model_a")
  expect_identical(n_dates$n_overlapping_dates, 2L)
})

test_that("format_ci_table labels and orders the columns", {
  ci <- data.frame(
    model = c("a", "b"), estimate = c(1, 2), boot_sd = 0.1, n_boot = 4L,
    lower = c(0.5, 1.5), upper = c(1.5, 2.5), n_comparisons = 2L,
    crit_value = 2.24, alpha = 0.05, log_scale = TRUE,
    stringsAsFactors = FALSE
  )
  n_dates <- data.frame(
    model = c("a", "b"), n_overlapping_dates = c(30L, 35L),
    stringsAsFactors = FALSE
  )
  table <- format_ci_table(ci, "CA", "brier_score", "SRSS", n_dates)

  expect_named(table, c(
    "region", "score_type", "metric", "model", "estimate", "ci_lower",
    "ci_upper", "n_overlapping_dates", "n_comparisons", "crit_value",
    "alpha", "scale", "boot_sd", "n_boot"
  ))
  expect_identical(table$scale, c("log", "log"))
  expect_identical(table$n_overlapping_dates, c(30L, 35L))
})

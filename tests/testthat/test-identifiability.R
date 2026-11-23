# Known MLR parameters for 3 clades over 100 days; clade "a" is the reference
true_intercept <- c(a = 0, b = -1, c = -3)
true_growth <- c(a = 0, b = 0.02, c = 0.06)
t_days <- 0:99

test_that("get_mlr_theta gives proportions that sum to 1", {
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  expect_identical(dim(theta), c(100L, 3L))
  expect_named(as.data.frame(theta), c("a", "b", "c"))
  expect_equal(rowSums(theta), rep(1, 100), tolerance = 1e-12)
})

test_that("fit_mlr recovers parameters exactly from expected counts", {
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  counts <- 25 * theta
  fit <- fit_mlr(counts, t_days, ref_clade = "a")

  expect_equal(fit$intercept, true_intercept, tolerance = 1e-6)
  expect_equal(fit$growth, true_growth, tolerance = 1e-8)
  expect_identical(dim(fit$vcov), c(4L, 4L))
  expect_equal(fit$vcov, t(fit$vcov), tolerance = 1e-12)
  expect_true(all(eigen(fit$vcov, only.values = TRUE)$values > 0))
})

test_that("fit_mlr matches nnet::multinom on noisy counts", {
  skip_if_not_installed("nnet")
  set.seed(1)
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  counts <- t(vapply(
    seq_along(t_days),
    function(i) as.numeric(rmultinom(1, 30, theta[i, ])),
    numeric(3)
  ))
  colnames(counts) <- c("a", "b", "c")
  fit <- fit_mlr(counts, t_days, ref_clade = "a")
  ref_fit <- nnet::multinom(counts ~ t_days, trace = FALSE, reltol = 1e-14)
  ref_coef <- coef(ref_fit)

  expect_equal(unname(fit$intercept[c("b", "c")]), unname(ref_coef[, 1]),
    tolerance = 1e-4
  )
  expect_equal(unname(fit$growth[c("b", "c")]), unname(ref_coef[, 2]),
    tolerance = 1e-4
  )
})

test_that("fit_mlr holds fixed growth rates and refits the rest", {
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  counts <- 25 * theta
  free_fit <- fit_mlr(counts, t_days, ref_clade = "a")
  fixed_fit <- fit_mlr(counts, t_days,
    ref_clade = "a",
    fixed_growth = c(c = 0.06 * 1.1)
  )

  expect_identical(fixed_fit$growth[["c"]], 0.06 * 1.1)
  # The constrained fit is worse, and the other parameters move to
  # compensate
  expect_lt(fixed_fit$loglik, free_fit$loglik)
  expect_false(isTRUE(all.equal(fixed_fit$intercept, free_fit$intercept)))
  # The fixed growth rate is not an estimated parameter
  expect_false("b_c" %in% rownames(fixed_fit$vcov))
  expect_identical(dim(fixed_fit$vcov), c(3L, 3L))
})

test_that("fit_mlr validates its inputs", {
  counts <- matrix(1, 3, 2, dimnames = list(NULL, c("a", "b")))
  expect_error(fit_mlr(counts, 1:3, ref_clade = "z"), "ref_clade")
  expect_error(
    fit_mlr(counts, 1:3, ref_clade = "a", fixed_growth = c(a = 0.1)),
    "fixed_growth"
  )
})

test_that("fit_mlr reports convergence on large counts", {
  # Large counts make an absolute gradient tolerance unreachable; the
  # relative log-likelihood criterion should still stop the fit
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  set.seed(2)
  counts <- t(vapply(
    seq_along(t_days),
    function(i) as.numeric(rmultinom(1, 500, theta[i, ])),
    numeric(3)
  ))
  colnames(counts) <- c("a", "b", "c")
  expect_no_warning(fit_mlr(counts, t_days, ref_clade = "a"))
  fit <- fit_mlr(counts, t_days, ref_clade = "a")
  expect_true(fit$converged)
  expect_lt(fit$iterations, 30)
})

test_that("sample_mlr_params centres on the fit and holds fixed growth", {
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  counts <- 25 * theta
  fit <- fit_mlr(counts, t_days, ref_clade = "a")
  set.seed(3)
  draws <- sample_mlr_params(fit, 4000)
  expect_identical(dim(draws$growth), c(4000L, 3L))
  expect_true(all(draws$growth[, "a"] == 0))
  expect_equal(colMeans(draws$growth), fit$growth, tolerance = 0.05)
  expect_equal(
    stats::var(draws$growth[, "c"]), fit$vcov["b_c", "b_c"],
    tolerance = 0.1
  )
  fixed_fit <- fit_mlr(counts, t_days,
    ref_clade = "a",
    fixed_growth = c(c = 0.066)
  )
  fixed_draws <- sample_mlr_params(fixed_fit, 100)
  expect_true(all(fixed_draws$growth[, "c"] == 0.066))
})

test_that("get_truth_fit_intervals covers the fit and the data", {
  dates <- as.Date("2025-01-01") + t_days
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  set.seed(4)
  counts <- t(vapply(
    seq_along(t_days),
    function(i) as.numeric(rmultinom(1, 20, theta[i, ])),
    numeric(3)
  ))
  truth_data <- data.frame(
    date = rep(dates, times = 3),
    t = rep(t_days, times = 3),
    clade = factor(rep(c("a", "b", "c"), each = 100)),
    n = as.vector(counts)
  )
  fit <- fit_truth_mlr(truth_data, ref_clade = "a")
  out <- get_truth_fit_intervals(truth_data, fit, n_draws = 500)
  latent <- merge(out$latent, fit$theta, by = c("date", "clade"))
  expect_true(all(latent$lower <= latent$theta & latent$theta <= latent$upper))
  expect_true(all(out$weekly$lower <= out$weekly$upper))
  expect_gt(
    mean(out$weekly$proportion >= out$weekly$lower &
      out$weekly$proportion <= out$weekly$upper),
    0.85
  )
})

# Small truth for the model tests: daily N = 20 over 100 days from Jan 1
id_truth_example <- function() {
  dates <- as.Date("2025-01-01") + t_days
  theta <- get_mlr_theta(true_intercept, true_growth, t_days)
  truth_data <- data.frame(
    date = rep(dates, times = 3),
    t = rep(t_days, times = 3),
    clade = factor(rep(c("a", "b", "c"), each = 100)),
    n = as.vector(round(20 * theta))
  )
  fit <- fit_truth_mlr(truth_data, ref_clade = "a")
  # Use the known parameters as the truth
  fit$intercept <- true_intercept
  fit$growth <- true_growth
  list(data = truth_data, fit = fit, nowcast = as.Date("2025-03-31"))
}

test_that("get_id_training_counts gives expected counts up to the nowcast", {
  ex <- id_truth_example()
  tr <- get_id_training_counts(ex$data, ex$fit, ex$nowcast)
  expect_identical(max(tr$date), ex$nowcast)
  expect_identical(nrow(tr$counts), length(tr$t))
  n_day <- tapply(ex$data$n, ex$data$date, sum)[as.character(tr$date)]
  expect_equal(
    unname(rowSums(tr$counts)), unname(as.numeric(n_day)),
    tolerance = 1e-12
  )
})

test_that("fit_id_model recovers the truth at eps = 0 and fixes it otherwise", {
  ex <- id_truth_example()
  tr <- get_id_training_counts(ex$data, ex$fit, ex$nowcast)
  true_fit <- fit_id_model(tr, ex$fit, rising_clade = "c", eps = 0)
  expect_equal(true_fit$growth, true_growth, tolerance = 1e-8)
  expect_identical(true_fit$eps, 0)
  mis_fit <- fit_id_model(tr, ex$fit, rising_clade = "c", eps = 0.25)
  expect_identical(mis_fit$growth[["c"]], 1.25 * true_growth[["c"]])
  expect_lt(mis_fit$loglik, true_fit$loglik)
})

test_that("sample_id_model shares draws across models and centres on the fit", {
  ex <- id_truth_example()
  tr <- get_id_training_counts(ex$data, ex$fit, ex$nowcast)
  true_fit <- fit_id_model(tr, ex$fit, rising_clade = "c", eps = 0)
  mis_fit <- fit_id_model(tr, ex$fit, rising_clade = "c", eps = 0.25)
  targets <- ex$nowcast + (-31:10)
  s_true <- sample_id_model(true_fit, true_fit$vcov, ex$fit$t0, targets)
  s_mis <- sample_id_model(mis_fit, true_fit$vcov, ex$fit$t0, targets)
  expect_identical(dim(s_true$samples), c(100L, 42L, 3L))
  expect_equal(apply(s_true$samples, c(1, 2), sum),
    matrix(1, 100, 42, dimnames = list(NULL, as.character(targets))),
    tolerance = 1e-12
  )
  t_target <- as.numeric(targets - ex$fit$t0)
  expect_equal(unname(s_true$mean),
    unname(get_mlr_theta(true_intercept, true_growth, t_target)),
    tolerance = 1e-8
  )
  # Same seed: the same standard normal draws, so each sample's parameter
  # offset from its model's centre is identical across models
  set.seed(141)
  d_true <- sample_mlr_params(true_fit, 100, vcov = true_fit$vcov)
  set.seed(141)
  d_mis <- sample_mlr_params(mis_fit, 100, vcov = true_fit$vcov)
  expect_equal(
    sweep(d_mis$growth, 2, mis_fit$growth),
    sweep(d_true$growth, 2, true_fit$growth),
    tolerance = 1e-12
  )
})

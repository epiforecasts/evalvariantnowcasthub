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

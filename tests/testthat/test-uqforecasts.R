# Validation of the UQforecasts API (#132) before using it on our scores.
#
# Signature (UQforecasts @ de919e0):
#   conf_bands(scores, metric = "SS", m_bench = length(scores),
#              type = "bonferroni", B = 1000,
#              l = 3 * floor(length(scores[[1]])^(1/4)), alpha = 0.05)
#
# `scores` is a nested list: model -> time (nowcast date) -> any further
# dimensions (e.g. location), with a single number at each leaf. The output
# drops the time dimension and returns c(LB_bs, <metric>, UB_bs) per cell.
# Metrics: "ES" expected score, "SS" skill score, "RA" relative accuracy
# (mean score / mean benchmark score), "D" difference in expected scores.

# Simulated per-nowcast-date scores for three models, with the benchmark
# (Hub-baseline) last, as conf_bands defaults to m_bench = length(scores).
n_dates <- 35
sim_scores <- function(seed = 123) {
  set.seed(seed)
  date_names <- paste0("d", seq_len(n_dates))
  sim <- function(mu) {
    setNames(as.list(abs(rnorm(n_dates, mu, 0.05))), date_names)
  }
  list(
    model_a = sim(0.3),
    model_b = sim(0.45),
    `Hub-baseline` = sim(0.4)
  )
}

test_that("ES estimate is the mean score of each model over dates", {
  scores <- sim_scores()
  out <- UQforecasts::conf_bands(scores, metric = "ES", B = 100)

  expect_named(out, names(scores))
  for (m in names(scores)) {
    expect_named(out[[m]], c("LB_bs", "ES", "UB_bs"))
    expect_equal(out[[m]][["ES"]], mean(unlist(scores[[m]])), tolerance = 1e-12)
    expect_lt(out[[m]][["LB_bs"]], out[[m]][["ES"]])
    expect_gt(out[[m]][["UB_bs"]], out[[m]][["ES"]])
  }
})

test_that("RA estimate is the ratio of mean scores to the benchmark", {
  scores <- sim_scores()
  out <- UQforecasts::conf_bands(scores, metric = "RA", B = 100)

  # The benchmark is dropped from the output
  expect_named(out, c("model_a", "model_b"))
  bench_mean <- mean(unlist(scores[["Hub-baseline"]]))
  expect_equal(
    out[["model_a"]][["RA"]],
    mean(unlist(scores[["model_a"]])) / bench_mean,
    tolerance = 1e-12
  )
  expect_equal(
    out[["model_b"]][["RA"]],
    mean(unlist(scores[["model_b"]])) / bench_mean,
    tolerance = 1e-12
  )
})

test_that("default block length is 6 for our 35 nowcast dates", {
  expect_identical(3 * floor(n_dates^(1 / 4)), 6)

  scores <- sim_scores()
  set.seed(1)
  out_default <- UQforecasts::conf_bands(scores, metric = "RA", B = 100)
  set.seed(1)
  out_l6 <- UQforecasts::conf_bands(scores, metric = "RA", B = 100, l = 6)
  expect_identical(out_default, out_l6)
})

test_that("Bonferroni critical value is the normal quantile at alpha over 2P", {
  # With the same seed the bootstrap SDs are identical, so the ratio of
  # half-widths isolates the critical value. P = 2 non-benchmark models.
  scores <- sim_scores()
  half_width <- function(type) {
    set.seed(1)
    out <- UQforecasts::conf_bands(scores, metric = "RA", type = type, B = 100)
    unname(vapply(out, function(x) x[["UB_bs"]] - x[["RA"]], numeric(1)))
  }
  n_comparisons <- 2
  expect_equal(
    half_width("bonferroni") / half_width("pointwise"),
    rep(
      qnorm(1 - 0.05 / (2 * n_comparisons)) / qnorm(1 - 0.05 / 2),
      n_comparisons
    ),
    tolerance = 1e-12
  )
})

test_that("missing dates are averaged per model, not on the overlap", {
  # conf_bands uses colMeans(na.rm = TRUE) separately for each model, so
  # the benchmark keeps dates the model did not submit for. Comparisons on
  # our data must therefore be restricted to overlapping dates beforehand.
  scores <- sim_scores()
  scores[["model_a"]][1:5] <- NA_real_
  out <- UQforecasts::conf_bands(scores, metric = "RA", B = 100)

  model_a <- unlist(scores[["model_a"]])
  bench <- unlist(scores[["Hub-baseline"]])
  expect_equal(
    out[["model_a"]][["RA"]],
    mean(model_a, na.rm = TRUE) / mean(bench),
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(
    out[["model_a"]][["RA"]],
    mean(model_a[-(1:5)]) / mean(bench[-(1:5)])
  )))
})

test_that("extra dimensions give one estimate per cell", {
  # Needed for the by-location extension: model -> date -> location.
  set.seed(123)
  locs <- c("CA", "NY", "TX")
  sim <- function(mu) {
    lapply(seq_len(n_dates), function(d) {
      setNames(as.list(abs(rnorm(length(locs), mu, 0.05))), locs)
    })
  }
  scores <- list(model_a = sim(0.3), `Hub-baseline` = sim(0.4))
  out <- UQforecasts::conf_bands(scores, metric = "RA", B = 100)

  expect_named(out, "model_a")
  expect_named(out[["model_a"]], locs)
  ny_mean <- function(m) {
    mean(vapply(scores[[m]], function(x) x[["NY"]], numeric(1)))
  }
  expect_equal(
    out[["model_a"]][["NY"]][["RA"]],
    ny_mean("model_a") / ny_mean("Hub-baseline"),
    tolerance = 1e-12
  )
})

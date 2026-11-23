#' Get clade counts for fitting the identifiability truth model
#'
#' Builds daily clade counts for one location around one nowcast date, for
#' fitting the multinomial logistic regression (MLR) that defines the true
#' clade proportions in the identifiability analysis (#141). Counts come from
#' the final data, which covers the whole fitting window, with clades mapped
#' onto those modelled at the nowcast date: any final clade not modelled at
#' the nowcast date is assigned to `"other"`. The Hub's own clade assignments
#' for a nowcast date only cover the -31 to +10 day evaluation window.
#'
#' @param final_data Data frame of final sequence counts by clade, location
#'   and date (`clean_variant_data_final_all_states`), with columns
#'   `clades_modeled`, `location`, `date` and `sequences`.
#' @param eval_data Data frame of evaluation data by nowcast date
#'   (`clean_variant_data_eval_all`), used to get the clades modelled at
#'   `nowcast_date`.
#' @param location Character string, the location to use.
#' @param nowcast_date Date or character string, the nowcast date.
#' @param days_before Integer, days before the nowcast date at which the
#'   fitting window starts (t0).
#' @param days_after Integer, days after the nowcast date at which the
#'   fitting window ends.
#' @importFrom dplyr filter mutate group_by summarise
#' @importFrom tidyr complete
#' @returns Data frame with columns `date`, `t` (days since t0), `clade`
#'   (factor ordered as in the nowcast date's clade list) and `n`, with one
#'   row per date and clade.
#' @autoglobal
get_truth_fit_data <- function(final_data,
                               eval_data,
                               location,
                               nowcast_date,
                               days_before = 90,
                               days_after = 10) {
  nowcast_date <- as.Date(nowcast_date)
  t0 <- nowcast_date - days_before
  clades <- sort(unique(
    eval_data$clades_modeled[eval_data$nowcast_date == nowcast_date]
  ))
  if (length(clades) == 0) {
    cli::cli_abort("No clades found for nowcast date {nowcast_date}.")
  }
  loc <- location
  final_data |>
    filter(
      location == loc,
      date >= t0,
      date <= nowcast_date + days_after
    ) |>
    mutate(
      clade = ifelse(clades_modeled %in% clades, clades_modeled, "other")
    ) |>
    group_by(date, clade) |>
    summarise(n = sum(sequences), .groups = "drop") |>
    mutate(clade = factor(clade, levels = clades)) |>
    complete(date, clade, fill = list(n = 0)) |>
    mutate(t = as.numeric(date - t0))
}

#' Fit a multinomial logistic regression to clade counts
#'
#' Fits clade proportions theta_t = softmax(a + b * t) by maximum likelihood,
#' with the reference clade's intercept and growth rate fixed at 0. Growth
#' rates of selected clades can be held fixed (`fixed_growth`), with the
#' remaining parameters re-estimated, which gives the constrained refits used
#' for the misspecified models in #141. Counts may be non-integer (e.g.
#' expected counts). The log-likelihood is concave, so Newton-Raphson with the
#' exact gradient and Hessian is used; the inverse Hessian at the optimum
#' gives the Laplace approximation to the posterior covariance of the
#' estimated parameters.
#'
#' @param counts Numeric matrix of counts, one row per time point and one
#'   column per clade, with column names giving the clades.
#' @param t Numeric vector of times (days since t0), one per row of
#'   `counts`.
#' @param ref_clade Character string, the reference clade.
#' @param fixed_growth Optional named numeric vector of growth rates to hold
#'   fixed, named by clade.
#' @param max_iter Maximum number of Newton iterations.
#' @param tol Convergence tolerance: the fit stops when the relative change
#'   in log-likelihood falls below `tol`, or the gradient is numerically
#'   zero. A purely absolute gradient tolerance is not reachable in floating
#'   point when counts are large.
#' @returns List with `intercept` and `growth` (named numeric vectors over
#'   all clades, 0 for the reference clade), `vcov` (covariance of the
#'   estimated parameters, named `a_<clade>` and `b_<clade>`), `loglik`,
#'   `iterations`, `converged`, `ref_clade`, `fixed_growth` and `clades`.
#' @autoglobal
fit_mlr <- function(counts,
                    t,
                    ref_clade,
                    fixed_growth = NULL,
                    max_iter = 100,
                    tol = 1e-12) {
  clades <- colnames(counts)
  if (!ref_clade %in% clades) {
    cli::cli_abort("{.arg ref_clade} {ref_clade} is not a column of counts.")
  }
  if (!all(names(fixed_growth) %in% setdiff(clades, ref_clade))) {
    cli::cli_abort(
      "{.arg fixed_growth} must be named by non-reference clades."
    )
  }
  others <- setdiff(clades, ref_clade)
  n_k <- length(others)
  x <- cbind(1, t)
  n_t <- rowSums(counts)
  # Parameter vector: intercepts then growth rates for the non-reference
  # clades; fixed growth rates are dropped from the estimated parameters
  par_names <- c(paste0("a_", others), paste0("b_", others))
  fixed_names <- if (is.null(fixed_growth)) {
    character(0)
  } else {
    paste0("b_", names(fixed_growth))
  }
  is_free <- !par_names %in% fixed_names
  params <- setNames(numeric(2 * n_k), par_names)
  params[fixed_names] <- fixed_growth

  theta_of <- function(params) {
    coefs <- rbind(params[seq_len(n_k)], params[n_k + seq_len(n_k)])
    eta <- cbind(0, x %*% coefs)
    eta <- eta - apply(eta, 1, max)
    p <- exp(eta)
    p / rowSums(p)
  }
  ordered_counts <- counts[, c(ref_clade, others), drop = FALSE]
  loglik_of <- function(params) {
    sum(ordered_counts * log(pmax(theta_of(params), .Machine$double.xmin)))
  }
  derivatives <- function(params) {
    theta <- theta_of(params)[, -1, drop = FALSE]
    resid_mat <- ordered_counts[, -1, drop = FALSE] - n_t * theta
    grad <- c(colSums(resid_mat), colSums(resid_mat * t))
    # Negative Hessian: sum_t N_t (diag(theta) - theta theta') (x) x x'
    info <- matrix(0, 2 * n_k, 2 * n_k)
    for (i in seq_len(nrow(theta))) {
      w <- n_t[i] * (diag(theta[i, ], n_k) - tcrossprod(theta[i, ]))
      info <- info + kronecker(tcrossprod(x[i, ]), w)
    }
    list(grad = grad, info = info)
  }

  loglik <- loglik_of(params)
  converged <- FALSE
  for (iter in seq_len(max_iter)) {
    d <- derivatives(params)
    grad <- d$grad[is_free]
    if (max(abs(grad)) < sqrt(.Machine$double.eps)) {
      converged <- TRUE
      break
    }
    newton_step <- solve(d$info[is_free, is_free], grad)
    # Step halving guards against overshooting from poor starting values
    shrink <- 1
    repeat {
      candidate <- params
      candidate[is_free] <- params[is_free] + shrink * newton_step
      new_loglik <- loglik_of(candidate)
      if (new_loglik >= loglik || shrink < 1e-8) break
      shrink <- shrink / 2
    }
    change <- abs(new_loglik - loglik)
    params <- candidate
    loglik <- new_loglik
    if (change <= tol * abs(loglik)) {
      converged <- TRUE
      break
    }
  }
  if (!converged) {
    cli::cli_warn("MLR fit did not converge in {max_iter} iterations.")
  }
  d <- derivatives(params)
  param_vcov <- solve(d$info[is_free, is_free])
  dimnames(param_vcov) <- list(par_names[is_free], par_names[is_free])

  all_clades <- c(ref_clade, others)
  intercept <- setNames(c(0, params[seq_len(n_k)]), all_clades)[clades]
  growth <- setNames(c(0, params[n_k + seq_len(n_k)]), all_clades)[clades]
  list(
    intercept = intercept,
    growth = growth,
    vcov = param_vcov,
    loglik = loglik,
    iterations = iter,
    converged = converged,
    ref_clade = ref_clade,
    fixed_growth = fixed_growth,
    clades = clades
  )
}

#' Clade proportions from fitted MLR parameters
#'
#' @param intercept Named numeric vector of intercepts, one per clade.
#' @param growth Named numeric vector of growth rates, one per clade, with
#'   the same names as `intercept`.
#' @param t Numeric vector of times (days since t0).
#' @returns Numeric matrix of clade proportions, one row per time and one
#'   column per clade.
#' @autoglobal
get_mlr_theta <- function(intercept, growth, t) {
  eta <- outer(t, growth) + rep(intercept, each = length(t))
  eta <- eta - apply(eta, 1, max)
  p <- exp(eta)
  theta <- p / rowSums(p)
  colnames(theta) <- names(intercept)
  theta
}

#' Fit the identifiability truth model
#'
#' Fits the MLR defining the true clade proportions for #141 to the daily
#' counts from [get_truth_fit_data()].
#'
#' @param truth_data Data frame from [get_truth_fit_data()].
#' @param ref_clade Character string, the reference clade.
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select arrange
#' @returns Output of [fit_mlr()], with `t0` (the date at t = 0) and
#'   `theta`, a data frame of the true proportions by date and clade.
#' @autoglobal
fit_truth_mlr <- function(truth_data, ref_clade) {
  wide <- truth_data |>
    select(date, t, clade, n) |>
    pivot_wider(names_from = clade, values_from = n) |>
    arrange(date)
  clades <- levels(truth_data$clade)
  counts <- as.matrix(wide[, clades])
  fit <- fit_mlr(counts, wide$t, ref_clade = ref_clade)
  theta <- get_mlr_theta(fit$intercept, fit$growth, wide$t)
  fit$t0 <- min(wide$date) - min(wide$t)
  fit$theta <- data.frame(
    date = rep(wide$date, times = length(clades)),
    t = rep(wide$t, times = length(clades)),
    clade = factor(rep(clades, each = nrow(wide)), levels = clades),
    theta = as.vector(theta)
  )
  fit
}

#' Sample MLR parameters from their Laplace approximation
#'
#' Draws intercepts and growth rates from N(phi_hat, Sigma_hat), where
#' phi_hat are the fitted parameters and Sigma_hat is `vcov`, the inverse
#' Hessian from [fit_mlr()]. Fixed growth rates (constrained fits) are held
#' at their fixed values.
#'
#' @param fit Output of [fit_mlr()] (or [fit_truth_mlr()]).
#' @param n_draws Integer, number of draws.
#' @param vcov Covariance matrix to sample from; defaults to the fit's own.
#' @returns List with `intercept` and `growth`, each an `n_draws` x clades
#'   matrix with columns named by clade (0 for the reference clade).
#' @autoglobal
sample_mlr_params <- function(fit, n_draws, vcov = fit$vcov) {
  clades <- fit$clades
  mean_par <- c(
    setNames(fit$intercept, paste0("a_", clades)),
    setNames(fit$growth, paste0("b_", clades))
  )
  free <- rownames(vcov)
  z <- matrix(stats::rnorm(n_draws * length(free)), n_draws)
  draws <- matrix(
    mean_par,
    nrow = n_draws, ncol = length(mean_par), byrow = TRUE,
    dimnames = list(NULL, names(mean_par))
  )
  draws[, free] <- draws[, free] + z %*% chol(vcov)
  intercept <- draws[, paste0("a_", clades), drop = FALSE]
  growth <- draws[, paste0("b_", clades), drop = FALSE]
  colnames(intercept) <- clades
  colnames(growth) <- clades
  list(intercept = intercept, growth = growth)
}

#' Intervals for the identifiability truth fit
#'
#' Computes two 95% intervals to check the truth fit against the data:
#' \itemize{
#'   \item the latent clade proportion theta_t, from parameter draws from
#'     the fit's Laplace approximation ([sample_mlr_params()]) passed
#'     through the softmax;
#'   \item the weekly observed proportion, from the predictive distribution
#'     of the counts given the fitted theta_t: multinomial draws on each day
#'     with that day's actual number of sequences, summed over the week. This
#'     includes multinomial observation noise only, not parameter
#'     uncertainty.
#' }
#' Weeks are as in [get_plot_truth_fit()] (starting on Wednesdays, plotted
#' at their midpoint).
#'
#' @param truth_data Data frame from [get_truth_fit_data()].
#' @param truth_fit Output of [fit_truth_mlr()].
#' @param n_draws Integer, number of draws for each interval.
#' @param probs Numeric vector of length 2, the interval quantiles.
#' @param seed Integer random seed.
#' @importFrom dplyr mutate group_by summarise left_join arrange
#' @importFrom lubridate floor_date
#' @returns List with `latent` (date, clade, lower, upper) and `weekly`
#'   (week, clade, proportion, sequences, lower, upper).
#' @autoglobal
get_truth_fit_intervals <- function(truth_data,
                                    truth_fit,
                                    n_draws = 2000,
                                    probs = c(0.025, 0.975),
                                    seed = 141) {
  set.seed(seed)
  clades <- truth_fit$clades
  dates <- sort(unique(truth_data$date))
  t_days <- as.numeric(dates - truth_fit$t0)

  # Latent proportions: parameter draws through the softmax
  draws <- sample_mlr_params(truth_fit, n_draws)
  latent <- array(NA_real_, c(n_draws, length(t_days), length(clades)))
  for (i in seq_len(n_draws)) {
    latent[i, , ] <- get_mlr_theta(
      setNames(draws$intercept[i, ], clades),
      setNames(draws$growth[i, ], clades),
      t_days
    )
  }
  latent_q <- apply(latent, c(2, 3), stats::quantile, probs = probs)
  latent_df <- data.frame(
    date = rep(dates, times = length(clades)),
    clade = factor(rep(clades, each = length(dates)), levels = clades),
    lower = as.vector(latent_q[1, , ]),
    upper = as.vector(latent_q[2, , ])
  )

  # Weekly observed proportions: daily multinomial draws given the fitted
  # theta and that day's sequences, aggregated to weeks
  theta <- get_mlr_theta(truth_fit$intercept, truth_fit$growth, t_days)
  n_day <- vapply(dates, function(d) {
    sum(truth_data$n[truth_data$date == d])
  }, numeric(1))
  week <- floor_date(dates, "week", week_start = 3)
  weeks <- sort(unique(week))
  sim <- array(0, c(n_draws, length(weeks), length(clades)))
  for (j in seq_along(dates)) {
    if (n_day[j] == 0) next
    w <- match(week[j], weeks)
    day_draws <- stats::rmultinom(n_draws, n_day[j], theta[j, ])
    sim[, w, ] <- sim[, w, ] + t(day_draws)
  }
  week_n <- vapply(weeks, function(w) sum(n_day[week == w]), numeric(1))
  prop <- sweep(sim, 2, pmax(week_n, 1), "/")
  prop_q <- apply(prop, c(2, 3), stats::quantile, probs = probs)
  pi_df <- data.frame(
    week = rep(weeks, times = length(clades)) + 3,
    clade = factor(rep(clades, each = length(weeks)), levels = clades),
    lower = as.vector(prop_q[1, , ]),
    upper = as.vector(prop_q[2, , ])
  )
  weekly <- truth_data |>
    mutate(week = floor_date(date, "week", week_start = 3)) |>
    group_by(week) |>
    mutate(week_n = sum(n)) |>
    group_by(week, clade) |>
    summarise(
      proportion = sum(n) / week_n[1],
      sequences = week_n[1],
      .groups = "drop"
    ) |>
    mutate(week = week + 3) |>
    left_join(pi_df, by = c("week", "clade")) |>
    arrange(clade, week)
  list(latent = latent_df, weekly = weekly)
}

#' Plot the identifiability truth fit against the data
#'
#' Shows the observed weekly clade proportions (points sized by the number
#' of sequences, with 95% prediction intervals for the observed proportion
#' given the fit and that week's sequences) and the fitted true proportions
#' (lines, with 95% intervals from the fit's Laplace approximation) over the
#' fitting window, one panel per clade, coloured with the clade colours used
#' in the rest of the paper. The scoring window (-31 to +10 days around the
#' nowcast date) is shaded and the nowcast date marked.
#'
#' @param truth_fit Output of [fit_truth_mlr()].
#' @param truth_intervals Output of [get_truth_fit_intervals()].
#' @param nowcast_date Date or character string, the nowcast date.
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon
#'   geom_linerange annotate geom_vline facet_wrap labs ggsave
#'   scale_size_area scale_color_manual scale_fill_manual
#' @returns ggplot object
#' @autoglobal
get_plot_truth_fit <- function(truth_fit,
                               truth_intervals,
                               nowcast_date,
                               plot_name,
                               output_fp = file.path(
                                 "output", "figs", "identifiability"
                               ),
                               save = TRUE) {
  nowcast_date <- as.Date(nowcast_date)
  clade_colors <- plot_components()$clade_colors

  p <- ggplot() +
    annotate("rect",
      xmin = nowcast_date - 31, xmax = nowcast_date + 10,
      ymin = -Inf, ymax = Inf, alpha = 0.1
    ) +
    geom_vline(xintercept = nowcast_date, linetype = "dashed") +
    geom_ribbon(
      data = truth_intervals$latent,
      aes(x = date, ymin = lower, ymax = upper, fill = clade),
      alpha = 0.3
    ) +
    geom_line(
      data = truth_fit$theta,
      aes(x = date, y = theta, color = clade)
    ) +
    geom_linerange(
      data = truth_intervals$weekly,
      aes(x = week, ymin = lower, ymax = upper, color = clade),
      alpha = 0.6
    ) +
    geom_point(
      data = truth_intervals$weekly,
      aes(x = week, y = proportion, size = sequences, color = clade),
      alpha = 0.8
    ) +
    facet_wrap(~clade, scales = "free_y") +
    scale_color_manual(values = clade_colors, guide = "none") +
    scale_fill_manual(values = clade_colors, guide = "none") +
    scale_size_area(max_size = 3, name = "Weekly\nsequences") +
    get_plot_theme(dates = TRUE) +
    labs(
      x = "Collection date", y = "Clade proportion",
      title = glue::glue(
        "Identifiability truth: MLR fit vs weekly observed proportions"
      ),
      subtitle = glue::glue(
        "Line, ribbon: fitted proportion, 95% interval. Points, bars: ",
        "weekly observed proportion, 95% prediction interval.\n",
        "Reference clade {truth_fit$ref_clade}; shaded = scoring window ",
        "(-31 to +10 days); dashed = nowcast date {nowcast_date}"
      )
    )

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 12, height = 8
    )
  }
  return(p)
}

#' Training counts for the identifiability models
#'
#' Builds the information that both the true and misspecified models are
#' fitted to in #141: the truth's expected counts N_t * theta_t over the
#' training window t0 to the nowcast date, using the location's final daily
#' number of sequences N_t. Fitting to expected counts rather than one noisy
#' dataset makes each fit deterministic. Days with no sequences contribute
#' nothing to the fit.
#'
#' @param truth_data Data frame from [get_truth_fit_data()].
#' @param truth_fit Output of [fit_truth_mlr()].
#' @param nowcast_date Date or character string, the nowcast date (the end
#'   of the training window).
#' @returns List with `counts` (numeric matrix of expected counts, one row
#'   per day and one column per clade), `t` (days since t0) and `date`.
#' @autoglobal
get_id_training_counts <- function(truth_data, truth_fit, nowcast_date) {
  nowcast_date <- as.Date(nowcast_date)
  dates <- sort(unique(truth_data$date[truth_data$date <= nowcast_date]))
  n_day <- vapply(dates, function(d) {
    sum(truth_data$n[truth_data$date == d])
  }, numeric(1))
  t_days <- as.numeric(dates - truth_fit$t0)
  theta <- get_mlr_theta(truth_fit$intercept, truth_fit$growth, t_days)
  list(counts = n_day * theta, t = t_days, date = dates)
}

#' Fit the true or a misspecified identifiability model
#'
#' For `eps = 0`, fits the unconstrained MLR (the true model), which recovers
#' the truth's parameters exactly because the MLR is correctly specified.
#' Otherwise fixes the rising clade's growth rate at (1 + eps) times its true
#' value and re-estimates every other parameter by maximum likelihood: the
#' best model available under a wrong growth advantage.
#'
#' @param training Output of [get_id_training_counts()].
#' @param truth_fit Output of [fit_truth_mlr()], giving the true growth rate
#'   and the reference clade.
#' @param rising_clade Character string, the clade whose growth rate is
#'   misspecified.
#' @param eps Numeric, the relative misspecification of the rising clade's
#'   growth rate (e.g. 0.1 for +10%).
#' @returns Output of [fit_mlr()] with `eps` and `rising_clade` added.
#' @autoglobal
fit_id_model <- function(training, truth_fit, rising_clade, eps) {
  fixed_growth <- if (eps == 0) {
    NULL
  } else {
    setNames((1 + eps) * truth_fit$growth[[rising_clade]], rising_clade)
  }
  fit <- fit_mlr(
    training$counts, training$t,
    ref_clade = truth_fit$ref_clade,
    fixed_growth = fixed_growth
  )
  if (!fit$converged) {
    cli::cli_abort("Model fit for eps = {eps} did not converge.")
  }
  fit$eps <- eps
  fit$rising_clade <- rising_clade
  fit
}

#' Sample clade proportions from an identifiability model
#'
#' Draws `n_samples` parameter vectors from N(phi_hat, Sigma_hat), with
#' phi_hat the model's own fitted parameters and Sigma_hat the covariance
#' shared by all models (from the unconstrained true-model fit), and passes
#' each through the softmax at every target date. The model's mean
#' proportion, used for the Brier score, is the softmax of phi_hat. The same
#' seed is used for every model, so all models use the same standard normal
#' draws and differ only in where they are centred.
#'
#' @param fit Output of [fit_id_model()].
#' @param vcov Shared covariance matrix (the true model's `vcov`).
#' @param t0 Date at t = 0.
#' @param target_dates Dates at which to sample (the scoring window).
#' @param n_samples Integer, number of samples per target date.
#' @param seed Integer random seed, the same for every model.
#' @returns List with `eps`, `samples` (array of samples x target dates x
#'   clades), `mean` (matrix of target dates x clades) and `target_date`.
#' @autoglobal
sample_id_model <- function(fit, vcov, t0, target_dates,
                            n_samples = 100, seed = 141) {
  set.seed(seed)
  clades <- fit$clades
  t_days <- as.numeric(as.Date(target_dates) - as.Date(t0))
  draws <- sample_mlr_params(fit, n_samples, vcov = vcov)
  samples <- array(
    NA_real_, c(n_samples, length(t_days), length(clades)),
    dimnames = list(NULL, as.character(target_dates), clades)
  )
  for (i in seq_len(n_samples)) {
    samples[i, , ] <- get_mlr_theta(
      draws$intercept[i, ], draws$growth[i, ], t_days
    )
  }
  mean_theta <- get_mlr_theta(fit$intercept, fit$growth, t_days)
  rownames(mean_theta) <- as.character(target_dates)
  list(
    eps = fit$eps,
    samples = samples,
    mean = mean_theta,
    target_date = as.Date(target_dates)
  )
}

#' Summarise identifiability model fits
#'
#' @param fits List of outputs of [fit_id_model()].
#' @returns Data frame with one row per model: `eps`, the rising clade's
#'   growth rate, the log-likelihood on the training counts, the number of
#'   iterations and whether the fit converged.
#' @autoglobal
summarise_id_fits <- function(fits) {
  do.call(rbind, lapply(fits, function(f) {
    data.frame(
      eps = f$eps,
      rising_growth = f$growth[[f$rising_clade]],
      loglik = f$loglik,
      iterations = f$iterations,
      converged = f$converged
    )
  }))
}

#' Plot the identifiability models against the truth
#'
#' One panel per clade over the scoring window: each model's mean
#' proportion (lines, coloured by eps) and the 95% range of its proportion
#' samples (ribbons, shown for the true model and the most extreme eps
#' values), with the truth as a dashed line over the true model (which
#' equals it) and the nowcast date marked.
#'
#' @param model_samples List of outputs of [sample_id_model()].
#' @param truth_fit Output of [fit_truth_mlr()].
#' @param nowcast_date Date or character string, the nowcast date.
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline
#'   facet_wrap labs ggsave scale_color_manual scale_fill_manual
#' @returns ggplot object
#' @autoglobal
get_plot_id_models <- function(model_samples,
                               truth_fit,
                               nowcast_date,
                               plot_name,
                               output_fp = file.path(
                                 "output", "figs", "identifiability"
                               ),
                               save = TRUE) {
  nowcast_date <- as.Date(nowcast_date)
  clades <- truth_fit$clades
  long <- function(mat, dates, eps) {
    data.frame(
      eps = eps,
      date = rep(dates, times = length(clades)),
      clade = factor(rep(clades, each = length(dates)), levels = clades),
      value = as.vector(mat)
    )
  }
  means <- do.call(rbind, lapply(model_samples, function(m) {
    long(m$mean, m$target_date, m$eps)
  }))
  eps_all <- vapply(model_samples, function(m) m$eps, numeric(1))
  ribbon_eps <- c(0, min(eps_all), max(eps_all))
  ribbons <- do.call(rbind, lapply(
    model_samples[eps_all %in% ribbon_eps],
    function(m) {
      qs <- apply(m$samples, c(2, 3), stats::quantile, probs = c(0.025, 0.975))
      lower <- long(qs[1, , ], m$target_date, m$eps)
      lower$upper <- as.vector(qs[2, , ])
      lower
    }
  ))
  eps_levels <- sort(unique(eps_all))
  eps_label <- function(x) {
    factor(sprintf("%+.0f%%", 100 * x), levels = sprintf(
      "%+.0f%%", 100 * eps_levels
    ))
  }
  means$eps_f <- eps_label(means$eps)
  ribbons$eps_f <- eps_label(ribbons$eps)
  truth <- truth_fit$theta[truth_fit$theta$date %in% means$date, ]
  # Blues for negative eps and reds for positive eps (darker for larger
  # |eps|); the true model in black
  n_neg <- sum(eps_levels < 0)
  n_pos <- sum(eps_levels > 0)
  eps_cols <- setNames(
    c(
      rev(grDevices::colorRampPalette(c("#9ECAE1", "#08306B"))(n_neg)),
      if (0 %in% eps_levels) "black",
      grDevices::colorRampPalette(c("#FCBBA1", "#67000D"))(n_pos)
    ),
    levels(eps_label(eps_levels))
  )
  p <- ggplot() +
    geom_vline(xintercept = nowcast_date, linetype = "dashed") +
    geom_ribbon(
      data = ribbons,
      aes(x = date, ymin = value, ymax = upper, fill = eps_f),
      alpha = 0.15
    ) +
    geom_line(data = means, aes(x = date, y = value, color = eps_f)) +
    geom_line(
      data = truth, aes(x = date, y = theta),
      linetype = "dashed", color = "white", linewidth = 0.3
    ) +
    facet_wrap(~clade, scales = "free_y") +
    scale_color_manual(
      values = eps_cols,
      name = "25A growth\nmisspecification"
    ) +
    scale_fill_manual(values = eps_cols, guide = "none") +
    get_plot_theme(dates = TRUE) +
    labs(
      x = "Target date", y = "Clade proportion",
      title = "Identifiability models over the scoring window",
      subtitle = glue::glue(
        "Lines: model mean proportion. Ribbons: 95% range of the 100 ",
        "proportion samples (true model and extreme eps).\n",
        "The true model (+0%, black) equals the truth (dashed white). ",
        "Dashed vertical: nowcast date {nowcast_date}"
      )
    )

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 12, height = 8
    )
  }
  return(p)
}

#' True clade proportions over the scoring window
#'
#' @param truth_fit Output of [fit_truth_mlr()].
#' @param target_dates Dates of the scoring window.
#' @returns Numeric matrix of true proportions, one row per target date
#'   (named by date) and one column per clade.
#' @autoglobal
get_id_truth_theta <- function(truth_fit, target_dates) {
  t_days <- as.numeric(as.Date(target_dates) - truth_fit$t0)
  theta <- get_mlr_theta(truth_fit$intercept, truth_fit$growth, t_days)
  rownames(theta) <- as.character(target_dates)
  theta
}

#' Daily numbers of sequences for a Loop A volume
#'
#' Converts a volume in sequences per week into a number of sequences on
#' each of the `n_days` scoring days, as set out in #141. From 7 per week
#' (1 per day) up, every day has the same number, rate / 7. Below that, the
#' rate x n_days / 7 sequences fall on that many randomly chosen days, with
#' 1 sequence each and none on the other days; these days are random, so
#' call this once per replicate.
#'
#' @param rate Numeric, sequences per week.
#' @param n_days Integer, number of scoring days.
#' @returns Numeric vector of length `n_days`, the sequences on each day.
#' @autoglobal
draw_id_daily_n <- function(rate, n_days = 42) {
  if (rate >= 7) {
    n_day <- rate / 7
    if (n_day != round(n_day)) {
      cli::cli_abort("Rates of 7 or more per week must be multiples of 7.")
    }
    return(rep(n_day, n_days))
  }
  n_seq <- rate * n_days / 7
  if (n_seq != round(n_seq) || n_seq < 1) {
    cli::cli_abort(
      "A rate of {rate} per week gives a non-integer number of sequences."
    )
  }
  n <- numeric(n_days)
  n[sample.int(n_days, n_seq)] <- 1
  n
}

#' Draw observed clade counts from the truth
#'
#' Draws C_t ~ Multinomial(N_t, theta_t) on each scoring day; days with
#' N_t = 0 get no sequences.
#'
#' @param truth_theta Matrix of true proportions from
#'   [get_id_truth_theta()].
#' @param n_day Numeric vector of sequences on each day.
#' @returns Numeric matrix of counts with the same shape as `truth_theta`.
#' @autoglobal
draw_id_observations <- function(truth_theta, n_day) {
  counts <- matrix(
    0, nrow(truth_theta), ncol(truth_theta),
    dimnames = dimnames(truth_theta)
  )
  for (j in which(n_day > 0)) {
    counts[j, ] <- stats::rmultinom(1, n_day[j], truth_theta[j, ])
  }
  counts
}

#' Brier score of a mean forecast, as computed by the Hub
#'
#' The Hub's `brier_point` (`src/model_scoring_functions.R`):
#' 0.5 / N * sum(C * (theta_bar - 1)^2 + (N - C) * theta_bar^2), which equals
#' 0.5 * (sum(theta_bar^2) - 2 * sum(theta_bar * p_hat) + 1) with
#' p_hat = C / N. Days with no sequences are not scored.
#'
#' @param mean_theta Matrix of mean proportions (days x clades).
#' @param counts Matrix of observed counts with the same shape.
#' @returns Numeric vector of daily Brier scores, `NA` where N = 0.
#' @autoglobal
score_id_brier <- function(mean_theta, counts) {
  n_day <- rowSums(counts)
  score <- 0.5 / n_day * rowSums(
    counts * (mean_theta - 1)^2 + (n_day - counts) * mean_theta^2
  )
  score[n_day == 0] <- NA_real_
  score
}

#' Predictive count draws for the energy score, as generated by the Hub
#'
#' For each day, draws 100 multinomial samples of size N from each of the
#' model's proportion samples, as the Hub does (10,000 count draws for 100
#' samples). The draws are made once per model and N, and reused in every
#' replicate. Also precomputes the half mean pairwise distance between draws,
#' 0.5 * mean ||X - X'||, which does not depend on the observations: it is
#' recovered exactly from [scoringRules::es_sample()] at y = 0, since
#' the energy score of y is the mean distance from the draws to y minus half
#' the mean distance between pairs of draws.
#'
#' @param samples Array of proportion samples (samples x days x clades),
#'   from [sample_id_model()].
#' @param n Numeric, the number of sequences N on each day to draw for.
#' @param days Integer indices of the days to draw for.
#' @param n_per_sample Integer, multinomial draws per proportion sample.
#' @param seed Integer random seed. Use the same seed for every model so
#'   that their draws share random numbers.
#' @returns List with `draws` (a list of clades x draws matrices, one per
#'   day in `days`, `NULL` elsewhere) and `half_spread` (numeric vector,
#'   `NA` for days not drawn).
#' @autoglobal
draw_id_predictive <- function(samples, n, days = seq_len(dim(samples)[2]),
                               n_per_sample = 100, seed = 141) {
  set.seed(seed)
  n_days <- dim(samples)[2]
  draws <- vector("list", n_days)
  half_spread <- rep(NA_real_, n_days)
  for (j in days) {
    dat <- do.call(cbind, lapply(seq_len(dim(samples)[1]), function(i) {
      stats::rmultinom(n_per_sample, n, samples[i, j, ])
    }))
    draws[[j]] <- dat
    half_spread[j] <- mean(sqrt(colSums(dat^2))) -
      scoringRules::es_sample(y = numeric(nrow(dat)), dat = dat)
  }
  list(draws = draws, half_spread = half_spread)
}

#' Energy score of count draws, as computed by the Hub
#'
#' Equals [scoringRules::es_sample()] on each day (as used by the Hub),
#' computed as mean ||X - y|| minus the precomputed half spread from
#' [draw_id_predictive()]. Days with no sequences are not scored.
#'
#' @param predictive Output of [draw_id_predictive()] for the N on these
#'   days.
#' @param counts Matrix of observed counts (days x clades).
#' @returns Numeric vector of daily energy scores, `NA` where N = 0.
#' @autoglobal
score_id_energy <- function(predictive, counts) {
  score <- rep(NA_real_, nrow(counts))
  for (j in which(rowSums(counts) > 0)) {
    dat <- predictive$draws[[j]]
    score[j] <- mean(sqrt(colSums((dat - counts[j, ])^2))) -
      predictive$half_spread[j]
  }
  score
}

#' Score every identifiability model over replicate observation sets
#'
#' For one Loop A volume: draws each model's predictive count draws once
#' (with the same seed for every model), then for each replicate draws one
#' set of observations from the truth (and, below 7/week, which days have a
#' sequence) and scores every model against it, giving each model's mean
#' Brier and energy score over the scored days.
#'
#' @param model_samples List of outputs of [sample_id_model()].
#' @param truth_theta Matrix of true proportions from
#'   [get_id_truth_theta()].
#' @param rate Numeric, sequences per week.
#' @param n_reps Integer, number of replicates.
#' @param seed Integer random seed.
#' @returns Data frame with one row per replicate and model: `rate`,
#'   `replicate`, `eps`, `n_days_scored`, `brier`, `energy`.
#' @autoglobal
run_id_replicates <- function(model_samples, truth_theta, rate, n_reps,
                              seed = 141) {
  n_days <- nrow(truth_theta)
  # Every day has the same N: rate / 7 from 7/week up, otherwise 1
  n <- if (rate >= 7) rate / 7 else 1
  predictive <- lapply(model_samples, function(m) {
    draw_id_predictive(m$samples, n, seed = seed)
  })
  eps <- vapply(model_samples, function(m) m$eps, numeric(1))
  set.seed(seed + 1)
  out <- vector("list", n_reps)
  for (r in seq_len(n_reps)) {
    n_day <- draw_id_daily_n(rate, n_days)
    counts <- draw_id_observations(truth_theta, n_day)
    out[[r]] <- data.frame(
      rate = rate,
      replicate = r,
      eps = eps,
      n_days_scored = sum(n_day > 0),
      brier = vapply(model_samples, function(m) {
        mean(score_id_brier(m$mean, counts), na.rm = TRUE)
      }, numeric(1)),
      energy = vapply(predictive, function(p) {
        mean(score_id_energy(p, counts), na.rm = TRUE)
      }, numeric(1))
    )
  }
  do.call(rbind, out)
}

#' SRSS of each misspecified model against the true model by replicate
#'
#' Both models are scored on the same days, so the pairwise tournament
#' reduces to mean score (misspecified) / mean score (true).
#'
#' @param replicate_scores Output of [run_id_replicates()].
#' @returns Data frame with one row per replicate and misspecified model:
#'   `rate`, `replicate`, `eps`, `srss_brier`, `srss_energy`.
#' @autoglobal
get_id_srss <- function(replicate_scores) {
  true_scores <- replicate_scores[replicate_scores$eps == 0, ]
  mis <- replicate_scores[replicate_scores$eps != 0, ]
  idx <- match(
    paste(mis$rate, mis$replicate),
    paste(true_scores$rate, true_scores$replicate)
  )
  data.frame(
    rate = mis$rate,
    replicate = mis$replicate,
    eps = mis$eps,
    srss_brier = mis$brier / true_scores$brier[idx],
    srss_energy = mis$energy / true_scores$energy[idx]
  )
}

#' Expected SRSS from replicate scores (sanity check)
#'
#' Estimates each misspecified model's expected SRSS as the ratio of its
#' mean score over replicates to the true model's, with a Monte Carlo
#' standard error from the delta method, and the proportion of replicates
#' in which the true model wins.
#'
#' @param replicate_scores Output of [run_id_replicates()].
#' @returns Data frame with one row per rate, score and eps.
#' @autoglobal
summarise_id_expected_srss <- function(replicate_scores) {
  srss <- get_id_srss(replicate_scores)
  true_scores <- replicate_scores[replicate_scores$eps == 0, ]
  out <- list()
  for (rate in unique(replicate_scores$rate)) {
    for (score in c("brier", "energy")) {
      ref <- true_scores[[score]][true_scores$rate == rate]
      for (e in setdiff(unique(replicate_scores$eps), 0)) {
        mis <- replicate_scores[[score]][
          replicate_scores$rate == rate & replicate_scores$eps == e
        ]
        ratio <- mean(mis) / mean(ref)
        # Delta-method SE of a ratio of means of paired samples
        lin <- (mis - ratio * ref) / mean(ref)
        s <- srss[[paste0("srss_", score)]][
          srss$rate == rate & srss$eps == e
        ]
        out[[length(out) + 1]] <- data.frame(
          rate = rate, score = score, eps = e,
          expected_srss = ratio,
          mc_se = stats::sd(lin) / sqrt(length(mis)),
          p_true_wins = mean(s > 1),
          n_reps = length(mis)
        )
      }
    }
  }
  do.call(rbind, out)
}

#' Summarise the SRSS across replicates for each volume and eps
#'
#' For each volume (sequences per week), score and misspecified model: the
#' median and 2.5-97.5% range of the SRSS across replicates (the headline),
#' the expected SRSS with its Monte Carlo SE ([summarise_id_expected_srss()]),
#' the proportion of replicates in which the true model wins with its Monte
#' Carlo SE, and whether the misspecification is detectable: the 95% range of
#' SRSS excludes 1, i.e. the true model wins in at least 97.5% of
#' replicates (#141).
#'
#' @param replicate_scores Output of [run_id_replicates()], for any number
#'   of volumes.
#' @returns Data frame with one row per rate, score and eps.
#' @autoglobal
summarise_id_srss <- function(replicate_scores) {
  srss <- get_id_srss(replicate_scores)
  long <- rbind(
    data.frame(
      srss[, c("rate", "replicate", "eps")],
      score = "brier", srss = srss$srss_brier, stringsAsFactors = FALSE
    ),
    data.frame(
      srss[, c("rate", "replicate", "eps")],
      score = "energy", srss = srss$srss_energy, stringsAsFactors = FALSE
    )
  )
  ranges <- long |>
    dplyr::summarise(
      median = stats::median(srss),
      lower = stats::quantile(srss, 0.025, names = FALSE),
      upper = stats::quantile(srss, 0.975, names = FALSE),
      .by = c(rate, score, eps)
    )
  expected <- summarise_id_expected_srss(replicate_scores)
  out <- dplyr::left_join(ranges, expected, by = c("rate", "score", "eps"))
  out$p_true_wins_se <- sqrt(
    out$p_true_wins * (1 - out$p_true_wins) / out$n_reps
  )
  out$detectable <- out$lower > 1
  # A 95% range entirely below 1 would mean the misspecified model reliably
  # wins; this should not happen given the sanity check, so flag it
  out$misspecified_wins <- out$upper < 1
  dplyr::arrange(out, score, eps, rate)
}

#' Minimum detectable misspecification for each volume
#'
#' For each volume, score and direction of misspecification (eps < 0 or
#' eps > 0), the smallest |eps| in the grid that is detectable and for which
#' every larger |eps| in that direction is also detectable. `NA` when even
#' the largest |eps| is not detectable.
#'
#' @param srss_summary Output of [summarise_id_srss()].
#' @returns Data frame with columns `rate`, `score`, `direction` ("slower"
#'   for eps < 0, "faster" for eps > 0) and `min_detectable_eps` (|eps|).
#' @autoglobal
get_id_min_detectable <- function(srss_summary) {
  srss_summary$direction <- ifelse(srss_summary$eps < 0, "slower", "faster")
  groups <- unique(srss_summary[, c("rate", "score", "direction")])
  groups$min_detectable_eps <- NA_real_
  for (i in seq_len(nrow(groups))) {
    g <- srss_summary[
      srss_summary$rate == groups$rate[i] &
        srss_summary$score == groups$score[i] &
        srss_summary$direction == groups$direction[i],
    ]
    g <- g[order(abs(g$eps), decreasing = TRUE), ]
    # Walk down from the largest |eps| while every one is detectable
    n_ok <- if (all(g$detectable)) nrow(g) else which(!g$detectable)[1] - 1
    if (n_ok > 0) {
      groups$min_detectable_eps[i] <- abs(g$eps[n_ok])
    }
  }
  groups[order(groups$score, groups$direction, groups$rate), ]
}

#' Plot SRSS against sequencing volume (Loop A)
#'
#' Median SRSS (lines and points) and its 2.5-97.5% range across replicates
#' (ribbons) against sequences per week on a log scale, with sequences per
#' day on the top axis, coloured by |eps|. Panels by score (rows) and
#' direction of the misspecification (columns).
#'
#' @param srss_summary Output of [summarise_id_srss()].
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon
#'   geom_hline facet_grid labs ggsave scale_x_log10 sec_axis
#'   scale_color_viridis_d scale_fill_viridis_d labeller
#' @returns ggplot object
#' @autoglobal
get_plot_id_srss_vs_n <- function(srss_summary,
                                  plot_name,
                                  output_fp = file.path(
                                    "output", "figs", "identifiability"
                                  ),
                                  save = TRUE) {
  plot_df <- srss_summary
  plot_df$abs_eps <- factor(
    sprintf("%.0f%%", 100 * abs(plot_df$eps)),
    levels = sprintf("%.0f%%", 100 * sort(unique(abs(plot_df$eps))))
  )
  plot_df$direction <- factor(
    ifelse(plot_df$eps < 0, "25A growth too slow", "25A growth too fast"),
    levels = c("25A growth too slow", "25A growth too fast")
  )
  plot_df$score <- factor(plot_df$score,
    levels = c("brier", "energy"),
    labels = c("Brier score", "Energy score")
  )
  p <- ggplot(plot_df, aes(x = rate, y = median, color = abs_eps)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = abs_eps),
      alpha = 0.15, color = NA
    ) +
    geom_line() +
    geom_point(size = 1) +
    facet_grid(score ~ direction, scales = "free_y") +
    scale_x_log10(sec.axis = sec_axis(~ . / 7, name = "Sequences per day")) +
    scale_color_viridis_d(
      name = "Misspecification\nsize", end = 0.9, direction = -1
    ) +
    scale_fill_viridis_d(end = 0.9, direction = -1, guide = "none") +
    get_plot_theme() +
    labs(
      x = "Sequences per week (42 scoring days)",
      y = "SRSS of misspecified vs true model",
      title = "Loop A: SRSS of misspecified models vs sequencing volume",
      subtitle = paste(
        "Lines: median across replicates. Ribbons: 2.5-97.5% range.",
        "Detectable when the range lies above 1."
      )
    )
  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 11, height = 7
    )
  }
  return(p)
}

#' Plot the minimum detectable misspecification against volume (Loop A)
#'
#' @param min_detectable Output of [get_id_min_detectable()].
#' @param eps_grid Numeric vector of the eps values tested, to mark the
#'   grid on the y-axis.
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_step geom_point labs ggsave
#'   scale_x_log10 scale_y_continuous sec_axis scale_color_manual
#'   facet_wrap
#' @returns ggplot object
#' @autoglobal
get_plot_id_min_detectable <- function(min_detectable,
                                       eps_grid,
                                       plot_name,
                                       output_fp = file.path(
                                         "output", "figs", "identifiability"
                                       ),
                                       save = TRUE) {
  plot_df <- min_detectable
  plot_df$score <- factor(plot_df$score,
    levels = c("brier", "energy"),
    labels = c("Brier score", "Energy score")
  )
  plot_df$direction <- factor(
    ifelse(plot_df$direction == "slower", "25A growth too slow",
      "25A growth too fast"
    ),
    levels = c("25A growth too slow", "25A growth too fast")
  )
  plot_df$detected <- !is.na(plot_df$min_detectable_eps)
  # Not detectable at any tested eps: plot above the grid
  top <- 1.25 * max(abs(eps_grid))
  plot_df$y <- ifelse(plot_df$detected, plot_df$min_detectable_eps, top)
  breaks <- sort(unique(abs(eps_grid[eps_grid != 0])))
  p <- ggplot(plot_df, aes(
    x = rate, y = y, color = direction, linetype = direction
  )) +
    geom_step(data = plot_df[plot_df$detected, ], direction = "hv") +
    geom_point(aes(shape = detected), size = 2) +
    facet_wrap(~score) +
    scale_x_log10(sec.axis = sec_axis(~ . / 7, name = "Sequences per day")) +
    scale_y_continuous(
      breaks = c(breaks, top),
      labels = c(sprintf("%.0f%%", 100 * breaks), "none"),
      limits = c(0, top)
    ) +
    scale_color_manual(values = c("#2166AC", "#B2182B"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    ggplot2::scale_shape_manual(
      values = c(`TRUE` = 16, `FALSE` = 4),
      labels = c(`TRUE` = "Detectable", `FALSE` = "None detectable"),
      name = NULL
    ) +
    get_plot_theme() +
    labs(
      x = "Sequences per week (42 scoring days)",
      y = "Minimum detectable misspecification",
      title = "Loop A: smallest detectable error in 25A's growth advantage",
      subtitle = paste(
        "Smallest tested misspecification (and every larger one) at which",
        "the true model wins in at least 97.5% of replicates"
      )
    )
  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 11, height = 5
    )
  }
  return(p)
}

#' Plot the probability that the true model wins against volume (Loop A)
#'
#' @param srss_summary Output of [summarise_id_srss()].
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline
#'   geom_errorbar facet_grid labs ggsave scale_x_log10 sec_axis
#'   scale_color_viridis_d
#' @returns ggplot object
#' @autoglobal
get_plot_id_p_true_wins <- function(srss_summary,
                                    plot_name,
                                    output_fp = file.path(
                                      "output", "figs", "identifiability"
                                    ),
                                    save = TRUE) {
  plot_df <- srss_summary
  plot_df$abs_eps <- factor(
    sprintf("%.0f%%", 100 * abs(plot_df$eps)),
    levels = sprintf("%.0f%%", 100 * sort(unique(abs(plot_df$eps))))
  )
  plot_df$direction <- factor(
    ifelse(plot_df$eps < 0, "25A growth too slow", "25A growth too fast"),
    levels = c("25A growth too slow", "25A growth too fast")
  )
  plot_df$score <- factor(plot_df$score,
    levels = c("brier", "energy"),
    labels = c("Brier score", "Energy score")
  )
  p <- ggplot(plot_df, aes(x = rate, y = p_true_wins, color = abs_eps)) +
    geom_hline(yintercept = 0.5, linetype = "dotted") +
    geom_hline(yintercept = 0.975, linetype = "dashed") +
    geom_errorbar(
      aes(
        ymin = p_true_wins - 1.96 * p_true_wins_se,
        ymax = p_true_wins + 1.96 * p_true_wins_se
      ),
      width = 0
    ) +
    geom_line() +
    geom_point(size = 1) +
    facet_grid(score ~ direction) +
    scale_x_log10(sec.axis = sec_axis(~ . / 7, name = "Sequences per day")) +
    scale_color_viridis_d(
      name = "Misspecification\nsize", end = 0.9, direction = -1
    ) +
    get_plot_theme() +
    labs(
      x = "Sequences per week (42 scoring days)",
      y = "P(true model has the lower score)",
      title = "Loop A: probability the evaluation ranks the true model first",
      subtitle = paste(
        "Bars: 95% Monte Carlo interval. Dashed: 0.975 (detection",
        "threshold); dotted: 0.5 (no better than chance)."
      )
    )
  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 11, height = 7
    )
  }
  return(p)
}

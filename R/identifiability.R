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

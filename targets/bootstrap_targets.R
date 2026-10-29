bootstrap_targets <- list(
  # Moving block bootstrap settings (Pohle, Zahn & Lerch 2026)-------------
  # Number of bootstrap resamples. Small for development; increase for the
  # final results.
  tar_target(
    name = n_boot,
    command = 100
  ),
  # Block length in nowcast dates, l = 3 * floor(N^(1/4)) = 6 for N = 35
  tar_target(
    name = boot_block_length,
    command = 3 * floor(length(nowcast_dates)^(1 / 4))
  ),
  # Significance level for the Bonferroni simultaneous bands
  tar_target(
    name = ci_alpha,
    command = 0.05
  ),
  # Resampled nowcast dates, drawn once and shared by every CI computation
  tar_target(
    name = boot_dates,
    command = get_block_bootstrap_dates(
      nowcast_dates = nowcast_dates,
      n_boot = n_boot,
      block_length = boot_block_length
    )
  ),
  # Absolute scores: mean score by model over all rows-------------------
  # Bootstrap estimates (one row per resample and model) are kept as
  # targets so the bootstrap distributions can be inspected.
  # CADPH-CATaMaran has only 2 CA nowcast dates, too few to bootstrap, so
  # it is excluded from the CA tables.
  ## US excluding CA
  tar_target(
    name = boot_abs_brier_ep,
    command = get_bootstrap_mean_scores(
      scores_obj = su_scores_ep,
      boot_dates = boot_dates,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = ci_abs_brier_ep,
    command = get_bonferroni_ci(
      estimates = get_mean_score_by_model(su_scores_ep, "brier_score"),
      boot_estimates = boot_abs_brier_ep,
      alpha = ci_alpha
    )
  ),
  tar_target(
    name = boot_abs_energy_ep,
    command = get_bootstrap_mean_scores(
      scores_obj = su_scores_ep,
      boot_dates = boot_dates,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = ci_abs_energy_ep,
    command = get_bonferroni_ci(
      estimates = get_mean_score_by_model(su_scores_ep, "energy_score"),
      boot_estimates = boot_abs_energy_ep,
      alpha = ci_alpha
    )
  ),
  ## CA
  tar_target(
    name = boot_abs_brier_ca,
    command = get_bootstrap_mean_scores(
      scores_obj = su_scores_ca,
      boot_dates = boot_dates,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = ci_abs_brier_ca,
    command = get_bonferroni_ci(
      estimates = filter(
        get_mean_score_by_model(su_scores_ca, "brier_score"),
        model != "CADPH-CATaMaran"
      ),
      boot_estimates = boot_abs_brier_ca,
      alpha = ci_alpha
    )
  ),
  tar_target(
    name = boot_abs_energy_ca,
    command = get_bootstrap_mean_scores(
      scores_obj = su_scores_ca,
      boot_dates = boot_dates,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = ci_abs_energy_ca,
    command = get_bonferroni_ci(
      estimates = filter(
        get_mean_score_by_model(su_scores_ca, "energy_score"),
        model != "CADPH-CATaMaran"
      ),
      boot_estimates = boot_abs_energy_ca,
      alpha = ci_alpha
    )
  )
)

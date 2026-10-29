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
  )
)

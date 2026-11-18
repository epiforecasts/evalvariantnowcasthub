# Bootstrap confidence bands by location (#138)--------------------------------
# Same shared resamples (boot_dates) and methods as bootstrap_targets.R, with
# all locations together (su_scores_all, CA included) as in
# get_plot_by_location(). Each table (one per metric and score type) is one
# Bonferroni family covering every location x model row.
# Rows with fewer nowcast dates than one bootstrap block (for SRSS: dates
# shared with the baseline) get no band: the bootstrap could only reweight
# those few weeks. This also excludes CADPH-CATaMaran (2 dates), as in #131.
bootstrap_location_targets <- list(
  tar_target(
    name = min_dates_ci,
    command = boot_block_length
  ),
  # Absolute scores: mean score by location and model----------------------
  tar_target(
    name = boot_abs_brier_loc,
    command = get_bootstrap_mean_scores(
      scores_obj = su_scores_all,
      boot_dates = boot_dates,
      score_type = "brier_score",
      by = "location"
    )
  ),
  tar_target(
    name = ci_abs_brier_loc,
    command = get_bonferroni_ci(
      estimates = filter(
        get_mean_score_by_model(su_scores_all, "brier_score", "location"),
        n_nowcast_dates >= min_dates_ci
      ),
      boot_estimates = boot_abs_brier_loc,
      alpha = ci_alpha,
      by = c("location", "model")
    )
  ),
  tar_target(
    name = boot_abs_energy_loc,
    command = get_bootstrap_mean_scores(
      scores_obj = su_scores_all,
      boot_dates = boot_dates,
      score_type = "energy_score",
      by = "location"
    )
  ),
  tar_target(
    name = ci_abs_energy_loc,
    command = get_bonferroni_ci(
      estimates = filter(
        get_mean_score_by_model(su_scores_all, "energy_score", "location"),
        n_nowcast_dates >= min_dates_ci
      ),
      boot_estimates = boot_abs_energy_loc,
      alpha = ci_alpha,
      by = c("location", "model")
    )
  ),
  # SRSS vs Hub-baseline: tournament per location---------------------------
  tar_target(
    name = boot_srss_brier_loc,
    command = get_bootstrap_srss(
      scores_obj = su_scores_all,
      boot_dates = boot_dates,
      score_type = "brier_score",
      by = "location"
    )
  ),
  tar_target(
    name = ci_srss_brier_loc,
    command = get_bonferroni_ci(
      estimates = filter_min_overlap(
        estimates = get_srss(su_scores_all, "brier_score", by = "location"),
        n_dates = get_n_overlapping_dates(
          su_scores_all, "brier_score",
          by = "location"
        ),
        min_dates = min_dates_ci
      ),
      boot_estimates = boot_srss_brier_loc,
      alpha = ci_alpha,
      by = c("location", "model"),
      log_scale = TRUE
    )
  ),
  tar_target(
    name = boot_srss_energy_loc,
    command = get_bootstrap_srss(
      scores_obj = su_scores_all,
      boot_dates = boot_dates,
      score_type = "energy_score",
      by = "location"
    )
  ),
  tar_target(
    name = ci_srss_energy_loc,
    command = get_bonferroni_ci(
      estimates = filter_min_overlap(
        estimates = get_srss(su_scores_all, "energy_score", by = "location"),
        n_dates = get_n_overlapping_dates(
          su_scores_all, "energy_score",
          by = "location"
        ),
        min_dates = min_dates_ci
      ),
      boot_estimates = boot_srss_energy_loc,
      alpha = ci_alpha,
      by = c("location", "model"),
      log_scale = TRUE
    )
  ),
  # Results tables------------------------------------------------------
  tar_target(
    name = table_ci_abs_loc,
    command = bind_rows(
      format_ci_table(
        ci = ci_abs_brier_loc,
        region = "All locations",
        score_type = "brier_score",
        metric = "mean score"
      ),
      format_ci_table(
        ci = ci_abs_energy_loc,
        region = "All locations",
        score_type = "energy_score",
        metric = "mean score"
      )
    )
  ),
  tar_target(
    name = table_ci_srss_loc,
    command = bind_rows(
      format_ci_table(
        ci = ci_srss_brier_loc,
        region = "All locations",
        score_type = "brier_score",
        metric = "SRSS"
      ),
      format_ci_table(
        ci = ci_srss_energy_loc,
        region = "All locations",
        score_type = "energy_score",
        metric = "SRSS"
      )
    )
  ),
  tar_target(
    name = csv_ci_abs_loc,
    command = save_table_csv(table_ci_abs_loc, "ci_mean_score_by_location"),
    format = "file"
  ),
  tar_target(
    name = csv_ci_srss_loc,
    command = save_table_csv(table_ci_srss_loc, "ci_srss_by_location"),
    format = "file"
  )
)

permutation_targets <- list(
  # Permutation tests of each model's SRSS against Hub-baseline (#140)-----
  # Under the null hypothesis a model and Hub-baseline are exchangeable, so
  # within each exchange unit (location x nowcast date) their scores can be
  # swapped. Each permutation swaps a random subset of units and re-runs
  # the full pairwise tournament. Units are treated as independent, which
  # ignores correlation across locations and over time, so these p-values
  # are smaller than the block bootstrap CIs would imply (as acknowledged
  # for similar tests in Ray et al.).
  # Number of permutations per model. Use e.g. 100 for quicker development
  # runs (the smallest possible p-value is 1 / (n_perm + 1)).
  tar_target(
    name = n_perm,
    command = 2000
  ),
  tar_target(
    name = perm_unit,
    command = c("location", "nowcast_date"),
    format = "rds"
  ),
  # Permutation SRSS for every model except CADPH-CATaMaran (2 CA nowcast
  # dates, as for the CIs), which stays in the tournament
  ## US excluding CA (~23 min for Brier, ~8 min for energy)
  tar_target(
    name = perm_srss_brier_ep,
    command = get_permutation_srss_all(
      scores_obj = su_scores_ep,
      score_type = "brier_score",
      exclude = "CADPH-CATaMaran",
      unit = perm_unit,
      n_perm = n_perm
    )
  ),
  tar_target(
    name = perm_srss_energy_ep,
    command = get_permutation_srss_all(
      scores_obj = su_scores_ep,
      score_type = "energy_score",
      exclude = "CADPH-CATaMaran",
      unit = perm_unit,
      n_perm = n_perm
    )
  ),
  ## CA (~5 min for Brier, ~1 min for energy)
  tar_target(
    name = perm_srss_brier_ca,
    command = get_permutation_srss_all(
      scores_obj = su_scores_ca,
      score_type = "brier_score",
      exclude = "CADPH-CATaMaran",
      unit = perm_unit,
      n_perm = n_perm
    )
  ),
  tar_target(
    name = perm_srss_energy_ca,
    command = get_permutation_srss_all(
      scores_obj = su_scores_ca,
      score_type = "energy_score",
      exclude = "CADPH-CATaMaran",
      unit = perm_unit,
      n_perm = n_perm
    )
  ),
  # P-values, Bonferroni-adjusted within each region x score table--------
  tar_target(
    name = table_perm_srss,
    command = bind_rows(
      format_perm_table(
        estimates = get_srss(su_scores_ep, "brier_score"),
        perm_estimates = perm_srss_brier_ep,
        region = "US excluding CA",
        score_type = "brier_score",
        unit = perm_unit
      ),
      format_perm_table(
        estimates = get_srss(su_scores_ep, "energy_score"),
        perm_estimates = perm_srss_energy_ep,
        region = "US excluding CA",
        score_type = "energy_score",
        unit = perm_unit
      ),
      format_perm_table(
        estimates = get_srss(su_scores_ca, "brier_score"),
        perm_estimates = perm_srss_brier_ca,
        region = "CA",
        score_type = "brier_score",
        unit = perm_unit
      ),
      format_perm_table(
        estimates = get_srss(su_scores_ca, "energy_score"),
        perm_estimates = perm_srss_energy_ca,
        region = "CA",
        score_type = "energy_score",
        unit = perm_unit
      )
    )
  ),
  tar_target(
    name = csv_perm_srss,
    command = save_table_csv(table_perm_srss, "permutation_srss_overall"),
    format = "file"
  )
)

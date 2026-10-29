eda_plot_targets <- list(
  tar_target(
    name = plot_model_nowcasts,
    command = get_plot_nowcasts(
      data = raw_selected_model_outputs,
      plot_name = "plot_nowcasts"
    )
  ),
  tar_target(
    name = plot_all_states_plot_data,
    command = get_plot_mult_locs(
      data = clean_variant_data_final_all_states,
      plot_name = "plot_obs_freq_all_locs"
    )
  ),
  tar_target(
    name = plot_25A_by_loc,
    command = get_plot_clade_by_loc(
      data = clean_variant_data_final_all_states,
      clade = "25A (LP.8.1)",
      date_of_interest = nowcast_date_for_vis,
      plot_name = "plot_25A_emergence"
    )
  ),
  tar_target(
    name = plot_a_few_states_data,
    command = get_plot_mult_locs(
      data = clean_variant_data_final_all_states |> # nolint
        filter(location %in% states_for_vis),
      plot_name = "plot_obs_freq_few_locs"
    )
  ),
  tar_target(
    name = plot_US_data,
    command = get_plot_mult_locs(
      data = clean_variant_data_final_all_states |> # nolint
        group_by(clades_modeled, date) |>
        summarise(sequences = sum(sequences)) |>
        mutate(location = "US"),
      plot_name = "plot_US"
    )
  ),
  tar_target(
    name = plot_scores,
    command = get_plot_scores_t(
      scores_data_hub = scores,
      locations = c("NY", "CA", "CT"),
      this_nowcast_date = max(nowcast_dates),
      score_type = "energy"
    )
  ),
  tar_target(
    name = plot_rel_skill_bs,
    command = get_plot_rel_skill_overall(
      scores_obj = su_scores_excl_partial,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = plot_rel_skill_es,
    command = get_plot_rel_skill_overall(
      scores_obj = su_scores_excl_partial,
      score_type = "energy_score"
    )
  ),
  # Bootstrap diagnostics: how resamples differ from the original data and
  # the resulting bootstrap distribution of the mean score by model
  tar_target(
    name = plot_boot_resamples_brier_ep,
    command = get_plot_bootstrap_resamples(
      scores_obj = su_scores_ep,
      boot_dates = boot_dates,
      boot_estimates = boot_abs_brier_ep,
      ci = ci_abs_brier_ep,
      score_type = "brier_score",
      title = "US excluding CA: mean Brier score",
      plot_name = "boot_resamples_brier_ep"
    )
  ),
  tar_target(
    name = plot_boot_resamples_brier_ca,
    command = get_plot_bootstrap_resamples(
      scores_obj = su_scores_ca,
      boot_dates = boot_dates,
      boot_estimates = boot_abs_brier_ca,
      ci = ci_abs_brier_ca,
      score_type = "brier_score",
      title = "CA: mean Brier score",
      plot_name = "boot_resamples_brier_ca"
    )
  ),
  tar_target(
    name = plot_boot_resamples_energy_ep,
    command = get_plot_bootstrap_resamples(
      scores_obj = su_scores_ep,
      boot_dates = boot_dates,
      boot_estimates = boot_abs_energy_ep,
      ci = ci_abs_energy_ep,
      score_type = "energy_score",
      title = "US excluding CA: mean energy score",
      plot_name = "boot_resamples_energy_ep"
    )
  ),
  tar_target(
    name = plot_boot_resamples_energy_ca,
    command = get_plot_bootstrap_resamples(
      scores_obj = su_scores_ca,
      boot_dates = boot_dates,
      boot_estimates = boot_abs_energy_ca,
      ci = ci_abs_energy_ca,
      score_type = "energy_score",
      title = "CA: mean energy score",
      plot_name = "boot_resamples_energy_ca"
    )
  ),
  # Nowcast dates drawn into every bootstrap resample
  tar_target(
    name = plot_boot_dates,
    command = get_plot_bootstrap_dates(
      boot_dates = boot_dates,
      plot_name = "boot_dates_all_resamples"
    )
  )
)

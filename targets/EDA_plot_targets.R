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
      clade = "25A",
      date_of_interest = nowcast_date_for_vis,
      plot_name = "plot_25A_emergence"
    )
  ),
  tar_target(
    name = plot_a_few_states_data,
    command = get_plot_mult_locs(
      data = clean_variant_data_final_all_states |>
        filter(location %in% states_for_vis),
      plot_name = "plot_obs_freq_few_locs"
    )
  ),
  tar_target(
    name = plot_US_data,
    command = get_plot_mult_locs(
      data = clean_variant_data_final_all_states |>
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
  )
)

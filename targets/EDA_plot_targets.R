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
      data = clean_variant_data,
      plot_name = "plot_obs_freq_all_locs"
    )
  ),
  tar_target(
    name = plot_a_few_states_data,
    command = get_plot_mult_locs(
      data = clean_variant_data |>
        filter(location %in% states_for_vis),
      plot_name = "plot_obs_freq_few_locs"
    )
  ),
  tar_target(
    name = plot_US_data,
    command = get_plot_mult_locs(
      data = clean_variant_data |>
        group_by(clades_modeled, date) |>
        summarise(sequences = sum(sequences)) |>
        mutate(location = "US"),
      plot_name = "plot_US"
    )
  ),
  tar_target(
    name = plot_scores,
    command = get_plot_scores(
      scores_data_hub = scores
    )
  )
)

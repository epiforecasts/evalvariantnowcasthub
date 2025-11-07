fig_data_targets <- list(
  tar_target(
    name = plot_final_obs_clade_freq_US,
    command = plot_obs_clade_freq(
      obs_data = clean_variant_data_final_all_states,
      location = "US",
      temporal_granularity = "days",
      plot_name = "final_obs_freq_US"
    )
  )
)
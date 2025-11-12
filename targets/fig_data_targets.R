fig_data_targets <- list(
  tar_target(
    name = plot_final_obs_clade_freq_US,
    command = get_plot_obs_clade_freq(
      obs_data = clean_variant_data_final_all_states,
      location = "US",
      temporal_granularity = "weeks",
      plot_name = "final_obs_freq_US"
    )
  ),
  tar_target(
    name = plot_seq_count_final_US,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_final_all_states,
      location = "US",
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_counts_US"
    )
  ),
  tar_target(
    name = plot_hosp_admissions,
    command = get_plot_hosp_admissions(
      location_to_plot = "US",
      temporal_granularity = "weeks",
      date_range = c(
        min(clean_variant_data_final_all_states$date),
        max(clean_variant_data_final_all_states$date)
      ),
      location_data = location_data,
      plot_name = "hospital_admissions_US"
    )
  ),
  tar_target(
    name = first_data_fig,
    command = get_first_data_fig(
      plot_freq = plot_final_obs_clade_freq_US,
      plot_seq = plot_seq_count_final_US,
      plot_hosp = plot_hosp_admissions,
      plot_name = "final_US_seq_data"
    )
  )
)

fig_data_targets <- list(
  tar_target(
    name = date_range_to_plot,
    command = c(
      min(clean_variant_data_final_all_states$date),
      max(clean_variant_data_final_all_states$date)
    )
  ),
  tar_target(
    name = plot_final_obs_clade_freq_US,
    command = get_plot_obs_clade_freq(
      obs_data = clean_variant_data_final_all_states,
      location = "US",
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      plot_name = "final_obs_freq_US",
      title = "US"
    )
  ),
  tar_target(
    name = plot_seq_count_final_US,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_final_all_states,
      location = "US",
      date_range = date_range_to_plot,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_counts_US"
    )
  ),
  tar_target(
    name = plot_hosp_admissions,
    command = get_plot_hosp_admissions(
      location_to_plot = "US",
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
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
  ),
  tar_target(
    name = first_data_fig_horizontal,
    command = get_first_data_fig_horizontal(
      plot_freq = plot_final_obs_clade_freq_US,
      plot_seq = plot_seq_count_final_US,
      plot_hosp = plot_hosp_admissions,
      plot_name = "final_US_seq_data_horizontal"
    )
  ),
  tar_target(
    name = first_data_fig_triangular,
    command = get_first_data_fig_triangular(
      plot_freq = plot_final_obs_clade_freq_US,
      plot_seq = plot_seq_count_final_US,
      plot_hosp = plot_hosp_admissions,
      plot_name = "final_US_seq_data_triangular"
    )
  ),
  # US minus CA figs-----------------------------------------------------
  tar_target(
    name = obs_data_us_min_ca,
    command = clean_variant_data_final_all_states |>
      filter(location != "CA")
  ),
  tar_target(
    name = plot_final_obs_clade_freq_usminca,
    command = get_plot_obs_clade_freq(
      obs_data = obs_data_us_min_ca,
      location = "US",
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      plot_name = "final_obs_freq_US_min_ca",
      title = "US minus CA"
    )
  ),
  tar_target(
    name = plot_seq_count_final_usminca,
    command = get_bar_chart_seq_count(
      obs_data = obs_data_us_min_ca,
      location = "US",
      date_range = date_range_to_plot,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_counts_US_min_ca"
    )
  ),
  tar_target(
    name = plot_hosp_admissions_usminca,
    command = get_plot_hosp_admissions(
      location_to_plot = "US minus CA",
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      location_data = location_data,
      plot_name = "hospital_admissions_US_min_ca"
    )
  ),
  tar_target(
    name = first_data_fig_alt,
    command = get_first_data_fig_alt(
      plot_freq1 = plot_final_obs_clade_freq_usminca,
      plot_seq1 = plot_seq_count_final_usminca,
      plot_hosp1 = plot_hosp_admissions_usminca,
      plot_freq2 = plot_final_obs_clade_freq_ex1,
      plot_seq2 = plot_seq_count_final_ex1,
      plot_hosp2 = plot_hosp_admissions_ex1,
      plot_name = "final_seq_data_fig_us_min_ca"
    )
  ),



  # Supplement ----------------------------------------------------
  tar_target(
    name = seq_count_by_loc,
    command = get_plot_seq_counts_by_loc(
      obs_data = clean_variant_data_final_all_states,
      plot_name = "bar_chart_all_seq_counts_final"
    )
  ),
  # Make a supplemental figure for example state-----------------
  tar_target(
    name = plot_final_obs_clade_freq_ex1,
    command = get_plot_obs_clade_freq(
      obs_data = clean_variant_data_final_all_states,
      location = states_for_vis[1],
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      plot_name = "final_obs_freq_ex",
      title = states_for_vis[1]
    )
  ),
  tar_target(
    name = plot_seq_count_final_ex1,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_final_all_states,
      location = states_for_vis[1],
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      plot_name = "bar_chart_seq_counts_ex"
    )
  ),
  tar_target(
    name = plot_hosp_admissions_ex1,
    command = get_plot_hosp_admissions(
      location_to_plot = states_for_vis[1],
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      location_data = location_data,
      plot_name = "hospital_admissions_ex"
    )
  ),
  tar_target(
    name = first_data_fig_ex1,
    command = get_first_data_fig(
      plot_freq = plot_final_obs_clade_freq_ex1,
      plot_seq = plot_seq_count_final_ex1,
      plot_hosp = plot_hosp_admissions_ex1,
      output_fp = file.path(
        "output", "figs",
        "data_figs", "supp"
      ),
      plot_name = "final_seq_data_ex1"
    )
  ),
  # And again for a different state
  tar_target(
    name = plot_final_obs_clade_freq_ex2,
    command = get_plot_obs_clade_freq(
      obs_data = clean_variant_data_final_all_states,
      location = states_for_vis[2],
      date_range = date_range_to_plot,
      temporal_granularity = "weeks",
      plot_name = "final_obs_freq_ex",
      title = states_for_vis[2]
    )
  ),
  tar_target(
    name = plot_seq_count_final_ex2,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_final_all_states,
      location = states_for_vis[2],
      date_range = date_range_to_plot,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_counts_ex"
    )
  ),
  tar_target(
    name = plot_hosp_admissions_ex2,
    command = get_plot_hosp_admissions(
      location_to_plot = states_for_vis[2],
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      location_data = location_data,
      plot_name = "hospital_admissions_ex"
    )
  ),
  tar_target(
    name = first_data_fig_ex2,
    command = get_first_data_fig(
      plot_freq = plot_final_obs_clade_freq_ex2,
      plot_seq = plot_seq_count_final_ex2,
      plot_hosp = plot_hosp_admissions_ex2,
      output_fp = file.path(
        "output", "figs",
        "data_figs", "supp"
      ),
      plot_name = "final_seq_data_ex2"
    )
  ),
  # And again for a different state
  tar_target(
    name = plot_final_obs_clade_freq_ex3,
    command = get_plot_obs_clade_freq(
      obs_data = clean_variant_data_final_all_states,
      location = states_for_vis[3],
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      plot_name = "final_obs_freq_ex",
      title = states_for_vis[3]
    )
  ),
  tar_target(
    name = plot_seq_count_final_ex3,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_final_all_states,
      location = states_for_vis[3],
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      plot_name = "bar_chart_seq_counts_ex"
    )
  ),
  tar_target(
    name = plot_hosp_admissions_ex3,
    command = get_plot_hosp_admissions(
      location_to_plot = states_for_vis[3],
      temporal_granularity = "weeks",
      date_range = date_range_to_plot,
      location_data = location_data,
      plot_name = "hospital_admissions_ex"
    )
  ),
  tar_target(
    name = first_data_fig_ex3,
    command = get_first_data_fig(
      plot_freq = plot_final_obs_clade_freq_ex3,
      plot_seq = plot_seq_count_final_ex3,
      plot_hosp = plot_hosp_admissions_ex3,
      output_fp = file.path(
        "output", "figs",
        "data_figs", "supp"
      ),
      plot_name = "final_seq_data_ex3"
    )
  )
)

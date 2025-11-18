fig_data_as_of_targets <- list(
  # Use the same figure function from the final data
  tar_target(
    name = plot_seq_counts_as_of_ex1,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_as_of_nowcast_date,
      location = states_for_vis[1],
      date_range = date_range_to_plot,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_as_of_ex",
      log_scale = FALSE,
      nowcast_date_line = TRUE,
      title = TRUE
    )
  ),
  # Want to compare what we have as of the nowcast date vs what
  # we later observe
  tar_target(
    name = plot_eval_seq_counts_ex1,
    command = get_bar_chart_comparison(
      obs_data = clean_variant_data_as_of_nowcast_date,
      final_data = clean_variant_data_final_all_states,
      location = states_for_vis[1],
      temporal_granularity = "weeks",
      log_scale = FALSE
    )
  ),
  tar_target(
    name = plot_eval_freq_ex1,
    command = get_plot_freq_as_of_vs_eval(
      obs_data = clean_variant_data_as_of_nowcast_date,
      final_data = clean_variant_data_final_all_states,
      location = states_for_vis[1],
      temporal_granularity = "weeks"
    )
  ),
  tar_target(
    name = plot_seq_counts_as_of_ex2,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_as_of_nowcast_date,
      location = "US",
      date_range = date_range_to_plot,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_as_of_ex",
      log_scale = FALSE,
      nowcast_date_line = TRUE,
      title = TRUE
    )
  ),
  # Want to compare what we have as of the nowcast date vs what
  # we later observe
  tar_target(
    name = plot_eval_seq_counts_ex2,
    command = get_bar_chart_comparison(
      obs_data = clean_variant_data_as_of_nowcast_date,
      final_data = clean_variant_data_final_all_states,
      location = "US",
      temporal_granularity = "weeks",
      log_scale = FALSE
    )
  ),
  tar_target(
    name = plot_eval_freq_ex2,
    command = get_plot_freq_as_of_vs_eval(
      obs_data = clean_variant_data_as_of_nowcast_date,
      final_data = clean_variant_data_final_all_states,
      location = "US",
      temporal_granularity = "weeks"
    )
  ),
  tar_target(
    name = second_data_fig,
    command = get_second_data_fig(
      seq_counts_as_of1 = plot_seq_counts_as_of_ex1,
      seq_counts_eval1 = plot_eval_seq_counts_ex1,
      eval_freq1 = plot_eval_freq_ex1,
      seq_counts_as_of2 = plot_seq_counts_as_of_ex2,
      seq_counts_eval2 = plot_eval_seq_counts_ex2,
      eval_freq2 = plot_eval_freq_ex2,
      plot_name = "data_as_of"
    )
  )
)

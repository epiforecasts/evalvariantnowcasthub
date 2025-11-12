fig_data_as_of_targets <- list(
  # Use the same figure function from the final data
  tar_target(
    name = plot_seq_counts_as_of_ex1,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_as_of_nowcast_date,
      location = states_for_vis[1],
      temporal_granularity = "days",
      plot_name = "bar_chart_seq_as_of_ex",
      log_scale = FALSE,
      nowcast_date_line = TRUE
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
      temporal_granularity = "days",
      plot_name = "bar_chart_seq_comparison",
      log_scale = FALSE
    )
  )
)

fig_data_as_of_targets <- list(
  tar_target(
    name = date_range_as_of,
    command = c(
      min(clean_variant_data_as_of_nowcast_date$date),
      max(clean_variant_data_as_of_nowcast_date$date)
    )
  ),
  # Use the same figure function from the final data
  tar_target(
    name = plot_seq_counts_as_of_ex1,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_as_of_nowcast_date,
      location = states_for_vis[1],
      date_range = date_range_as_of,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_as_of_ex",
      log_scale = FALSE,
      nowcast_date_line = TRUE,
      remove_xticks = TRUE,
      title = "CA"
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
      date_range = date_range_as_of,
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
      date_range = date_range_as_of,
      temporal_granularity = "weeks",
      clades_to_plot = c("24E", "24F", "25A")
    )
  ),
  tar_target(
    name = plot_seq_counts_as_of_usminca,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_as_of_nowcast_date |>
        filter(location != "CA"),
      location = "US",
      date_range = date_range_as_of,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_as_of_ex",
      log_scale = FALSE,
      nowcast_date_line = TRUE,
      title = "US excluding CA",
      remove_xticks = TRUE
    )
  ),
  # Want to compare what we have as of the nowcast date vs what
  # we later observe
  tar_target(
    name = plot_eval_seq_counts_usminca,
    command = get_bar_chart_comparison(
      obs_data = clean_variant_data_as_of_nowcast_date |>
        filter(location != "CA"),
      final_data = clean_variant_data_final_all_states |>
        filter(location != "CA"),
      location = "US",
      temporal_granularity = "weeks",
      date_range = date_range_as_of,
      log_scale = FALSE
    )
  ),
  tar_target(
    name = plot_eval_freq_usminca,
    command = get_plot_freq_as_of_vs_eval(
      obs_data = clean_variant_data_as_of_nowcast_date |>
        filter(location != "CA"),
      final_data = clean_variant_data_final_all_states |>
        filter(location != "CA"),
      date_range = date_range_as_of,
      location = "US",
      temporal_granularity = "weeks",
      clades_to_plot = clades_to_plot
    )
  ),
  tar_target(
    name = second_data_fig,
    command = get_second_data_fig(
      seq_counts_as_of1 = plot_seq_counts_as_of_usminca,
      seq_counts_eval1 = plot_eval_seq_counts_usminca,
      eval_freq1 = plot_eval_freq_usminca,
      seq_counts_as_of2 = plot_seq_counts_as_of_ex1,
      seq_counts_eval2 = plot_eval_seq_counts_ex1,
      eval_freq2 = plot_eval_freq_ex1,
      plot_name = "data_as_of"
    )
  ),
  # All US versions (no CA split)
  tar_target(
    name = plot_seq_counts_as_of_all_us,
    command = get_bar_chart_seq_count(
      obs_data = clean_variant_data_as_of_nowcast_date,
      location = "US",
      date_range = date_range_as_of,
      temporal_granularity = "weeks",
      plot_name = "bar_chart_seq_as_of_all_us",
      log_scale = FALSE,
      nowcast_date_line = TRUE,
      remove_xticks = TRUE,
      title = "US"
    )
  ),
  tar_target(
    name = plot_eval_seq_counts_all_us,
    command = get_bar_chart_comparison(
      obs_data = clean_variant_data_as_of_nowcast_date,
      final_data = clean_variant_data_final_all_states,
      location = "US",
      temporal_granularity = "weeks",
      date_range = date_range_as_of,
      log_scale = FALSE
    )
  ),
  tar_target(
    name = plot_eval_freq_all_us,
    command = get_plot_freq_as_of_vs_eval(
      obs_data = clean_variant_data_as_of_nowcast_date,
      final_data = clean_variant_data_final_all_states,
      date_range = date_range_as_of,
      location = "US",
      temporal_granularity = "weeks",
      clades_to_plot = clades_to_plot
    )
  ),
  tar_target(
    name = data_fig_all_us_vertical,
    command = get_data_fig_all_us(
      seq_counts_as_of = plot_seq_counts_as_of_all_us,
      seq_counts_eval = plot_eval_seq_counts_all_us,
      eval_freq = plot_eval_freq_all_us,
      plot_name = "data_as_of_all_us_vertical"
    )
  ),
  tar_target(
    name = data_fig_all_us_horizontal,
    command = get_data_fig_all_us_horizontal(
      seq_counts_as_of = plot_seq_counts_as_of_all_us,
      seq_counts_eval = plot_eval_seq_counts_all_us,
      eval_freq = plot_eval_freq_all_us,
      plot_name = "data_as_of_all_us_horizontal"
    )
  ),
  tar_target(
    name = data_fig_all_us_triangular,
    command = get_data_fig_all_us_triangular(
      seq_counts_as_of = plot_seq_counts_as_of_all_us,
      seq_counts_eval = plot_eval_seq_counts_all_us,
      eval_freq = plot_eval_freq_all_us,
      plot_name = "data_as_of_all_us_triangular"
    )
  )
)

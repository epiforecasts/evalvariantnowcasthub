fig_pred_plus_data_targets <- list(
  tar_target(
    name = plot_grid_model_preds,
    command = get_plot_model_pred_obs(
      model_pred_obs_df = df_summary,
      eval_seq = clean_variant_data_for_eval,
      clades_to_plot = clades_to_plot
    )
  ),
  tar_target(
    name = plot_seq_count_underlay,
    command = get_plot_seq_count(
      eval_seq = clean_variant_data_for_eval,
      temporal_granularity = "days"
    )
  ),
  tar_target(
    name = plot_scores_underlay,
    command = get_plot_scores_over_time(
      scores = su_scores,
      nowcast_date = nowcast_date_for_vis,
      locs = states_for_vis
    )
  )
)

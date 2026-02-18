fig_pred_plus_data_targets <- list(
  tar_target(
    name = plot_grid_model_preds,
    command = get_plot_model_pred_obs(
      model_pred_obs_df = df_summary,
      eval_seq = clean_variant_data_for_eval,
      clades_to_plot = "25A (LP.8.1)"
    )
  ),
  tar_target(
    name = plot_grid_model_preds_24F,
    command = get_plot_model_pred_obs(
      model_pred_obs_df = df_summary,
      eval_seq = clean_variant_data_for_eval,
      clades_to_plot = "24F (XEC)"
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
  ),
  tar_target(
    name = plot_scores_w_exclusions,
    command = get_plot_scores_w_exclusions(
      scores = su_scores,
      nowcast_date = nowcast_date_for_vis,
      locs = states_for_vis
    )
  ),
  tar_target(
    name = fig_preds,
    command = get_fig_preds(
      faceted_preds = plot_grid_model_preds,
      faceted_preds2 = plot_grid_model_preds_24F,
      seq_count_row = plot_seq_count_underlay,
      scores_row = plot_scores_underlay,
      plot_name = "fig_preds"
    )
  )
)

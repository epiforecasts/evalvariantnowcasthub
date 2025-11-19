fig_pred_plus_data_targets <- list(
  tar_target(
    name = plot_grid_model_preds,
    command = get_plot_model_pred_obs(
      model_pred_obs_df = df_summary,
      eval_seq = clean_variant_data_for_eval,
      clades_to_plot = clades_to_plot
    )
  )
)

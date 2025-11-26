fig_submission_heatmaps_targets <- list(
  # Prepare submission presence/absence data
  tar_target(
    name = submission_presence_data,
    command = prepare_submission_data(
      all_model_outputs_for_heatmap,
      location_data,
      nowcast_dates
    )
  ),
  # Create per-model heatmap figure
  tar_target(
    name = fig_submission_heatmaps_by_model,
    command = create_per_model_heatmap_fig(
      submission_presence_data,
      plot_components()
    )
  ),
  # Create summary heatmap figure
  tar_target(
    name = fig_submission_heatmaps_summary,
    command = create_summary_heatmap_figure(
      submission_presence_data,
      plot_components()
    )
  ),
  # Prepare evaluation sequence count data
  tar_target(
    name = eval_sequence_counts_data,
    command = prepare_eval_sequence_data(clean_variant_data_eval_all)
  ),
  # Create heatmap
  tar_target(
    name = fig_eval_sequence_heatmap,
    command = plot_eval_sequence_heatmap(
      eval_sequence_counts_data,
      plot_components()
    )
  )
)

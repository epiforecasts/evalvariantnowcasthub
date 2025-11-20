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
  # Create the combined heatmap figure
  tar_target(
    name = fig_submission_heatmaps,
    command = create_submission_heatmap_figure(
      submission_presence_data,
      plot_components
    )
  )
)

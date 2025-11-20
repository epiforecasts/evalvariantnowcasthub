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
    command = create_per_model_heatmap_figure(
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
  # Save per-model figure to output/figs/metadata/supp/
  tar_target(
    name = submission_heatmaps_by_model_file,
    command = {
      dir.create("output/figs/metadata/supp", recursive = TRUE, showWarnings = FALSE)
      ggsave(
        filename = "output/figs/metadata/supp/submission_heatmaps_by_model.png",
        plot = fig_submission_heatmaps_by_model,
        width = 16,
        height = 20,
        dpi = 300
      )
      "output/figs/metadata/supp/submission_heatmaps_by_model.png"
    }
  ),
  # Save summary figure to output/figs/metadata/supp/
  tar_target(
    name = submission_heatmaps_summary_file,
    command = {
      dir.create("output/figs/metadata/supp", recursive = TRUE, showWarnings = FALSE)
      ggsave(
        filename = "output/figs/metadata/supp/submission_heatmaps_summary.png",
        plot = fig_submission_heatmaps_summary,
        width = 10,
        height = 8,
        dpi = 300
      )
      "output/figs/metadata/supp/submission_heatmaps_summary.png"
    }
  )
)

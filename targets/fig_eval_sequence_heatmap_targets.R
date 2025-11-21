fig_eval_sequence_heatmap_targets <- list(
  # Prepare evaluation sequence count data
  tar_target(
    name = eval_sequence_counts_data,
    command = prepare_eval_sequence_data(clean_variant_data_for_eval)
  ),
  # Create heatmap
  tar_target(
    name = fig_eval_sequence_heatmap,
    command = plot_eval_sequence_heatmap(
      eval_sequence_counts_data,
      plot_components()
    )
  ),
  # Save figure
  tar_target(
    name = eval_sequence_heatmap_file,
    command = {
      dir.create("output/figs/metadata/supp", recursive = TRUE, showWarnings = FALSE)
      ggsave(
        filename = "output/figs/metadata/supp/eval_sequence_counts_heatmap.png",
        plot = fig_eval_sequence_heatmap,
        width = 10,
        height = 8,
        dpi = 300
      )
      "output/figs/metadata/supp/eval_sequence_counts_heatmap.png"
    }
  )
)

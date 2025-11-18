fig_nowcast_targets <- list(
  tar_target(
    name = faceted_model_outputs,
    command = get_plot_model_outputs(raw_selected_model_outputs)
  )
)

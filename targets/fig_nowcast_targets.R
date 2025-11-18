fig_nowcast_targets <- list(
  # This will contain all of the models and locations, we will need to separate
  # to avoid having empty space
  tar_target(
    name = faceted_model_outputs,
    command = get_plot_model_outputs(
      nowcasts = raw_selected_model_outputs |>
        filter(model_id != "CADPH-CATaLog"),
      data_as_of =
        clean_variant_data_as_of_nowcast_date
    )
  ),
  # Sequence counts by location
  tar_target(
    name = bar_chart_seq_counts_as_of,
    command = get_faceted_seq_as_of(
      obs_data = clean_variant_data_as_of_nowcast_date,
      log_scale = FALSE,
      remove_legend = FALSE
    )
  ),
  tar_target(
    name = fig_nowcasts_outputs,
    command = get_fig_nowcasts(
      faceted_nowcasts = faceted_model_outputs,
      row_seq_counts = bar_chart_seq_counts_as_of,
      plot_name = "fig_model_nowcasts"
    )
  ),
  # Make the additional model for CA
  tar_target(
    name = model_output_add_mod,
    command = get_plot_model_outputs(
      nowcasts = raw_selected_model_outputs |>
        filter(
          model_id == "CADPH-CATaLog",
          location == "CA"
        ),
      data_as_of =
        clean_variant_data_as_of_nowcast_date |>
          filter(location == "CA")
    )
  ),
  # Sequence counts by location
  tar_target(
    name = bar_chart_as_of_add_mod,
    command = get_faceted_seq_as_of(
      obs_data = clean_variant_data_as_of_nowcast_date |>
        filter(location == "CA"),
      log_scale = FALSE,
      remove_legend = FALSE,
    )
  ),
  tar_target(
    name = fig_nowcasts_outputs_CADPH,
    command = get_fig_nowcasts_supp(
      faceted_nowcasts = model_output_add_mod,
      row_seq_counts = bar_chart_as_of_add_mod,
      plot_name = "fig_model_nowcasts_CADPH"
    )
  )
)

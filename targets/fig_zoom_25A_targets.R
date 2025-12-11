fig_zoom_25A_targets <- list(
  tar_target(
    name = plot_model_preds_mult_nowcasts,
    command = get_plot_model_preds_mult(
      model_preds_mult_nowcasts = df_mult_nowcasts,
      final_eval_data = clean_variant_data_final_all_states,
      clade_to_zoom = "25A"
    )
  ),
  tar_target(
    name = plot_score_underlay,
    command = get_plot_scores_by_date(
      scores = su_scores,
      locs = states_for_vis,
      nowcast_dates = nowcast_date_range_to_zoom,
      date_range = c(
        min(nowcast_date_range_to_zoom) - days(6),
        max(nowcast_date_range_to_zoom)
      )
    )
  ),
  tar_target(
    name = forecast_obj_25A_prepared,
    command = prepare_data_for_scoring(
      df_mult_nowcasts = df_mult_nowcasts,
      clade = "25A",
      horizon_range = c(-6, 0)
    )
  ),
  tar_target(
    name = bias_25A_scores,
    command = compute_bias(
      df_prepared = forecast_obj_25A_prepared,
      locs = states_for_vis,
      nowcast_dates = nowcast_date_range_to_zoom
    )
  ),
  tar_target(
    name = plot_bias_by_date,
    command = get_plot_bias_by_date(
      bias_data = bias_25A_scores,
      locs = states_for_vis,
      nowcast_dates = nowcast_date_range_to_zoom,
      date_range = c(
        min(nowcast_date_range_to_zoom) - days(6),
        max(nowcast_date_range_to_zoom)
      )
    )
  ),
  tar_target(
    name = coverage_25A_scores,
    command = compute_coverage(
      df_prepared = forecast_obj_25A_prepared,
      locs = states_for_vis,
      nowcast_dates = nowcast_date_range_to_zoom
    )
  ),
  tar_target(
    name = plot_coverage_overall,
    command = get_plot_coverage_overall(
      coverage = coverage_25A_scores,
      locs = states_for_vis
    )
  ),
  tar_target(
    name = fig_zoom_25A,
    command = get_fig_zoom_25A(
      grid = plot_model_preds_mult_nowcasts,
      scores = plot_score_underlay,
      coverage = plot_coverage_overall,
      plot_name = "fig_zoom_25A"
    )
  )
)

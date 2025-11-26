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
    name = fig_zoom_25A,
    command = get_fig_zoom_25A(
      plot_model_preds_mult_nowcasts,
      plot_score_underlay,
      plot_name = "fig_zoom_25A"
    )
  )
)

# Make the exact same set of score analysis figures but without excluding 
# partial observations 
fig_overall_targets_no_exclusions <- list(
  # Separate CA scores and other scores
  tar_target(
    name = su_scores_ne_ca,
    command = su_scores |>
      filter(location == "CA",
             !is.na(brier_score))
  ),
  tar_target(
    name = su_scores_all_ne,
    command = su_scores |>
      filter(!is.na(brier_score))
  ),
  tar_target(
    name = su_scores_ne,
    command = su_scores |>
      filter(location != "CA",
             !is.na(brier_score))
  ),
  # Overall score figures----------------------------------------------
  # Energy/Brier score on relative/absolute scores overall
  ## US----------------------------
  tar_target(
    name = bar_chart_brier_overall_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      title = "US excluding CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_overall_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne,
      rel_skill_plot = TRUE,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = bar_chart_energy_overall_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = rel_skill_energy_overall_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  # Energy/Brier score by horizon
  tar_target(
    name = absolute_brier_horizon_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      title = "US excluding CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_horizon_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne,
      rel_skill_plot = TRUE,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = absolute_energy_horizon_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = rel_skill_energy_horizon_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  ## CA-----------------------------------------
  tar_target(
    name = bar_chart_brier_overall_ca_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      title = "CA",
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = rel_skill_brier_overall_ca_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = TRUE,
      score_type = "brier_score",
      remove_legend = FALSE
    )
  ),
  tar_target(
    name = bar_chart_energy_overall_ca_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = FALSE,
      score_type = "energy_score",
      remove_legend = TRUE
    )
  ),
  tar_target(
    name = rel_skill_energy_overall_ca_ne,
    command = get_plot_overall(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  # Energy/Brier score by horizon
  tar_target(
    name = absolute_brier_horizon_ca_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      show_legend = TRUE,
      title = "CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_horizon_ca_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = TRUE,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = absolute_energy_horizon_ca_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = FALSE,
      score_type = "energy_score",
    )
  ),
  tar_target(
    name = rel_skill_energy_horizon_ca_ne,
    command = get_plot_horizon(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  ## Overall
  tar_target(
    name = overall_scores_fig_ne,
    command = get_overall_scores_figure(
      a = bar_chart_brier_overall_ne,
      b = rel_skill_brier_overall_ne,
      c = bar_chart_brier_overall_ca_ne,
      d = rel_skill_brier_overall_ca_ne,
      e = bar_chart_energy_overall_ne,
      f = rel_skill_energy_overall_ne,
      g = bar_chart_energy_overall_ca_ne,
      h = rel_skill_energy_overall_ca_ne,
      i = rel_skill_brier_horizon_ne,
      j = rel_skill_brier_horizon_ca_ne,
      k = rel_skill_energy_horizon_ne,
      l = rel_skill_energy_horizon_ca_ne,
      plot_name = "overall_score_comparison_ne"
    )
  ),

  # Supplement absolute score by horizon
  tar_target(
    name = abs_horizon_fig_ne,
    command = get_panel_horizon(
      a = absolute_brier_horizon_ne,
      b = absolute_energy_horizon_ne,
      c = absolute_brier_horizon_ca_ne,
      d = absolute_energy_horizon_ca_ne,
      plot_name = "absolute_scores_by_horizon_ne"
    )
  ),

  # By location -------------------------------
  tar_target(
    name = bar_chart_brier_location_ne,
    command = get_plot_by_location(
      scores_obj = su_scores_all_ne,
      seq_counts_by_loc = seq_counts_by_loc,
      score_type = "brier_score",
      rel_skill_plot = FALSE,
      remove_legend = FALSE
    )
  ),
  tar_target(
    name = rel_skill_brier_location_ne,
    command = get_plot_by_location(
      scores_obj = su_scores_all_ne,
      seq_counts_by_loc = seq_counts_by_loc,
      score_type = "brier_score",
      rel_skill_plot = TRUE,
      remove_legend = FALSE
    )
  ),
  tar_target(
    name = rel_skill_energy_location_ne,
    command = get_plot_by_location(
      scores_obj = su_scores_all_ne,
      seq_counts_by_loc = seq_counts_by_loc,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = bar_chart_energy_location_ne,
    command = get_plot_by_location(
      scores_obj = su_scores_all_ne,
      seq_counts_by_loc = seq_counts_by_loc,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = by_loc_figure_ne,
    command = get_by_loc_figure(
      a = bar_chart_brier_location_ne,
      b = rel_skill_brier_location_ne,
      c = bar_chart_energy_location_ne,
      d = rel_skill_energy_location_ne,
      e = plot_seq_counts_by_loc,
      plot_name = "by_location"
    )
  ),

  # By nowcast date ------------------------------------------------
  ## US ---------------------------------------------
  tar_target(
    name = abs_brier_nowcast_date_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne,
      score_type = "brier_score",
      rel_skill_plot = FALSE,
      title = "US excluding CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_nowcast_date_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne,
      score_type = "brier_score",
      rel_skill_plot = TRUE
    )
  ),
  tar_target(
    name = rel_skill_energy_nowcast_date_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = abs_energy_nowcast_date_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
 
  ## CA --------------------------------------------------

  tar_target(
    name = abs_brier_nowcast_date_ca_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne_ca,
      score_type = "brier_score",
      rel_skill_plot = FALSE,
      remove_legend = FALSE,
      title = "CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_nowcast_date_ca_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne_ca,
      score_type = "brier_score",
      rel_skill_plot = TRUE
    )
  ),
  tar_target(
    name = rel_skill_energy_nowcast_date_ca_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = abs_energy_nowcast_date_ca_ne,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ne_ca,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = by_nowcast_date_fig_ne,
    command = get_scores_by_nowcast_date(
      a = abs_brier_nowcast_date_ne,
      b = abs_brier_nowcast_date_ca_ne,
      c = rel_skill_brier_nowcast_date_ne,
      d = rel_skill_brier_nowcast_date_ca_ne,
      e = abs_energy_nowcast_date_ne,
      f = abs_energy_nowcast_date_ca_ne,
      g = rel_skill_energy_nowcast_date_ne,
      h = rel_skill_energy_nowcast_date_ca_ne,
      i = plot_seq_counts_by_date_us,
      j = plot_seq_counts_by_date_ca,
      k = plot_seq_by_eval_date_us,
      l = plot_seq_by_eval_date_ca,
      plot_name = "by_nowcast_date_ne"
    )
  )

)

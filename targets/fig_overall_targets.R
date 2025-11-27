fig_overall_targets <- list(
  # Separate CA scores and other scores
  tar_target(
    name = su_scores_ca,
    command = su_scores_all |>
      filter(location == "CA")
  ),
  tar_target(
    name = su_scores_ep,
    command = su_scores_all |>
      filter(location != "CA")
  ),
  # Overall score figures----------------------------------------------
  # Energy/Brier score on relative/absolute scores overall
  ## US----------------------------
  tar_target(
    name = bar_chart_brier_overall,
    command = get_plot_overall(
      scores_obj = su_scores_ep,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      title = "US minus CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_overall,
    command = get_plot_overall(
      scores_obj = su_scores_ep,
      rel_skill_plot = TRUE,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = bar_chart_energy_overall,
    command = get_plot_overall(
      scores_obj = su_scores_ep,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = rel_skill_energy_overall,
    command = get_plot_overall(
      scores_obj = su_scores_ep,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  # Energy/Brier score by horizon
  tar_target(
    name = absolute_brier_horizon,
    command = get_plot_horizon(
      scores_obj = su_scores_ep,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      title = "US minus CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_horizon,
    command = get_plot_horizon(
      scores_obj = su_scores_ep,
      rel_skill_plot = TRUE,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = absolute_energy_horizon,
    command = get_plot_horizon(
      scores_obj = su_scores_ep,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = rel_skill_energy_horizon,
    command = get_plot_horizon(
      scores_obj = su_scores_ep,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  ## CA-----------------------------------------
  tar_target(
    name = bar_chart_brier_overall_ca,
    command = get_plot_overall(
      scores_obj = su_scores_ca,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      title = "CA",
      remove_legend = FALSE
    )
  ),
  tar_target(
    name = rel_skill_brier_overall_ca,
    command = get_plot_overall(
      scores_obj = su_scores_ca,
      rel_skill_plot = TRUE,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = bar_chart_energy_overall_ca,
    command = get_plot_overall(
      scores_obj = su_scores_ca,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = rel_skill_energy_overall_ca,
    command = get_plot_overall(
      scores_obj = su_scores_ca,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  # Energy/Brier score by horizon
  tar_target(
    name = absolute_brier_horizon_ca,
    command = get_plot_horizon(
      scores_obj = su_scores_ca,
      rel_skill_plot = FALSE,
      score_type = "brier_score",
      show_legend = TRUE,
      title = "CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_horizon_ca,
    command = get_plot_horizon(
      scores_obj = su_scores_ca,
      rel_skill_plot = TRUE,
      score_type = "brier_score"
    )
  ),
  tar_target(
    name = absolute_energy_horizon_ca,
    command = get_plot_horizon(
      scores_obj = su_scores_ca,
      rel_skill_plot = FALSE,
      score_type = "energy_score",
    )
  ),
  tar_target(
    name = rel_skill_energy_horizon_ca,
    command = get_plot_horizon(
      scores_obj = su_scores_ca,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  ## Overall
  tar_target(
    name = overall_scores_fig,
    command = get_overall_scores_figure(
      a = bar_chart_brier_overall,
      b = rel_skill_brier_overall,
      c = bar_chart_brier_overall_ca,
      d = rel_skill_brier_overall_ca,
      e = bar_chart_energy_overall,
      f = rel_skill_energy_overall,
      g = bar_chart_energy_overall_ca,
      h = rel_skill_energy_overall_ca,
      i = rel_skill_brier_horizon,
      j = rel_skill_brier_horizon_ca,
      k = rel_skill_energy_horizon,
      l = rel_skill_energy_horizon_ca,
      plot_name = "overall_score_comparison"
    )
  ),

  # Supplement absolute score by horizon
  tar_target(
    name = abs_horizon_fig,
    command = get_panel_horizon(
      a = absolute_brier_horizon,
      b = absolute_energy_horizon,
      c = absolute_brier_horizon_ca,
      d = absolute_energy_horizon_ca,
      plot_name = "absolute_scores_by_horizon"
    )
  ),

  # By location -------------------------------
  tar_target(
    name = bar_chart_brier_location,
    command = get_plot_by_location(
      scores_obj = su_scores_all,
      seq_counts_by_loc = seq_counts_by_loc,
      score_type = "brier_score",
      rel_skill_plot = FALSE,
      remove_legend = FALSE
    )
  ),
  tar_target(
    name = rel_skill_brier_location,
    command = get_plot_by_location(
      scores_obj = su_scores_all,
      seq_counts_by_loc = seq_counts_by_loc,
      score_type = "brier_score",
      rel_skill_plot = TRUE
    )
  ),
  tar_target(
    name = rel_skill_energy_location,
    command = get_plot_by_location(
      scores_obj = su_scores_all,
      seq_counts_by_loc = seq_counts_by_loc,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = bar_chart_energy_location,
    command = get_plot_by_location(
      scores_obj = su_scores_all,
      seq_counts_by_loc = seq_counts_by_loc,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  # Sequence counts by location
  tar_target(
    name = plot_seq_counts_by_loc,
    command = get_plot_seq_counts_loc(seq_counts_by_loc)
  ),
  tar_target(
    name = by_loc_figure,
    command = get_by_loc_figure(
      a = bar_chart_brier_location,
      b = rel_skill_brier_location,
      c = bar_chart_energy_location,
      d = rel_skill_energy_location,
      e = plot_seq_counts_by_loc,
      plot_name = "by_location"
    )
  ),

  # By nowcast date ------------------------------------------------
  ## US ---------------------------------------------
  tar_target(
    name = abs_brier_nowcast_date,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ep,
      score_type = "brier_score",
      rel_skill_plot = FALSE,
      title = "US minus CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_nowcast_date,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ep,
      score_type = "brier_score",
      rel_skill_plot = TRUE
    )
  ),
  tar_target(
    name = rel_skill_energy_nowcast_date,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ep,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = abs_energy_nowcast_date,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ep,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  # Sequence counts by nowcast_date
  tar_target(
    name = plot_seq_counts_by_date_us,
    command = get_plot_seq_counts_date(seq_counts_by_date_us)
  ),
  ## CA --------------------------------------------------
  tar_target(
    name = plot_seq_counts_by_date_ca,
    command = get_plot_seq_counts_date(seq_counts_by_date_ca)
  ),
  tar_target(
    name = abs_brier_nowcast_date_ca,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ca,
      score_type = "brier_score",
      rel_skill_plot = FALSE,
      remove_legend = FALSE,
      title = "CA"
    )
  ),
  tar_target(
    name = rel_skill_brier_nowcast_date_ca,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ca,
      score_type = "brier_score",
      rel_skill_plot = TRUE
    )
  ),
  tar_target(
    name = rel_skill_energy_nowcast_date_ca,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ca,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = abs_energy_nowcast_date_ca,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ca,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = by_nowcast_date_fig,
    command = get_scores_by_nowcast_date(
      a = abs_brier_nowcast_date,
      b = abs_brier_nowcast_date_ca,
      c = rel_skill_brier_nowcast_date,
      d = rel_skill_brier_nowcast_date_ca,
      e = abs_energy_nowcast_date,
      f = abs_energy_nowcast_date_ca,
      g = rel_skill_energy_nowcast_date,
      h = rel_skill_energy_nowcast_date_ca,
      i = plot_seq_counts_by_date_us,
      j = plot_seq_counts_by_date_ca,
      plot_name = "by_nowcast_date"
    )
  )
)

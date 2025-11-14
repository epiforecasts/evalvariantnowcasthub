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
      score_type = "brier_score"
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
      score_type = "brier_score"
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
      score_type = "energy_score"
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
      i = absolute_brier_horizon,
      j = absolute_brier_horizon_ca,
      k = absolute_energy_horizon,
      l = absolute_energy_horizon_ca,
      plot_name = "overall_score_comparison"
    )
  ),


  # Brier and energy by location -------------------------------
  tar_target(
    name = bar_chart_brier_location,
    command = get_plot_by_location(
      scores_obj = su_scores_ep,
      seq_counts_by_loc = seq_counts_by_loc,
      score_type = "brier_score",
      rel_skill_plot = FALSE
    )
  ),
  tar_target(
    name = rel_skill_brier_location,
    command = get_plot_by_location(
      scores_obj = su_scores_ep,
      seq_counts_by_loc = seq_counts_by_loc,
      score_type = "brier_score",
      rel_skill_plot = TRUE
    )
  ),
  tar_target(
    name = rel_skill_energy_location,
    command = get_plot_by_location(
      scores_obj = su_scores_ep,
      seq_counts_by_loc = seq_counts_by_loc,
      rel_skill_plot = TRUE,
      score_type = "energy_score"
    )
  ),
  tar_target(
    name = bar_chart_energy_location,
    command = get_plot_by_location(
      scores_obj = su_scores_ep,
      seq_counts_by_loc = seq_counts_by_loc,
      rel_skill_plot = FALSE,
      score_type = "energy_score"
    )
  ),
  # Brier and energy by nowcast date
  tar_target(
    name = abs_brier_nowcast_date,
    command = get_plot_by_nowcast_date(
      scores_obj = su_scores_ep,
      score_type = "brier_score",
      rel_skill_plot = FALSE
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
  # Sequence counts by location
  tar_target(
    name = plot_seq_counts_by_loc,
    command = get_plot_seq_counts_loc(seq_counts_by_loc)
  ),
  # Sequence counts by nowcast_date
  tar_target(
    name = plot_seq_counts_by_date_us,
    command = get_plot_seq_counts_date(seq_counts_by_date_us)
  ),
  tar_target(
    name = plot_seq_counts_by_date_ca,
    command = get_plot_seq_counts_date(seq_counts_by_date_ca)
  )



  # # Chart F: Weekly sequence averages by location
  # tar_target(
  #   name = chart_f_weekly_seq,
  #   command = get_chart_f_weekly_seq_by_location(
  #     variant_data = clean_variant_data_final_all_states
  #   )
  # ),
  #
  # # Chart G: Time-series skill trends for all US regions
  # tar_target(
  #   name = chart_g_timeseries_all,
  #   command = get_chart_g_timeseries_all_regions(
  #     scores_obj = su_scores_excl_partial,
  #     score_type = "brier_score"
  #   )
  # ),
  #
  # # Chart H: Time-series skill trends for California
  # tar_target(
  #   name = chart_h_timeseries_ca,
  #   command = get_chart_h_timeseries_california(
  #     scores_obj = su_scores_excl_partial,
  #     score_type = "brier_score",
  #     location = "CA"
  #   )
  # ),
  #
  # # Chart I: Average scores over time
  # tar_target(
  #   name = chart_i_avg_scores,
  #   command = get_chart_i_avg_scores_over_time(
  #     scores_obj = su_scores_excl_partial
  #   )
  # ),
  #
  # # Combined figure with all 9 charts
  # tar_target(
  #   name = fig_overall_scores_combined,
  #   command = get_overall_scores_figure(
  #     chart_a = chart_a_brier_location,
  #     chart_b = chart_b_energy_location,
  #     chart_c = chart_c_brier_date,
  #     chart_d = chart_d_energy_date,
  #     chart_e = chart_e_skill_location,
  #     chart_f = chart_f_weekly_seq,
  #     chart_g = chart_g_timeseries_all,
  #     chart_h = chart_h_timeseries_ca,
  #     chart_i = chart_i_avg_scores,
  #     plot_name = "overall_scores_summary"
  #   )
  # )
)

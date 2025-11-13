fig_overall_targets <- list(
  # Chart A: Brier score skill by location (half-width panel)
  tar_target(
    name = chart_a_brier_location,
    command = get_chart_a_brier_by_location(
      scores_obj = su_scores_excl_partial
    )
  ),

  # Chart B: Energy score skill by location (half-width panel)
  tar_target(
    name = chart_b_energy_location,
    command = get_chart_b_energy_by_location(
      scores_obj = su_scores_excl_partial
    )
  ),

  # Chart C: Brier score skill by nowcast date (half-width panel)
  tar_target(
    name = chart_c_brier_date,
    command = get_chart_c_brier_by_date(
      scores_obj = su_scores_excl_partial
    )
  ),

  # Chart D: Energy score skill by nowcast date (half-width panel)
  tar_target(
    name = chart_d_energy_date,
    command = get_chart_d_energy_by_date(
      scores_obj = su_scores_excl_partial
    )
  ),

  # Chart E: Overall skill by location with model color-coding
  tar_target(
    name = chart_e_skill_location,
    command = get_chart_e_skill_by_location(
      scores_obj = su_scores_excl_partial,
      score_type = "brier_score"
    )
  ),

  # Chart F: Weekly sequence averages by location
  tar_target(
    name = chart_f_weekly_seq,
    command = get_chart_f_weekly_seq_by_location(
      variant_data = clean_variant_data_final_all_states
    )
  ),

  # Chart G: Time-series skill trends for all US regions
  tar_target(
    name = chart_g_timeseries_all,
    command = get_chart_g_timeseries_all_regions(
      scores_obj = su_scores_excl_partial,
      score_type = "brier_score"
    )
  ),

  # Chart H: Time-series skill trends for California
  tar_target(
    name = chart_h_timeseries_ca,
    command = get_chart_h_timeseries_california(
      scores_obj = su_scores_excl_partial,
      score_type = "brier_score",
      location = "CA"
    )
  ),

  # Chart I: Average scores over time
  tar_target(
    name = chart_i_avg_scores,
    command = get_chart_i_avg_scores_over_time(
      scores_obj = su_scores_excl_partial
    )
  ),

  # Combined figure with all 9 charts
  tar_target(
    name = fig_overall_scores_combined,
    command = get_overall_scores_figure(
      chart_a = chart_a_brier_location,
      chart_b = chart_b_energy_location,
      chart_c = chart_c_brier_date,
      chart_d = chart_d_energy_date,
      chart_e = chart_e_skill_location,
      chart_f = chart_f_weekly_seq,
      chart_g = chart_g_timeseries_all,
      chart_h = chart_h_timeseries_ca,
      chart_i = chart_i_avg_scores,
      plot_name = "overall_scores_summary"
    )
  )
)

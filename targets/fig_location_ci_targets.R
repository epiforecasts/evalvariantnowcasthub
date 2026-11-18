# By-location figures with bootstrap confidence bands (#138)-------------------
# Same panels as by_location and by_location_smaller (fig_overall_targets.R),
# with Bonferroni simultaneous bands from bootstrap_location_targets.R added
# as error bars. Absolute score error bars are dodged to match the bars
# (width 0.9); relative skill points and their (model-coloured) error bars
# are dodged side by side (width 0.7) so each model's band is readable.
# Rows without a band (fewer than min_dates_ci nowcast dates) keep their bar
# or point without an error bar. Absolute score bands are drawn truncated at
# 0 (a mean score cannot be negative; a band reaching 0 means the mean is
# poorly determined); the CSV keeps the untruncated values.
fig_location_ci_targets <- list(
  # Relative skill axes: symmetric about 1 on the log scale and just wide
  # enough for each panel's estimates and bands (as for the overall CI
  # figure)
  tar_target(
    name = loc_srss_ylim_brier,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_brier_loc),
        get_panel_srss_values(rel_skill_brier_location)
      )
    )
  ),
  tar_target(
    name = loc_srss_ylim_energy,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_energy_loc),
        get_panel_srss_values(rel_skill_energy_location)
      )
    )
  ),
  tar_target(
    name = loc_srss_dodge_width,
    command = 0.7
  ),
  tar_target(
    name = loc_srss_point_size,
    command = 2
  ),
  tar_target(
    name = by_loc_ci_fig,
    command = get_by_loc_figure(
      a = add_ci_errorbars_by_location(
        bar_chart_brier_location, ci_abs_brier_loc,
        dodge_width = 0.9, floor = 0
      ),
      b = add_ci_errorbars_by_location(
        rel_skill_brier_location, ci_srss_brier_loc,
        dodge_width = loc_srss_dodge_width, dodge_points = TRUE,
        point_size = loc_srss_point_size,
        ylim = loc_srss_ylim_brier,
        y_breaks = get_log_breaks(loc_srss_ylim_brier)
      ),
      c = add_ci_errorbars_by_location(
        bar_chart_energy_location, ci_abs_energy_loc,
        dodge_width = 0.9, floor = 0
      ),
      d = add_ci_errorbars_by_location(
        rel_skill_energy_location, ci_srss_energy_loc,
        dodge_width = loc_srss_dodge_width, dodge_points = TRUE,
        point_size = loc_srss_point_size,
        ylim = loc_srss_ylim_energy,
        y_breaks = get_log_breaks(loc_srss_ylim_energy)
      ),
      e = plot_seq_counts_by_loc,
      plot_name = "by_location_ci"
    )
  ),
  tar_target(
    name = by_loc_smaller_ci_fig,
    command = get_by_loc_figure_smaller(
      a = add_ci_errorbars_by_location(
        rel_skill_brier_location, ci_srss_brier_loc,
        dodge_width = loc_srss_dodge_width, dodge_points = TRUE,
        point_size = loc_srss_point_size,
        ylim = loc_srss_ylim_brier,
        y_breaks = get_log_breaks(loc_srss_ylim_brier)
      ),
      b = add_ci_errorbars_by_location(
        rel_skill_energy_location, ci_srss_energy_loc,
        dodge_width = loc_srss_dodge_width, dodge_points = TRUE,
        point_size = loc_srss_point_size,
        ylim = loc_srss_ylim_energy,
        y_breaks = get_log_breaks(loc_srss_ylim_energy)
      ),
      c = plot_seq_counts_by_loc,
      plot_name = "by_location_smaller_ci"
    )
  )
)

# Overall score figure with bootstrap confidence bands------------------------
# Same panels as overall_score_comparison (fig_overall_targets.R), with
# Bonferroni simultaneous bands from bootstrap_targets.R added: error bars
# on the absolute score bars and SRSS points (A-H) and ribbons on the SRSS
# by horizon lines (I-L). Each relative skill panel has its own y axis,
# just wide enough for its estimates and bands. SRSS markers are smaller
# than in the original so narrow bands stay visible.
fig_overall_ci_targets <- list(
  # Per-panel axis limits for the relative skill panels (B, D, F, H, I-L),
  # symmetric about 1 on the log scale and just wide enough for the panel's
  # estimates and bands. Shared with the main overall score figure
  # (overall_scores_fig) so the two figures use the same axes.
  tar_target(
    name = ylim_srss_brier_ep,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_brier_ep),
        get_panel_srss_values(rel_skill_brier_overall)
      )
    )
  ),
  tar_target(
    name = ylim_srss_brier_ca,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_brier_ca),
        get_panel_srss_values(rel_skill_brier_overall_ca)
      )
    )
  ),
  tar_target(
    name = ylim_srss_energy_ep,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_energy_ep),
        get_panel_srss_values(rel_skill_energy_overall)
      )
    )
  ),
  tar_target(
    name = ylim_srss_energy_ca,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_energy_ca),
        get_panel_srss_values(rel_skill_energy_overall_ca)
      )
    )
  ),
  tar_target(
    name = ylim_srss_horizon_brier_ep,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_horizon_brier_ep),
        get_panel_srss_values(rel_skill_brier_horizon)
      )
    )
  ),
  tar_target(
    name = ylim_srss_horizon_brier_ca,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_horizon_brier_ca),
        get_panel_srss_values(rel_skill_brier_horizon_ca)
      )
    )
  ),
  tar_target(
    name = ylim_srss_horizon_energy_ep,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_horizon_energy_ep),
        get_panel_srss_values(rel_skill_energy_horizon)
      )
    )
  ),
  tar_target(
    name = ylim_srss_horizon_energy_ca,
    command = get_symmetric_log_limits(
      c(
        get_ci_values(ci_srss_horizon_energy_ca),
        get_panel_srss_values(rel_skill_energy_horizon_ca)
      )
    )
  ),
  tar_target(
    name = srss_point_size,
    command = 3
  ),
  tar_target(
    name = overall_scores_ci_fig,
    command = get_overall_scores_figure(
      a = add_ci_errorbars(bar_chart_brier_overall, ci_abs_brier_ep),
      b = add_ci_errorbars(
        rel_skill_brier_overall, ci_srss_brier_ep,
        ylim = ylim_srss_brier_ep,
        y_breaks = get_log_breaks(ylim_srss_brier_ep),
        point_size = srss_point_size
      ),
      c = add_ci_errorbars(bar_chart_brier_overall_ca, ci_abs_brier_ca),
      d = add_ci_errorbars(
        rel_skill_brier_overall_ca, ci_srss_brier_ca,
        ylim = ylim_srss_brier_ca,
        y_breaks = get_log_breaks(ylim_srss_brier_ca),
        point_size = srss_point_size
      ),
      e = add_ci_errorbars(bar_chart_energy_overall, ci_abs_energy_ep),
      f = add_ci_errorbars(
        rel_skill_energy_overall, ci_srss_energy_ep,
        ylim = ylim_srss_energy_ep,
        y_breaks = get_log_breaks(ylim_srss_energy_ep),
        point_size = srss_point_size
      ),
      g = add_ci_errorbars(bar_chart_energy_overall_ca, ci_abs_energy_ca),
      h = add_ci_errorbars(
        rel_skill_energy_overall_ca, ci_srss_energy_ca,
        ylim = ylim_srss_energy_ca,
        y_breaks = get_log_breaks(ylim_srss_energy_ca),
        point_size = srss_point_size
      ),
      i = add_ci_ribbons(
        rel_skill_brier_horizon, ci_srss_horizon_brier_ep,
        ylim = ylim_srss_horizon_brier_ep,
        y_breaks = get_log_breaks(ylim_srss_horizon_brier_ep)
      ),
      j = add_ci_ribbons(
        rel_skill_brier_horizon_ca, ci_srss_horizon_brier_ca,
        ylim = ylim_srss_horizon_brier_ca,
        y_breaks = get_log_breaks(ylim_srss_horizon_brier_ca)
      ),
      k = add_ci_ribbons(
        rel_skill_energy_horizon, ci_srss_horizon_energy_ep,
        ylim = ylim_srss_horizon_energy_ep,
        y_breaks = get_log_breaks(ylim_srss_horizon_energy_ep)
      ),
      l = add_ci_ribbons(
        rel_skill_energy_horizon_ca, ci_srss_horizon_energy_ca,
        ylim = ylim_srss_horizon_energy_ca,
        y_breaks = get_log_breaks(ylim_srss_horizon_energy_ca)
      ),
      plot_name = "overall_score_comparison_ci"
    )
  )
)

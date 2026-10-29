# Overall score figure with bootstrap confidence bands------------------------
# Same panels as overall_score_comparison (fig_overall_targets.R), with
# Bonferroni simultaneous bands from bootstrap_targets.R added: error bars
# on the absolute score bars and SRSS points (A-H) and ribbons on the SRSS
# by horizon lines (I-L). The SRSS and horizon y axes are widened to show
# the full bands, using the same limits within each row.
fig_overall_ci_targets <- list(
  tar_target(
    name = srss_ylim,
    command = c(1 / 1.7, 1.7)
  ),
  tar_target(
    name = srss_breaks,
    command = c(0.6, 0.8, 1.0, 1.25, 1.5)
  ),
  tar_target(
    name = horizon_ylim,
    command = c(1 / 2.8, 2.8)
  ),
  tar_target(
    name = horizon_breaks,
    command = c(0.5, 0.67, 1.0, 1.5, 2.0, 2.5)
  ),
  tar_target(
    name = overall_scores_ci_fig,
    command = get_overall_scores_figure(
      a = add_ci_errorbars(bar_chart_brier_overall, ci_abs_brier_ep),
      b = add_ci_errorbars(
        rel_skill_brier_overall, ci_srss_brier_ep,
        ylim = srss_ylim, y_breaks = srss_breaks
      ),
      c = add_ci_errorbars(bar_chart_brier_overall_ca, ci_abs_brier_ca),
      d = add_ci_errorbars(
        rel_skill_brier_overall_ca, ci_srss_brier_ca,
        ylim = srss_ylim, y_breaks = srss_breaks
      ),
      e = add_ci_errorbars(bar_chart_energy_overall, ci_abs_energy_ep),
      f = add_ci_errorbars(
        rel_skill_energy_overall, ci_srss_energy_ep,
        ylim = srss_ylim, y_breaks = srss_breaks
      ),
      g = add_ci_errorbars(bar_chart_energy_overall_ca, ci_abs_energy_ca),
      h = add_ci_errorbars(
        rel_skill_energy_overall_ca, ci_srss_energy_ca,
        ylim = srss_ylim, y_breaks = srss_breaks
      ),
      i = add_ci_ribbons(
        rel_skill_brier_horizon, ci_srss_horizon_brier_ep,
        ylim = horizon_ylim, y_breaks = horizon_breaks
      ),
      j = add_ci_ribbons(
        rel_skill_brier_horizon_ca, ci_srss_horizon_brier_ca,
        ylim = horizon_ylim, y_breaks = horizon_breaks
      ),
      k = add_ci_ribbons(
        rel_skill_energy_horizon, ci_srss_horizon_energy_ep,
        ylim = horizon_ylim, y_breaks = horizon_breaks
      ),
      l = add_ci_ribbons(
        rel_skill_energy_horizon_ca, ci_srss_horizon_energy_ca,
        ylim = horizon_ylim, y_breaks = horizon_breaks
      ),
      plot_name = "overall_score_comparison_ci"
    )
  )
)

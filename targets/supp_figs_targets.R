supp_figs_targets <- list(
  tar_target(
    name = clade_comp_weekly,
    command = read_csv(fp_ca_clades)
  ),
  tar_target(
    name = volume_comp_weekly,
    command = read_csv(fp_ca_volume)
  ),
  tar_target(
    name = clade_prop_comp_weekly_plot,
    command = get_bar_chart_clade_comp(clade_comp_weekly)
  ),
  tar_target(
    name = volume_prop_comp_weekly_plot,
    command = get_volume_prop_comp_weekly(volume_comp_weekly)
  ),
  # Proportion of days that were excluded due to partial observations
  # at time of nowcast
  tar_target(
    name = prop_excluded,
    command = su_scores |>
      filter(model == "Hub-baseline") |>
      mutate(
        n_total = sum(!is.na(scored))
      ) |>
      group_by(scored) |>
      summarise(
        n_scored = n(),
        prop_scored = n_scored / max(n_total)
      ) |>
      filter(scored == TRUE) |>
      pull(prop_scored)
  ),
  tar_target(
    name = plot_overall_prop_excl_by_horizon,
    command = get_plot_overall_prop_excl_by_horizon(su_scores)
  )
)

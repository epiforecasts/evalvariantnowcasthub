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
  )
)

config_targets <- list(
  # Vector of nowcasts for evaluation
  tar_target(
    name = nowcast_dates,
    command = as.character(
      seq(
        from = ymd("2024-10-09"),
        to = ymd("2025-06-04"),
        by = "week"
      )
    ),
  ),
  tar_target(
    name = clades_to_plot,
    command = c("24E", "24F", "25A")
  ),
  # Url to load in the oracle output, which contains the data available as of the nowcast date
  tar_target(
    name = hub_path,
    command = "https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/main/"
  ),
  # Bucket name for querying the nowcasts
  tar_target(
    name = nowcast_bucket_name,
    command = "covid-variant-nowcast-hub"
  ),
  # This needs to be appended with {nowcast_date}.json
  tar_target(
    name = clades_by_nowcast_date_dir,
    command = "https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/main/auxiliary-data/modeled-clades/" # nolint
  ),
  # Need the location codes and population sizes
  tar_target(
    name = location_fp,
    command = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/auxiliary-data/locations.csv" # nolint
  ),
  tar_target(
    name = scores_fp,
    command = "https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/main/auxiliary-data/scores/scores.tsv" # nolint
  ),
  # Will be updated when merged to main
  tar_target(
    name = coverage_fp,
    command = "https://github.com/reichlab/variant-nowcast-hub/raw/refs/heads/883-add-coverage/auxiliary-data/coverage/coverage.parquet" # nolint
  ),
  # Url to load in the raw data on number of sequences by clade for all clades
  tar_target(
    name = raw_variant_data_ns_url,
    command = "https://data.nextstrain.org/files/workflows/forecasts-ncov/open/nextstrain_clades/usa.tsv.gz" # nolint
  ),
  tar_target(
    name = fp_ca_clades,
    command = file.path("output", "figs", "covidnet_vs_ncbi_clade_proportions.csv")
  ),
  tar_target(
    name = fp_ca_volume,
    command = file.path("output", "figs", "covidnet_vs_ncbi_sequence_volume.csv")
  ),

  # Specify which states and nowcast dates we want to focus on in the example
  # outputs figures
  tar_target(
    name = states_for_vis,
    command = c("CA", "MN", "IL")
  ),
  tar_target(
    name = nowcast_date_for_vis,
    command = as.character(ymd("2025-02-19"))
  ),
  tar_target(
    name = nowcast_date_range_to_zoom,
    command = seq(
      from = ymd("2025-02-05"),
      to = ymd("2025-03-19"),
      by = "week"
    )
  ),
  tar_target(
    name = quantiles_to_vis,
    command = c(0.05, 0.25, 0.5, 0.75, 0.975)
  ),

  # Put into a single config file,
  tar_target(
    name = config,
    command = list(
      nowcast_dates = nowcast_dates,
      states_for_vis = states_for_vis,
      nowcast_date_for_vis = nowcast_date_for_vis,
      quantiles_to_vis = quantiles_to_vis,
      nowcast_bucket_name = nowcast_bucket_name,
      hub_path = hub_path,
      clades_by_nowcast_date_dir = clades_by_nowcast_date_dir,
      location_fp = location_fp,
      scores_fp = scores_fp,
      coverage_fp = coverage_fp,
      raw_variant_data_ns_url = raw_variant_data_ns_url
    )
  ),
  tar_target(
    name = save_config,
    command = yaml::write_yaml(config,
      file = file.path(
        "input",
        "config",
        "config.yaml"
      )
    )
  )
)

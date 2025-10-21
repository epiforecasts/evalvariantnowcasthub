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
  # Url to load in the raw data on number of sequences by clade for all clades
  tar_target(
    name = raw_variant_data_ns_url,
    command = "https://data.nextstrain.org/files/workflows/forecasts-ncov/open/nextstrain_clades/usa.tsv.gz" # nolint
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
  # To be updated
  tar_target(
    name = scores_fp,
    command = "https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/adding-scores/auxiliary-data/scores/scores_2025-09-02.tsv" # nolint
  ),

  # Specify which states and nowcast dates we want to focus on in the example
  # outputs figures
  tar_target(
    name = states_for_vis,
    command = c("CA", "MA", "OH")
  ),
  tar_target(
    name = nowcast_date_for_vis,
    command = as.character(ymd("2025-02-19"))
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

load_data_targets <- list(
  # "Final" variant data
  tar_target(
    name = raw_variant_data,
    command = open_zip(raw_variant_data_ns_url)
  ),
  # Variant data available as of the nowcast date
  tar_target(
    name = variant_data_as_of_nowcast_date,
    command = get_target_data(
      hub_path = hub_path,
      nowcast_dates = nowcast_date_for_vis,
      states = states_for_vis
    )
  ),
  # Variant data for evaluation
  tar_target(
    name = variant_data_for_evaluation,
    command = get_oracle_output(
      hub_path = hub_path,
      nowcast_dates = nowcast_date_for_vis,
      states = states_for_vis
    )
  ),
  # Number of sequences available within the nowcast horizon
  tar_target(
    name = seq_counts_by_date_us,
    command = get_target_data(
      hub_path = hub_path,
      nowcast_dates = nowcast_dates,
      states = NULL
    ) |> mutate(horizon = target_date - nowcast_date) |>
      filter(horizon >= -31, location != "CA") |>
      group_by(nowcast_date) |>
      summarise(total_sequences = sum(observation))
  ),
  tar_target(
    name = seq_counts_by_date_ca,
    command = get_target_data(
      hub_path = hub_path,
      nowcast_dates = nowcast_dates,
      states = "CA"
    ) |> mutate(horizon = target_date - nowcast_date) |>
      filter(horizon >= -31, location == "CA") |>
      group_by(nowcast_date) |>
      summarise(total_sequences = sum(observation))
  ),
  # Clades
  tar_target(
    name = clade_list,
    command = get_clade_list(
      nowcast_dates,
      clades_by_nowcast_date_dir
    )
  ),
  # Location table
  tar_target(
    name = location_data,
    command = read_csv(location_fp) |> select(abbreviation:population)
  ),
  # Model outputs
  tar_target(
    name = raw_selected_model_outputs,
    command = extract_nowcasts(
      nowcast_dates = nowcast_date_for_vis,
      states = states_for_vis,
      bucket_name = nowcast_bucket_name
    )
  ),
  # All model outputs for heatmap (all dates and locations)
  tar_target(
    name = all_model_outputs_for_heatmap,
    command = extract_nowcasts(
      nowcast_dates = nowcast_dates,
      states = location_data$abbreviation,
      bucket_name = nowcast_bucket_name
    )
  ),
  # Scores corresponding to the nowcast dates we will evaluate
  tar_target(
    name = scores,
    command = read_tsv(scores_fp) |>
      filter(
        nowcast_date >= min(nowcast_dates),
        nowcast_date <= max(nowcast_dates)
      )
  )
)

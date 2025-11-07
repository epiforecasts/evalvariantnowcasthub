load_data_targets <- list(
  # "Final" variant data
  tar_target(
    name = final_variant_data_all_states,
    command = get_oracle_output(
      hub_path = hub_path,
      nowcast_dates = max(nowcast_dates),
      states = NULL
    )
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
  # Scores
  tar_target(
    name = scores,
    command = read_tsv(scores_fp)
  )
)

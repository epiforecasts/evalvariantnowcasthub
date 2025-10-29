clean_data_targets <- list(
  tar_target(
    name = clean_variant_data,
    command = get_clean_variant_data(
      raw_variant_data,
      clade_list,
      location_data,
      nowcast_dates
    )
  ),
  tar_target(
    name = final_seq_counts,
    command = clean_variant_data |>
      group_by(date, location) |>
      summarise(n_final_seq = sum(sequences))
  ),
  tar_target(
    name = su_scores,
    command = convert_to_su_object(
      scores_data = scores,
      brier_to_use = "brier_point"
    )
  )
)

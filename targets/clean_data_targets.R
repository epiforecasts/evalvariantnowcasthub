clean_data_targets <- list(
  tar_target(
    name = clean_variant_data_final_all_states,
    command = get_clean_variant_data_ns(
      raw_variant_data,
      clade_list,
      location_data,
      nowcast_dates,
      type = "final"
    )
  ),
  tar_target(
    name = clean_variant_data_as_of_nowcast_date,
    command = get_clean_variant_data(
      variant_data_as_of_nowcast_date,
      clade_list,
      location_data,
      nowcast_date_for_vis,
      seq_col_name = "observation",
      type = "as of nowcast date"
    )
  ),
  tar_target(
    name = clean_variant_data_for_eval,
    command = get_clean_variant_data(
      variant_data_for_evaluation,
      clade_list,
      location_data,
      nowcast_date_for_vis,
      seq_col_name = "oracle_value",
      type = "evaluation"
    )
  ),
  tar_target(
    name = final_seq_counts,
    command = clean_variant_data_final_all_states |>
      group_by(date, location) |>
      summarise(n_final_seq = sum(sequences))
  ),
  tar_target(
    name = su_scores,
    command = convert_to_su_object(
      scores_data = scores,
      brier_to_use = "brier_point"
    )
  ),
  tar_target(
    name = su_scores_excl_partial,
    command = su_scores |>
      dplyr::filter(scored == TRUE)
  )
)

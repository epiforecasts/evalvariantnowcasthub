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
    name = clean_variant_data_for_eval_mult_nowcasts,
    command = get_clean_variant_data(
      variant_data_for_eval_mult_nowcasts,
      clade_list,
      location_data,
      nowcast_date_range_to_zoom,
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
    name = seq_counts_by_loc,
    command = clean_variant_data_final_all_states |>
      group_by(location) |>
      summarise(total_seq = sum(sequences)) |>
      arrange(desc(total_seq))
  ),
  tar_target(
    name = scores_w_seq_count,
    command = scores |> as.data.frame() |>
      left_join(seq_counts_by_loc) |>
      arrange(desc(total_seq))
  ),
  tar_target(
    name = su_scores,
    command = convert_to_su_object(
      scores_data = scores_w_seq_count,
      brier_to_use = "brier_point"
    )
  ),
  tar_target(
    name = su_scores_excl_partial,
    command = su_scores |>
      dplyr::filter(scored == TRUE)
  ),
  # Get an object with NAs removed for days
  # we can't score due to no sequences
  tar_target(
    name = su_scores_all,
    command = su_scores_excl_partial |>
      dplyr::filter(!is.na(brier_score))
  )
)

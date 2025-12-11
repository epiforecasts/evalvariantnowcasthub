pred_int_targets <- list(
  tar_target(
    name = df_summary,
    command = get_pred_int(
      model_pred_prop = raw_selected_model_outputs,
      eval_seq = clean_variant_data_for_eval
    )
  ),
  tar_target(
    name = df_mult_nowcasts,
    command = get_pred_int(
      model_pred_prop = model_outputs_mult_nowcasts,
      eval_seq = clean_variant_data_for_eval_mult_nowcasts
    )
  ),
  tar_target(
    name = df_quantiled_nowcasts,
    command = get_pred_int(
      model_pred_prop = all_model_outputs,
      eval_seq = clean_variant_data_for_eval_all_nowcasts
    )
  )
)

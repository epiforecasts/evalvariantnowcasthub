pred_int_targets <- list(
  tar_target(
    name = df_summary,
    command = get_pred_int(
      model_pred_prop = raw_selected_model_outputs,
      eval_seq = clean_variant_data_for_eval
    )
  )
)

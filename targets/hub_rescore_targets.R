hub_rescore_targets <- list(
  # Rescore the CADPH-CATaMaran submissions that failed Hub scoring (#150)----
  # From 23 April 2025 its submissions have a duplicate zero-valued row for
  # clades 24A and 24D on every target date, which fails the Hub's check that
  # the submitted clades match the observed clades. We score them with a
  # verbatim copy of the Hub's scoring code (inst/hub_scoring/), after
  # removing those rows, using inputs pinned to the Hub commit below.
  tar_target(
    name = hub_sha,
    command = "279038bd773cb904ad0f1c7596fb6a757911bcdf"
  ),
  # The Hub scored the 4 June 2025 nowcasts before the last update of their
  # oracle output (4 September 2025, which added one New Mexico sequence for
  # 6 June), so we take that file from the commit before the update. All
  # other nowcast dates were scored with their final oracle output.
  tar_target(
    name = hub_oracle_sha,
    command = c("2025-06-04" = "69e97c330f24a848a7a415f6144e7cbfa363d100"),
    format = "rds"
  ),
  tar_target(
    name = hub_scoring_fp,
    command = file.path("inst", "hub_scoring", "model_scoring_functions.R"),
    format = "file"
  ),
  tar_target(
    name = hub_rescore_root,
    command = file.path("output", "data", "hub_rescore")
  ),
  # CADPH-CATaMaran's nowcast dates in our period, and those that errored
  tar_target(
    name = catamaran_dates,
    command = get_model_scoring_status(scores, model = "CADPH-CATaMaran")
  ),
  # Mean-only California models the Hub scored on the same dates, to check
  # that our copy of the Hub's code reproduces its scores
  tar_target(
    name = hub_check_models,
    command = c("CADPH-CATaLog", "LANL-CovTransformer")
  ),
  tar_target(
    name = hub_files,
    command = get_hub_scoring_files(
      models = c("CADPH-CATaMaran", hub_check_models),
      nowcast_dates = catamaran_dates$nowcast_date,
      hub_sha = hub_sha,
      oracle_sha = hub_oracle_sha
    )
  ),
  tar_target(
    name = hub_files_local,
    command = mapply(download_hub_file,
      path = hub_files$path, hub_sha = hub_files$sha,
      MoreArgs = list(dest_root = hub_rescore_root),
      USE.NAMES = FALSE
    ),
    format = "file"
  ),
  # Step 3: check that we reproduce the Hub's scores-----------------------
  # CADPH-CATaMaran's Hub-scored dates plus the other mean-only models on
  # every date
  tar_target(
    name = hub_check_submissions,
    command = get_hub_check_submissions(
      hub_files = hub_files,
      status_by_date = catamaran_dates,
      model = "CADPH-CATaMaran"
    )
  ),
  tar_target(
    name = hub_check_scores,
    command = {
      force(hub_files_local)
      score_hub_submission(
        hub_path = hub_rescore_root,
        model_output_file = hub_check_submissions$model_output_file,
        nowcast_date = hub_check_submissions$nowcast_date,
        scoring_fp = hub_scoring_fp
      )
    },
    pattern = map(hub_check_submissions)
  ),
  tar_target(
    name = hub_check_summary,
    command = check_hub_reproduction(
      rescored = hub_check_scores,
      hub_scores = scores
    ),
    error = "stop"
  ),
  # Without removing the duplicate rows, every errored CADPH-CATaMaran
  # submission fails with the clade-mismatch error, as on the Hub
  tar_target(
    name = hub_check_errors,
    command = {
      force(hub_files_local)
      errored <- catamaran_dates$nowcast_date[catamaran_dates$status == "error"]
      vapply(as.character(errored), function(d) {
        check_hub_scoring_error(
          hub_path = hub_rescore_root,
          model_output_file = file.path(
            "CADPH-CATaMaran", paste0(d, "-CADPH-CATaMaran.parquet")
          ),
          nowcast_date = d,
          scoring_fp = hub_scoring_fp
        )
      }, character(1))
    },
    format = "rds",
    error = "stop"
  ),
  # Step 4: remove the zero-valued duplicate rows-------------------------
  # Only from CADPH-CATaMaran's errored submissions. Only rows with value
  # exactly 0 are dropped, and only where the same clade also has a single
  # non-zero row; any other duplication stops the pipeline.
  tar_target(
    name = hub_check_only_catamaran_errored,
    command = check_only_model_errored(scores, model = "CADPH-CATaMaran"),
    format = "rds",
    error = "stop"
  ),
  tar_target(
    name = catamaran_errored_files,
    command = get_errored_submissions(
      hub_files = hub_files,
      status_by_date = catamaran_dates,
      model = "CADPH-CATaMaran"
    )
  ),
  tar_target(
    name = hub_rescore_dedup_root,
    command = file.path("output", "data", "hub_rescore_dedup")
  ),
  tar_target(
    name = catamaran_dedup_local,
    command = {
      force(hub_files_local)
      force(hub_check_only_catamaran_errored)
      write_deduplicated_submission(
        src_root = hub_rescore_root,
        dest_root = hub_rescore_dedup_root,
        model_output_file = catamaran_errored_files$model_output_file,
        nowcast_date = catamaran_errored_files$nowcast_date
      )
    },
    pattern = map(catamaran_errored_files),
    format = "file",
    error = "stop"
  ),
  # The removed rows, for the record
  tar_target(
    name = catamaran_removed_rows,
    command = {
      force(hub_files_local)
      get_removed_zero_duplicates(
        src_root = hub_rescore_root,
        model_output_files = catamaran_errored_files$model_output_file
      )
    }
  )
)

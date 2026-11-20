#' Relative paths of the Hub files needed to score submissions
#'
#' Lists, for each model and nowcast date, the submission file and the two
#' files the Hub's scoring code reads for that nowcast date: the oracle
#' output (observed clade counts) and the unscored location-dates.
#'
#' @param models Character vector of model IDs.
#' @param nowcast_dates Vector of nowcast dates.
#' @param hub_sha Commit of reichlab/variant-nowcast-hub to take files from.
#' @param oracle_sha Named character vector of commits to take the oracle
#'   output from instead of `hub_sha`, named by nowcast date. Use this where
#'   the Hub scored a nowcast date before the last update of its oracle
#'   output.
#' @returns Data.frame with columns model_id, nowcast_date, model_output_file
#'   (relative to `model-output/`, as the Hub's scoring code expects), path
#'   (relative to the Hub root) and sha, with one row per file. The oracle
#'   and unscored files have NA model_id and model_output_file.
#' @autoglobal
get_hub_scoring_files <- function(models, nowcast_dates, hub_sha,
                                  oracle_sha = character()) {
  nowcast_dates <- as.character(as.Date(nowcast_dates))
  submissions <- expand.grid(
    model_id = models,
    nowcast_date = nowcast_dates,
    stringsAsFactors = FALSE
  )
  submissions$model_output_file <- file.path(
    submissions$model_id,
    paste0(submissions$nowcast_date, "-", submissions$model_id, ".parquet")
  )
  submissions$path <- file.path("model-output", submissions$model_output_file)
  submissions$sha <- hub_sha
  oracle_shas <- ifelse(nowcast_dates %in% names(oracle_sha),
    oracle_sha[nowcast_dates], hub_sha
  )
  date_files <- data.frame(
    model_id = NA_character_,
    nowcast_date = rep(nowcast_dates, each = 2),
    model_output_file = NA_character_,
    path = c(rbind(
      file.path(
        "target-data", "oracle-output",
        paste0("nowcast_date=", nowcast_dates), "oracle.parquet"
      ),
      file.path(
        "auxiliary-data", "unscored-location-dates",
        paste0(nowcast_dates, ".csv")
      )
    )),
    sha = c(rbind(oracle_shas, hub_sha))
  )
  return(rbind(submissions, date_files))
}

#' Download one file from the Variant Nowcast Hub at a fixed commit
#'
#' Saves the file under `dest_root` at the same relative path as in the Hub,
#' so that `dest_root` can be passed as `hub_path` to the Hub's scoring code.
#'
#' @param path Path of the file relative to the Hub root.
#' @param hub_sha Commit of reichlab/variant-nowcast-hub to download from.
#' @param dest_root Local folder standing in for the Hub root.
#' @returns The local path of the downloaded file.
download_hub_file <- function(path, hub_sha, dest_root) {
  dest <- file.path(dest_root, path)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file_url <- file.path(
    "https://raw.githubusercontent.com/reichlab/variant-nowcast-hub",
    hub_sha, utils::URLencode(path)
  )
  utils::download.file(file_url, dest, mode = "wb", quiet = TRUE)
  return(dest)
}

#' Load the Hub's scoring functions
#'
#' Sources the copy of the Hub's `process_target_data()` and
#' `calc_energy_scores()` in `inst/hub_scoring/` into a new environment, so
#' they don't mask anything in the pipeline.
#'
#' @param scoring_fp Path to `inst/hub_scoring/model_scoring_functions.R`.
#' @returns Environment containing the Hub's scoring functions.
load_hub_scoring <- function(scoring_fp) {
  hub_env <- new.env(parent = globalenv())
  sys.source(scoring_fp, envir = hub_env)
  return(hub_env)
}

#' Score one submission exactly as the Hub does
#'
#' Mirrors the Hub's `get_energy_scores()` and the scoring loop in
#' `src/update_scores_weekly.R` (commit 279038b): set the seed from the date
#' in the file name, read the inputs with `process_target_data()`, score with
#' `calc_energy_scores()`, and add model_id, nowcast_date and status in the
#' Hub's column order. The seed only affects the energy score; the Brier
#' score of a mean-only submission involves no random sampling.
#'
#' @param hub_path Local folder standing in for the Hub root, holding the
#'   files listed by [get_hub_scoring_files()].
#' @param model_output_file Path of the submission relative to
#'   `model-output/`, e.g.
#'   `"CADPH-CATaMaran/2025-04-23-CADPH-CATaMaran.parquet"`.
#' @param nowcast_date Nowcast date of the submission.
#' @param scoring_fp Path to `inst/hub_scoring/model_scoring_functions.R`.
#' @returns Data.frame with the columns of the Hub's `scores.tsv`.
#' @autoglobal
score_hub_submission <- function(hub_path, model_output_file, nowcast_date,
                                 scoring_fp) {
  hub_env <- load_hub_scoring(scoring_fp)
  date_str <- sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", model_output_file)
  set.seed(as.integer(gsub("-", "", date_str, fixed = TRUE)))
  hub_inputs <- hub_env$process_target_data(
    hub_path = hub_path,
    model_output_file = model_output_file,
    ref_date = as.Date(nowcast_date)
  )
  df_scores <- hub_env$calc_energy_scores(
    targets = hub_inputs[[1]],
    df_model_output = hub_inputs[[2]]
  )
  df_scores <- df_scores |>
    dplyr::mutate(
      model_id = dirname(model_output_file),
      nowcast_date = as.Date(nowcast_date),
      status = "success"
    ) |>
    dplyr::select(
      model_id, nowcast_date, target_date, location,
      brier_point, brier_dist, energy, scored, status
    )
  return(df_scores)
}

#' Check that our scores reproduce the Hub's
#'
#' Joins scores computed with [score_hub_submission()] to the Hub's
#' `scores.tsv` rows for the same submissions and stops if any row is
#' missing on either side, if the `scored` flags differ, if a score is
#' missing on one side only, or if any score differs by more than `tol`.
#'
#' @param rescored Scores from [score_hub_submission()].
#' @param hub_scores Scores read from the Hub's `scores.tsv`.
#' @param tol Largest absolute difference allowed (the Hub's TSV loses the
#'   last bit or so of precision).
#' @returns Data.frame with one row per submission: model_id, nowcast_date,
#'   n_rows and the largest absolute difference in each score.
#' @autoglobal
check_hub_reproduction <- function(rescored, hub_scores, tol = 1e-12) {
  keys <- c("model_id", "nowcast_date", "target_date", "location")
  hub_rows <- dplyr::semi_join(
    hub_scores, rescored,
    by = c("model_id", "nowcast_date")
  )
  joined <- dplyr::full_join(
    rescored, hub_rows,
    by = keys, suffix = c("_ours", "_hub")
  )
  if (anyNA(joined$status_ours) || anyNA(joined$status_hub)) {
    cli::cli_abort("Rows differ between our scores and the Hub's.")
  }
  if (!all(joined$scored_ours == joined$scored_hub)) {
    cli::cli_abort("The {.field scored} flag differs from the Hub's.")
  }
  score_cols <- c("brier_point", "brier_dist", "energy")
  for (col in score_cols) {
    ours <- joined[[paste0(col, "_ours")]]
    hub <- joined[[paste0(col, "_hub")]]
    if (!identical(is.na(ours), is.na(hub))) {
      cli::cli_abort("{.field {col}} is missing in one set of scores only.")
    }
    joined[[paste0("diff_", col)]] <- abs(ours - hub)
  }
  max_diffs <- joined |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      dplyr::across(
        dplyr::starts_with("diff_"),
        ~ if (all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE)
      ),
      .by = c(model_id, nowcast_date)
    )
  max_diff <- max(
    unlist(max_diffs[paste0("diff_", score_cols)]),
    na.rm = TRUE
  )
  if (max_diff > tol) {
    cli::cli_abort("Scores differ from the Hub's by up to {max_diff}.")
  }
  return(max_diffs)
}

#' Check that a submission fails Hub scoring with the clade-mismatch error
#'
#' @inheritParams score_hub_submission
#' @returns The error message, invisibly; stops if scoring succeeds or fails
#'   for a different reason.
check_hub_scoring_error <- function(hub_path, model_output_file, nowcast_date,
                                    scoring_fp) {
  expected <- "Clades in observed data do not match clades in mean model output"
  result <- tryCatch(
    suppressWarnings(score_hub_submission(
      hub_path, model_output_file, nowcast_date, scoring_fp
    )),
    error = conditionMessage
  )
  if (!is.character(result) || !grepl(expected, result, fixed = TRUE)) {
    cli::cli_abort(
      "{.file {model_output_file}} did not fail with the clade-mismatch error."
    )
  }
  return(invisible(result))
}

#' Nowcast dates of one model's submissions and their Hub scoring status
#'
#' @param scores Scores read from the Hub's `scores.tsv`.
#' @param model Model ID.
#' @returns Data.frame with columns nowcast_date and status ("success" or
#'   "error"), one row per nowcast date.
#' @autoglobal
get_model_scoring_status <- function(scores, model = "CADPH-CATaMaran") {
  status_by_date <- scores |>
    dplyr::filter(model_id == model) |>
    dplyr::distinct(nowcast_date, status) |>
    dplyr::arrange(nowcast_date)
  return(status_by_date)
}

#' Submissions to score when checking that we reproduce the Hub
#'
#' All submissions in `hub_files` except those of `model` on the dates it
#' failed Hub scoring, which have no Hub scores to compare with.
#'
#' @param hub_files Output of [get_hub_scoring_files()].
#' @param status_by_date Output of [get_model_scoring_status()] for `model`.
#' @param model Model ID.
#' @returns Data.frame with columns model_id, nowcast_date and
#'   model_output_file.
#' @autoglobal
get_hub_check_submissions <- function(hub_files, status_by_date,
                                      model = "CADPH-CATaMaran") {
  errored <- as.character(
    status_by_date$nowcast_date[status_by_date$status == "error"]
  )
  submissions <- hub_files |>
    dplyr::filter(
      !is.na(model_id),
      !(model_id == model & nowcast_date %in% errored)
    ) |>
    dplyr::select(model_id, nowcast_date, model_output_file)
  return(submissions)
}

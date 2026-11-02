# Identifiability analysis (#141)---------------------------------------------
# Can the evaluation tell a true model from one that misestimates the rising
# clade's growth advantage, as the number of sequences N varies? The truth is
# an MLR fit to final data for one location around one nowcast date.
identifiability_targets <- list(
  # Settings-------------------------------------------------------------
  tar_target(
    name = id_nowcast_date,
    command = as.Date("2025-02-19")
  ),
  tar_target(
    name = id_location,
    command = "MN"
  ),
  # Fitting window: t0 is 90 days before the nowcast date; the truth runs to
  # +10 days so it covers the whole scoring window
  tar_target(
    name = id_days_before,
    command = 90
  ),
  tar_target(
    name = id_days_after,
    command = 10
  ),
  # Growth rates are relative to the reference clade: the dominant clade at
  # the nowcast date, so the rising clade's growth advantage is relative to it
  tar_target(
    name = id_ref_clade,
    command = "24F (XEC)"
  ),
  tar_target(
    name = id_rising_clade,
    command = "25A (LP.8.1)"
  ),
  # Loop A: sequences per week across the 42 scoring days (-31 to +10),
  # spanning observed volumes over location x nowcast date windows (zeros
  # included): 25th percentile 0.5/week, median 4.2/week, 90th percentile
  # 48/week, max 346/week. Below 7/week (1/day), sequences fall on a random
  # subset of days (N = 1 each, redrawn each replicate); from 7/week up, N is
  # constant on every day.
  tar_target(
    name = id_rate_grid,
    command = c(0.5, 1, 2, 4, 7, 14, 21, 35, 70, 105, 175, 350)
  ),
  # Truth-----------------------------------------------------------------
  tar_target(
    name = id_truth_data,
    command = get_truth_fit_data(
      final_data = clean_variant_data_final_all_states,
      eval_data = clean_variant_data_eval_all,
      location = id_location,
      nowcast_date = id_nowcast_date,
      days_before = id_days_before,
      days_after = id_days_after
    )
  ),
  tar_target(
    name = id_truth_fit,
    command = fit_truth_mlr(id_truth_data, ref_clade = id_ref_clade),
    format = "rds"
  ),
  tar_target(
    name = plot_id_truth_fit,
    command = get_plot_truth_fit(
      truth_data = id_truth_data,
      truth_fit = id_truth_fit,
      nowcast_date = id_nowcast_date,
      plot_name = "truth_fit"
    )
  )
)

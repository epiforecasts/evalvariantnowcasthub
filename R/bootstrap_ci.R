#' Draw moving block bootstrap resamples of nowcast dates
#'
#' Draws `n_boot` moving block bootstrap resamples of the nowcast dates
#' (Kunsch 1989), as used in Pohle, Zahn & Lerch (2026). Each resample
#' concatenates `ceiling(N / block_length)` blocks of consecutive nowcast
#' dates, with block starts drawn uniformly with replacement, and is
#' trimmed to the original length `N`. Blocks may overlap, so a nowcast date
#' can appear more than once in a resample; `position` identifies each draw
#' uniquely within a resample.
#'
#' The resamples are drawn once and shared across all metrics, score types
#' and regions so that every bootstrap estimate uses the same resampled
#' nowcast dates.
#'
#' @param nowcast_dates Vector of nowcast dates in the evaluation period.
#'   Must be evenly spaced so that a block of positions is a block of time.
#' @param n_boot Integer number of bootstrap resamples.
#' @param block_length Integer block length, in number of nowcast dates.
#' @returns Data frame with columns `boot_id`, `position` and `nowcast_date`,
#'   with `n_boot * length(nowcast_dates)` rows.
#' @autoglobal
get_block_bootstrap_dates <- function(nowcast_dates,
                                      n_boot,
                                      block_length) {
  nowcast_dates <- sort(unique(as.Date(nowcast_dates)))
  n_dates <- length(nowcast_dates)
  if (block_length < 1 || block_length > n_dates) {
    cli::cli_abort(
      "{.arg block_length} must be between 1 and {n_dates}, not {block_length}."
    )
  }
  if (length(unique(diff(nowcast_dates))) > 1) {
    cli::cli_abort("{.arg nowcast_dates} must be evenly spaced.")
  }

  n_blocks <- ceiling(n_dates / block_length)
  boot_ids <- lapply(seq_len(n_boot), function(b) {
    starts <- sample.int(n_dates - block_length + 1, n_blocks, replace = TRUE)
    idx <- as.vector(outer(seq_len(block_length) - 1, starts, "+"))
    idx[seq_len(n_dates)]
  })

  data.frame(
    boot_id = rep(seq_len(n_boot), each = n_dates),
    position = rep(seq_len(n_dates), times = n_boot),
    nowcast_date = nowcast_dates[unlist(boot_ids)]
  )
}

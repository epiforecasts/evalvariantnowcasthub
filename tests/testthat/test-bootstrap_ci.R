weekly_dates <- seq(as.Date("2024-10-09"), by = "week", length.out = 35)

test_that("get_block_bootstrap_dates returns N dates per resample", {
  set.seed(1)
  boot <- get_block_bootstrap_dates(weekly_dates, n_boot = 20, block_length = 6)

  expect_named(boot, c("boot_id", "position", "nowcast_date"))
  expect_identical(nrow(boot), 20L * 35L)
  expect_true(all(table(boot$boot_id) == 35))
  expect_true(all(boot$nowcast_date %in% weekly_dates))
  # position uniquely identifies each draw within a resample
  expect_false(anyDuplicated(boot[, c("boot_id", "position")]) > 0)
})

test_that("get_block_bootstrap_dates resamples blocks of consecutive dates", {
  set.seed(1)
  boot <- get_block_bootstrap_dates(weekly_dates, n_boot = 20, block_length = 6)

  # Positions 1-6, 7-12, ... each come from one block of consecutive weeks;
  # the final block of 6 is trimmed to 5 so each resample has 35 dates.
  boot$block <- (boot$position - 1) %/% 6
  steps <- tapply(
    boot$nowcast_date,
    list(boot$boot_id, boot$block),
    function(d) unique(diff(d))
  )
  expect_true(all(unlist(steps) == 7))
})

test_that("get_block_bootstrap_dates is reproducible given a seed", {
  set.seed(42)
  boot1 <- get_block_bootstrap_dates(weekly_dates, n_boot = 5, block_length = 6)
  set.seed(42)
  boot2 <- get_block_bootstrap_dates(weekly_dates, n_boot = 5, block_length = 6)
  expect_identical(boot1, boot2)
})

test_that("get_block_bootstrap_dates validates its inputs", {
  expect_error(
    get_block_bootstrap_dates(weekly_dates, n_boot = 5, block_length = 36),
    "block_length"
  )
  expect_error(
    get_block_bootstrap_dates(weekly_dates[-3], n_boot = 5, block_length = 6),
    "evenly spaced"
  )
})

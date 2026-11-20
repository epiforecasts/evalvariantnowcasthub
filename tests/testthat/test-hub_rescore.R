sha <- "abc123"

test_that("get_hub_scoring_files lists submissions and per-date files", {
  files <- get_hub_scoring_files(
    models = c("m1", "m2"),
    nowcast_dates = as.Date(c("2025-04-02", "2025-04-09")),
    hub_sha = sha
  )
  expect_identical(nrow(files), 8L)
  expect_true(
    file.path("model-output", "m2", "2025-04-09-m2.parquet") %in% files$path
  )
  expect_true(
    file.path("m1", "2025-04-02-m1.parquet") %in% files$model_output_file
  )
  expect_true(all(c(
    file.path(
      "target-data", "oracle-output", "nowcast_date=2025-04-02",
      "oracle.parquet"
    ),
    file.path("auxiliary-data", "unscored-location-dates", "2025-04-09.csv")
  ) %in% files$path))
  expect_true(all(files$sha == sha))
})

test_that("get_hub_scoring_files takes overridden oracle commits", {
  files <- get_hub_scoring_files(
    models = "m1",
    nowcast_dates = c("2025-04-02", "2025-04-09"),
    hub_sha = sha,
    oracle_sha = c("2025-04-09" = "old")
  )
  oracle <- files[grepl("oracle", files$path, fixed = TRUE), ]
  expect_identical(
    oracle$sha[grepl("2025-04-09", oracle$path, fixed = TRUE)], "old"
  )
  expect_identical(
    oracle$sha[grepl("2025-04-02", oracle$path, fixed = TRUE)], sha
  )
  others <- files[!grepl("oracle", files$path, fixed = TRUE), ]
  expect_true(all(others$sha == sha))
})

hub_like <- function(brier = c(0.1, 0.2), scored = c(TRUE, FALSE)) {
  data.frame(
    model_id = "m1",
    nowcast_date = as.Date("2025-04-02"),
    target_date = as.Date(c("2025-03-30", "2025-03-31")),
    location = "CA",
    brier_point = brier,
    brier_dist = NA_real_,
    energy = NA_real_,
    scored = scored,
    status = "success",
    stringsAsFactors = FALSE
  )
}

test_that("check_hub_reproduction passes on matching scores", {
  hub <- rbind(hub_like(), transform(hub_like(), model_id = "other"))
  out <- check_hub_reproduction(hub_like(c(0.1, 0.2 + 1e-16)), hub)
  expect_identical(nrow(out), 1L)
  expect_identical(out$n_rows, 2L)
})

test_that("check_hub_reproduction stops on differences", {
  expect_error(
    check_hub_reproduction(hub_like(c(0.1, 0.3)), hub_like()),
    "differ from the Hub"
  )
  expect_error(
    check_hub_reproduction(hub_like(scored = c(TRUE, TRUE)), hub_like()),
    "scored"
  )
  expect_error(
    check_hub_reproduction(hub_like(c(0.1, NA)), hub_like()),
    "missing in one set"
  )
  expect_error(
    check_hub_reproduction(hub_like()[1, ], hub_like()),
    "Rows differ"
  )
})

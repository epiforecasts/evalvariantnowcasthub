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

submission <- function(clades, values, target_dates = "2025-04-01") {
  data.frame(
    nowcast_date = as.Date("2025-04-23"),
    location = "CA",
    target_date = as.Date(target_dates),
    output_type = "mean",
    output_type_id = NA_character_,
    clade = clades,
    value = values,
    stringsAsFactors = FALSE
  )
}

test_that("remove_zero_duplicates drops only zero-valued duplicates", {
  x <- submission(
    clades = c("24A", "24A", "24D", "24D", "25A", "other"),
    values = c(0.3, 0, 0, 0.2, 0.5, 0)
  )
  out <- remove_zero_duplicates(x)
  expect_identical(out$cleaned$clade, c("24A", "24D", "25A", "other"))
  expect_identical(out$cleaned$value, c(0.3, 0.2, 0.5, 0))
  expect_identical(out$removed$clade, c("24A", "24D"))
  expect_true(all(out$removed$value == 0))
  expect_invisible(check_zero_duplicates_removed(x, out$cleaned))
})

test_that("remove_zero_duplicates leaves clean submissions unchanged", {
  x <- submission(c("24A", "25A"), c(0.4, 0.6))
  out <- remove_zero_duplicates(x)
  expect_identical(out$cleaned, x)
  expect_identical(nrow(out$removed), 0L)
})

test_that("remove_zero_duplicates stops when it is unclear what to keep", {
  expect_error(
    remove_zero_duplicates(submission(c("24A", "24A"), c(0.3, 0.2))),
    "exactly one non-zero"
  )
  expect_error(
    remove_zero_duplicates(submission(c("24A", "24A"), c(0, 0))),
    "exactly one non-zero"
  )
  expect_error(
    remove_zero_duplicates(submission(c("24A", "24A", "24A"), c(0.3, 0, 0.3))),
    "exactly one non-zero"
  )
  expect_error(
    remove_zero_duplicates(submission(c("24A", "25A"), c(NA, 1))),
    "missing values"
  )
})

test_that("check_zero_duplicates_removed ignores group order", {
  # Removing the first row changes which target date appears first
  x <- submission(
    clades = c("24A", "24A", "25A", "24A", "25A"),
    values = c(0, 0.4, 0.6, 0.3, 0.7),
    target_dates = c(
      "2025-04-01", "2025-04-02", "2025-04-02", "2025-04-01", "2025-04-01"
    )
  )
  out <- remove_zero_duplicates(x)
  expect_identical(nrow(out$removed), 1L)
  expect_invisible(check_zero_duplicates_removed(x, out$cleaned))
})

test_that("check_zero_duplicates_removed catches bad de-duplication", {
  x <- submission(c("24A", "24A", "25A"), c(0.4, 0, 0.6))
  expect_error(check_zero_duplicates_removed(x, x), "remain")
  expect_error(
    check_zero_duplicates_removed(x, x[c(2, 3), ]),
    "other than zero rows"
  )
})

test_that("check_only_model_errored allows only the named model", {
  ok <- data.frame(
    model_id = c("A", "B"), status = c("error", "success"),
    stringsAsFactors = FALSE
  )
  expect_invisible(check_only_model_errored(ok, model = "A"))
  expect_error(check_only_model_errored(ok, model = "B"), "A")
})

test_that("add_rescored_scores replaces only the errored rows", {
  hub <- rbind(
    hub_like(),
    data.frame(
      model_id = "m2", nowcast_date = as.Date("2025-04-02"),
      target_date = as.Date(NA), location = NA_character_,
      brier_point = NA_real_, brier_dist = NA_real_, energy = NA_real_,
      scored = NA, status = "error", stringsAsFactors = FALSE
    )
  )
  rescored <- transform(hub_like(c(0.5, 0.6)), model_id = "m2")
  out <- add_rescored_scores(hub, rescored)
  expect_identical(nrow(out), 4L)
  expect_false(any(out$status == "error"))
  expect_identical(out$status[out$model_id == "m2"], rep("rescored", 2))
  expect_identical(out$brier_point[out$model_id == "m1"], c(0.1, 0.2))
  expect_error(
    add_rescored_scores(hub, hub_like()),
    "only an error row"
  )
})

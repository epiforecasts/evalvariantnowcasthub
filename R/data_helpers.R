#' Get vector of clades
#'
#' @param nowcast_dates Vector of nowcast dates to be evaluated
#' @param clades_by_nowcast_date_dir Character string indicating the directory
#'   of the clades
#'
#' @returns Character string of all clades used during the nowcast dates
get_clade_list <- function(nowcast_dates,
                           clades_by_nowcast_date_dir) {
  clades <- c()
  for (i in seq_along(nowcast_dates)) {
    json_url <- glue::glue("{clades_by_nowcast_date_dir}{nowcast_dates[i]}.json") # nolint
    clade_list <- suppressWarnings(rjson::fromJSON(
      paste(readLines(json_url), collapse = "")
    ))
    clades_i <- clade_list$clades
    clades <- unique(c(clades, clades_i))
  }
  return(clades)
}

#' Extract nowcasts from S3 bucket
#'
#' @param nowcast_dates Nowcast date(s) to extract
#' @param states Locations(s) to extract
#' @param bucket_name Name of the S3 bucket
#'
#' @returns Raw nowcast outputs as a table for the selected locations and
#'   nowcast dates
extract_nowcasts <- function(nowcast_dates,
                             states,
                             bucket_name) {
  hub_bucket <- arrow::s3_bucket(bucket_name)
  hub_con <- hubData::connect_hub(hub_bucket,
    file_format = "parquet",
    skip_checks = TRUE
  )
  nowcast_data <- hub_con |>
    filter(
      location %in% states,
      nowcast_date %in% nowcast_dates
    ) |>
    hubData::collect_hub()
  return(nowcast_data)
}


#' Get clean variant data from raw data
#'
#' @param raw_variant_data Data.frame of latest data extracted directly from
#'    next straing
#' @param clade_list Vector of character strings of the clade names
#' @param location_data Data.frame of location information
#' @param nowcast_days Number of days we nowcast, default is `31`.
#'
#' @returns Data.frame of counts of sequences of each clade we nowcasted during
#'   the season
get_clean_variant_data <- function(raw_variant_data,
                                   clade_list,
                                   location_data,
                                   nowcast_dates,
                                   nowcast_days = 31) {
  clean_latest_data <- raw_variant_data |>
    mutate(
      location =
        case_when(
          location == "Washington DC" ~ "District of Columbia",
          location == "Deleware" ~ "Delaware",
          location == "Louisana" ~ "Louisiana",
          TRUE ~ location
        ),
      clades_modeled = ifelse(clade %in% clade_list, clade, "other")
    ) |>
    filter(
      location %in% location_data$location_name,
      date <= ymd(max(nowcast_dates)),
      date >= ymd(min(nowcast_dates)) - days(nowcast_days)
    )

  return(clean_latest_data)
}

#' Extract oracle output data which will be used for evaluation
#'
#' @param hub_path Character string of hub path url
#' @param nowcast_dates Vector of character strings of dates to extract
#' @param states Vector of charatcer strings of locations to extract
#'
#' @returns Data.frame of sequence counts by location, date, and clade
#'   as of 90 days after each nowcast date, for evalauting that nowcast date.
get_oracle_output <- function(hub_path,
                              nowcast_dates,
                              states = NULL) {
  all_oracle_data <- data.frame()
  for (i in seq_along(nowcast_dates)) {
    oracle_file <- paste0("nowcast_date=", nowcast_dates[i])
    oracle_path <- file.path(
      hub_path, "target-data", "oracle-output",
      oracle_file, "oracle.parquet"
    )
    oracle_data <- arrow::read_parquet(oracle_path)
    all_oracle_data <- rbind(all_oracle_data, oracle_data)
  }

  if (is.null(states)) {
    return(all_oracle_data)
  } else {
    subset_oracle_data <- all_oracle_data |>
      filter(location %in% states)
    return(subset_oracle_data)
  }
}

#' Extract target data output so we have what would have been available
#'   to teams as of the nowcast date
#'
#' @param hub_path Character string of hub path url
#' @param nowcast_dates Vector of character strings of dates to extract
#' @param states Vector of charatcer strings of locations to extract
#'
#' @returns Data.frame of sequence counts by location, date, and clade
#'   as of 90 days after each nowcast date, for evalauting that nowcast date.
get_target_data <- function(hub_path,
                            nowcast_dates,
                            states = NULL) {
  all_target_data <- data.frame()
  for (i in seq_along(nowcast_dates)) {
    ts_date <- as.character(ymd(nowcast_dates[i]) - days(1))
    target_data_folder <- paste0("as_of=", ts_date)
    nowcast_date_folder <- paste0("nowcast_date=", nowcast_dates[i])
    target_data_path <- file.path(
      hub_path, "target-data", "time-series",
      target_data_folder,
      nowcast_date_folder,
      "timeseries.parquet"
    )
    target_data <- arrow::read_parquet(target_data_path)
    all_target_data <- rbind(all_target_data, target_data)
  }

  if (is.null(states)) {
    return(all_target_data)
  } else {
    subset_target_data <- all_target_data |>
      filter(location %in% states)
    return(subset_target_data)
  }
}

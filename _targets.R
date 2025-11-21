library(targets)
library(tarchetypes)
library(readr)
library(here)
library(purrr)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(ggpattern)
library(readr)
library(tidyr)
library(yaml)
library(glue)
library(scoringutils)
library(RColorBrewer)
library(patchwork)
library(fs)
library(arrow)
library(hubData)
library(rjson)
library(rlang)

# load functions
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
rm("functions")

# load target modules
targets <- list.files(here("targets"), full.names = TRUE)
targets <- grep("*\\.R", targets, value = TRUE)
purrr::walk(targets, source)

tar_option_set(
  packages = c(
    "tibble", "dplyr", "lubridate",
    "targets", "ggplot2", "ggpattern",
    "baselinenowcast",
    "purrr",
    "readr", "tidyr",
    "yaml",
    "scoringutils",
    "RColorBrewer",
    "patchwork",
    "fs",
    "arrow",
    "hubData",
    "rjson",
    "rlang"
  ),
  workspace_on_error = TRUE,
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  garbage_collection = TRUE,
  format = "parquet", # default storage format
  error = "null"
)


# Create a config file within targets pipeline to track individual items
config_targets <- list(
  config_targets
)

# Load the variant data, the clades, the scores, and the (selected) model
# outputs and create datasets needed for figures
data_targets <- list(
  load_data_targets,
  clean_data_targets
)

# Summarise and analyse the scores and predictions
analysis_targets <- list(
  pred_int_targets
)

# Make plots
plot_targets <- list(
  eda_plot_targets,
  fig_data_targets,
  fig_data_as_of_targets,
  fig_nowcast_targets,
  fig_pred_plus_data_targets,
  fig_overall_targets,
  fig_submission_heatmaps_targets,
  fig_eval_sequence_heatmap_targets
)


list(
  config_targets,
  data_targets,
  analysis_targets,
  plot_targets
)

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
    "rjson"
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
  # pred_interval_targets
)

# Summarise and analyse the scores
# score_targets <- list(
#   score_table_targets
# )

# Make plots
# plot_targets <- list(
#   EDA_plots,
#   fig_data_targets,
#   fig_nowcast_targets,
#   fig_pred_plus_data_targets,
#   fig_overall_targets
# )


list(
  config_targets,
  data_targets
  # score_targets,
  # plot_targets
)

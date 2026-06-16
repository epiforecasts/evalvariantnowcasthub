# Collaborative estimation and evaluation of SARS-CoV-2 variant nowcasting in the United States

This repository contains the code to generate the results of analysing the SARS-CoV-2 variant nowcasts during the first "season" of the [U.S. SARS-CoV-2 Variant Nowcast Hub](https://github.com/reichlab/variant-nowcast-hub) (from October 9th, 2024 to June 4th, 2025). For up-to-date nowcasts and evaluation of nowcasts, please see the interactive [dashboard](https://reichlab.io/variant-nowcast-hub-dashboard/explore.html) associated with the Variant Nowcast Hub. 
A pre-print describing the results of this work is available on [arXiv](https://arxiv.org/abs/2606.07129). 

This README is organized into the following sections:

- [System requirements](#system-requirements)
- [Installation guide](#installation-guide)
- [Demo and instructions for use](#demo-and-instructions-for-use)
- [Project structure](#project-structure) describing the contents of this repository
- [Data sources](#data-sources) providing links and a description to external data sources used

## System requirements
**Hardware requirements**
Reproducing the analysis using the `evalvariantnowcasthub` analysis functions (which can be installed locally as an R package) requires a standard compute with enough RAM to support the operations of loading the nowcasts and scoring them. For optimal performance we recommend a computer with the following specs:
RAM: 16+ GB
CPU: 4+ cores, 3.3+ GHz/core

**OS Requirements**
The code has been tested on the following systems:
Mac OSX: Tahoe 26.5.1

It has not been tested on Linux or Windows, but it should be compatible with all operating systems. 

**Software dependencies**

*R versions run on*: R 4.5.2

*Platform*: aarch64-apple-darwin20

*Running under*: macOS Tahoe 26.5.1

*Attached base packages*: parallel,stats,graphics,grDevices,utils,datasets,methods,base   

*Other attached packages*: evalvariantnowcasthub_0.0.0.1000, testthat_3.3.2   

*Loaded via namespace (and not attached)*: svUnit_1.0.8, tidyselect_1.2.1, dplyr_1.2.0, farver_2.1.2, scoringRules_1.1.3, arrow_22.0.0.1, tidybayes_3.0.7, S7_0.2.1, fastmap_1.2.0, gh_1.5.0, tensorA_0.36.2.1, timechange_0.4.0, lifecycle_1.0.5, ellipsis_0.3.2, processx_3.8.6, magrittr_2.0.4, posterior_1.6.1.9000, compiler_4.5.2, rlang_1.1.7, tools_4.5.2, data.table_1.18.2.1, knitr_1.51, bit_4.6.0, pkgbuild_1.4.8, curl_7.0.0, xml2_1.5.2, RColorBrewer_1.1-3, cmdstanr_0.9.0.9000, pkgload_1.4.1, abind_1.4-8, purrr_1.2.1, zoltr_1.0.2, desc_1.4.3, grid_4.5.2, roxygen2_7.3.3, ggplot2_4.0.1, scales_1.4.0, MASS_7.3-65, cli_3.6.5, reformulas_0.4.3.1, generics_0.1.4, remotes_2.5.0, otel_0.2.0, rstudioapi_0.18.0, httr_1.4.8, tzdb_0.5.0, rjson_0.2.23, scoringutils_2.2.0, sessioninfo_1.2.3, cachem_1.1.0, stringr_1.6.0, assertthat_0.2.1, hubData_2.1.0.9000, vctrs_0.7.1, devtools_2.4.6, Matrix_1.7-4, epinowcast_0.6.0, jsonlite_2.0.0, hms_1.1.4, arrayhelpers_1.1-0, bit64_4.6.0-1, roxyglobals_1.0.0, ggdist_3.3.3, tidyr_1.3.2, glue_1.8.0, ps_1.9.1, cowplot_1.2.0, distributional_0.6.0, lubridate_1.9.5, stringi_1.8.7, gtable_0.3.6, hubUtils_1.2.0.9000, tibble_3.3.1, pillar_1.11.1, brio_1.1.5, R6_2.6.1, Rdpack_2.6.5, rprojroot_2.1.1, evaluate_1.0.5, lattice_0.22-7, readr_2.2.0, rbibutils_2.4.1, backports_1.5.0, memoise_2.0.1, Rcpp_1.1.1, coda_0.19-4.1, checkmate_2.3.3, xfun_0.56, fs_1.6.6, usethis_3.2.1, pkgconfig_2.0.3

## Installation guide
1. Clone the repo from `https://github.com/epiforecasts/evalvariantnowcasthub`
2. Run `devtools::load_all()` within the project directory. Install any dependencies required

You have now installed the analysis package `evalvariantnowcasthub_0.0.0.1000` locally. This isn't necessary to run the pipeline but will ensure you have installed the required packages.

## Demo and instructions for use
To reproduce all results, simply run `targets::tar_make()` from the console in the project directory. 
This will reproduce all figures (see [Project structure](#project-structure) for where they will be saved) and populate the `docs/supplement.qmd`. 
All data is extracted from publicly available data sources (see [Data Sources](#data-sources) for more information) or provided in the repository.
The expected run-time to reproduce the full results is a 2-3 hours on a normal desktop computer.

## Project structure

| Folder or file | Purpose |
|---|---|
|[`_targets.R`](_targets.R) | The [targets](https://books.ropensci.org/targets/) pipeline used to generate the figures and results in this work. |
|[`targets`](targets) | The folder containing the files of targets lists grouped by their outputs. |
|[`input`](input) | Contains the configuration file to specify the evaluation runs plus a publicly available data source on the percent of ED visits due to COVID-19 in each U.S. jurisdiction. |
|[`output`](output) | Folder where all outputs will be saved. Figures are saved in the `figs` folder, with supplementary figures in the `figs/supp` folder. |
|[`R`](R) | Functions needed to generate targets. |
|[`docs`](docs) | Files needed to generate the supplement plus a rendered version of the latest Supplement. |


## Data sources

Most data used in this paper comes directly from the Hub data itself. The full-time series of variant data in the U.S. is accessed directly from [Nextstrain](https://nextstrain.org/), which curates data from the National Center for Biotechnology and Information (NCBI) [GenBank](https://www.ncbi.nlm.nih.gov/genbank/) database.

| Data type | Description | Link |
|---|---|---|
| Modeled clades | The clades modeled each week | https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/main/auxiliary-data/modeled-clades/ |
| Energy and Brier scores | Energy and Brier scores for each model, nowcast date, target date, and location | https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/main/auxiliary-data/scores/scores.tsv |
|50th and 90th interval coverage | The 50th and 90th interval coverage for each model, nowcast date, target date, location, and clade| https://github.com/reichlab/variant-nowcast-hub/raw/refs/heads/main/auxiliary-data/scores/coverage.parquet |
| Final variant data | The latest variant data using the latest reference tree | https://data.nextstrain.org/files/workflows/forecasts-ncov/open/nextstrain_clades/usa.tsv.gz |
| Data available as of the nowcast date | The data teams had available to them when nowcasts were solicited | https://github.com/reichlab/variant-nowcast-hub/tree/main/target-data/time-series |
| Evaluation data by nowcast date| The data used to evaluate nowcasts, containing the sequence counts 90 days after the nowcast date assigned the clade they would have been assigned on the nowcast date | https://github.com/reichlab/variant-nowcast-hub/tree/main/target-data/oracle-output |
| Model nowcasts | Teams' nowcasts, accessed via S3 bucket | Accessed via `hubData` R package `hubData::connect_hub("covid-variant-nowcast-hub", file_format = "parquet", skip_checks = TRUE)` |
| California data comparison | A comparison of the clade proportions and amount of sequencing between California COVID-Net and the GenBank data | https://github.com/epiforecasts/evalvariantnowcasthub/tree/main/output/data |

For more details on the data stored on the Hub and the outputs solicited from teams for submission to the Hub, please see the [Variant Nowcast Hub guidelines](https://github.com/reichlab/variant-nowcast-hub/blob/main/README.md).

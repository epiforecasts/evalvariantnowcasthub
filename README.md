# Collaborative estimation and evaluation of SARS-CoV-2 variant nowcasting in the U.S.

This repository contains the code to generate the results of analysing the SARS-CoV-2 variant nowcasts during the first "season" of the [U.S. SARS-CoV-2 Variant Nowcast Hub](https://github.com/reichlab/variant-nowcast-hub) (from October 9th, 2024 to June 4th, 2025). For up-to-date nowcasts and evaluation of nowcasts, please see the interactive [dashboard](https://reichlab.io/variant-nowcast-hub-dashboard/explore.html) associated with the Variant Nowcast Hub.

This README is organized into the following sections:

- [Project structure](#project-structure) describing the contents of this repository
- [Data sources](#data-sources) providing links and a description to external data sources used

## Project structure

| Folder or file | Purpose |
|---|---|
|[`_targets.R`](_targets.R) | The [targets](https://books.ropensci.org/targets/) pipeline used to generate the figures and results in this work. |
|[`targets`](targets) | The folder containing the files of targets lists grouped by their outputs. |
|[`input`](input) | Contains the configuration file to specify the evaluation runs plus a publicly available data source on the percent of ED visits due to COVID-19 in each U.S. jurisdiction. |
|[`R`](R) | Functions needed to generate targets. |
|[`docs`](docs) | Files needed to generate the supplement plus a rendered version of the latest Supplement. |


## Data sources

Most data used in this paper comes directly from the Hub data itself. The full-time series of variant data in the U.S. is accessed directly from [Nextstrain](https://nextstrain.org/), which curates data from the Natioanl Center for Biotechnoligy and Information (NCBI) [GenBank](https://www.ncbi.nlm.nih.gov/genbank/) database. 

| Data type | Description | Link |
|---|---|---|
| Modeled clades | The clades modeled each week | https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/main/auxiliary-data/modeled-clades/ |
| Energy and Brier scores | Energy and Brier scores for each model, nowcast date, target date, and location | https://raw.githubusercontent.com/reichlab/variant-nowcast-hub/refs/heads/main/auxiliary-data/scores/scores.tsv |
|50th and 90th interval coverage | The 50th and 90th interval coverage for each model, nowcast date, target date, location, and clade| https://github.com/reichlab/variant-nowcast-hub/raw/refs/heads/main/auxiliary-data/scores/coverage.parquet |
| Final variant data | The latest variant data using the latest reference tree | https://data.nextstrain.org/files/workflows/forecasts-ncov/open/nextstrain_clades/usa.tsv.gz
| Data available as of the nowcast date | The data teams had available to them when nowcasts were solicited | https://github.com/reichlab/variant-nowcast-hub/tree/main/target-data/time-series |
| Evaluation data by nowcast date| The data used to evaluate nowcasts, containing the sequence counts 90 days after the nowcast date assigned the clade they would have been assigned on the nowcast date | https://github.com/reichlab/variant-nowcast-hub/tree/main/target-data/oracle-output |
| Model nowcasts | Team's nowcasts, accessed via S3 bucket | Accessed via `hubData` R package `hubData::connect_hub("covid-variant-nowcast-hub", file_format = "parquet", skip_checks = TRUE)` |
| California data comparison | A comparison of the clade proportions and amount of sequencing between California COVID-Net and the GenBank data | https://github.com/epiforecasts/evalvariantnowcasthub/tree/main/output/data |

For more details on the data stored on the Hub and the outputs solicited from teams for submission to the Hub, please see the [Variant Nowcast Hub guidelines](https://github.com/reichlab/variant-nowcast-hub/blob/main/README.md).


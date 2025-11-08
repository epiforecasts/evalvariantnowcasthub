#' Plot observed clade frequency
#'
#' @param obs_data Data.frame of number of sequences by clade and location
#' @param location Location to plot (abbreviation)
#' @param temporal_granularity Temporal granularity to plot
#' @param plot_name Name of plot
#' @param output_fp File.path to save plot
#'
#' @returns ggplot object
#' @autoglobal
get_plot_obs_clade_freq <- function(obs_data,
                                    location,
                                    temporal_granularity,
                                    plot_name,
                                    output_fp = file.path(
                                      "output", "figs",
                                      "data_figs"
                                    )) {
  if (location == "US") {
    obs_data <- obs_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
  } else {
    obs_data <- filter(
      obs_data,
      location %in% !!location
    )
  }

  if (temporal_granularity == "days") {
    obs_data <- obs_data
  } # add something to aggregate by week

  seq_data <- obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  variant_data_by_loc <- obs_data |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(seq_data) |>
    mutate(obs_freq = seq_clade / n_seq)

  plot_comps <- plot_components()

  p <- ggplot(variant_data_by_loc) +
    geom_line(aes(x = date, y = obs_freq, color = clades_modeled)) +
    get_plot_theme() +
    scale_color_manual(
      name = "Clades",
      values = plot_comps$clade_colors
    ) +
    xlab("") +
    ylab("Observed clade frequency") +
    ggtitle(glue::glue("{location}")) +
    guides(
      color = "none"
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 10
      ),
      legend.position = "bottom"
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p,
    width = 8,
    height = 6
  )
  return(p)
}

#' Bar chart of sequence counts stacked and colored by clade
#'
#' @param obs_data Data.frame of number of sequences by clade and location
#' @param location Location to plot (abbreviation)
#' @param temporal_granularity Temporal granularity to plot
#' @param plot_name Name of plot
#' @param output_fp File.path to save plot
#'
#' @returns ggplot object
#' @autoglobal
get_bar_chart_seq_count <- function(obs_data,
                                    location,
                                    temporal_granularity,
                                    plot_name,
                                    output_fp = file.path(
                                      "output", "figs",
                                      "data_figs"
                                    )) {
  if (location == "US") {
    obs_data <- obs_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
  } else {
    obs_data <- filter(
      obs_data,
      location %in% !!location
    )
  }

  if (temporal_granularity == "days") {
    obs_data <- obs_data
  } # add something to aggregate by week

  plot_comps <- plot_components()

  p <- ggplot(obs_data) +
    geom_bar(aes(x = date, y = sequences, fill = clades_modeled),
      stat = "identity", position = "stack"
    ) +
    get_plot_theme() +
    scale_fill_manual(
      name = "Clades",
      values = plot_comps$clade_colors
    ) +
    xlab("") +
    ylab("Sequence counts") +
    scale_y_continuous(transform = "log10") +
    guides(
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2
      )
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 10
      ),
      legend.position = "bottom"
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p,
    width = 8,
    height = 6
  )
  return(p)
}

#' Hospital admissions over time
#'
#' @param location Location to plot (abbreviation)
#' @param date_range Vector of date range to plot, will use min and max
#' @param temporal_granularity Temporal granularity to plot
#' @param location_data Data.frame of location metadata to translate codes to
#'   abbreviations
#' @param plot_name Name of plot
#' @param output_fp File.path to save plot
#' @param url URL for hospital admissions
#'
#' @returns ggplot object
#' @autoglobal
get_plot_hosp_admissions <- function(location_to_plot,
                                     date_range,
                                     temporal_granularity,
                                     location_data,
                                     plot_name,
                                     output_fp = file.path(
                                       "output", "figs",
                                       "data_figs"
                                     ),
                                     url = "https://raw.githubusercontent.com/CDCgov/covid19-forecast-hub/refs/heads/main/target-data/covid-hospital-admissions.csv") { # nolint

  raw_data <- read_csv(url) |>
    left_join(location_data, by = "location") |>
    filter(
      abbreviation == !!location_to_plot,
      date >= min(date_range),
      date <= max(date_range)
    )
  if (temporal_granularity == "days") {
    raw_data <- raw_data
  } # add something to aggregate by week


  p <- ggplot(raw_data) +
    geom_bar(aes(x = date, y = value),
      fill = "black",
      stat = "identity", position = "dodge"
    ) +
    get_plot_theme() +
    xlab("") +
    ylab("Hospital admissions") +
    # scale_y_continuous(transform = "log10")+
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 10
      ),
      legend.position = "bottom"
    )
  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p,
    width = 8,
    height = 6
  )
  return(p)
}

get_first_data_fig <- function(plot_freq,
                               plot_seq,
                               plot_hosp,
                               plot_name,
                               output_fp = file.path(
                                 "output", "figs",
                                 "data_figs", "final"
                               )) {
  fig_layout <- "
  AAAA
  AAAA
  BBBB
  CCCC"

  fig_data <- plot_freq +
    plot_seq +
    plot_hosp +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "", # adds a period after each letter
      tag_sep = "", # no separator between tag levels
      theme = theme(
        legend.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.justification = "center",
        plot.tag = element_text(size = 20)
      )
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = fig_data,
    width = 8,
    height = 10
  )

  return(fig_data)
}

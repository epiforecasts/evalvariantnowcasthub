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
                                    date_range,
                                    output_fp = file.path(
                                      "output", "figs",
                                      "data_figs"
                                    ),
                                    title = NULL) {
  if (temporal_granularity == "weeks") {
    obs_data <- daily_to_weekly(obs_data)
  }

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
    guides(
      color = "none"
    ) +
    scale_x_date(
      limits = date_range,
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 10
      ),
      legend.position = "bottom"
    ) +
    coord_cartesian(ylim = c(0, 1))
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

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
#' @param log_scale Boolean indicating whether or not y axis should be on log
#'   scale, default is TRUE
#' @param nowcast_date_line Boolean indicating whether or not to include a
#'   dashed line for the nowcast date
#' @param remove_x_ticks Boolean indicating to remove x tick text, default is
#'   FALSE
#' @param title Title to plot, default is NULL which plots no title.
#'
#' @returns ggplot object
#' @autoglobal
get_bar_chart_seq_count <- function(obs_data,
                                    location,
                                    temporal_granularity,
                                    plot_name,
                                    date_range,
                                    output_fp = file.path(
                                      "output", "figs",
                                      "data_figs"
                                    ),
                                    log_scale = FALSE,
                                    nowcast_date_line = FALSE,
                                    title = NULL,
                                    remove_xticks = FALSE) {
  if (isTRUE(nowcast_date_line)) {
    nowcast_date <- obs_data |>
      select(nowcast_date) |>
      distinct() |>
      pull()
  }

  if (temporal_granularity == "days") {
    obs_data <- obs_data
  } else {
    obs_data <- daily_to_weekly(obs_data)
  }

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
    guides(
      fill = guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        nrow = 3
      )
    ) +
    scale_x_date(
      limits = date_range,
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
  if (isTRUE(log_scale)) {
    p <- p + scale_y_continuous(transform = "log10")
  }
  if (isTRUE(nowcast_date_line)) {
    p <- p + geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (isTRUE(remove_xticks)) {
    p <- p + theme(axis.text.x = element_blank())
  }

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p,
    width = 8,
    height = 6
  )
  return(p)
}

#' Percent of ED visits due to covid over time
#'
#' @param location Location to plot (abbreviation)
#' @param date_range Vector of date range to plot, will use min and max
#' @param temporal_granularity Temporal granularity to plot
#' @param location_data Data.frame of location metadata to translate codes to
#'   abbreviations
#' @param plot_name Name of plot
#' @param output_fp File path to save plot
#' @param data_fp Filepath to NSSP data at the state and national level.
#'  Originally obtained from: https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56/about_data #nolint
#'
#' @returns ggplot object
#' @autoglobal
get_plot_hosp_admissions <- function(location_to_plot,
                                     date_range,
                                     temporal_granularity = "weeks",
                                     location_data,
                                     plot_name,
                                     output_fp = file.path(
                                       "output", "figs",
                                       "data_figs"
                                     ),
                                     data_fp = file.path("input", "nssp_states_and_national.csv")) { # nolint

  nssp_cdcgov <- read_csv(data_fp)
  raw_data <- nssp_cdcgov |>
    left_join(location_data,
      by = c("geography" = "location_name") # nolint
    ) |>
    rename(date = week_end) |>
    filter(
      abbreviation == !!location_to_plot,
      date >= min(date_range),
      date <= max(date_range)
    )

  p <- ggplot(raw_data) +
    geom_line(aes(x = date, y = percent_visits_covid),
      fill = "black",
      stat = "identity", position = "dodge"
    ) +
    get_plot_theme() +
    xlab("") +
    ylab("Percent of ED visits\ndue to COVID") +
    # scale_y_continuous(transform = "log10")+
    scale_x_date(
      limits = c(min(date_range), max(date_range)),
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 10
      ),
      legend.position = "bottom"
    ) +
    coord_cartesian(ylim = c(0, 2.5)) +
    ggtitle(glue::glue("{location_to_plot}"))
  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p,
    width = 8,
    height = 6
  )
  return(p)
}

#' Patchwork first data figure
#'
#' @param plot_freq A
#' @param plot_seq B
#' @param plot_hosp C
#' @param plot_name name of figure
#' @param output_fp directory to save
#'
#' @returns combined figure
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

#' Patchwork first data figure -- alternate version with US mins CA and CA
#'
#' @param plot_freq1 A
#' @param plot_seq1 B
#' @param plot_hosp1 C
#' @param plot_freq2 D
#' @param plot_seq2 E
#' @param plot_hosp2 F
#' @param plot_name name of figure
#' @param output_fp directory to save
#'
#' @returns combined figure
get_first_data_fig_alt <- function(plot_freq1,
                                   plot_seq1,
                                   plot_hosp1,
                                   plot_freq2,
                                   plot_seq2,
                                   plot_hosp2,
                                   plot_name,
                                   output_fp = file.path(
                                     "output", "figs",
                                     "data_figs", "final"
                                   )) {
  fig_layout <- "
  AABB
  CCDD
  EEFF"

  fig_data <- plot_freq1 +
    plot_freq2 +
    (plot_seq1 + theme(plot.tag.position = c(-0.03, 1.05))) +
    (plot_seq2 + theme(plot.tag.position = c(-0.03, 1.05))) +
    plot_hosp1 +
    plot_hosp2 +
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
    width = 9,
    height = 10
  )

  return(fig_data)
}

#' Get a plot of sequence counts by location for the duration of the nowcast
#' period
#'
#' @inheritParams get_plot_obs_clade_freq
#'
#' @returns Bar chart of sequence counts by location
#' @autoglobal
get_plot_seq_counts_by_loc <- function(obs_data,
                                       plot_name,
                                       output_fp = file.path(
                                         "output", "figs",
                                         "data_figs", "supp"
                                       )) {
  seq_counts_by_loc <- obs_data |>
    group_by(location) |>
    summarise(
      total_seq = sum(sequences),
      total_pop = max(population)
    ) |>
    arrange(desc(total_seq)) |>
    mutate(
      location = factor(location, levels = location),
      cdf_seq = cumsum(total_seq) / sum(total_seq),
      top_90_seq = ifelse(cdf_seq <= 0.9, "top 90%", "bottom 10%"),
      cdf_pop = cumsum(total_pop) / sum(total_pop),
      top_90_pop = ifelse(cdf_pop <= 0.9, "top 90%", "bottom 10%")
    )

  plot_comps <- plot_components()

  p1 <- ggplot(seq_counts_by_loc) +
    geom_bar(aes(x = location, y = total_seq, fill = top_90_seq),
      stat = "identity",
      position = "dodge"
    ) +
    get_plot_theme() +
    scale_fill_manual(
      name = "",
      values = plot_comps$percentile_colors
    ) +
    scale_y_continuous(trans = "log10") +
    xlab("") +
    ylab("Total sequences collected")


  p2 <- ggplot(seq_counts_by_loc) +
    geom_bar(aes(x = location, y = total_pop, fill = top_90_pop),
      stat = "identity",
      position = "dodge"
    ) +
    get_plot_theme() +
    scale_fill_manual(
      name = "",
      values = plot_comps$percentile_colors
    ) +
    scale_y_continuous(trans = "log10") +
    xlab("") +
    ylab("Population size")

  fig_layout <- "
  A
  B"

  fig_distrib <- p1 + p2 +
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
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.justification = "left",
        plot.tag = element_text(size = 20)
      )
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = fig_distrib,
    width = 12,
    height = 8
  )
  return(fig_distrib)
}

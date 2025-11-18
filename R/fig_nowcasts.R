#' Get a plot of the nowcasts for a specific nowcast date
#'
#' @param nowcasts Data.frame with nowcasts for a single nowcast date for
#'   multiple teams and (a few) locations
#' @param data_as_of Data.frame with data for a few locations up until a single
#'   nowcast date
#' @param remove_legend Boolean indicating whether to keep legend, default
#'   is TRUE.
#' @param title Character string indicating title, default is NULL.
#' @param temporal_granularity Temporal granularity to plot
#' @param prev_data_to_show Number of days before nowcast date to show data,
#'   default is 70.
#' @importFrom ggplot2 ggplot geom_line aes facet_grid theme_bw vars xlab
#'   ylab theme_bw ggsave ggtitle geom_vline
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_model_outputs <- function(nowcasts,
                                   data_as_of,
                                   remove_legend = TRUE,
                                   title = NULL,
                                   temporal_granularity = "weeks",
                                   prev_data_to_show = 70) {
  nowcast_date <- unique(nowcasts$nowcast_date)
  if (temporal_granularity == "weeks") {
    obs_data <- daily_to_weekly(data_as_of)
  } else {
    obs_data <- data_as_of
  }

  seq_data <- obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  variant_data_by_loc <- obs_data |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(seq_data) |>
    mutate(
      obs_freq = seq_clade / n_seq,
      data_availability = "as of nowcast date"
    ) |>
    filter(date >= nowcast_date - days(prev_data_to_show))

  nc_data <- mutate(
    nowcasts,
    output_type_id_clade = glue::glue("{output_type_id}-{clade}")
  )
  plot_comps <- plot_components()
  p <- ggplot(nc_data) +
    geom_point(
      data = variant_data_by_loc,
      aes(x = date, y = obs_freq, color = clades_modeled)
    ) +
    geom_line(
      data = variant_data_by_loc,
      aes(x = date, y = obs_freq, color = clades_modeled)
    ) +
    geom_line(
      data = filter(nc_data, output_type == "mean"),
      aes(x = target_date, y = value, color = clade)
    ) +
    geom_line(
      data = filter(nc_data, output_type == "sample"),
      aes(
        x = target_date, y = value, color = clade,
        group = output_type_id_clade
      ),
      linewidth = 0.2, alpha = 0.2
    ) +
    geom_vline(aes(xintercept = nowcast_date), linetype = "dashed") +
    facet_grid(rows = vars(model_id), cols = vars(location)) +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Clade",
      values = plot_comps$clade_colors
    ) +
    xlab("") +
    ylab("Clade proportion") +
    scale_x_date(
      limits = c(min(variant_data_by_loc$date), max(nc_data$target_date)),
      breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    theme(axis.text.x = element_blank())
  if (isTRUE(remove_legend)) {
    p <- p + guides(
      color = "none",
      fill = "none"
    )
  }
  if (!is.null(title)) {
    p <- p + ggtitle(glue::glue("{title}"))
  }

  return(p)
}

#' Bar chart of sequence counts stacked and colored by clade
#'
#' @param obs_data Data.frame of number of sequences by clade and location
#' @param location Location to plot (abbreviation)
#' @param temporal_granularity Temporal granularity to plot
#' @param log_scale Boolean indicating whether or not y axis should be on log
#'   scale, default is TRUE.
#' @param prev_data_to_show Number of days before nowcast date to show data,
#'   default is 70.
#'
#' @returns ggplot object
#' @autoglobal
get_faceted_seq_as_of <- function(obs_data,
                                  location,
                                  temporal_granularity = "weeks",
                                  log_scale = TRUE,
                                  prev_data_to_show = 70,
                                  remove_legend = TRUE) {
  nowcast_date <- obs_data |>
    select(nowcast_date) |>
    distinct() |>
    pull()

  if (temporal_granularity == "weeks") {
    obs_data <- daily_to_weekly(obs_data)
  }

  obs_data <- obs_data |>
    filter(date >= ymd(nowcast_date) - days(prev_data_to_show))
  plot_comps <- plot_components()


  p <- ggplot(obs_data) +
    geom_bar(
      aes(
        x = date, y = sequences, fill = clades_modeled
      ),
      stat = "identity", position = position_stack()
    ) +
    get_plot_theme(dates = TRUE) +
    facet_wrap(~location) +
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
        nrow = 2
      )
    ) +
    scale_x_date(
      limits = c(min(obs_data$date), max(obs_data$date)),
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 10
      ),
      legend.position = "bottom"
    ) +
    geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )
  if (isTRUE(log_scale)) {
    p <- p + scale_y_continuous(transform = "log10")
  }
  if (isTRUE(remove_legend)) {
    p <- p + guides(
      color = "none",
      fill = "none"
    )
  }

  return(p)
}

#' Make data comparing data as of nowcast date vs when evaluating
#'
#' @param faceted_nowcasts A
#' @param row_seq_counts B
#' @inheritParams get_plot_obs_clade_freq
#'
#' @returns patchwork figure
get_fig_nowcasts <- function(faceted_nowcasts,
                             row_seq_counts,
                             plot_name,
                             output_fp = file.path(
                               "output", "figs",
                               "nowcasts", "final"
                             )) {
  fig_layout <- "
  AAA
  AAA
  AAA
  AAA
  BBB"

  fig_nowcasts <- faceted_nowcasts +
    row_seq_counts +
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
    plot = fig_nowcasts,
    width = 8,
    height = 11
  )
  return(fig_nowcasts)
}

#' Make figure for just the CADPH model for the supplement
#'
#' @param faceted_nowcasts A
#' @param row_seq_counts B
#' @inheritParams get_plot_obs_clade_freq
#'
#' @returns patchwork figure
get_fig_nowcasts_supp <- function(faceted_nowcasts,
                                  row_seq_counts,
                                  plot_name,
                                  output_fp = file.path(
                                    "output", "figs",
                                    "nowcasts", "final"
                                  )) {
  fig_layout <- "
  A
  B"

  fig_nowcasts <- faceted_nowcasts +
    row_seq_counts +
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
    plot = fig_nowcasts,
    width = 6,
    height = 7
  )
  return(fig_nowcasts)
}

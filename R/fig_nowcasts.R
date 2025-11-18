#' Get a plot of the nowcasts for a specific nowcast date
#'
#' @param nowcasts Data.frame with nowcasts for a single nowcast date for
#'   multiple teams and (a few) locations
#' @param data_as_of Data.frame with data for a few locations up until a single
#'   nowcast date
#' @param plot_name Character string indicating name of plot
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @param remove_legend Boolean indicating whether to keep legend, default
#'   is TRUE.
#' @param title Character string indicating title, default is NULL.
#' @importFrom ggplot2 ggplot geom_line aes facet_grid theme_bw vars xlab
#'   ylab theme_bw ggsave ggtitle geom_vline
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_model_outputs <- function(nowcasts,
                                   data_as_of,
                                   plot_name,
                                   output_fp = file.path("output", "figs", "nowcasts"),
                                   save = TRUE,
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
    xlab("Target date") +
    ylab("Estimated proportion") +
    scale_x_date(
      breaks = "1 week",
      date_labels = "%d %b %Y"
    )
  if (isTRUE(remove_legend)) {
    p <- p + guides(
      color = "none",
      fill = "none"
    )
  }
  if (!is.null(title)) {
    p <- p + ggtitle(glue::glue("{title}"))
  }


  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p
    )
  }
  return(p)
}

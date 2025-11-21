#' Get a plot of nowcasts for multiple nowcast dates vs observations
#'
#' @param model_preds_mult_nowcasts Model predicted observations for multiple
#'   nowcasts
#' @param final_eval_data Final sequence counts by clade and location
#' @param clade_to_zoom Character string indicating the clade to zoom
#' @param horizon_to_plot Range of integers to filter to for the plot
#'
#' @returns ggplot object
get_plot_model_preds_mult_nowcasts <- function(model_preds_mult_nowcasts,
                                               final_eval_data,
                                               clade_to_zoom = "25A",
                                               horizon_to_plot = c(-6, 0)) {
  df <- model_preds_mult_nowcasts |>
    filter(clade == clade_to_zoom) |>
    mutate(horizon = as.integer(target_date - nowcast_date)) |>
    filter(horizon <= max(horizon_to_plot), horizon >= min(horizon_to_plot))

  weekly_obs_data <- daily_to_weekly(final_eval_data) |>
    filter(location %in% unique(df$location))
  total_seq <- weekly_obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  weekly_obs <- left_join(weekly_obs_data, total_seq) |>
    filter(clades_modeled == clade_to_zoom)

  plot_comps <- plot_components()

  p <- ggplot(df) +
    geom_line(aes(
      x = target_date, y = q_0.5, color = model_id,
      group = nowcast_date
    )) +
    geom_ribbon(
      aes(
        x = target_date,
        ymin = q_0.25,
        ymax = q_0.75, fill = model_id,
        group = nowcast_date
      ),
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(
        x = target_date,
        ymin = q_0.025,
        ymax = q_0.975, fill = model_id,
        group = nowcast_date
      ),
      alpha = 0.1
    ) +
    geom_point(
      data = weekly_obs,
      aes(x = date, y = sequences / n_seq),
      color = "#CAB2D6"
    ) +
    geom_line(
      data = weekly_obs,
      aes(x = date, y = sequences / n_seq),
      color = "#CAB2D6"
    ) +
    facet_grid(vars(model_id), vars(location)) +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    xlab("") +
    ylab("Model predictions across nowcast dates") +
    guides(
      color = "none",
      fill = "none"
    ) +
    scale_x_date(
      limits = c(min(df$target_date), max(df$target_date)),
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    ggtitle("25A emergence")

  return(p)
}

#' Get a plot of scores by nowcast date for locations and models
#'
#' @param scores Data.frame of scores
#' @param locs Vector of character strings of locations
#' @param nowcast_dates Set of nowcast dates to summarise over
#' @param date_range
#' @param horizon_to_plot
#'
#' @returns
#' @export
#'
#' @examples
get_plot_scores_by_nowcast_date <- function(scores,
                                            locs,
                                            nowcast_dates,
                                            date_range,
                                            horizon_to_plot = c(-6, 0)) {
  scores_avg <- scores |>
    filter(
      location %in% locs,
      nowcast_date %in% nowcast_dates,
      !is.na(energy_score)
    ) |>
    group_by(model, location) |>
    summarise(energy_score = mean(energy_score, na.rm = TRUE))

  scores_df <- scores |>
    mutate(horizon = as.numeric(target_date - nowcast_date)) |>
    filter(
      location %in% locs,
      nowcast_date %in% nowcast_dates,
      !is.na(energy_score),
      horizon <= max(horizon_to_plot),
      horizon >= min(horizon_to_plot)
    ) |>
    group_by(nowcast_date, location, model) |>
    summarise(energy_score = mean(energy_score, na.rm = TRUE))

  plot_comps <- plot_components()
  ggplot(scores_df) +
    geom_point(aes(
      x = nowcast_date, y = energy_score,
      color = model
    )) +
    geom_line(aes(
      x = nowcast_date, y = energy_score,
      color = model
    )) +
    geom_hline(
      data = scores_avg,
      aes(yintercept = energy_score, color = model),
      linetype = "dashed"
    ) +
    facet_wrap(~location) +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    xlab("") +
    ylab("Average energy score") +
    scale_x_date(
      limits = date_range,
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    )
}

#' Multiplot panel looking at 25A emergence across nowcast dates
#'
#' @param grid A
#' @param underlay B
#' @param plot_name name of plot
#' @param output_fp filepath directory
#'
#' @returns patchwork
get_fig_zoom_25A <- function(grid,
                             underlay,
                             plot_name,
                             output_fp = file.path(
                               "output", "figs",
                               "zoom_25A", "final"
                             )) {
  fig_layout <- "
  AAA
  AAA
  AAA
  BBB
  "

  fig_zoom <- grid +
    underlay +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "",
      tag_sep = "",
      theme = theme(
        legend.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.justification = "center",
        plot.tag = element_text(size = 20)
      )
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = fig_zoom,
    width = 8,
    height = 11
  )
  return(fig_zoom)
}

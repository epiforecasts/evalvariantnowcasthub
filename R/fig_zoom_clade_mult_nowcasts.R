#' Get a plot of nowcasts for multiple nowcast dates vs observations
#'
#' @param model_preds_mult_nowcasts Model predicted observations for multiple
#'   nowcasts
#' @param final_eval_data Final sequence counts by clade and location
#' @param clade_to_zoom Character string indicating the clade to zoom
#' @param horizon_to_plot Range of integers to filter to for the plot
#'
#' @returns ggplot object
get_plot_model_preds_mult <- function(model_preds_mult_nowcasts,
                                      final_eval_data,
                                      clade_to_zoom = "25A",
                                      horizon_to_plot = c(-6, 0)) {
  df_filt <- model_preds_mult_nowcasts |>
    filter(clade == clade_to_zoom) |>
    mutate(horizon = as.integer(target_date - nowcast_date)) |>
    filter(horizon <= max(horizon_to_plot), horizon >= min(horizon_to_plot))

  # Use daily observations instead of weekly
  daily_obs_data <- final_eval_data |>
    filter(location %in% unique(df_filt$location))
  total_seq <- daily_obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  daily_obs <- left_join(daily_obs_data, total_seq) |>
    filter(clades_modeled == clade_to_zoom)

  plot_comps <- plot_components()

  p <- ggplot(df_filt) +
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
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = target_date,
        ymin = q_0.025,
        ymax = q_0.975, fill = model_id,
        group = nowcast_date
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_point(
      data = daily_obs,
      aes(x = date, y = sequences / n_seq, fill = "25A"),
      color = "#CAB2D6",
      shape = 21,
      size = 0.8
    ) +
    facet_grid(vars(model_id), vars(location)) +
    coord_cartesian(ylim = c(0, 1)) +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_fill_manual(
      name = "Clade",
      values = c(plot_comps$model_colors, "25A" = "#CAB2D6"),
      breaks = "25A"
    ) +
    xlab("") +
    ylab("Model predictions across nowcast dates") +
    guides(
      color = "none",
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1
      )
    ) +
    scale_x_date(
      limits = c(min(df_filt$target_date), max(df_filt$target_date)),
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    ggtitle("25A emergence") +
    theme(
      plot.margin = margin(5.5, 5.5, 5.5, 40, "pt") # Increase left margin
    )

  return(p)
}

#' Get a plot of scores by nowcast date for locations and models
#'
#' @param scores Data.frame of scores
#' @param locs Vector of character strings of locations
#' @param nowcast_dates Set of nowcast dates to summarise over
#' @param date_range Range of dates to plot
#' @param horizon_to_plot horizon days to plot
#'
#' @returns ggplot
get_plot_scores_by_date <- function(scores,
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
  p <- ggplot(scores_df) +
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
    facet_wrap(~location, ncol = 3, scales = "free_y") +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    xlab("") +
    guides(
      color = "none"
    ) +
    ylab("Average\nenergy score") +
    scale_x_date(
      limits = date_range,
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    theme(
      plot.margin = margin(5.5, 5.5, 5.5, 40, "pt") # Increase left margin
    )
  return(p)
}

#' Get a plot of bias by nowcast date for locations and models
#'
#' @param bias_data Data.frame of bias scores
#' @param locs Vector of character strings of locations
#' @param nowcast_dates Set of nowcast dates to include
#' @param date_range Range of dates to plot
#'
#' @returns ggplot
#' @autoglobal
get_plot_bias_by_date <- function(bias_data,
                                  locs,
                                  nowcast_dates,
                                  date_range,
                                  plot_name = "bias_over_time_25A",
                                  output_fp = file.path(
                                    "output", "figs",
                                    "zoom_25A", "supp"
                                  )) {
  # Calculate average bias across all nowcast dates for reference lines
  bias_avg <- filter(
    bias_data,
    location %in% locs,
    nowcast_date %in% nowcast_dates
  ) |>
    group_by(model, location) |>
    summarise(avg_bias = mean(bias, na.rm = TRUE), .groups = "drop")

  # Filter data for plotting
  bias_df <- filter(
    bias_data,
    location %in% locs,
    nowcast_date %in% nowcast_dates
  )

  plot_comps <- plot_components()

  p <- ggplot(bias_df) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
    geom_hline(
      data = bias_avg,
      aes(yintercept = avg_bias, color = model),
      linetype = "dashed",
      alpha = 0.7
    ) +
    geom_point(aes(
      x = nowcast_date, y = bias,
      color = model
    )) +
    geom_line(aes(
      x = nowcast_date, y = bias,
      color = model
    )) +
    facet_wrap(~location, ncol = 3) +
    coord_cartesian(ylim = c(-1, 1)) +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    guides(color = "none") +
    xlab("") +
    ylab("Bias") +
    scale_x_date(
      limits = date_range,
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    )
  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p,
    width = 8,
    height = 6
  )

  return(p)
}

#' Get a plot of prediction interval coverage summarized across nowcast dates
#'
#' @param coverage_data Data.frame of coverage scores with interval_range
#' @param locs Vector of character strings of locations
#'
#' @returns ggplot
#' @autoglobal
get_plot_coverage_overall <- function(coverage,
                                      locs) {
  # Filter and summarize coverage across nowcast dates
  coverage_summary <- coverage |>
    group_by(model_id, location, interval_range) |>
    summarise(empirical_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = empirical_coverage
    ) |>
    mutate(`95` = `95` - `50`) |>
    pivot_longer(
      cols = c(`50`, `95`),
      names_to = "interval_range",
      values_to = "empirical_coverage"
    ) |>
    mutate(
      interval_label = paste0(interval_range, "%"),
      interval_label = factor(interval_label, levels = c("95%", "50%"))
    )


  plot_comps <- plot_components()

  p <- ggplot(coverage_summary) +
    # Add horizontal reference lines for nominal coverage
    # Create stacked bar chart
    geom_bar(
      aes(
        x = model_id, y = empirical_coverage, fill = model_id,
        alpha = interval_label
      ),
      stat = "identity",
      position = "stack",
      width = 0.7
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0.95, linetype = "dashed") +
    facet_wrap(~location, ncol = 3) +
    get_plot_theme(dates = FALSE) +
    theme(axis.text.x = element_blank()) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_alpha_manual(
      name = "Interval coverage",
      values = plot_comps$pred_int_alpha
    ) +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      )
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    xlab("Model") +
    ylab("Empirical\ncoverage") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

  return(p)
}

#' Multiplot panel looking at 25A emergence across nowcast dates
#'
#' @param grid Model predictions plot
#' @param scores Energy scores plot
#' @param coverage Prediction interval coverage plot
#' @param plot_name name of plot
#' @param output_fp filepath directory
#'
#' @returns patchwork
#' @autoglobal
get_fig_zoom_25A <- function(grid,
                             scores,
                             coverage,
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
  CCC
  "

  fig_zoom <- grid +
    scores +
    coverage +
    plot_layout(
      design = fig_layout,
      axes = "collect_x",
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
    height = 14
  )
  return(fig_zoom)
}

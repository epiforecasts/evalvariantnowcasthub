#' Plot the model predicted observations vs the weekly frequencies
#'
#' @param model_pred_obs_df Data.frame of model predicted observed frequency
#'   quantile values
#' @param eval_seq Counts of sequences by clade for evaluating on a particular
#'   nowcast date
#' @param clades_to_plot Vector of chharacter strings of clades to plot,
#'   default is NULL which plots all of them
#'
#' @returns ggplot object
get_plot_model_pred_obs <- function(model_pred_obs_df,
                                    eval_seq,
                                    clades_to_plot = NULL) {
  if (!is.null(clades_to_plot)) {
    model_pred_obs_df <- model_pred_obs_df |>
      filter(clade %in% clades_to_plot)
  }

  weekly_obs_data <- daily_to_weekly(eval_seq)
  total_seq <- weekly_obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  weekly_obs <- weekly_obs_data |>
    left_join(total_seq)

  if (!is.null(clades_to_plot)) {
    weekly_obs <- weekly_obs |>
      filter(clades_modeled %in% clades_to_plot)
  }

  plot_comps <- plot_components()
  p <- ggplot(model_pred_obs_df) +
    geom_line(aes(
      x = target_date, y = q_0.5,
      color = clade
    )) +
    geom_ribbon(aes(
      x = target_date,
      ymin = q_0.25, ymax = q_0.75,
      fill = clade
    ), alpha = 0.1) +
    geom_ribbon(aes(
      x = target_date,
      ymin = q_0.025, ymax = q_0.975,
      fill = clade
    ), alpha = 0.1) +
    geom_point(
      data = weekly_obs,
      aes(
        x = date, y = sequences / n_seq,
        color = clades_modeled
      )
    ) +
    facet_grid(rows = vars(model_id), cols = vars(location)) +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Clade",
      values = plot_comps$clade_colors
    ) +
    scale_fill_manual(
      name = "Clade",
      values = plot_comps$clade_colors
    ) +
    xlab("") +
    ylab("Model predicted\nobserved frequencies") +
    guides(
      fill = "none",
      color = guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        nrow = 2
      )
    ) +
    scale_x_date(
      limits = c(min(eval_seq$date), max(eval_seq$date)),
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )
  return(p)
}

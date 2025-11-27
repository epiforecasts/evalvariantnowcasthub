#' Plot the model predicted observations vs the weekly frequencies
#'
#' @param model_pred_obs_df Data.frame of model predicted observed frequency
#'   quantile values
#' @param eval_seq Counts of sequences by clade for evaluating on a particular
#'   nowcast date
#' @param clades_to_plot Vector of character strings of clades to plot,
#'   default is NULL which plots all of them
#'
#' @returns ggplot object
get_plot_model_pred_obs <- function(model_pred_obs_df,
                                    eval_seq,
                                    clades_to_plot = NULL) {
  if (!is.null(clades_to_plot)) {
    model_pred_obs_df <- filter(
      model_pred_obs_df,
      clade %in% clades_to_plot
    )
  }

  weekly_obs_data <- daily_to_weekly(eval_seq)
  total_seq <- weekly_obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  weekly_obs <- left_join(weekly_obs_data, total_seq)

  if (!is.null(clades_to_plot)) {
    weekly_obs <- filter(
      weekly_obs,
      clades_modeled %in% clades_to_plot
    )
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
    ), alpha = 0.2) +
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
    ylab("Model predicted observed frequencies") +
    guides(
      fill = "none",
      color = "none"
    ) +
    theme(axis.text.x = element_blank()) +
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

get_plot_seq_count <- function(eval_seq,
                               temporal_granularity) {
  if (temporal_granularity == "weeks") {
    eval_seq <- daily_to_weekly(eval_seq)
  }

  total_seq <- eval_seq |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  obs <- left_join(eval_seq, total_seq)
  plot_comps <- plot_components()
  p <- ggplot(obs) +
    geom_bar(
      aes(
        x = date, y = sequences,
        fill = clades_modeled
      ),
      stat = "identity",
      position = "stack"
    ) +
    facet_wrap(~location, ncol = 3) +
    get_plot_theme(dates = TRUE) +
    scale_fill_manual(
      name = "Clade",
      values = plot_comps$clade_colors
    ) +
    xlab("") +
    ylab("Sequence counts\nfor evaluation") +
    guides(
      fill = guide_legend(
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

#' Plot scores over time colored by model as a line plot
#'
#' @param scores Data.frame of all scores for all nowcast dates and locations
#' @param locs Vector of character strings of locations
#' @param nowcast_date Nowcast date to focus on
#'
#' @returns ggplot
get_plot_scores_over_time <- function(scores,
                                      locs,
                                      nowcast_date) {
  scores_filtered <- scores |>
    filter(
      location %in% locs,
      nowcast_date == !!nowcast_date,
      !is.na(energy_score)
    )
  weekly_scores <- daily_to_weekly_scores(scores_filtered)
  plot_comps <- plot_components()
  p <- ggplot(weekly_scores) +
    geom_point(aes(
      x = date, y = energy_score,
      color = model
    )) +
    geom_line(aes(
      x = date, y = energy_score,
      color = model
    )) +
    facet_wrap(~location, ncol = 3) +
    get_plot_theme(dates = TRUE) +
    xlab("") +
    ylab("Energy score") +
    guides(
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      )
    ) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_x_date(
      limits = c(
        min(scores_filtered$target_date),
        max(scores_filtered$target_date)
      ),
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )

  return(p)
}

#' Plot scores over time colored by whether or not it was scored
#'
#' @param scores Data.frame of all scores for all nowcast dates and locations
#' @param locs Vector of character strings of locations
#' @param nowcast_date Nowcast date to focus on
#'
#' @returns ggplot
get_plot_scores_w_exclusions <- function(scores,
                                         locs,
                                         nowcast_date,
                                         plot_name =
                                           "scores_showing_exclusions",
                                         output_fp = file.path(
                                           "output", "figs",
                                           "pred_obs", "supp"
                                         )) {
  scores_filtered <- scores |>
    filter(
      location %in% locs,
      nowcast_date == !!nowcast_date,
      !is.na(energy_score)
    )
  plot_comps <- plot_components()
  p <- ggplot(scores_filtered) +
    geom_point(aes(
      x = target_date, y = energy_score,
      color = scored
    )) +
    geom_line(aes(
      x = target_date, y = energy_score,
      color = scored
    )) +
    facet_grid(rows = vars(model), cols = vars(location)) +
    get_plot_theme(dates = TRUE) +
    xlab("") +
    ylab("Energy score") +
    guides(
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      )
    ) +
    scale_x_date(
      limits = c(
        min(scores_filtered$target_date),
        max(scores_filtered$target_date)
      ),
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p,
    width = 8,
    height = 8
  )

  return(p)
}

#' Plot scores over time colored by model as a dodged bar chart
#'
#' @inheritParams get_plot_scores_over_time
#'
#' @returns ggplot
get_plot_scores_over_time_bar <- function(scores,
                                          locs,
                                          nowcast_date) {
  scores_filtered <- scores |>
    filter(
      location %in% locs,
      nowcast_date == !!nowcast_date,
      !is.na(energy_score)
    )
  weekly_scores <- daily_to_weekly_scores(scores_filtered)
  plot_comps <- plot_components()
  p <- ggplot(weekly_scores) +
    geom_bar(
      aes(
        x = date, y = energy_score,
        fill = model
      ),
      stat = "identity",
      position = "dodge"
    ) +
    facet_wrap(~location, ncol = 3) +
    get_plot_theme(dates = TRUE) +
    xlab("") +
    ylab("Energy score") +
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
    scale_x_date(
      limits = c(
        min(scores_filtered$target_date),
        max(scores_filtered$target_date)
      ),
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )

  return(p)
}

get_fig_preds <- function(faceted_preds,
                          faceted_preds2,
                          seq_count_row,
                          scores_row,
                          plot_name,
                          output_fp = file.path(
                            "output", "figs",
                            "pred_obs", "final"
                          )) {
  fig_layout <- "
  AAA
  AAA
  BBB
  BBB
  CCC
  DDD"

  fig_preds <- faceted_preds +
    faceted_preds2 +
    seq_count_row +
    scores_row +
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
    plot = fig_preds,
    width = 8,
    height = 10
  )
  return(fig_preds)
}

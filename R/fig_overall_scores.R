date_breaks <- ymd(c("2024-10-07", "2025-06-02"))

#' Get relative skill scores with specified grouping
#'
#' @param scores_obj Scoringutils scores object
#' @param score_type Character string indicating which score metric to use
#' @param by Character vector of grouping variables for relative skill
#' calculation
#' @importFrom scoringutils add_relative_skill
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @returns Data frame with relative skill scores
#' @autoglobal
get_relative_skill <- function(scores_obj,
                               score_type = c("brier_score", "energy_score"),
                               by = "target_date") {
  score_type <- rlang::arg_match(score_type)

  rel_skill <- scoringutils::add_relative_skill(
    scores_obj,
    metric = score_type,
    baseline = "Hub-baseline",
    by = by
  )

  # Filter out NA scores
  rel_skill_filtered <- rel_skill |>
    filter(!is.na(!!sym(score_type)))

  return(rel_skill_filtered)
}

#' Brier/Energy Relative/Overall by location
#'
#' @param scores_obj Scoringutils scores object
#' @param seq_counts_by_loc Total sequences for each location
#' @param rel_skill_plot Boolean indicating to return a relative skill plot
#' @param score_type Character string indicating which score metric to use
#' @param remove_legend Boolean indicating whether to keep legend, default
#'   is TRUE.
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_by_location <- function(scores_obj,
                                 seq_counts_by_loc,
                                 score_type = c("brier_score", "energy_score"),
                                 rel_skill_plot = TRUE,
                                 remove_legend = TRUE) {
  score_type <- rlang::arg_match(score_type)
  plot_components_list <- plot_components()
  if (score_type == "brier_score") {
    label <- "Brier score"
  } else {
    label <- "Energy score"
  }

  if (isTRUE(rel_skill_plot)) {
    rel_skill <- scores_obj |>
      ungroup() |>
      filter(!is.na(!!sym(score_type))) |>
      scoringutils::get_pairwise_comparisons(
        baseline = "Hub-baseline",
        metric = score_type,
        by = "location"
      ) |>
      filter(model != "Hub-baseline") |>
      left_join(seq_counts_by_loc) |>
      arrange(desc(total_seq)) |>
      mutate(location = factor(location, levels = unique(location))) |>
      filter(model != "Hub-baseline")

    p <- ggplot(rel_skill) +
      geom_point(
        aes(
          x = location,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        ),
        size = 4
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = plot_components_list$model_colors) +
      get_plot_theme() +
      labs(
        x = "Location",
        y = glue::glue("Relative scaled skill\n({label})"),
        color = "Model"
      ) +
      scale_y_continuous(trans = "log10") +
      coord_cartesian(ylim = c(1 / 4.5, 4.5))
  } else {
    scores_sum <- scores_obj |>
      ungroup() |>
      filter(!is.na(score_type)) |>
      scoringutils::summarise_scores(by = c("model", "location")) |>
      left_join(seq_counts_by_loc) |>
      arrange(desc(total_seq)) |>
      mutate(location = factor(location, levels = unique(location)))
    p <- ggplot(scores_sum) +
      geom_bar(
        aes(
          x = location,
          y = !!sym(glue::glue(
            "{score_type}"
          )),
          fill = model
        ),
        stat = "identity", position = position_dodge(width = 0.9),
        width = 0.5
      ) +
      scale_fill_manual(values = plot_components_list$model_colors) +
      get_plot_theme() +
      labs(
        x = "",
        y = label,
        fill = "Model"
      ) +
      guides(
        fill = guide_legend(
          title.position = "top",
          nrow = 1
        )
      )
  }

  if (isTRUE(remove_legend)) {
    p <- p + guides(
      color = "none",
      fill = "none"
    )
  }
  return(p)
}

#' Brier/Energy Relative/Overall by nowcast date
#'
#' @param scores_obj Scoringutils scores object
#' @param rel_skill_plot Boolean indicating to return a relative skill plot
#' @param score_type Character string indicating which score metric to use
#' @param remove_legend Boolean indicating whether to keep legend, default
#'   is TRUE.
#' @param title Character string indicating title, default is NULL.
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_by_nowcast_date <- function(scores_obj,
                                     score_type = c(
                                       "brier_score",
                                       "energy_score"
                                     ),
                                     rel_skill_plot = TRUE,
                                     remove_legend = TRUE,
                                     title = NULL) {
  score_type <- rlang::arg_match(score_type)
  plot_components_list <- plot_components()
  if (score_type == "brier_score") {
    label <- "Brier score"
  } else {
    label <- "Energy score"
  }

  if (isTRUE(rel_skill_plot)) {
    rel_skill <- scores_obj |>
      ungroup() |>
      filter(!is.na(!!sym(score_type))) |>
      scoringutils::get_pairwise_comparisons(
        baseline = "Hub-baseline",
        metric = score_type,
        by = "nowcast_date"
      ) |>
      filter(model != "Hub-baseline")

    p <- ggplot(rel_skill) +
      geom_point(
        aes(
          x = nowcast_date,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        ),
        alpha = 0.5
      ) +
      geom_line(
        aes(
          x = nowcast_date,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        )
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = plot_components_list$model_colors) +
      get_plot_theme(dates = TRUE) +
      scale_x_date(
        limits = date_breaks,
        breaks = "2 weeks",
        date_labels = "%d %b %Y"
      ) +
      labs(
        x = "",
        y = glue::glue("Relative scaled\nskill ({label})"),
        color = "Model"
      ) +
      scale_y_continuous(trans = "log10") +
      coord_cartesian(ylim = c(1 / 2.4, 2.4)) +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 12)
      )
  } else {
    scores_sum <- scores_obj |>
      filter(!is.na(score_type)) |>
      scoringutils::summarise_scores(by = c("model", "nowcast_date"))
    p <- ggplot(scores_sum) +
      geom_point(
        aes(
          x = nowcast_date,
          y = !!sym(glue::glue(
            "{score_type}"
          )),
          color = model
        ),
        size = 1.5
      ) +
      geom_line(
        aes(
          x = nowcast_date,
          y = !!sym(glue::glue(
            "{score_type}"
          )),
          color = model
        )
      ) +
      scale_color_manual(values = plot_components_list$model_colors) +
      get_plot_theme(dates = TRUE) +
      scale_x_date(
        limits = date_breaks,
        breaks = "2 weeks",
        date_labels = "%d %b %Y"
      ) +
      labs(
        x = "",
        y = label,
        color = "Model"
      ) +
      guides(
        color = guide_legend(
          title.position = "top",
          nrow = 1
        )
      ) +
      theme(axis.text.x = element_blank())
  }
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


#' Brier/Energy Relative/Overall by model
#'
#' @param scores_obj Scoringutils scores object
#' @param rel_skill_plot Boolean indicating to return a relative skill plot
#' @param score_type Character string indicating which score metric to use
#' @param remove_legend Boolean indicating whether to keep legend, default
#'   is TRUE.
#' @param title Character string indicating title, default is NULL.
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_overall <- function(scores_obj,
                             score_type = c("brier_score", "energy_score"),
                             rel_skill_plot = TRUE,
                             remove_legend = TRUE,
                             title = NULL) {
  score_type <- rlang::arg_match(score_type)
  plot_components_list <- plot_components()
  if (score_type == "brier_score") {
    label <- "Brier score"
  } else {
    label <- "Energy score"
  }

  if (isTRUE(rel_skill_plot)) {
    rel_skill <- scores_obj |>
      ungroup() |>
      filter(!is.na(!!sym(score_type))) |>
      scoringutils::get_pairwise_comparisons(
        baseline = "Hub-baseline",
        metric = score_type
      ) |>
      filter(model != "Hub-baseline")

    p <- ggplot(rel_skill) +
      geom_point(
        aes(
          x = model,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        ),
        size = 6
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = plot_components_list$model_colors) +
      get_plot_theme() +
      labs(
        x = "",
        y = glue::glue("Relative scaled skill\n({label})"),
        color = "Model"
      ) +
      scale_y_continuous(trans = "log10") +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      coord_cartesian(ylim = c(1 / 1.3, 1.3))
  } else {
    scores_sum <- scores_obj |>
      filter(!is.na(score_type)) |>
      scoringutils::summarise_scores(by = "model") |>
      filter(!is.na(!!sym(glue::glue("{score_type}"))))

    p <- ggplot(scores_sum) +
      geom_bar(
        aes(
          x = model,
          y = !!sym(glue::glue(
            "{score_type}"
          )),
          fill = model
        ),
        stat = "identity", position = "dodge"
      ) +
      scale_fill_manual(values = plot_components_list$model_colors) +
      get_plot_theme() +
      labs(
        x = "",
        y = label,
        fill = "Model"
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      guides(
        fill = guide_legend(
          title.position = "top",
          nrow = 1
        )
      )
  }

  if (isTRUE(remove_legend)) {
    p <- p + guides(
      fill = "none",
      color = "none"
    )
  }

  if (!is.null(title)) {
    p <- p + ggtitle(glue::glue("{title}"))
  }
  return(p)
}

#' Brier/Energy Relative/Absolute by horizon
#'
#' @param scores_obj Scoringutils scores object
#' @param rel_skill_plot Boolean indicating to return a relative skill plot
#' @param score_type Character string indicating which score metric to use
#' @param show_legend Boolean indicating to add legend, default is FALSE
#' @param title Character string indicating title, default is NULL.
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_horizon <- function(scores_obj,
                             output_fp = file.path(
                               "output", "figs",
                               "overall_scores", "supp"
                             ),
                             score_type = c("brier_score", "energy_score"),
                             rel_skill_plot = TRUE,
                             title = NULL,
                             show_legend = FALSE) {
  score_type <- rlang::arg_match(score_type)
  plot_components_list <- plot_components()
  if (score_type == "brier_score") {
    label <- "Brier score"
  } else {
    label <- "Energy score"
  }

  if (isTRUE(rel_skill_plot)) {
    rel_skill <- get_relative_skill(
      scores_obj,
      score_type = score_type,
      by = c("target_date", "nowcast_date")
    ) |>
      mutate(horizon = as.integer(target_date - nowcast_date))

    rel_skill_summarised <- rel_skill |>
      scoringutils::summarise_scores(by = c("model", "horizon")) |>
      filter(model != "Hub-baseline")

    p <- ggplot(rel_skill_summarised) +
      geom_line(
        aes(
          x = horizon,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        ),
        linewidth = 1.5
      ) +
      geom_point(
        aes(
          x = horizon,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        )
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = plot_components_list$model_colors) +
      get_plot_theme() +
      labs(
        x = "Horizon (days)",
        y = glue::glue("Relative scaled skill\n({label})"),
        color = "Model"
      ) +
      guides(color = "none") +
      scale_y_continuous(trans = "log10") +
      coord_cartesian(ylim = c(1 / 1.4, 1.4))
  } else {
    scores_sum <- scores_obj |>
      mutate(horizon = as.integer(target_date - nowcast_date)) |>
      scoringutils::summarise_scores(by = c("model", "horizon")) |>
      filter(!is.na(!!sym(glue::glue("{score_type}"))))

    p <- ggplot(scores_sum) +
      geom_line(
        aes(
          x = horizon,
          y = !!sym(glue::glue(
            "{score_type}"
          )),
          color = model
        ),
        linewidth = 1.5
      ) +
      geom_point(
        aes(
          x = horizon,
          y = !!sym(glue::glue(
            "{score_type}"
          )),
          color = model
        )
      ) +
      scale_color_manual(values = plot_components_list$model_colors) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      get_plot_theme() +
      labs(
        x = "Horizon (days)",
        y = label,
        color = "Model"
      ) +
      guides(color = "none")
  }
  if (isTRUE(show_legend)) {
    p <- p + guides(
      color = guide_legend(
        title.position = "top",
        position = "right",
        nrow = 6
      )
    )
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  return(p)
}


#' Sequence counts by location
#'
#' @param seq_counts_by_loc Total sequences for each location
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_seq_counts_loc <- function(seq_counts_by_loc) {
  plot_comps <- plot_components()
  seq_counts <- seq_counts_by_loc |>
    arrange(desc(total_seq)) |>
    mutate(location = factor(location, levels = unique(location)))
  p <- ggplot(seq_counts) +
    geom_bar(aes(x = location, y = total_seq),
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
    ylab("Total sequences\n Sept. 2024-June 2025") +
    theme(axis.title.x = element_text(size = 12))

  return(p)
}

#' Sequence counts by nowcast date
#'
#' @param seq_counts_by_date Total sequences for each nowcast date
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_seq_counts_date <- function(seq_counts_by_date) {
  plot_comps <- plot_components()
  p <- ggplot(seq_counts_by_date) +
    geom_bar(aes(x = nowcast_date, y = total_sequences),
      stat = "identity", position = "dodge"
    ) +
    get_plot_theme(dates = TRUE) +
    xlab("") +
    scale_x_date(
      limits = date_breaks,
      breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    ylab("Sequences\nwithin nowcast period") +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_text(size = 12)
    )
  return(p)
}

#' Sequence counts by nowcast date
#'
#' @param seq_counts_by_eval_date Total sequences for each nowcast date
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_seq_eval_date <- function(seq_counts_by_eval_date) {
  plot_comps <- plot_components()
  p <- ggplot(seq_counts_by_eval_date) +
    geom_bar(aes(x = nowcast_date, y = total_sequences),
      stat = "identity", position = "dodge"
    ) +
    get_plot_theme(dates = TRUE) +
    xlab("") +
    scale_x_date(
      limits = date_breaks,
      breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    ylab("Sequences\n for evaluation") +
    theme(axis.title = element_text(size = 12))
  return(p)
}

#' Get a plot of the summary of the overall scores
#'
#' @param a A
#' @param b B
#' @param c C
#' @param d D
#' @param e E
#' @param f F
#' @param g G
#' @param h H
#' @param i I
#' @param j J
#' @param k K
#' @param l L
#' @param output_fp directory to save figures
#' @param plot_name name of the plot
#'
#' @returns ggplot object
get_overall_scores_figure <- function(a, b, c, d, e, f, g, h, i, j, k, l,
                                      output_fp = file.path(
                                        "output", "figs",
                                        "overall_scores"
                                      ),
                                      plot_name = "overall_scores") {
  fig_layout <- "
  ABCD
  EFGH
  IIJJ
  KKLL"

  combined_fig <- a + b + c + d +
    e + f + g + h +
    i + j +
    k + l +
    plot_layout(
      design = fig_layout,
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "",
      theme = theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = element_text(hjust = 0.5),
        plot.tag = element_text(size = 14, face = "bold")
      )
    )

  # Create output directory if it doesn't exist
  dir_create(output_fp, recurse = TRUE)

  # Save figure
  ggsave(
    file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = combined_fig,
    width = 12,
    height = 15,
    dpi = 300
  )

  return(combined_fig)
}

#' Get a plot of absolute scores by horizon
#'
#' @param a A
#' @param b B
#' @param c C
#' @param d D
#' @param output_fp directory to save figures
#' @param plot_name name of the plot
#'
#' @returns ggplot object
get_panel_horizon <- function(a, b, c, d,
                              plot_name,
                              output_fp = file.path(
                                "output", "figs",
                                "overall_scores", "supp"
                              )) {
  fig_layout <- "
  AC
  BD"

  combined_fig <- a + b + c + d +
    plot_layout(
      design = fig_layout,
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "",
      theme = theme(
        legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(hjust = 0.5),
        plot.tag = element_text(size = 14, face = "bold")
      )
    )

  # Create output directory if it doesn't exist
  dir_create(output_fp, recurse = TRUE)

  # Save figure
  ggsave(
    file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = combined_fig,
    width = 12,
    height = 8,
    dpi = 300
  )

  return(combined_fig)
}

#' Get a plot of the summary of the overall scores
#'
#' @param a A
#' @param b B
#' @param c C
#' @param d D
#' @param e E
#' @param output_fp directory to save figures
#' @param plot_name name of the plot
#'
#' @returns ggplot object
get_by_loc_figure <- function(a, b, c, d, e,
                              output_fp = file.path(
                                "output", "figs",
                                "overall_scores"
                              ),
                              plot_name = "by_location") {
  fig_layout <- "
  AAAA
  BBBB
  CCCC
  DDDD
  EEEE
  "

  combined_fig <- a + b +
    c + d +
    e +
    plot_layout(
      design = fig_layout,
      guides = "collect",
      axis = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "",
      theme = theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = element_text(hjust = 0.5),
        plot.tag = element_text(size = 14, face = "bold")
      )
    )

  # Create output directory if it doesn't exist
  dir_create(output_fp, recurse = TRUE)

  # Save figure
  ggsave(
    file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = combined_fig,
    width = 12,
    height = 15,
    dpi = 300
  )

  return(combined_fig)
}

#' Get a plot of the overall scores by nowcast date
#'
#' @param a A
#' @param b B
#' @param c C
#' @param d D
#' @param e E
#' @param f F
#' @param g G
#' @param h H
#' @param i I
#' @param j J
#' @param k K
#' @param l L
#' @param output_fp directory to save figures
#' @param plot_name name of the plot
#'
#' @returns ggplot object
get_scores_by_nowcast_date <- function(a, b, c, d, e, f, g, h, i, j, k, l,
                                       output_fp = file.path(
                                         "output", "figs",
                                         "overall_scores"
                                       ),
                                       plot_name = "scores_by_nowcast_date") {
  fig_layout <- "
  AABB
  CCDD
  EEFF
  GGHH
  IIJJ
  KKLL"

  combined_fig <- a + b + c + d +
    e + f + g + h +
    i + j + k + l +
    plot_layout(
      design = fig_layout,
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "",
      theme = theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = element_text(hjust = 0.5),
        plot.tag = element_text(size = 14, face = "bold")
      )
    )

  # Create output directory if it doesn't exist
  dir_create(output_fp, recurse = TRUE)

  # Save figure
  ggsave(
    file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = combined_fig,
    width = 12,
    height = 15,
    dpi = 300
  )

  return(combined_fig)
}

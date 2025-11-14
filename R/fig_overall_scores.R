#' Get relative skill scores with specified grouping
#'
#' @param scores_obj Scoringutils scores object
#' @param score_type Character string indicating which score metric to use
#' @param by Character vector of grouping variables for relative skill calculation
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
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_by_location <- function(scores_obj,
                                 seq_counts_by_loc,
                                 score_type = c("brier_score", "energy_score"),
                                 rel_skill_plot = TRUE) {
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
      by = c("target_date", "location")
    )

    rel_skill_summarised <- rel_skill |>
      scoringutils::summarise_scores(by = c("model", "location")) |>
      left_join(seq_counts_by_loc) |>
      arrange(desc(total_seq)) |>
      mutate(location = factor(location, levels = unique(location))) |>
      filter(model != "Hub-baseline")

    p <- ggplot(rel_skill_summarised) +
      geom_point(
        aes(
          x = location,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        )
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = plot_components_list$model_colors) +
      get_plot_theme() +
      labs(
        x = "Location",
        y = glue::glue("Relative scaled skill ({label})"),
        color = "Model"
      ) +
      scale_y_continuous(trans = "log10") +
      coord_cartesian(ylim = c(1 / 6, 6))
  } else {
    scores_sum <- scores_obj |>
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
        x = "Location",
        y = label,
        fill = "Model"
      )
  }
  return(p)
}

#' Brier/Energy Relative/Overall by nowcast date
#'
#' @param scores_obj Scoringutils scores object
#' @param rel_skill_plot Boolean indicating to return a relative skill plot
#' @param score_type Character string indicating which score metric to use
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_by_nowcast_date <- function(scores_obj,
                                     score_type = c("brier_score", "energy_score"),
                                     rel_skill_plot = TRUE) {
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
    )

    rel_skill_summarised <- rel_skill |>
      scoringutils::summarise_scores(by = c("model", "nowcast_date")) |>
      filter(model != "Hub-baseline")

    p <- ggplot(rel_skill_summarised) +
      geom_point(
        aes(
          x = nowcast_date,
          y = !!sym(glue::glue(
            "{score_type}_scaled_relative_skill"
          )),
          color = model
        )
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
        breaks = "2 weeks",
        date_labels = "%d %b %Y"
      ) +
      labs(
        x = "",
        y = glue::glue("Relative scaled skill ({label})"),
        color = "Model"
      ) +
      scale_y_continuous(trans = "log10") +
      coord_cartesian(ylim = c(1 / 1.5, 1.5))
  } else {
    scores_sum <- scores_obj |>
      scoringutils::summarise_scores(by = c("model", "nowcast_date"))
    p <- ggplot(scores_sum) +
      geom_point(
        aes(
          x = nowcast_date,
          y = !!sym(glue::glue(
            "{score_type}"
          )),
          color = model
        )
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
        breaks = "2 weeks",
        date_labels = "%d %b %Y"
      ) +
      labs(
        x = "",
        y = label,
        color = "Model"
      )
  }
  return(p)
}


#' Brier/Energy Relative/Overall by nowcast date
#'
#' @param scores_obj Scoringutils scores object
#' @param rel_skill_plot Boolean indicating to return a relative skill plot
#' @param score_type Character string indicating which score metric to use
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
    rel_skill <- get_relative_skill(
      scores_obj,
      score_type = score_type,
      by = c("target_date", "nowcast_date")
    )

    rel_skill_summarised <- rel_skill |>
      scoringutils::summarise_scores(by = c("model")) |>
      filter(model != "Hub-baseline")

    p <- ggplot(rel_skill_summarised) +
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
      )
  } else {
    scores_sum <- scores_obj |>
      scoringutils::summarise_scores(by = c("model")) |>
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
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_horizon <- function(scores_obj,
                             score_type = c("brier_score", "energy_score"),
                             rel_skill_plot = TRUE) {
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
        y = glue::glue("Relative scaled skill ({label})"),
        color = "Model"
      ) +
      scale_y_continuous(trans = "log10") +
      guides(color = "none")
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
    ylab("Number of sequences")
  return(p)
}

#' Sequence counts by location
#'
#' @param seq_counts_by_loc Total sequences for each location
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_plot_seq_counts_date <- function(seq_counts_by_date) {
  plot_comps <- plot_components()
  p <- ggplot(seq_counts_by_date) +
    geom_line(aes(x = nowcast_date, y = total_sequences), ) +
    get_plot_theme(dates = TRUE) +
    xlab("") +
    scale_x_date(
      breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    ylab("Number of sequences\nwithin nowcast period")
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
                                      output_fp = file.path("output", "figs", "overall_scores"),
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
    width = 16,
    height = 20,
    dpi = 300
  )

  return(combined_fig)
}



#' Chart C: Brier score skill by nowcast date (half-width panel)
#'
#' @param scores_obj Scoringutils scores object
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_line aes geom_hline
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_chart_c_brier_by_date <- function(scores_obj) {
  plot_components_list <- plot_components()

  rel_skill <- get_relative_skill(
    scores_obj,
    score_type = "brier_score",
    by = c("target_date", "location")
  )

  rel_skill_summarised <- rel_skill |>
    scoringutils::summarise_scores(by = c("model", "nowcast_date"))

  p <- ggplot(rel_skill_summarised) +
    geom_line(
      aes(
        x = as.Date(nowcast_date),
        y = brier_score_scaled_relative_skill,
        color = model
      ),
      linewidth = 0.8
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = plot_components_list$model_colors) +
    get_plot_theme() +
    labs(
      x = "Nowcast Date",
      y = "Relative Skill (Brier Score)",
      color = "Model"
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

#' Chart D: Energy score skill by nowcast date (half-width panel)
#'
#' @param scores_obj Scoringutils scores object
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_line aes geom_hline
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_chart_d_energy_by_date <- function(scores_obj) {
  plot_components_list <- plot_components()

  rel_skill <- get_relative_skill(
    scores_obj,
    score_type = "energy_score",
    by = c("target_date", "location")
  )

  rel_skill_summarised <- rel_skill |>
    scoringutils::summarise_scores(by = c("model", "nowcast_date"))

  p <- ggplot(rel_skill_summarised) +
    geom_line(
      aes(
        x = as.Date(nowcast_date),
        y = energy_score_scaled_relative_skill,
        color = model
      ),
      linewidth = 0.8
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = plot_components_list$model_colors) +
    get_plot_theme() +
    labs(
      x = "Nowcast Date",
      y = "Relative Skill (Energy Score)",
      color = "Model"
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

#' Chart E: Overall skill by location with model color-coding
#'
#' @param scores_obj Scoringutils scores object
#' @param score_type Character string indicating which score to plot
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_point aes geom_hline
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_chart_e_skill_by_location <- function(scores_obj,
                                          score_type = c("brier_score", "energy_score")) {
  score_type <- rlang::arg_match(score_type)
  plot_components_list <- plot_components()

  rel_skill <- get_relative_skill(
    scores_obj,
    score_type = score_type,
    by = c("target_date", "location")
  )

  rel_skill_summarised <- rel_skill |>
    scoringutils::summarise_scores(by = c("model", "location"))

  skill_col <- glue::glue("{score_type}_scaled_relative_skill")

  p <- ggplot(rel_skill_summarised) +
    geom_point(
      aes(
        x = location,
        y = !!sym(skill_col),
        color = model
      ),
      size = 3,
      position = position_dodge(width = 0.5)
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = plot_components_list$model_colors) +
    coord_flip() +
    get_plot_theme() +
    labs(
      x = "Location",
      y = glue::glue("Relative Skill ({gsub('_', ' ', tools::toTitleCase(score_type))})"),
      color = "Model"
    ) +
    theme(legend.position = "bottom")

  return(p)
}

#' Chart F: Weekly sequence averages by location
#'
#' @param variant_data Data frame with variant sequence data
#' @importFrom dplyr group_by summarise mutate
#' @importFrom lubridate floor_date
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap
#' @returns ggplot object
#' @autoglobal
get_chart_f_weekly_seq_by_location <- function(variant_data) {
  weekly_data <- variant_data |>
    mutate(week = floor_date(date, unit = "week")) |>
    group_by(week, location) |>
    summarise(
      avg_sequences = mean(sequences, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot(weekly_data) +
    geom_line(
      aes(x = week, y = avg_sequences),
      color = "steelblue",
      linewidth = 0.7
    ) +
    facet_wrap(~location, scales = "free_y", ncol = 3) +
    get_plot_theme() +
    labs(
      x = "Week",
      y = "Average Sequences",
      title = "Weekly Sequence Averages by Location"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 8)
    )

  return(p)
}

#' Chart G: Time-series skill trends for all US regions
#'
#' @param scores_obj Scoringutils scores object
#' @param score_type Character string indicating which score to plot
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap geom_hline
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_chart_g_timeseries_all_regions <- function(scores_obj,
                                               score_type = c("brier_score", "energy_score")) {
  score_type <- rlang::arg_match(score_type)
  plot_components_list <- plot_components()

  rel_skill <- get_relative_skill(
    scores_obj,
    score_type = score_type,
    by = c("target_date", "location")
  )

  rel_skill_summarised <- rel_skill |>
    scoringutils::summarise_scores(by = c("model", "location", "nowcast_date"))

  skill_col <- glue::glue("{score_type}_scaled_relative_skill")

  p <- ggplot(rel_skill_summarised) +
    geom_line(
      aes(
        x = as.Date(nowcast_date),
        y = !!sym(skill_col),
        color = model
      ),
      linewidth = 0.5
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    facet_wrap(~location, scales = "free_y", ncol = 4) +
    scale_color_manual(values = plot_components_list$model_colors) +
    get_plot_theme() +
    labs(
      x = "Nowcast Date",
      y = glue::glue("Relative Skill ({gsub('_', ' ', tools::toTitleCase(score_type))})"),
      color = "Model"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      strip.text = element_text(size = 8)
    )

  return(p)
}

#' Chart H: Time-series skill trends for California
#'
#' @param scores_obj Scoringutils scores object
#' @param score_type Character string indicating which score to plot
#' @param location Character string indicating location (default "CA")
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_line aes geom_hline
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_chart_h_timeseries_california <- function(scores_obj,
                                              score_type = c("brier_score", "energy_score"),
                                              location = "CA") {
  score_type <- rlang::arg_match(score_type)
  plot_components_list <- plot_components()

  rel_skill <- get_relative_skill(
    scores_obj,
    score_type = score_type,
    by = c("target_date", "location")
  )

  rel_skill_filtered <- rel_skill |>
    filter(location == !!location)

  rel_skill_summarised <- rel_skill_filtered |>
    scoringutils::summarise_scores(by = c("model", "nowcast_date"))

  skill_col <- glue::glue("{score_type}_scaled_relative_skill")

  p <- ggplot(rel_skill_summarised) +
    geom_line(
      aes(
        x = as.Date(nowcast_date),
        y = !!sym(skill_col),
        color = model
      ),
      linewidth = 1
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = plot_components_list$model_colors) +
    get_plot_theme() +
    labs(
      x = "Nowcast Date",
      y = glue::glue("Relative Skill ({gsub('_', ' ', tools::toTitleCase(score_type))})"),
      color = "Model",
      title = glue::glue("Skill Trends for {location}")
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

#' Chart I: Average scores over time
#'
#' @param scores_obj Scoringutils scores object
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_line aes
#' @importFrom tidyr pivot_longer
#' @returns ggplot object
#' @autoglobal
get_chart_i_avg_scores_over_time <- function(scores_obj) {
  plot_components_list <- plot_components()

  # Summarize both Brier and Energy scores by model and nowcast_date
  scores_summarised <- scores_obj |>
    filter(!is.na(brier_score) & !is.na(energy_score)) |>
    scoringutils::summarise_scores(by = c("model", "nowcast_date")) |>
    select(model, nowcast_date, brier_score, energy_score) |>
    pivot_longer(
      cols = c(brier_score, energy_score),
      names_to = "score_type",
      values_to = "score_value"
    ) |>
    mutate(
      score_type = case_when(
        score_type == "brier_score" ~ "Brier Score",
        score_type == "energy_score" ~ "Energy Score",
        TRUE ~ score_type
      )
    )

  p <- ggplot(scores_summarised) +
    geom_line(
      aes(
        x = as.Date(nowcast_date),
        y = score_value,
        color = model
      ),
      linewidth = 0.8
    ) +
    facet_wrap(~score_type, scales = "free_y", ncol = 1) +
    scale_color_manual(values = plot_components_list$model_colors) +
    get_plot_theme() +
    labs(
      x = "Nowcast Date",
      y = "Average Score",
      color = "Model"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

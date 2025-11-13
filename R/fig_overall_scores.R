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

#' Chart A: Brier score skill by location (half-width panel)
#'
#' @param scores_obj Scoringutils scores object
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_chart_a_brier_by_location <- function(scores_obj) {
  plot_components_list <- plot_components()

  rel_skill <- get_relative_skill(
    scores_obj,
    score_type = "brier_score",
    by = c("target_date", "location")
  )

  rel_skill_summarised <- rel_skill |>
    scoringutils::summarise_scores(by = c("model", "location"))

  p <- ggplot(rel_skill_summarised) +
    geom_bar(
      aes(
        x = location,
        y = brier_score_scaled_relative_skill,
        fill = model
      ),
      stat = "identity", position = "dodge"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = plot_components_list$model_colors) +
    coord_flip() +
    get_plot_theme() +
    labs(
      x = "Location",
      y = "Relative Skill (Brier Score)",
      fill = "Model"
    ) +
    theme(legend.position = "none")

  return(p)
}

#' Chart B: Energy score skill by location (half-width panel)
#'
#' @param scores_obj Scoringutils scores object
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 ggplot geom_bar aes geom_hline coord_flip
#' @importFrom rlang sym
#' @returns ggplot object
#' @autoglobal
get_chart_b_energy_by_location <- function(scores_obj) {
  plot_components_list <- plot_components()

  rel_skill <- get_relative_skill(
    scores_obj,
    score_type = "energy_score",
    by = c("target_date", "location")
  )

  rel_skill_summarised <- rel_skill |>
    scoringutils::summarise_scores(by = c("model", "location"))

  p <- ggplot(rel_skill_summarised) +
    geom_bar(
      aes(
        x = location,
        y = energy_score_scaled_relative_skill,
        fill = model
      ),
      stat = "identity", position = "dodge"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = plot_components_list$model_colors) +
    coord_flip() +
    get_plot_theme() +
    labs(
      x = "Location",
      y = "Relative Skill (Energy Score)",
      fill = "Model"
    ) +
    theme(legend.position = "none")

  return(p)
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

#' Combine all 9 charts into a comprehensive figure using patchwork
#'
#' @param chart_a Chart A plot object
#' @param chart_b Chart B plot object
#' @param chart_c Chart C plot object
#' @param chart_d Chart D plot object
#' @param chart_e Chart E plot object
#' @param chart_f Chart F plot object
#' @param chart_g Chart G plot object
#' @param chart_h Chart H plot object
#' @param chart_i Chart I plot object
#' @param plot_name Character string indicating name of output file
#' @param output_fp Character string indicating directory to save file
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom ggplot2 ggsave theme element_text
#' @importFrom fs dir_create
#' @returns Combined patchwork plot object
#' @autoglobal
get_overall_scores_figure <- function(chart_a, chart_b, chart_c, chart_d,
                                      chart_e, chart_f, chart_g, chart_h, chart_i,
                                      plot_name = "overall_scores_summary",
                                      output_fp = file.path("output", "figs", "overall_scores")) {
  # Create layout design
  # Charts A-D are half-width stacked panels
  # Charts E-I are full-width panels
  fig_layout <- "
  AABBCCDD
  AABBCCDD
  EEEEEEEE
  EEEEEEEE
  FFFFFFFF
  FFFFFFFF
  GGGGGGGG
  GGGGGGGG
  GGGGGGGG
  HHHHHHHH
  IIIIIIII
  IIIIIIII"

  combined_fig <- chart_a + chart_b + chart_c + chart_d +
    chart_e + chart_f + chart_g + chart_h + chart_i +
    plot_layout(
      design = fig_layout,
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "",
      theme = theme(
        legend.position = "bottom",
        legend.box = "vertical",
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

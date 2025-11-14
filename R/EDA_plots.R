#' Get a plot of the nowcasts for a specific nowcast date
#'
#' @param data Data.frame with nowcasts for a single nowcast date for
#'   multiple teams and (a few) locations
#' @param plot_name Character string indicating name of plot
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot geom_line aes facet_grid theme_bw vars xlab
#'   ylab theme_bw ggsave ggtitle geom_vline
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcasts <- function(data,
                              plot_name,
                              output_fp = file.path("output", "figs", "eda"),
                              save = TRUE) {
  nowcast_date <- unique(data$nowcast_date)

  nc_data <- mutate(data,
    output_type_id_clade = glue::glue("{output_type_id}-{clade}")
  ) # nolint
  p <- ggplot(nc_data) +
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
    theme_bw() +
    xlab("Target date") +
    ylab("Estimated proportion")
  ggtitle(glue::glue("Nowcasts as of {nowcast_date}"))

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p
    )
  }
  return(p)
}

#' Plot multiple locations
#'
#' @param data Data.frame with all sequence data for multiple locations
#' @inheritParams get_plot_nowcasts
#' @importFrom ggplot2 ggplot geom_line aes facet_grid theme_bw vars
#'   element_blank theme xlab ylab facet_wrap ggtitle
#' @importFrom dplyr filter group_by summarise mutate left_join
#' @returns ggplot object
#' @autoglobal
get_plot_mult_locs <- function(data,
                               plot_name,
                               output_fp = file.path("output", "figs", "eda"),
                               save = TRUE) {
  seq_data <- data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  variant_data_by_loc <- data |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(seq_data) |>
    mutate(obs_freq = seq_clade / n_seq)

  p <- ggplot(variant_data_by_loc) +
    geom_line(aes(x = date, y = obs_freq, color = clades_modeled)) +
    theme_bw() +
    facet_wrap(~location) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    xlab("Sequence collection date") +
    ylab("Observed frequency")
  ggtitle("Observed variant proportions")

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p
    )
  }
  return(p)
}

#' Plot scores over time for a few locations and a specific nowcast date
#'
#' @param scores_data_hub Data.frame of scores from the Hub
#' @param locations Vector of character strings indicating the set of locations
#'    to plot as facets
#' @param this_nowcast_date Character string indicating nowcast date to plot
#' @param score_type Character string indicating which score to plot.
#' @inheritParams get_plot_nowcasts
#' @importFrom ggplot2 ggplot geom_line aes facet_grid theme_bw vars
#'   element_blank theme xlab ylab facet_wrap ggtitle
#' @importFrom dplyr filter group_by summarise mutate left_join
#' @importFrom rlang sym arg_match
#' @returns ggplot object
#' @autoglobal
get_plot_scores_t <- function(scores_data_hub,
                              locations,
                              this_nowcast_date,
                              score_type = c(
                                "energy",
                                "brier_point",
                                "brier_dist"
                              ),
                              plot_name = "scores_over_time",
                              output_fp = file.path("output", "figs", "eda"),
                              save = TRUE) {
  score_type <- rlang::arg_match(score_type)
  score_sym <- rlang::sym(score_type)
  scores_single_date <- filter(
    scores_data_hub,
    nowcast_date == this_nowcast_date,
    location %in% locations
  )
  p <- ggplot(scores_single_date) +
    geom_point(aes(
      x = target_date, y = !!score_sym, color = model_id,
      shape = scored
    )) +
    geom_line(aes(
      x = target_date, y = !!score_sym, color = model_id
    )) +
    facet_wrap(~location,
      scales = "free_y",
      nrow = 3
    ) +
    geom_vline(aes(xintercept = max(nowcast_date)), linetype = "dashed") +
    theme_bw() +
    ggtitle(glue("scores over time for a few states on {this_nowcast_date}"))

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p
    )
  }
  return(p)
}

#' Get a plot of relative scaled skill overall
#'
#' @param scores_obj Scoringutils scores objected
#' @param score_type Character string indicating which score to plot.
#' @inheritParams get_plot_nowcasts
#' @importFrom scoringutils add_relative_skill summarise_scores
#' @importFrom ggplot2 geom_hline
#' @importFrom rlang sym arg_match
#' @returns ggplot object bar chart of scores
#' @autoglobal
get_plot_rel_skill_overall <- function(scores_obj,
                                       score_type = c(
                                         "brier_score",
                                         "energy_score"
                                       ),
                                       plot_name = "overall_rel_scaled_skill",
                                       output_fp = file.path(
                                         "output",
                                         "figs",
                                         "eda"
                                       ),
                                       save = TRUE) {
  score_type <- rlang::arg_match(score_type)
  rel_skill <- scoringutils::add_relative_skill(
    scores_obj,
    metric = score_type,
    baseline = "Hub-baseline",
    by = "target_date"
  )

  rel_skill_summarised <- rel_skill |>
    filter(!is.na(!!sym(score_type))) |>
    scoringutils::summarise_scores(by = "model")

  p <- ggplot(rel_skill_summarised) +
    geom_bar(
      aes(
        x = model,
        y = !!sym(glue::glue("{score_type}_scaled_relative_skill")),
        fill = model
      ),
      stat = "identity", position = "dodge"
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    theme_bw()

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}_{score_type}.png")),
      plot = p
    )
  }
  return(p)
}

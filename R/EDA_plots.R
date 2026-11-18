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

#' Plot multiple locations
#'
#' @param data Data.frame with all sequence data for multiple locations
#' @inheritParams get_plot_nowcasts
#' @importFrom ggplot2 ggplot geom_line aes facet_grid theme_bw vars
#'   element_blank theme xlab ylab facet_wrap ggtitle
#' @importFrom dplyr filter group_by summarise mutate left_join
#' @returns ggplot object
#' @autoglobal
get_plot_clade_by_loc <- function(data,
                                  clade,
                                  plot_name,
                                  date_of_interest,
                                  output_fp = file.path(
                                    "output",
                                    "figs", "eda"
                                  ),
                                  temporal_granularity = "weeks",
                                  save = TRUE) {
  seq_counts_by_loc <- data |>
    group_by(location) |>
    summarise(
      total_seq = sum(sequences)
    ) |>
    arrange(desc(total_seq), location) |>
    head(10)

  if (temporal_granularity == "weeks") {
    obs_data <- daily_to_weekly(data)
  } else {
    obs_data <- data
  }
  seq_data <- obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  variant_data_by_loc <- obs_data |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(seq_data) |>
    mutate(obs_freq = seq_clade / n_seq) |>
    filter(
      clades_modeled == clade,
      date >= ymd(date_of_interest) - days(120),
      date <= ymd(date_of_interest) + days(60),
      location %in% c(seq_counts_by_loc$location)
    )


  p <- ggplot(variant_data_by_loc) +
    geom_line(aes(x = date, y = obs_freq, color = location)) +
    get_plot_theme(dates = TRUE) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%d %b %Y"
    ) +
    xlab("") +
    ylab(glue::glue("Observed frequency {clade}"))

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

#' Plot how bootstrap resamples differ from the original data
#'
#' Diagnostic figure for the moving block bootstrap of the mean score by
#' model. Panel A shows which nowcast dates (and how many copies of each)
#' make up the original data and a few example resamples. Panel B shows the
#' mean score by model for each nowcast date in the original data, so the
#' weeks a resample over- or under-represents can be read off. Panel C
#' compares the mean score by model in the original data and in the
#' example resamples. Panel D shows the mean score by model across all
#' resamples, with the original estimate and its Bonferroni simultaneous
#' confidence band.
#'
#' @param scores_obj Scoringutils scores object.
#' @param boot_dates Data frame of resampled nowcast dates, as returned by
#'   [get_block_bootstrap_dates()].
#' @param boot_estimates Bootstrap estimates for `scores_obj`, as returned
#'   by [get_bootstrap_mean_scores()].
#' @param ci Confidence bands, as returned by [get_bonferroni_ci()].
#' @param score_type Character string indicating which score metric to use.
#' @param boot_ids Integer vector of example resamples to show.
#' @param title Character string for the figure title.
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point
#'   geom_errorbar geom_jitter facet_grid vars labs scale_color_manual
#'   position_dodge ggsave theme element_blank
#' @importFrom dplyr filter group_by summarise mutate bind_rows count select
#' @importFrom rlang sym
#' @returns patchwork object
#' @autoglobal
get_plot_bootstrap_resamples <- function(scores_obj,
                                         boot_dates,
                                         boot_estimates,
                                         ci,
                                         score_type = c(
                                           "brier_score",
                                           "energy_score"
                                         ),
                                         boot_ids = c(1, 2),
                                         title = NULL,
                                         plot_name,
                                         output_fp = file.path(
                                           "output", "figs", "eda"
                                         ),
                                         save = TRUE) {
  score_type <- rlang::arg_match(score_type)
  label <- ifelse(score_type == "brier_score", "Brier score", "Energy score")
  model_colors <- plot_components()$model_colors
  sample_levels <- c("Original", paste("Resample", boot_ids))
  models <- ci$model
  scores_obj <- filter(scores_obj, model %in% models)

  # A: copies of each nowcast date in the original and example resamples
  original_dates <- data.frame(
    sample = "Original",
    nowcast_date = sort(unique(as.Date(boot_dates$nowcast_date))),
    stringsAsFactors = FALSE
  )
  resample_dates <- boot_dates |>
    filter(boot_id %in% boot_ids) |>
    mutate(sample = paste("Resample", boot_id))
  date_counts <- bind_rows(original_dates, resample_dates) |>
    count(sample, nowcast_date) |>
    mutate(sample = factor(sample, levels = sample_levels))

  p_dates <- ggplot(date_counts) +
    geom_col(aes(x = nowcast_date, y = n), fill = "gray40") +
    facet_grid(rows = vars(sample)) +
    get_plot_theme(dates = TRUE) +
    labs(x = "Nowcast date", y = "Copies in sample", title = "A")

  # B: mean score by model and nowcast date in the original data
  weekly <- as.data.frame(scores_obj) |>
    filter(!is.na(!!sym(score_type))) |>
    group_by(model, nowcast_date) |>
    summarise(estimate = mean(!!sym(score_type)), .groups = "drop")
  p_weekly <- ggplot(weekly) +
    geom_line(aes(x = nowcast_date, y = estimate, color = model)) +
    scale_color_manual(values = model_colors) +
    get_plot_theme(dates = TRUE) +
    labs(
      x = "Nowcast date", y = glue::glue("Mean {label}"),
      color = "Model", title = "B"
    )

  # C: the summary metric in the original data and example resamples
  original_means <- mutate(select(ci, model, estimate), sample = "Original")
  resample_means <- boot_estimates |>
    filter(boot_id %in% boot_ids) |>
    mutate(sample = paste("Resample", boot_id))
  sample_means <- bind_rows(original_means, resample_means) |>
    filter(model %in% models) |>
    mutate(sample = factor(sample, levels = sample_levels))

  p_samples <- ggplot(sample_means) +
    geom_point(
      aes(x = model, y = estimate, color = model, shape = sample),
      size = 3, position = position_dodge(width = 0.6)
    ) +
    scale_color_manual(values = model_colors, guide = "none") +
    get_plot_theme() +
    theme(axis.text.x = element_blank()) +
    labs(
      x = "", y = glue::glue("Mean {label}"), shape = "Sample",
      title = "C"
    )

  # D: all bootstrap estimates with the original estimate and its band
  p_boot <- ggplot() +
    geom_jitter(
      data = filter(boot_estimates, model %in% models),
      aes(x = model, y = estimate, color = model),
      width = 0.15, alpha = 0.4, size = 1
    ) +
    geom_errorbar(
      data = ci, aes(x = model, ymin = lower, ymax = upper),
      width = 0.3
    ) +
    geom_point(data = ci, aes(x = model, y = estimate), size = 3) +
    scale_color_manual(values = model_colors, guide = "none") +
    get_plot_theme() +
    theme(axis.text.x = element_blank()) +
    labs(
      x = "", y = glue::glue("Mean {label}"), color = "Model",
      title = "D"
    )

  p <- patchwork::wrap_plots(
    p_dates, p_weekly, p_samples, p_boot,
    ncol = 2, guides = "collect"
  ) +
    patchwork::plot_annotation(title = title)

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 12, height = 8
    )
  }
  return(p)
}

#' Plot the nowcast dates in every bootstrap resample
#'
#' Overview of the moving block bootstrap resamples. The top panel is a
#' heatmap of the first `n_show` resamples, with one row per resample and
#' one column per nowcast date, coloured by the number of copies of that
#' date in the resample (blank if not drawn), so the blocks of consecutive
#' dates are visible. The bottom panel shows the total number of times each
#' nowcast date was drawn across all resamples, with a dashed line at the
#' number expected if every date were equally likely.
#'
#' @param boot_dates Data frame of resampled nowcast dates, as returned by
#'   [get_block_bootstrap_dates()].
#' @param n_show Integer number of resamples to show in the heatmap.
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_tile geom_col geom_hline
#'   scale_fill_viridis_c scale_y_reverse labs ggsave
#' @importFrom dplyr count filter
#' @returns patchwork object
#' @autoglobal
get_plot_bootstrap_dates <- function(boot_dates,
                                     plot_name,
                                     n_show = 50,
                                     output_fp = file.path(
                                       "output", "figs", "eda"
                                     ),
                                     save = TRUE) {
  n_boot <- length(unique(boot_dates$boot_id))
  copies <- boot_dates |>
    filter(boot_id <= n_show) |>
    count(boot_id, nowcast_date)
  totals <- count(boot_dates, nowcast_date)

  p_heatmap <- ggplot(copies) +
    geom_tile(aes(x = nowcast_date, y = boot_id, fill = n)) +
    scale_fill_viridis_c(name = "Copies in\nresample") +
    scale_y_reverse(expand = c(0, 0)) +
    get_plot_theme(dates = TRUE) +
    labs(
      x = "", y = "Resample",
      title = glue::glue("First {n_show} of {n_boot} resamples")
    )

  p_totals <- ggplot(totals) +
    geom_col(aes(x = nowcast_date, y = n), fill = "gray40") +
    geom_hline(yintercept = n_boot, linetype = "dashed") +
    get_plot_theme(dates = TRUE) +
    labs(
      x = "Nowcast date", y = "Total draws",
      title = glue::glue("All {n_boot} resamples")
    )

  p <- patchwork::wrap_plots(
    p_heatmap, p_totals,
    ncol = 1, heights = c(4, 1)
  )

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 8, height = 10
    )
  }
  return(p)
}

#' Plot the bootstrap distribution of the scaled relative skill
#'
#' Shows the overall scaled relative skill (SRSS) of each model against the
#' baseline in every bootstrap resample (jittered points), with the
#' original-data estimate (black point) and its Bonferroni simultaneous band
#' as reported (black error bar). The y axis is log10, as in the relative
#' skill panels of the overall score figure.
#'
#' @param boot_estimates Bootstrap SRSS, as returned by
#'   [get_bootstrap_srss()] (or its full-roster subset).
#' @param ci Log-scale confidence bands, as returned by
#'   [get_bonferroni_ci()] with `log_scale = TRUE`.
#' @param score_type Character string indicating which score metric to use.
#' @param title Character string for the plot title.
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @param save Boolean indicating whether or not to save the plot.
#' @importFrom ggplot2 ggplot aes geom_jitter geom_errorbar geom_point
#'   geom_hline scale_color_manual scale_y_continuous labs ggsave theme
#'   element_blank guides guide_legend
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_bootstrap_srss <- function(boot_estimates,
                                    ci,
                                    score_type = c(
                                      "brier_score",
                                      "energy_score"
                                    ),
                                    title = NULL,
                                    plot_name,
                                    output_fp = file.path(
                                      "output", "figs", "eda"
                                    ),
                                    save = TRUE) {
  score_type <- rlang::arg_match(score_type)
  label <- ifelse(score_type == "brier_score", "Brier score", "Energy score")
  model_colors <- plot_components()$model_colors
  boot_estimates <- filter(
    boot_estimates,
    model %in% ci$model, is.finite(estimate), estimate > 0
  )

  p <- ggplot() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_jitter(
      data = boot_estimates,
      aes(x = model, y = estimate, color = model),
      width = 0.15, alpha = 0.3, size = 0.8
    ) +
    geom_errorbar(
      data = ci, aes(x = model, ymin = lower, ymax = upper),
      width = 0.3
    ) +
    geom_point(data = ci, aes(x = model, y = estimate), size = 2.5) +
    scale_color_manual(values = model_colors) +
    scale_y_continuous(trans = "log10") +
    get_plot_theme() +
    theme(axis.text.x = element_blank()) +
    labs(
      x = "",
      y = glue::glue("Scaled relative skill\n({label})"),
      color = "Model",
      title = title
    ) +
    guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 8, height = 6
    )
  }
  return(p)
}

#' Combine the bootstrap SRSS distribution plots into one figure
#'
#' Arranges the four plots from [get_plot_bootstrap_srss()] (U.S. excluding
#' California and California, Brier and energy scores) in a 2 x 2 grid with
#' a shared legend.
#'
#' @param a,b,c,d ggplot objects for the four panels, in reading order.
#' @param plot_name Character string indicating name of plot.
#' @param output_fp Character string indicating directory to save file.
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggplot2 theme element_text ggsave
#' @returns patchwork object
#' @autoglobal
get_boot_srss_figure <- function(a, b, c, d,
                                 plot_name,
                                 output_fp = file.path(
                                   "output", "figs", "overall_scores", "supp"
                                 )) {
  # The panels show different sets of models, so patchwork can't merge
  # their legends. Keep the legend of the first panel that shows every
  # model, and hide the others
  panels <- list(a, b, c, d)
  panel_models <- lapply(
    panels, function(p) unique(as.character(p$layers[[2]]$data$model))
  )
  all_models <- unique(unlist(panel_models))
  keep <- which(vapply(
    panel_models, function(m) all(all_models %in% m), logical(1)
  ))[1]
  if (is.na(keep)) {
    keep <- 1
  }
  panels[-keep] <- lapply(
    panels[-keep], function(p) p + theme(legend.position = "none")
  )
  combined_fig <- wrap_plots(panels, ncol = 2, guides = "collect") +
    plot_annotation(
      tag_levels = "A",
      theme = theme(
        legend.position = "top",
        plot.tag = element_text(size = 14, face = "bold")
      )
    )
  dir_create(output_fp, recurse = TRUE)
  ggsave(
    file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = combined_fig, width = 12, height = 9, dpi = 300
  )
  return(combined_fig)
}

#' Plot the scaled relative skill by horizon with confidence bands
#'
#' Shows the scaled relative skill (SRSS) by horizon for each model, as in
#' the relative skill by horizon panels of the overall score figure, with
#' Bonferroni simultaneous bands (shaded) that hold jointly across all
#' models and horizons in the table. One facet per model.
#'
#' @param ci Confidence bands by model and horizon, as returned by
#'   [get_bonferroni_ci()] with `by = c("model", "horizon")`.
#' @inheritParams get_plot_bootstrap_srss
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline
#'   geom_vline facet_wrap scale_color_manual scale_fill_manual
#'   scale_y_continuous labs ggsave
#' @returns ggplot object
#' @autoglobal
get_plot_srss_horizon_ci <- function(ci,
                                     score_type = c(
                                       "brier_score",
                                       "energy_score"
                                     ),
                                     title = NULL,
                                     plot_name,
                                     output_fp = file.path(
                                       "output", "figs", "eda"
                                     ),
                                     save = TRUE) {
  score_type <- rlang::arg_match(score_type)
  label <- ifelse(score_type == "brier_score", "Brier score", "Energy score")
  model_colors <- plot_components()$model_colors

  p <- ggplot(ci) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(
      aes(x = horizon, ymin = lower, ymax = upper, fill = model),
      alpha = 0.3
    ) +
    geom_line(aes(x = horizon, y = estimate, color = model), linewidth = 1) +
    facet_wrap(~model) +
    scale_color_manual(values = model_colors) +
    scale_fill_manual(values = model_colors) +
    scale_y_continuous(trans = "log10") +
    get_plot_theme() +
    labs(
      x = "Horizon (days)",
      y = glue::glue("Scaled relative skill\n({label})"),
      color = "Model", fill = "Model",
      title = title,
      subtitle = glue::glue(
        "Bonferroni bands across {ci$n_comparisons[1]} model-horizon ",
        "comparisons (c = {round(ci$crit_value[1], 2)})"
      )
    )

  if (isTRUE(save)) {
    dir_create(output_fp, recurse = TRUE)
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p, width = 10, height = 6
    )
  }
  return(p)
}

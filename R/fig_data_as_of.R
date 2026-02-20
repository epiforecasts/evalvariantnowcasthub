#' Bar chart of sequence counts stacked and colored by clade
#'
#' @param obs_data Data.frame of number of sequences by clade and location
#' @param final_data Data.frame of final sequences by clade and location
#' @param location Location to plot (abbreviation)
#' @param temporal_granularity Temporal granularity to plot
#' @param date_range Date range to plot
#' @param log_scale Boolean indicating whether or not y axis should be on log
#'   scale, default is TRUE
#'
#' @returns ggplot object
#' @autoglobal
get_bar_chart_comparison <- function(obs_data,
                                     final_data,
                                     location,
                                     temporal_granularity,
                                     date_range,
                                     log_scale = TRUE) {
  nowcast_date <- obs_data |>
    select(nowcast_date) |>
    distinct() |>
    pull()

  if (temporal_granularity == "weeks") {
    obs_data <- daily_to_weekly(obs_data)
    final_data <- daily_to_weekly(final_data)
  }

  if (location == "US") {
    obs_data2 <- obs_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
    final_data2 <- final_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
  } else {
    obs_data2 <- filter(
      obs_data,
      location %in% !!location
    )
    final_data2 <- filter(
      final_data,
      location %in% !!location
    )
  }

  # filter final data to dates we want
  final_data3 <- final_data2 |>
    filter(
      date >= min(obs_data2$date),
      date <= max(obs_data2$date)
    ) |>
    rename(sequences_final = sequences) |>
    select(location, clades_modeled, sequences_final, date)

  comb_data <- obs_data2 |>
    left_join(final_data3, by = c(
      "date", "location",
      "clades_modeled"
    )) |>
    mutate(sequences_add = sequences_final - sequences) |>
    rename(sequences_init = sequences) |>
    pivot_longer(
      cols = c("sequences_final", "sequences_add", "sequences_init"),
      names_prefix = "sequences_",
      names_to = "data_availability",
      values_to = "sequence_counts"
    ) |>
    filter(data_availability != "final") |>
    mutate(
      data_availability = ifelse(data_availability == "init",
        "as of nowcast date",
        "evaluation"
      ),
      data_availability = factor(data_availability,
        levels = c(
          "as of nowcast date",
          "evaluation"
        )
      ),
      fill_group = interaction(clades_modeled, data_availability)
    )

  plot_comps <- plot_components()

  # Build canonical clade order from clade_colors (base names, in definition
  # order), then filter to clades present in the data.
  clade_color_names <- names(plot_comps$clade_colors)
  base_clade_names <- unique(
    clade_color_names[!grepl(
      "\\.(as of nowcast date|evaluation)$",
      clade_color_names
    )]
  )
  clades_in_data <- unique(as.character(comb_data$clades_modeled))
  ordered_clades <- base_clade_names[base_clade_names %in% clades_in_data]

  # Interleave fill_group levels so each clade's two shading levels are
  # consecutive in the stack: [24A.as-of, 24A.eval, 24B.as-of, 24B.eval, ...]
  fill_group_levels <- c(rbind(
    paste0(ordered_clades, ".as of nowcast date"),
    paste0(ordered_clades, ".evaluation")
  ))

  comb_data <- comb_data |>
    mutate(
      clades_modeled = factor(clades_modeled, levels = ordered_clades),
      fill_group = factor(fill_group, levels = fill_group_levels)
    )

  p <- ggplot(comb_data) +
    geom_bar(
      aes(
        x = date, y = sequence_counts, fill = fill_group,
        alpha = data_availability
      ),
      stat = "identity", position = position_stack()
    ) +
    get_plot_theme(dates = TRUE) +
    scale_fill_manual(
      name = "Clades",
      values = plot_comps$clade_colors
    ) +
    scale_alpha_manual(
      name = "Data availability",
      values = plot_comps$data_availability_alpha
    ) +
    xlab("") +
    ylab("Sequence counts") +
    guides(
      fill = "none",
      alpha = guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        nrow = 2
      )
    ) +
    scale_x_date(
      limits = date_range,
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank()
    ) +
    geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )
  if (isTRUE(log_scale)) {
    p <- p + scale_y_continuous(transform = "log10")
  }

  return(p)
}

#' Get a plot of the variant frequency as of the nowcast date versus
#' once we are ready to evaluate
#'
#' @inheritParams get_bar_chart_comparison
#' @param clades_to_plot Vector of character strings of the clades to show
#'
#' @returns plot of clade frequencies as of nowcats date and when we evaluate
get_plot_freq_as_of_vs_eval <- function(obs_data,
                                        final_data,
                                        location,
                                        date_range,
                                        temporal_granularity,
                                        clades_to_plot =
                                          c("24E", "24F", "25A")) {
  nowcast_date <- obs_data |>
    select(nowcast_date) |>
    distinct() |>
    pull()
  if (temporal_granularity == "weeks") {
    obs_data <- daily_to_weekly(obs_data)
    final_data <- daily_to_weekly(final_data)
  }

  if (location == "US") {
    obs_data2 <- obs_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
    final_data2 <- final_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
  } else {
    obs_data2 <- filter(
      obs_data,
      location %in% !!location
    )
    final_data2 <- filter(
      final_data,
      location %in% !!location
    )
  }

  final_data3 <- filter(
    final_data2,
    date >= min(obs_data$date),
    date <= ymd(nowcast_date) + days(10)
  )

  seq_data <- obs_data2 |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  variant_data_by_loc <- obs_data2 |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(seq_data) |>
    mutate(
      obs_freq = seq_clade / n_seq,
      data_availability = "as of nowcast date"
    )

  seq_data_final <- final_data3 |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  variant_data_by_loc_final <- final_data3 |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(seq_data_final) |>
    mutate(
      obs_freq = seq_clade / n_seq,
      data_availability = "evaluation"
    )

  comb_data <- bind_rows(
    variant_data_by_loc,
    variant_data_by_loc_final
  )

  if (!is.null(clades_to_plot)) {
    comb_data <-
      filter(
        comb_data,
        clades_modeled %in% clades_to_plot
      )
  }
  plot_comps <- plot_components()
  p <- ggplot(comb_data) +
    geom_line(aes(
      x = date, y = obs_freq, color = clades_modeled,
      alpha = data_availability
    )) +
    geom_point(aes(
      x = date, y = obs_freq, color = clades_modeled,
      alpha = data_availability
    )) +
    geom_vline(aes(xintercept = ymd(nowcast_date)), linetype = "dashed") +
    get_plot_theme(dates = TRUE) +
    scale_color_manual(
      name = "Clades",
      values = plot_comps$clade_colors
    ) +
    scale_alpha_manual(
      name = "Data availability",
      values = plot_comps$data_availability_alpha
    ) +
    xlab("") +
    ylab("Observed\nclade frequency") +
    guides(
      color = "none",
      linetype = "none",
      alpha = "none"
    ) +
    scale_x_date(
      limits = date_range,
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 10
      ),
      legend.position = "bottom"
    ) +
    coord_cartesian(ylim = c(0, 1))
  return(p)
}

#' Make data comparing data as of nowcast date vs when evaluating
#'
#' @param seq_counts_as_of1 A
#' @param seq_counts_eval1 C
#' @param eval_freq1 E
#' @param seq_counts_as_of2 B
#' @param seq_counts_eval2 D
#' @param eval_freq2 F
#' @inheritParams get_plot_obs_clade_freq
#'
#' @returns patchwork figure
get_second_data_fig <- function(seq_counts_as_of1,
                                seq_counts_as_of2,
                                seq_counts_eval1,
                                seq_counts_eval2,
                                eval_freq1,
                                eval_freq2,
                                plot_name,
                                output_fp = file.path(
                                  "output", "figs",
                                  "as_of_data_figs", "final"
                                )) {
  fig_layout <- "
  AAABBB
  CCCDDD
  EEEFFF"

  fig_eval <- (seq_counts_as_of1 +
    ylab("Sequence counts as of\nthe nowcast date")) +
    (seq_counts_as_of2 +
      ylab("Sequence counts as of\nthe nowcast date")) +
    (seq_counts_eval1 + ylab("Sequence counts\nfor evaluation") +
      theme(plot.tag.position = c(-0.01, 1.05))) +
    (seq_counts_eval2 + ylab("Sequence counts\nfor evaluation") +
      theme(plot.tag.position = c(-0.01, 1.05))) +
    eval_freq1 +
    eval_freq2 +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      tag_levels = "A",
      tag_suffix = "", # adds a period after each letter
      tag_sep = "", # no separator between tag levels
      theme = theme(
        legend.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.justification = "center",
        plot.tag = element_text(size = 20)
      )
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = fig_eval,
    width = 10,
    height = 10
  )
  return(fig_eval)
}

#' Make data comparing data as of nowcast date vs when evaluating -
#'  All US version
#'
#' @param seq_counts_as_of A plot showing sequence counts as of nowcast date
#' @param seq_counts_eval B plot showing sequence counts for evaluation
#' @param eval_freq C plot showing clade frequencies
#' @inheritParams get_plot_obs_clade_freq
#'
#' @returns patchwork figure
get_data_fig_all_us <- function(seq_counts_as_of,
                                seq_counts_eval,
                                eval_freq,
                                plot_name,
                                output_fp = file.path(
                                  "output", "figs",
                                  "as_of_data_figs", "final"
                                )) {
  fig_layout <- "
  AAA
  BBB
  CCC"

  fig_eval <- (seq_counts_as_of +
    ylab("Sequence counts as of\nthe nowcast date")) +
    (seq_counts_eval + ylab("Sequence counts\nfor evaluation")) +
    eval_freq +
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
    plot = fig_eval,
    width = 10,
    height = 10
  )
  return(fig_eval)
}

#' Make data comparing data as of nowcast date vs when evaluating -
#'  Horizontal layout
#'
#' @param seq_counts_as_of A plot showing sequence counts as of nowcast date
#' @param seq_counts_eval B plot showing sequence counts for evaluation
#' @param eval_freq C plot showing clade frequencies
#' @inheritParams get_plot_obs_clade_freq
#'
#' @returns patchwork figure
get_data_fig_all_us_horizontal <- function(seq_counts_as_of,
                                           seq_counts_eval,
                                           eval_freq,
                                           plot_name,
                                           output_fp = file.path(
                                             "output", "figs",
                                             "as_of_data_figs", "final"
                                           )) {
  fig_layout <- "ABC"

  # Add x-axis text to all plots
  seq_counts_as_of <- seq_counts_as_of +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

  seq_counts_eval <- seq_counts_eval +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

  fig_eval <- (seq_counts_as_of +
    ylab("Sequence counts as of\nthe nowcast date")) +
    (seq_counts_eval +
      ylab("Sequence counts\nfor evaluation")) +
    eval_freq +
    plot_layout(
      design = fig_layout,
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
    plot = fig_eval,
    width = 15,
    height = 5
  )
  return(fig_eval)
}

#' Make data comparing data as of nowcast date vs when evaluating -
#' Triangular layout
#'
#' @param seq_counts_as_of A plot showing sequence counts as of nowcast date
#' @param seq_counts_eval B plot showing sequence counts for evaluation
#' @param eval_freq C plot showing clade frequencies
#' @inheritParams get_plot_obs_clade_freq
#'
#' @returns patchwork figure
get_data_fig_all_us_triangular <- function(seq_counts_as_of,
                                           seq_counts_eval,
                                           eval_freq,
                                           plot_name,
                                           output_fp = file.path(
                                             "output", "figs",
                                             "as_of_data_figs", "final"
                                           )) {
  # Inverted triangle: two plots on top, one wide plot on bottom
  fig_layout <- "
  AAABBB
  CCCCCC"

  # Add x-axis text to top plots (they were set to remove it)
  seq_counts_as_of <- seq_counts_as_of +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

  seq_counts_eval <- seq_counts_eval +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

  fig_eval <- (seq_counts_as_of +
    ylab("Sequence counts as of\nthe nowcast date")) +
    (seq_counts_eval + ylab("Sequence counts\nfor evaluation")) +
    eval_freq +
    plot_layout(
      design = fig_layout,
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
    plot = fig_eval,
    width = 12,
    height = 8
  )
  return(fig_eval)
}

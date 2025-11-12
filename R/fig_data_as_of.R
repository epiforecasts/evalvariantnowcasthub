#' Bar chart of sequence counts stacked and colored by clade
#'
#' @param obs_data Data.frame of number of sequences by clade and location
#' @param final_data Data.frame of final sequences by clade and location
#' @param location Location to plot (abbreviation)
#' @param temporal_granularity Temporal granularity to plot
#' @param plot_name Name of plot
#' @param output_fp File.path to save plot
#' @param log_scale Boolean indicating whether or not y axis should be on log
#'   scale, default is TRUE
#' @param nowcast_date_line Boolean indicating whether or not to include a
#'   dashed line for the nowcast date
#'
#' @returns ggplot object
#' @autoglobal
get_bar_chart_comparison <- function(obs_data,
                                     final_data,
                                     location,
                                     temporal_granularity,
                                     plot_name,
                                     output_fp = file.path(
                                       "output", "figs",
                                       "data_figs"
                                     ),
                                     log_scale = TRUE) {
  nowcast_date <- obs_data |>
    select(nowcast_date) |>
    distinct() |>
    pull()

  if (temporal_granularity == "days") {
    obs_data <- obs_data
    final_data <- final_data
  } else {
    obs_data <- daily_to_weekly(obs_data)
    final_data <- daily_to_weekly(final_data)
  }

  if (location == "US") {
    obs_data <- obs_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
    final_data <- final_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
  } else {
    obs_data <- filter(
      obs_data,
      location %in% !!location
    )
    final_data <- filter(
      final_data,
      location %in% !!location
    )
  }

  # filter final data to dates we want
  final_data <- final_data |>
    filter(
      date >= min(obs_data$date),
      date <= ymd(nowcast_date) + days(10)
    ) |>
    rename(sequences_final = sequences) |>
    select(location, date, clades_modeled, sequences_final)

  comb_data <- obs_data |>
    left_join(final_data, by = c(
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
    mutate(data_availability = ifelse(data_availability == "init",
      "as of nowcast date",
      "evaluation"
    ))


  plot_comps <- plot_components()


  p <- ggplot(comb_data) +
    geom_bar(
      aes(
        x = date, y = sequence_counts, fill = clades_modeled,
        alpha = data_availability
      ),
      stat = "identity", position = "stack"
    ) +
    get_plot_theme() +
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
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2
      )
    ) +
    scale_x_date(
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
    geom_vline(aes(xintercept = ymd(nowcast_date)),
      linetype = "dashed"
    )
  if (isTRUE(log_scale)) {
    p <- p + scale_y_continuous(transform = "log10")
  }

  return(p)
}

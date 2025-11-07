#' Plot observed clade frequency
#'
#' @param obs_data Data.frame of number of sequences by clade and location
#' @param location Location to plot
#' @param temporal_granularity Temporal granularity to plot
#' @param plot_name Name of plot
#' @param output_fp File.path to save plot
#'
#' @returns ggplot object
plot_obs_clade_freq <- function(obs_data,
                                location,
                                temporal_granularity,
                                plot_name,
                                output_fp = file.path(
                                  "output", "figs",
                                  "data_figs"
                                )) {
  if (location == "US") {
    obs_data <- obs_data |>
      group_by(date, clades_modeled) |>
      summarise(sequences = sum(sequences)) |>
      mutate(location = !!location)
  } else {
    obs_data <- filter(
      obs_data,
      location %in% !!location
    )
  }

  if (temporal_granularity == "days") {
    obs_data <- obs_data
  } # add something to aggregate by week

  seq_data <- obs_data |>
    group_by(date, location) |>
    summarise(n_seq = sum(sequences))
  variant_data_by_loc <- obs_data |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(seq_data) |>
    mutate(obs_freq = seq_clade / n_seq)

  plot_comps <- plot_components()

  p <- ggplot(variant_data_by_loc) +
    geom_line(aes(x = date, y = obs_freq, color = clades_modeled)) +
    get_plot_theme() +
    scale_color_manual(
      name = "Clades",
      values = plot_comps$clade_colors
    ) +
    xlab("") +
    ylab("Observed clade frequency") +
    ggtitle(glue::glue("{location}: Observed frequency")) +
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
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  dir_create(output_fp, recurse = TRUE)
  ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
    plot = p
  )
  return(p)
}

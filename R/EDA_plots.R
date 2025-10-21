#' Get a plot of the nowcasts for a specific nowcast date
#'
#' @param data Data.frame with nowcasts for a single nowcast date for
#'   multiple teams and (a few) locations
#' @importFrom ggplot2 ggplot geom_line aes facet_grid theme_bw vars
#' @importFrom dplyr filter
#' @returns ggplot object
get_plot_nowcasts <- function(data,
                              plot_name,
                              output_fp = file.path("output", "figs", "eda"),
                              save = TRUE) {
  nowcast_date <- unique(data$nowcast_date)

  df <- data |>
    mutate(output_type_id_clade = glue::glue("{output_type_id}-{clade}"))
  p <- ggplot(df) +
    geom_line(
      data = df |>
        filter(output_type == "mean"),
      aes(x = target_date, y = value, color = clade)
    ) +
    geom_line(
      data = df |>
        filter(output_type == "sample"),
      aes(
        x = target_date, y = value, color = clade,
        group = output_type_id_clade
      ),
      linewidth = 0.2, alpha = 0.2
    ) +
    geom_vline(aes(xintercept = nowcast_date)) +
    facet_grid(rows = vars(model_id), cols = vars(location)) +
    theme_bw() +
    xlab("Target date") +
    ylab("Estimated proportion")
  ggtitle(glue::glue("Nowcasts as of {nowcast_date}"))

  if (isTRUE(save)) {
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
#' @importFrom dplyr filter group_by summarise mutate
#' @returns ggplot object
get_plot_mult_locs <- function(data,
                               plot_name,
                               output_fp = file.path("output", "figs", "eda"),
                               save = TRUE) {
  variant_data_by_loc <- data |>
    group_by(clades_modeled, date, location) |>
    summarise(seq_clade = sum(sequences)) |>
    left_join(data |>
      group_by(date, location) |>
      summarise(n_seq = sum(sequences))) |>
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
    ggsave(file.path(output_fp, glue::glue("{plot_name}.png")),
      plot = p
    )
  }
  return(p)
}

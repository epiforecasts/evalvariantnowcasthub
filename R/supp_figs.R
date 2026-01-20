#' Get a plot of the weekly observed clade frequency in CA dataset compared
#'   to in the NCBI dataset
#'
#' @param clade_comp_weekly Dataframe with counts of number of sequences by
#'   clade and nowcast date for both data sources
#' @param fig_fp Character string indicating filepath of figure
#'
#' @returns ggplot of weekly clade frequencies for both data sources
get_bar_chart_clade_comp <- function(clade_comp_weekly,
                                     fig_fp = file.path(
                                       "output",
                                       "figs",
                                       "supp"
                                     )) {
  weekly_prop <- clade_comp_weekly |>
    group_by(nowcast_date, week, source) |>
    mutate(prop = nseq / sum(nseq)) |>
    ungroup()
  plot_comps <- plot_components()
  p <- ggplot(weekly_prop) +
    geom_bar(aes(x = week, y = prop, fill = clade), stat = "identity") +
    geom_vline(aes(xintercept = nowcast_date), linetype = "dashed") +
    facet_grid(source ~ nowcast_date, scales = "free_x") +
    get_plot_theme(dates = TRUE) +
    scale_y_continuous("Observed weekly clade frequency") +
    scale_fill_manual(
      name = "Clades",
      values = plot_comps$clade_colors
    ) +
    scale_x_date(
      date_breaks = "4 weeks",
      date_labels = "%d %b %Y"
    ) +
    xlab("")

  dir_create(fig_fp, recurse = TRUE)
  ggsave(file.path(fig_fp, glue::glue("CA_source_seq_props.png")),
    plot = p,
    width = 10,
    height = 6
  )

  return(p)
}

#' Get a plot of the ratio of number of sequences between the two datasets from
#'  CA and NCBI
#' @param volume_comp_weekly Dataframe with number of sequences by week and
#'   nowcast date for each dataset
#' @param fig_fp Character string indicating filepath of figure
#'
#' @returns ggplot of ratio of NCBI to CA data.
get_volume_prop_comp_weekly <- function(volume_comp_weekly,
                                        fig_fp = file.path(
                                          "output",
                                          "figs",
                                          "supp"
                                        )) {
  weekly_ratio <- mutate(volume_comp_weekly,
    ratio = ncbi / covidnet
  )
  plot_comps <- plot_components()
  p <- ggplot(weekly_ratio) +
    geom_line(aes(x = week, y = ratio)) +
    geom_hline(aes(yintercept = 1), color = "red") +
    geom_vline(aes(xintercept = nowcast_date), linetype = "dashed") +
    facet_grid(~nowcast_date, scales = "free_x") +
    get_plot_theme(dates = TRUE) +
    scale_y_continuous("Ratio of NCBI to CA\nCOVIDNet sequence volume") +
    scale_fill_discrete("Data") +
    scale_x_date(
      date_breaks = "4 weeks",
      date_labels = "%d %b %Y"
    ) +
    xlab("")

  dir_create(fig_fp, recurse = TRUE)
  ggsave(file.path(fig_fp, glue::glue("CA_source_seq_level_comparison.png")),
    plot = p,
    width = 10,
    height = 4
  )
}

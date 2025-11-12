#' Get standardized plot theme to add to figures
#'
#' @returns a theme object to add to a [ggplot2::ggplot()] object
#' @autoglobal
#' @importFrom ggplot2 theme element_rect
#' @importFrom cowplot theme_half_open background_grid
get_plot_theme <- function() {
  plot_theme <- cowplot::theme_half_open() +
    cowplot::background_grid() +
    theme(
      plot.background = element_rect(fill = "white"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 10)
    )

  return(plot_theme)
}

#' Get plot components (colors and shapes)
#'
#' @returns a list of the model colors to be passed to `scale_fill_manual` and
#'    `scale_color_manual`
#' @autoglobal
#' @importFrom RColorBrewer brewer.pal
plot_components <- function() {
  pal_models <- brewer.pal(5, "Spectral")
  pal_clades <- brewer.pal(12, "Paired")
  # nolint start
  model_colors <- c(
    "Hub-baseline" = "gray",
    "CADPH-CATaLog" = pal_models[1],
    "CADPH-CATaMaran" = pal_models[2],
    "LANL-CovTransformer" = pal_models[3],
    "UGA-multicast" = pal_models[4],
    "UMass-HMLR" = pal_models[5]
  )
  clade_colors <- c(
    "24A" = pal_clades[1],
    "24B" = pal_clades[2],
    "24C" = pal_clades[3],
    "24E" = pal_clades[4],
    "recombinant" = "brown4",
    "24F" = pal_clades[5],
    "24G" = pal_clades[6],
    "24H" = pal_clades[7],
    "24I" = pal_clades[8],
    "25A" = pal_clades[9],
    "24D" = pal_clades[10],
    "23A" = pal_clades[11],
    "25B" = pal_clades[12],
    "other" = "darkgray",
    "24A.as of nowcast date" = pal_clades[1],
    "24B.as of nowcast date" = pal_clades[2],
    "24C.as of nowcast date" = pal_clades[3],
    "24E.as of nowcast date" = pal_clades[4],
    "recombinant.as of nowcast date" = "brown4",
    "24F.as of nowcast date" = pal_clades[5],
    "24G.as of nowcast date" = pal_clades[6],
    "24H.as of nowcast date" = pal_clades[7],
    "24I.as of nowcast date" = pal_clades[8],
    "25A.as of nowcast date" = pal_clades[9],
    "24D.as of nowcast date" = pal_clades[10],
    "23A.as of nowcast date" = pal_clades[11],
    "25B.as of nowcast date" = pal_clades[12],
    "other.as of nowcast date" = "darkgray",
    "24A.evaluation" = pal_clades[1],
    "24B.evaluation" = pal_clades[2],
    "24C.evaluation" = pal_clades[3],
    "24E.evaluation" = pal_clades[4],
    "recombinant.evaluation" = "brown4",
    "24F.evaluation" = pal_clades[5],
    "24G.evaluation" = pal_clades[6],
    "24H.evaluation" = pal_clades[7],
    "24I.evaluation" = pal_clades[8],
    "25A.evaluation" = pal_clades[9],
    "24D.evaluation" = pal_clades[10],
    "23A.evaluation" = pal_clades[11],
    "25B.evaluation" = pal_clades[12],
    "other.evaluation" = "darkgray"
  )
  percentile_colors <- c(
    "top 90%" = "darkblue",
    "bottom 10%" = "brown"
  )
  data_availability_alpha <- c(
    "as of nowcast date" = 1,
    "evaluation" = 0.3
  )
  data_availability_linetype <- c(
    "as of nowcast date" = "solid",
    "evaluation" = "dashed"
  )
  # nolint end

  plot_comp_list <-
    list(
      model_colors = model_colors,
      clade_colors = clade_colors,
      percentile_colors = percentile_colors,
      data_availability_alpha = data_availability_alpha,
      data_availability_linetype = data_availability_linetype
    )
  return(plot_comp_list)
}

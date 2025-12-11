#' Get standardized plot theme to add to figures
#' @param dates Boolean indicating whether dates are on x-axis
#' @returns a theme object to add to a [ggplot2::ggplot()] object
#' @autoglobal
#' @importFrom ggplot2 theme element_rect
#' @importFrom cowplot theme_half_open background_grid
get_plot_theme <- function(dates = FALSE) {
  plot_theme <- cowplot::theme_half_open() +
    cowplot::background_grid() +
    theme(
      plot.background = element_rect(fill = "white"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 10)
    )
  if (isTRUE(dates)) {
    plot_theme <- plot_theme +
      theme(
        axis.text.x = element_text(
          vjust = 1,
          hjust = 1,
          angle = 45,
          size = 11
        )
      )
  }

  return(plot_theme)
}

#' Get plot components (colors and shapes)
#'
#' @returns a list of the model colors to be passed to `scale_fill_manual` and
#'    `scale_color_manual`
#' @autoglobal
#' @importFrom RColorBrewer brewer.pal
plot_components <- function() {
  pal_clades <- brewer.pal(12, "Paired")
  # Combine them for 12+ distinct colors
  pal_models <- c(
    "#E41A1C", # red
    "#377EB8", # blue
    "#984EA3", # purple
    "#A65628", # brown
    "#66C2A5", # teal
    "#8DA0CB" # lavender
  )
  # nolint start
  model_colors <- c(
    "Hub-baseline" = "gray30",
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
    "recombinant" = "#8B0000",
    "24F" = pal_clades[5],
    "24G" = pal_clades[6],
    "24H" = pal_clades[7],
    "24I" = pal_clades[8],
    "25A" = pal_clades[9],
    "24D" = pal_clades[10],
    "23A" = pal_clades[11],
    "25B" = pal_clades[12],
    "other" = "gray50",
    "24A.as of nowcast date" = pal_clades[1],
    "24B.as of nowcast date" = pal_clades[2],
    "24C.as of nowcast date" = pal_clades[3],
    "24E.as of nowcast date" = pal_clades[4],
    "recombinant.as of nowcast date" = "#8B0000",
    "24F.as of nowcast date" = pal_clades[5],
    "24G.as of nowcast date" = pal_clades[6],
    "24H.as of nowcast date" = pal_clades[7],
    "24I.as of nowcast date" = pal_clades[8],
    "25A.as of nowcast date" = pal_clades[9],
    "24D.as of nowcast date" = pal_clades[10],
    "23A.as of nowcast date" = pal_clades[11],
    "25B.as of nowcast date" = pal_clades[12],
    "other.as of nowcast date" = "gray50",
    "24A.evaluation" = pal_clades[1],
    "24B.evaluation" = pal_clades[2],
    "24C.evaluation" = pal_clades[3],
    "24E.evaluation" = pal_clades[4],
    "recombinant.evaluation" = "#8B0000",
    "24F.evaluation" = pal_clades[5],
    "24G.evaluation" = pal_clades[6],
    "24H.evaluation" = pal_clades[7],
    "24I.evaluation" = pal_clades[8],
    "25A.evaluation" = pal_clades[9],
    "24D.evaluation" = pal_clades[10],
    "23A.evaluation" = pal_clades[11],
    "25B.evaluation" = pal_clades[12],
    "other.evaluation" = "gray50"
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
  pred_int_alpha = c(
    "50%" = 1,
    "95%" = 0.5
  )
  # nolint end

  plot_comp_list <-
    list(
      model_colors = model_colors,
      clade_colors = clade_colors,
      percentile_colors = percentile_colors,
      data_availability_alpha = data_availability_alpha,
      data_availability_linetype = data_availability_linetype,
      pred_int_alpha = pred_int_alpha
    )
  return(plot_comp_list)
}

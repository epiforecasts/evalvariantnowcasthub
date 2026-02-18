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
      legend.text = element_text(size = 11),
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
  model_shapes <- c(
    "Hub-baseline" = 16,
    "CADPH-CATaLog" = 8,
    "CADPH-CATaMaran" = 4,
    "LANL-CovTransformer" = 18,
    "UGA-multicast" = 17,
    "UMass-HMLR" = 15
  )

  type_rel_skill_shapes <- c(
    "By overlapping set" = 21,
    "Average across individual days" = 25
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
    "24A (JN.1)" = pal_clades[1],
    "24B (JN.1.11.1)" = pal_clades[2],
    "24C (KP.3)" = pal_clades[3],
    "24E (KP.3.1.1)" = pal_clades[4],
    "recombinant" = "#8B0000",
    "24F (XEC)" = pal_clades[5],
    "24G (KP.2.3)" = pal_clades[6],
    "24H (LF.7)" = pal_clades[7],
    "24I (MV.1)" = pal_clades[8],
    "25A (LP.8.1)" = pal_clades[9],
    "24D (XDV.1)" = pal_clades[10],
    "23A (XBB.1.5)" = pal_clades[11],
    "25B (NB.1.8.1)" = pal_clades[12],
    "24A (JN.1).as of nowcast date" = pal_clades[1],
    "24B (JN.1.11.1).as of nowcast date" = pal_clades[2],
    "24C (KP.3).as of nowcast date" = pal_clades[3],
    "24E (KP.3.1.1).as of nowcast date" = pal_clades[4],
    "recombinant.as of nowcast date" = "#8B0000",
    "24F (XEC).as of nowcast date" = pal_clades[5],
    "24G (KP.2.3).as of nowcast date" = pal_clades[6],
    "24H (LF.7).as of nowcast date" = pal_clades[7],
    "24I (MV.1).as of nowcast date" = pal_clades[8],
    "25A (LP.8.1).as of nowcast date" = pal_clades[9],
    "24D (XDV.1).as of nowcast date" = pal_clades[10],
    "23A (XBB.1.5).as of nowcast date" = pal_clades[11],
    "25B (NB.1.8.1).as of nowcast date" = pal_clades[12],
    "other.as of nowcast date" = "gray50",
    "24A (JN.1).evaluation" = pal_clades[1],
    "24B (JN.1.11.1).evaluation" = pal_clades[2],
    "24C (KP.3).evaluation" = pal_clades[3],
    "24E (KP.3.1.1.).evaluation" = pal_clades[4],
    "recombinant.evaluation" = "#8B0000",
    "24F (XEC).evaluation" = pal_clades[5],
    "24G (KP.2.3).evaluation" = pal_clades[6],
    "24H (LF.7).evaluation" = pal_clades[7],
    "24I (MV.1).evaluation" = pal_clades[8],
    "25A (LP.8.1).evaluation" = pal_clades[9],
    "24D (XDV.1).evaluation" = pal_clades[10],
    "23A (XBB.1.5).evaluation" = pal_clades[11],
    "25B (NB.1.8.1).evaluation" = pal_clades[12],
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
  pred_int_alpha <- c(
    "50%" = 1,
    "90%" = 0.5
  )
  # nolint end

  plot_comp_list <-
    list(
      model_colors = model_colors,
      model_shapes = model_shapes,
      clade_colors = clade_colors,
      percentile_colors = percentile_colors,
      data_availability_alpha = data_availability_alpha,
      data_availability_linetype = data_availability_linetype,
      pred_int_alpha = pred_int_alpha,
      type_rel_skill_shapes = type_rel_skill_shapes
    )
  return(plot_comp_list)
}

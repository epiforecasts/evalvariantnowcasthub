#' Add confidence band error bars to an overall score panel
#'
#' Adds Bonferroni simultaneous confidence bands as error bars, by model, to
#' a panel from [get_plot_overall()] (absolute score bars or scaled
#' relative skill points), leaving the panel otherwise unchanged.
#'
#' @param p ggplot object from [get_plot_overall()].
#' @param ci Confidence bands with columns `model`, `lower` and `upper`, as
#'   returned by [get_bonferroni_ci()].
#' @param ylim Optional numeric vector of length 2 giving new y axis limits,
#'   e.g. to show bands extending beyond the panel's original limits.
#' @param y_breaks Optional numeric vector of breaks for a log10 y axis,
#'   replacing the panel's original log10 scale. Use only for relative
#'   skill panels.
#' @param point_size Optional size for the panel's points, e.g. so large
#'   relative skill markers do not hide narrow error bars.
#' @importFrom ggplot2 geom_errorbar geom_point aes coord_cartesian
#'   scale_y_continuous
#' @returns ggplot object
#' @autoglobal
add_ci_errorbars <- function(p,
                             ci,
                             ylim = NULL,
                             y_breaks = NULL,
                             point_size = NULL) {
  if (!is.null(point_size)) {
    # Replace each point layer with a new one with the same mapping, so
    # the original panel object is not modified
    p$layers <- lapply(p$layers, function(layer) {
      if (!inherits(layer$geom, "GeomPoint")) {
        return(layer)
      }
      geom_point(mapping = layer$mapping, size = point_size)
    })
  }
  p <- p +
    geom_errorbar(
      data = ci,
      aes(x = model, ymin = lower, ymax = upper),
      width = 0.3,
      inherit.aes = FALSE
    )
  replace_y_axis(p, ylim, y_breaks)
}

#' Add confidence band ribbons to a relative skill by horizon panel
#'
#' Adds Bonferroni simultaneous confidence bands by model and horizon as
#' shaded ribbons, drawn behind the lines of a relative skill panel from
#' [get_plot_horizon()]. Models without bands (e.g. those excluded from the
#' tables) keep their line without a ribbon.
#'
#' @param p ggplot object from [get_plot_horizon()] with
#'   `rel_skill_plot = TRUE`.
#' @param ci Confidence bands with columns `model`, `horizon`, `lower` and
#'   `upper`, as returned by [get_bonferroni_ci()] with
#'   `by = c("model", "horizon")`.
#' @inheritParams add_ci_errorbars
#' @importFrom ggplot2 geom_ribbon aes scale_fill_manual
#' @returns ggplot object
#' @autoglobal
add_ci_ribbons <- function(p, ci, ylim = NULL, y_breaks = NULL) {
  ribbon <- geom_ribbon(
    data = ci,
    aes(x = horizon, ymin = lower, ymax = upper, fill = model),
    alpha = 0.2,
    inherit.aes = FALSE
  )
  p$layers <- c(ribbon, p$layers)
  p <- p + scale_fill_manual(
    values = plot_components()$model_colors,
    guide = "none"
  )
  replace_y_axis(p, ylim, y_breaks)
}

#' Replace the y axis limits and log10 breaks of a panel
#'
#' The panels already set their own limits and scales, so ggplot2 reports
#' that they are being replaced; those messages are expected and silenced.
#'
#' @inheritParams add_ci_errorbars
#' @importFrom ggplot2 coord_cartesian scale_y_continuous
#' @returns ggplot object
replace_y_axis <- function(p, ylim = NULL, y_breaks = NULL) {
  if (!is.null(y_breaks)) {
    p <- suppressMessages(
      p + scale_y_continuous(trans = "log10", breaks = y_breaks)
    )
  }
  if (!is.null(ylim)) {
    p <- suppressMessages(p + coord_cartesian(ylim = ylim))
  }
  p
}

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

#' Add confidence band error bars to a by-location panel
#'
#' Adds Bonferroni simultaneous confidence bands as error bars to a panel
#' from [get_plot_by_location()], with locations on the x axis and one bar
#' or point per model. Error bars are dodged by model to line up with the
#' panel's dodged bars; with `dodge_points = TRUE` the relative skill
#' points, drawn on top of each other in the original panel, are dodged the
#' same way so each model's band is readable, with error bars coloured
#' by model. Model and location
#' combinations without a band (e.g. too few nowcast dates) keep their
#' place in the dodge but have no error bar.
#'
#' @param p ggplot object from [get_plot_by_location()].
#' @param ci Confidence bands with columns `location`, `model`, `lower` and
#'   `upper`, as returned by [get_bonferroni_ci()] with
#'   `by = c("location", "model")`.
#' @param dodge_width Width used to dodge models within a location; use the
#'   panel's own dodge width (0.9) for bar panels.
#' @param dodge_points Logical; dodge the panel's points by model. Use for
#'   relative skill panels.
#' @inheritParams add_ci_errorbars
#' @importFrom ggplot2 geom_errorbar geom_point aes position_dodge
#' @importFrom dplyr distinct left_join mutate
#' @importFrom rlang %||%
#' @returns ggplot object
#' @autoglobal
add_ci_errorbars_by_location <- function(p,
                                         ci,
                                         dodge_width,
                                         dodge_points = FALSE,
                                         point_size = NULL,
                                         ylim = NULL,
                                         y_breaks = NULL) {
  dodge <- position_dodge(width = dodge_width)
  if (isTRUE(dodge_points)) {
    # Replace each point layer with a dodged one with the same mapping, so
    # the original panel object is not modified
    p$layers <- lapply(p$layers, function(layer) {
      if (!inherits(layer$geom, "GeomPoint")) {
        return(layer)
      }
      size <- point_size %||% layer$aes_params$size
      geom_point(mapping = layer$mapping, size = size, position = dodge)
    })
  }
  # One row per model and location shown in the panel, so that error bars
  # are dodged into the same slots as the panel's bars or points
  plot_rows <- distinct(as.data.frame(p$data), location, model)
  ci_plot <- plot_rows |>
    left_join(
      mutate(ci, location = as.character(location)),
      by = c("location", "model")
    ) |>
    mutate(location = factor(location, levels = levels(p$data$location)))
  if (isTRUE(dodge_points)) {
    # Coloured by model, so bands stay distinguishable next to the points
    errorbars <- geom_errorbar(
      data = ci_plot,
      aes(x = location, ymin = lower, ymax = upper, color = model),
      width = 0.4 * dodge_width,
      linewidth = 0.4,
      position = dodge,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  } else {
    errorbars <- geom_errorbar(
      data = ci_plot,
      aes(x = location, ymin = lower, ymax = upper, group = model),
      width = 0.4 * dodge_width,
      linewidth = 0.3,
      position = dodge,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  }
  replace_y_axis(p + errorbars, ylim, y_breaks)
}

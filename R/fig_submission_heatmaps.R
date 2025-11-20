#' Prepare submission presence/absence data
#'
#' @param all_model_outputs Full model outputs across all dates and locations
#' @param location_data Location metadata
#' @param nowcast_dates All nowcast dates to include
#'
#' @returns Data frame with presence/absence indicator for each
#'   model-location-nowcast_date combination
#' @autoglobal
prepare_submission_data <- function(all_model_outputs,
                                    location_data,
                                    nowcast_dates) {
  # Create complete grid of all combinations
  # Convert nowcast_dates to Date to match all_model_outputs
  all_combinations <- expand.grid(
    model_id = unique(all_model_outputs$model_id),
    location = location_data$abbreviation,
    nowcast_date = ymd(nowcast_dates),
    stringsAsFactors = FALSE
  )

  # Identify which combinations actually have submissions
  actual_submissions <- all_model_outputs |>
    distinct(model_id, location, nowcast_date) |>
    mutate(submitted = TRUE)

  # Join and fill missing with FALSE
  submission_status <- all_combinations |>
    left_join(actual_submissions,
      by = c("model_id", "location", "nowcast_date")
    ) |>
    mutate(submitted = replace_na(submitted, FALSE))

  return(submission_status)
}

#' Plot submission heatmap for a single model
#'
#' @param submission_data Prepared submission status data
#' @param model_id Model ID to plot
#' @param plot_components List containing theme and color information
#'
#' @returns ggplot2 object
#' @autoglobal
plot_model_submission_heatmap <- function(submission_data,
                                          model_id,
                                          plot_components) {
  model_data <- submission_data |>
    filter(model_id == !!model_id)

  model_color <- plot_components$model_colors[model_id]

  ggplot(model_data, aes(x = nowcast_date, y = location, fill = submitted)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_manual(
      values = c("TRUE" = model_color, "FALSE" = "gray90"),
      labels = c("TRUE" = "Submitted", "FALSE" = "Not submitted"),
      name = "Status"
    ) +
    labs(
      title = model_id,
      x = "Nowcast Date",
      y = "Location"
    ) +
    plot_components$theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 6),
      legend.position = "bottom"
    )
}

#' Plot summary heatmap across all models
#'
#' @param submission_data Prepared submission status data
#' @param plot_components List containing theme and color information
#'
#' @returns ggplot2 object
#' @autoglobal
plot_submission_summary_heatmap <- function(submission_data,
                                            plot_components) {
  summary_data <- submission_data |>
    group_by(location, nowcast_date) |>
    summarise(n_models = sum(submitted), .groups = "drop")

  ggplot(summary_data, aes(x = nowcast_date, y = location, fill = n_models)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_viridis_c(
      option = "viridis",
      limits = c(0, 6),
      breaks = 0:6,
      name = "Number of\nModels"
    ) +
    labs(
      title = "All Models Summary",
      x = "Nowcast Date",
      y = "Location"
    ) +
    plot_components$theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 6),
      legend.position = "bottom"
    )
}

#' Create complete submission heatmap figure
#'
#' @param submission_data Prepared submission status data
#' @param plot_components List containing theme and color information
#'
#' @returns Combined patchwork plot
#' @autoglobal
create_submission_heatmap_figure <- function(submission_data,
                                             plot_components) {
  # Get all unique models
  model_ids <- unique(submission_data$model_id)

  # Create per-model plots
  model_plots <- lapply(model_ids, function(mid) {
    plot_model_submission_heatmap(submission_data, mid, plot_components)
  })

  # Create summary plot
  summary_plot <- plot_submission_summary_heatmap(
    submission_data,
    plot_components
  )

  # Combine using patchwork
  combined <- wrap_plots(c(model_plots, list(summary_plot)),
    ncol = 2
  ) +
    plot_annotation(
      title = "Model Submission Coverage by Location and Nowcast Date"
    )

  return(combined)
}

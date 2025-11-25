# Analysis script to generate CORP decomposition of Brier scores using reliabilitydiag
# This script loads predictions and observations from the targets pipeline,
# processes them to create binary outcomes for each clade, and applies reliabilitydiag.

library(targets)
library(dplyr)
library(tidyr)
library(reliabilitydiag)
library(ggplot2)
library(ggrepel)
library(geomtextpath)
library(glue)
library(arrow)

# Load the necessary data from the targets store
tar_load(clean_variant_data_final_all_states)
all_model_outputs_for_heatmap <- read_parquet(
    "~/variant-25a.parquet"
)

# Define function to run analysis and plot
run_corp_analysis <- function(
    location_subset_name,
    loc_filter_fn,
    model_selector,
    output_file
) {
    cat(glue::glue("Processing {location_subset_name}...\n"))

    # 1. Prepare Observations
    obs <- clean_variant_data_final_all_states |>
        filter(loc_filter_fn(location)) |>
        rename(target_date = date, clade = clades_modeled) |>
        group_by(location, target_date) |>
        mutate(total_sequences = sum(sequences)) |>
        ungroup() |>
        select(location, target_date, clade, sequences, total_sequences)

    # 2. Prepare Predictions
    preds_long <- all_model_outputs_for_heatmap |>
        filter(
            loc_filter_fn(location),
            output_type == "mean",
            clade == "25A"
        ) |>
        select(model_id, location, nowcast_date, target_date, clade, value) |>
        rename(prediction = value)

    # Pivot to wide to handle model selection
    preds_wide <- preds_long |>
        pivot_wider(names_from = model_id, values_from = prediction)

    # Apply model selection logic
    preds_wide <- model_selector(preds_wide)

    # 3. Join Predictions and Observations
    data_joined_full <- preds_wide |>
        inner_join(obs, by = c("location", "target_date", "clade"))

    # Identify model columns
    non_model_cols <- c(
        "location",
        "target_date",
        "clade",
        "sequences",
        "total_sequences",
        "nowcast_date"
    )
    model_cols <- setdiff(names(data_joined_full), non_model_cols)

    # Drop rows with NAs in any of the selected models to ensure fair comparison
    data_joined <- data_joined_full |>
        drop_na(all_of(model_cols))

    dropped_dates <- setdiff(
        data_joined_full$nowcast_date,
        data_joined$nowcast_date
    )
    if (length(dropped_dates) > 0) {
        cli::cli_alert_info(
            "Dropped nowcast dates {dropped_dates} due to missing model predictions."
        )
    }

    if (nrow(data_joined) == 0) {
        message(glue::glue(
            "No data available for {location_subset_name} after filtering."
        ))
        return(NULL)
    }

    # 4. Expand data to binary observations
    # Successes
    df_success <- data_joined |>
        select(all_of(c(model_cols, "sequences"))) |>
        uncount(sequences) |>
        mutate(y = 1)

    # Failures
    df_failure <- data_joined |>
        mutate(failures = total_sequences - sequences) |>
        select(all_of(c(model_cols, "failures"))) |>
        uncount(failures) |>
        mutate(y = 0)

    # Combine
    df_expanded <- bind_rows(df_success, df_failure)

    # Extract predictions (X) and observations (y)
    X <- df_expanded[model_cols]
    y <- df_expanded$y

    cat("Running reliabilitydiag...\n")
    rd <- reliabilitydiag(X, y = y)

    # 5. Collect results
    corp_summary <- summary(rd) |>
        rename(model_id = forecast) |>
        as_tibble() |>
        mutate(`R*` = (discrimination - miscalibration) / uncertainty)

    print(corp_summary)

    # Calculate Brier Score
    corp_summary <- corp_summary |>
        mutate(brier_score = mean_score)

    # Prepare for plotting
    unc_val <- mean(corp_summary$uncertainty)
    unc_str <- round(unc_val, 4)
    min_date <- min(data_joined$nowcast_date)
    max_date <- max(data_joined$nowcast_date)

    brier_range <- range(corp_summary$brier_score)
    brier_iso <- seq(
        from = brier_range[1] - diff(brier_range) * 0.2,
        to = brier_range[2] + diff(brier_range) * 0.2,
        length.out = 10
    )

    iso <- data.frame(
        intercept = unc_val - brier_iso,
        slope = 1,
        brier_iso = round(brier_iso, 4)
    )

    p <- ggplot(corp_summary) +
        geom_abline(
            data = iso,
            aes(intercept = intercept, slope = slope),
            color = "lightgray",
            alpha = 0.5,
            linewidth = 0.5
        ) +
        geom_labelabline(
            data = iso,
            aes(intercept = intercept, slope = slope, label = brier_iso),
            color = "gray50",
            hjust = 0.5,
            size = 3,
            text_only = TRUE,
            boxcolour = NA,
            straight = TRUE
        ) +
        geom_point(
            aes(x = miscalibration, y = discrimination, color = model_id),
            size = 3,
            show.legend = FALSE
        ) +
        geom_text_repel(
            aes(
                x = miscalibration,
                y = discrimination,
                label = model_id,
                color = model_id
            ),
            show.legend = FALSE,
            max.overlaps = NA,
            size = 4,
            seed = 42
        ) +
        labs(
            title = "CORP Decomposition of Brier Score",
            subtitle = glue::glue(
                "Location: {location_subset_name}; Variant: 25A; Dates: {min_date} to {max_date}; Uncertainty = {unc_str}"
            ),
            btitle = glue::glue(
                "Location: {location_subset_name}; Variant: 25A; Uncertainty = {unc_str}"
            ),
            x = "Miscalibration (MCB) - Lower is better",
            y = "Discrimination (DSC) - Higher is better"
        ) +
        theme_bw(base_size = 11) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            aspect.ratio = 1,
            legend.position = "bottom"
        )

    print(p)
    ggsave(output_file, p, width = 8, height = 8)
}

# --- Run 1: California ---
run_corp_analysis(
    location_subset_name = "California",
    loc_filter_fn = function(x) x == "CA",
    model_selector = function(df) {
        df |>
            select(-any_of("CADPH-CATaMaran")) |>
            filter(!is.na(`CADPH-CATaLog`))
    },
    output_file = "analysis/corp_decomposition_plot_CA.png"
)

# --- Run 2: Other States ---
run_corp_analysis(
    location_subset_name = "US ex CA",
    loc_filter_fn = function(x) x != "CA",
    model_selector = function(df) {
        df |>
            # Remove columns that are all NA (models not defined for these states)
            select(where(~ !all(is.na(.)))) |>
            # Explicitly remove CA models if they exist
            select(-matches("CADPH"))
    },
    output_file = "analysis/corp_decomposition_plot_Other.png"
)

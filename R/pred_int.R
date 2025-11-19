get_pred_int <- function(model_pred_prop,
                         eval_seq) {
  # Get the final sequence counts for each location target date
  seq_counts_by_date_loc <- eval_seq |>
    group_by(location, nowcast_date, date) |>
    summarise(n_seq = sum(sequences)) |>
    ungroup()

  # For each nowcast_date, location, target, date, and draw, sample from a
  # multinomial 100 times
  nowcast_dates <- unique(model_pred_prop$nowcast_date)
  locs <- unique(model_pred_prop$location)
  target_dates <- unique(model_pred_prop$target_date)
  df_summary <- data.frame()
  i <- 0
  for (nowcast_date_i in nowcast_dates) {
    for (loc in locs) {
      for (date in target_dates) {
        N <- seq_counts_by_date_loc |>
          filter(
            nowcast_date == nowcast_date_i,
            location == loc,
            date == !!date
          ) |>
          pull(n_seq)
        obs_data <- eval_seq |>
          filter(
            nowcast_date == nowcast_date_i,
            location == loc,
            date == !!date
          ) |>
          mutate(nowcast_date = ymd(nowcast_date))
        # Get sample of modeled counts from each model with samples
        # for this target date and location
        df_samp <- filter(
          model_pred_prop,
          nowcast_date == nowcast_date_i,
          location == loc,
          target_date == date,
          output_type == "sample"
        )
        # Combine and renumber
        df_comb <- df_samp |>
          # nolint start
          left_join(obs_data, by = c("nowcast_date",
            "location",
            "target_date" = "date",
            "clade" = "clades_modeled"
          )) |>
          # nolint end
          mutate(
            n_seq = N,
            sample_id = as.numeric(gsub("[^0-9]", "", output_type_id))
          ) |>
          group_by(model_id) |>
          mutate(sample_id = sample_id - min(sample_id)) |>
          select(-output_type_id)


        df_samp_wide <- pivot_wider(df_comb,
          names_from = sample_id,
          values_from = value
        ) |>
          ungroup()
        models_this_combo <- unique(df_samp_wide$model_id)
        for (model in models_this_combo) {
          i <- i + 1
          if (length(N) == 1 && N > 0) {
            df_model <- filter(
              df_samp_wide,
              model_id == !!model
            )

            df_spine <- select(
              df_model,
              model_id, nowcast_date, target_date,
              location, clade, output_type,
              location_name, location_code, population,
              type, sequences, n_seq
            )

            # 100 column matrix 1 for each draw
            samp_matrix <- as.matrix(df_model[, 13:ncol(df_model)])

            # 10,000 column matrix 100 draws for each posterior draw
            samp_multinomial_counts <- do.call(
              cbind,
              lapply(
                seq_len(ncol(samp_matrix)),
                function(col) {
                  return(rmultinom(
                    n = 100,
                    size = N,
                    prob = samp_matrix[, col]
                  ))
                }
              )
            )

            df_sampled <- cbind(df_spine, samp_multinomial_counts)
            df_sampled_long <- df_sampled |>
              pivot_longer(
                cols = 13:ncol(df_sampled),
                names_to = "multinomial_sample",
                values_to = "predicted_observation"
              )

            df_sampled_summarised <- df_sampled_long |>
              mutate(
                obs_freq = sequences / n_seq,
                predicted_freq = predicted_observation / n_seq
              ) |>
              group_by(model_id, nowcast_date, clade, target_date, location) |>
              summarise(
                n_seq = first(n_seq),
                sequences = first(sequences),
                q_0.5 = quantile(predicted_freq, 0.5),
                q_0.025 = quantile(predicted_freq, 0.025),
                q_0.975 = quantile(predicted_freq, 0.975),
                q_0.25 = quantile(predicted_freq, 0.25),
                q_0.75 = quantile(predicted_freq, 0.75)
              )
          } else {
            df_sample_summarised <- NULL
          }

          df_summary <- bind_rows(
            df_summary,
            df_sampled_summarised
          )
        }
      }
    }
  }

  return(df_summary)
}

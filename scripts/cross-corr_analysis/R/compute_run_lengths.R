compute_run_lengths <- function(df_long) {
  df_long %>%
    group_by(individual_ID) %>%
    arrange(variable, .by_group = TRUE) %>%
    reframe({
      r <- rle(value)
      start_idx <- cumsum(c(1, head(r$lengths, -1)))
      n_other_emerged <- sapply(start_idx, function(idx) {
        row <- df_long[idx, ]
        row$n_emerged - row$value
      })
      tibble(
        run_type = r$values,
        run_length = r$lengths,
        n_other_emerged = n_other_emerged,
        individual_ID = unique(individual_ID)
      )
    }) %>%
    ungroup()
}

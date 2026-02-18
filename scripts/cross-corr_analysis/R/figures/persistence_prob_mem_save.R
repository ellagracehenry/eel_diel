library(data.table)
library(glmmTMB)
library(ggplot2)

persistence_prob_safe <- function(each_ind_each_sec_df, bin_size = 30) {
  
  # Convert to data.table
  dt <- as.data.table(each_ind_each_sec_df)
  
  # Create factor for date if not already
  dt[, date_f := as.factor(date_f)]
  dt[, site := as.factor(site)]
  dt[, colony := as.factor(colony)]
  dt[, individual_ID := as.factor(individual_ID)]
  
  # Define bins by size in seconds
  bin_breaks <- seq(0, max(dt$sec_since_midnight, na.rm = TRUE) + bin_size, by = bin_size)
  
  # Aggregate directly
  agg_dt <- dt[, .(
    value = first(value),
    next_state = last(next_state),
    n_emerged = first(n_emerged),
    n_obs = .N
  ), by = .(
    date_f, site, colony, individual_ID, colony_size,
    time_bin = cut(sec_since_midnight, breaks = bin_breaks, labels = FALSE, include.lowest = TRUE)
  )]
  
  # Count unique sites and colonies per date
  date_counts <- agg_dt[, .(n_sites = uniqueN(site), n_colonies = uniqueN(colony)), by = date_f]
  
  # Keep only dates with >1 site AND >1 colony
  dates_keep <- date_counts[n_sites > 1 & n_colonies > 1, date_f]
  
  # Subset original data
  agg_dt <- agg_dt[date_f %in% dates_keep]
  
  
  # Compute stay_hidden / stay_emerged
  agg_dt[, stay_hidden := as.numeric(value == 0 & next_state == 0)]
  agg_dt[, stay_emerged := as.numeric(value == 1 & next_state == 1)]
  
  # Scale n_emerged and time_bin
  n_emerged_mean <- mean(agg_dt$n_emerged)
  n_emerged_sd   <- sd(agg_dt$n_emerged)
  agg_dt[, n_emerged_s := (n_emerged - n_emerged_mean) / n_emerged_sd]
  agg_dt[, time_bin_s := as.numeric(scale(time_bin))]
  colony_size_mean <- mean(agg_dt$colony_size)
  colony_size_sd <- sd(agg_dt$colony_size)
  agg_dt[, colony_size_s := (colony_size - colony_size_mean) / colony_size_sd]
  
  # Split hidden and emerged data
  hidden_agg <- agg_dt[value == 0]
  emerged_agg <- agg_dt[value == 1]
  
  # Fit GLMMs
  hidden_model <- glmmTMB(
    stay_hidden ~ time_bin_s + n_emerged_s*site + colony_size_s + (1 | colony:individual_ID) + (1 | date_f),
    data = hidden_agg,
    family = binomial
  )
  
  emerged_model <- glmmTMB(
    stay_emerged ~ time_bin_s + n_emerged_s*site + colony_size_s + (1 | colony:individual_ID) + (1 | date_f),
    data = emerged_agg,
    family = binomial
  )
  
  # Predictions
  full_n_emerged <- 0:max(agg_dt$n_emerged)
  n_emerged_s_full <- (full_n_emerged - n_emerged_mean) / n_emerged_sd
  # pred_df <- data.frame(n_emerged_s = n_emerged_s_full,
  #                       time_bin_s = mean(hidden_agg$time_bin_s, na.rm = TRUE),
  #                       colony_size_s = mean(hidden_agg$colony_size_s, na.rm = TRUE),
  #                       site = factor(levels(hidden_agg$site), levels = levels(hidden_agg$site)))
  
  # Choose time bins you want to predict for
  #time_bins_seq <- seq(min(hidden_agg$time_bin_s), max(hidden_agg$time_bin_s), length.out = 5)
  
  # Choose time bins you want to predict for safely
  if(nrow(hidden_agg) > 0 && any(is.finite(hidden_agg$time_bin_s))){
    time_bins_seq <- seq(
      min(hidden_agg$time_bin_s, na.rm = TRUE),
      max(hidden_agg$time_bin_s, na.rm = TRUE),
      length.out = 5
    )
  } else {
    # fallback if no valid data
    time_bins_seq <- 0
    warning("No valid time_bin_s in hidden_agg; using single default value 0")
  }
  
  # All sites
  sites <- levels(hidden_agg$site)
  
  # Build prediction grid
  pred_df <- expand.grid(
    n_emerged_s = n_emerged_s_full,
    time_bin_s = time_bins_seq,
    colony_size_s = mean(hidden_agg$colony_size_s, na.rm = TRUE),
    site = sites
  )
  pred_df$site <- factor(pred_df$site, levels = sites)  # preserve factor levels
  
  # Check for NA or NaN
  stopifnot(all(is.finite(pred_df$time_bin_s)))
  stopifnot(all(is.finite(pred_df$n_emerged_s)))
  
  # Hidden → Hidden
  pred_hidden <- predict(hidden_model, newdata = pred_df, type = "response", re.form = NA, se.fit = TRUE)
  # pred_hidden_df <- data.frame(
  #   n_emerged = full_n_emerged,
  #   predicted = pred_hidden$fit,
  #   conf.low  = pred_hidden$fit - 1.96 * pred_hidden$se.fit,
  #   conf.high = pred_hidden$fit + 1.96 * pred_hidden$se.fit,
  #   transition = "Hidden → Hidden"
  # )
  
  pred_hidden_df <- data.frame(
    n_emerged = rep(full_n_emerged, times = length(time_bins_seq) * length(sites)),
    time_bin_s = rep(time_bins_seq, each = length(full_n_emerged), times = length(sites)),
    site = rep(sites, each = length(full_n_emerged) * length(time_bins_seq)),
    predicted = pred_hidden$fit,
    conf.low  = pred_hidden$fit - 1.96 * pred_hidden$se.fit,
    conf.high = pred_hidden$fit + 1.96 * pred_hidden$se.fit,
    transition = "Hidden → Hidden"
  )
  
  
  # Emerged → Emerged
  pred_emerged <- predict(emerged_model, newdata = pred_df, type = "response", re.form = NA, se.fit = TRUE)
  # pred_emerged_df <- data.frame(
  #   n_emerged = full_n_emerged,
  #   predicted = pred_emerged$fit,
  #   conf.low  = pred_emerged$fit - 1.96 * pred_emerged$se.fit,
  #   conf.high = pred_emerged$fit + 1.96 * pred_emerged$se.fit,
  #   transition = "Emerged → Emerged"
  # )
  
  pred_emerged_df <- data.frame(
    n_emerged = rep(full_n_emerged, times = length(time_bins_seq) * length(sites)),
    time_bin_s = rep(time_bins_seq, each = length(full_n_emerged), times = length(sites)),
    site = rep(sites, each = length(full_n_emerged) * length(time_bins_seq)),
    predicted = pred_emerged$fit,
    conf.low  = pred_emerged$fit - 1.96 * pred_emerged$se.fit,
    conf.high = pred_emerged$fit + 1.96 * pred_emerged$se.fit,
    transition = "Emerged → Emerged"
  )
  
  
  pred_combined <- rbind(pred_hidden_df, pred_emerged_df)
  
  return(list(
    hidden_model = hidden_model,
    emerged_model = emerged_model,
    pred_hidden = pred_hidden_df,
    pred_emerged = pred_emerged_df,
    full_n_emerged = full_n_emerged
  ))
  
  # # Plot
  # raw_hidden <- hidden_agg[, .(n_emerged, prob = stay_hidden)]
  # raw_hidden[, transition := "Hidden → Hidden"]
  # 
  # raw_emerged <- emerged_agg[, .(n_emerged, prob = stay_emerged)]
  # raw_emerged[, transition := "Emerged → Emerged"]
  # 
  # raw_combined <- rbind(raw_hidden, raw_emerged)
  # 
  # p <- ggplot() +
  #   geom_jitter(
  #     data = raw_combined,
  #     aes(x = n_emerged, y = prob, color = transition),
  #     height = 0.05, width = 0.2, alpha = 0.05
  #   ) +
  #   geom_line(
  #     data = pred_combined,
  #     aes(x = n_emerged, y = predicted, color = transition),
  #     size = 1.2
  #   ) +
  #   geom_ribbon(
  #     data = pred_combined,
  #     aes(x = n_emerged, ymin = conf.low, ymax = conf.high, fill = transition),
  #     alpha = 0.1
  #   ) +
  #   labs(
  #     x = "Number emerged in colony",
  #     y = "Probability of staying in same state",
  #     color = "Transition",
  #     fill = "Transition"
  #   ) +
  #   scale_color_manual(values = c("Hidden → Hidden" = "blue", "Emerged → Emerged" = "red")) +
  #   scale_fill_manual(values = c("Hidden → Hidden" = "blue", "Emerged → Emerged" = "red")) +
  #   theme_minimal()
  # 
  # return(list(hidden_model = hidden_model,
  #             emerged_model = emerged_model,
  #             plot = p))
  
  
}

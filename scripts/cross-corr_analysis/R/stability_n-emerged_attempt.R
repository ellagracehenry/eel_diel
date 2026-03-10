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
n_emerged_mean <- mean(agg_dt$n_emerged, na.rm=TRUE)
n_emerged_sd   <- sd(agg_dt$n_emerged, na.rm=TRUE)
agg_dt[, n_emerged_s := (n_emerged - n_emerged_mean) / n_emerged_sd]
agg_dt[, time_bin_s := as.numeric(scale(time_bin))]
colony_size_mean <- mean(agg_dt$colony_size)
colony_size_sd <- sd(agg_dt$colony_size)
agg_dt[, colony_size_s := (colony_size - colony_size_mean) / colony_size_sd]

# Split hidden and emerged data
hidden_agg <- agg_dt[value == 0]
emerged_agg <- agg_dt[value == 1]

emerged_agg <- emerged_agg %>%
  filter(!(colony %in% c("F1","L1"))) %>%
  droplevels()

emerged_model <- glmmTMB(
  stay_emerged ~ time_bin_s + n_emerged + colony + (1| individual_ID) + (1 | date_f),
  data = emerged_agg,
  family = binomial
)

# Predictions
full_n_emerged <- 0:max(agg_dt$n_emerged, na.rm=TRUE)
n_emerged_s_full <- (full_n_emerged - n_emerged_mean) / n_emerged_sd

# Choose time bins you want to predict for safely
if(nrow(emerged_agg) > 0 && any(is.finite(emerged_agg$time_bin_s))){
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
sites <- levels(emerged_agg$site)
colonies <- levels(emerged_agg$colony)

# Build prediction grid
pred_df <- expand.grid(
  n_emerged_s = n_emerged_s_full,
  time_bin_s = time_bins_seq,
  #colony_size_s = mean(emerged_agg$colony_size_s, na.rm = TRUE),
  #site = sites
  colony = colonies
)

pred_df$site <- factor(pred_df$site, levels = sites)  # preserve factor levels

# Emerged → Emerged
pred_emerged <- predict(emerged_model, newdata = pred_df, type = "response", re.form = NA, se.fit = TRUE)


pred_emerged_df <- pred_df %>%
  mutate(
    predicted = pred_emerged$fit,
    conf.low  = pred_emerged$fit - 1.96 * pred_emerged$se.fit,
    conf.high = pred_emerged$fit + 1.96 * pred_emerged$se.fit,
    n_emerged_orig = round(n_emerged_s * n_emerged_sd + n_emerged_mean)  # back to original counts
  )

mid_time_bin <- median(time_bins_seq)
pred_single_time <- pred_emerged_df %>%
  filter(time_bin_s == mid_time_bin)
ggplot(pred_single_time, aes(x = n_emerged_orig, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_wrap(~ site) +
  theme_minimal() +
  labs(
    x = "Number already emerged",
    y = "Predicted probability to stay emerged",
    title = paste0("Predicted probability to stay emerged at time bin ", round(mid_time_bin))
  )





emerged_model <- glmmTMB(
  stay_emerged ~ time_bin_s + n_emerged + colony + (1| individual_ID) + (1 | date_f),
  data = emerged_agg,
  family = binomial
)

summary(emerged_model)
# Generate predictions for each colony across n_emerged_s
preds <- ggpredict(emerged_model, terms = c("n_emerged [all]", "colony"))

# Optional: reorder colonies however you like
preds$group <- factor(preds$group, levels = c("F2","L1","D2","F1","L4")) 

# Plot all colonies
pred_emerged <- ggplot(preds, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  theme_minimal() +
  labs(
    x = "Number Emerged (scaled)",
    y = "Probability of Staying Emerged (predicted)",
    color = "Colony",
    fill = "Colony",
    title = "Effect of Number Emerged on Staying Emerged by Colony"
  ) +
  facet_wrap(~group)



ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/pred_emerged.png', pred_emerged,
       width = 8, height = 10, units = 'in', dpi = 300)


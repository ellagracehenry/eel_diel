
persistence_prob <- function(each_ind_each_sec_df) {
##### PREPARE 10-SECOND BINS #####
each_ind_each_sec_df <- each_ind_each_sec_df %>%
  mutate(time_bin = floor(sec_since_midnight / 10))

##### HIDDEN → HIDDEN #####
hidden_data <- subset(each_ind_each_sec_df, value == 0)

hidden_agg <- hidden_data %>%
  group_by(date, site, colony, individual_ID, time_bin) %>%
  summarise(
    current_state = first(value, default = 0),
    stay_hidden   = as.numeric(current_state == last(next_state, default = 0)),
    n_obs         = n(),
    n_emerged     = first(n_emerged),  # use existing n_emerged at start of bin
    .groups = "drop"
  )

##### EMERGED → EMERGED #####
emerged_data <- subset(each_ind_each_sec_df, value == 1)

emerged_agg <- emerged_data %>%
  group_by(date, site, colony, individual_ID, time_bin) %>%
  summarise(
    current_state = first(value, default = 1),
    stay_emerged  = as.numeric(current_state == last(next_state, default = 1)),
    n_obs         = n(),
    n_emerged     = first(n_emerged),
    .groups = "drop"
  )

##### SCALE #####
n_emerged_mean <- mean(each_ind_each_sec_df$n_emerged)
n_emerged_sd   <- sd(each_ind_each_sec_df$n_emerged)

hidden_agg <- hidden_agg %>%
  mutate(n_emerged_s = (n_emerged - n_emerged_mean) / n_emerged_sd,
         time_bin_s  = scale(time_bin))

emerged_agg <- emerged_agg %>%
  mutate(n_emerged_s = (n_emerged - n_emerged_mean) / n_emerged_sd,
         time_bin_s  = scale(time_bin))

##### MODEL #####
hidden_model <- glmmTMB(
  stay_hidden ~ time_bin_s + n_emerged_s + (1|site/colony/individual_ID) + (1|date),
  data = hidden_agg, family = binomial
)

emerged_model <- glmmTMB(
  stay_emerged ~ time_bin_s + n_emerged_s + (1|site/colony/individual_ID) + (1|date),
  data = emerged_agg, family = binomial
)

##### PREDICTIONS #####
full_n_emerged <- 0:max(each_ind_each_sec_df$n_emerged)
n_emerged_s_full <- (full_n_emerged - n_emerged_mean) / n_emerged_sd
pred_df <- data.frame(n_emerged_s = n_emerged_s_full,
                      time_bin_s = mean(hidden_agg$time_bin_s))

# Hidden → Hidden
pred_hidden <- predict(hidden_model, newdata = pred_df, type = "response", se.fit = TRUE, re.form = NA)
pred_hidden_df <- data.frame(
  n_emerged = full_n_emerged,
  predicted = pred_hidden$fit,
  conf.low  = pred_hidden$fit - 1.96*pred_hidden$se.fit,
  conf.high = pred_hidden$fit + 1.96*pred_hidden$se.fit,
  transition = "Hidden → Hidden"
)

# Emerged → Emerged
pred_emerged <- predict(emerged_model, newdata = pred_df, type = "response", se.fit = TRUE, re.form = NA)
pred_emerged_df <- data.frame(
  n_emerged = full_n_emerged,
  predicted = pred_emerged$fit,
  conf.low  = pred_emerged$fit - 1.96*pred_emerged$se.fit,
  conf.high = pred_emerged$fit + 1.96*pred_emerged$se.fit,
  transition = "Emerged → Emerged"
)

pred_combined <- bind_rows(pred_hidden_df, pred_emerged_df)

##### PLOT #####
raw_hidden <- hidden_agg %>% mutate(prob = stay_hidden, transition = "Hidden → Hidden")
raw_emerged <- emerged_agg %>% mutate(prob = stay_emerged, transition = "Emerged → Emerged")
raw_combined <- bind_rows(raw_hidden, raw_emerged)

ggplot() +
  geom_jitter(
    data = raw_combined,
    aes(x = n_emerged, y = prob, color = transition),
    height = 0.05, width = 0.2, alpha = 0.05
  ) +
  geom_line(
    data = pred_combined,
    aes(x = n_emerged, y = predicted, color = transition),
    size = 1.2
  ) +
  geom_ribbon(
    data = pred_combined,
    aes(x = n_emerged, ymin = conf.low, ymax = conf.high, fill = transition),
    alpha = 0.1
  ) +
  labs(
    x = "Number emerged in colony",
    y = "Probability of staying in same state",
    color = "Transition",
    fill = "Transition"
  ) +
  scale_color_manual(values = c("Hidden → Hidden" = "blue", "Emerged → Emerged" = "red")) +
  scale_fill_manual(values = c("Hidden → Hidden" = "blue", "Emerged → Emerged" = "red")) +
  theme_minimal()

}

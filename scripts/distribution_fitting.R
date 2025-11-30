df_run_length_hide <- df_run_length %>%
  filter(value == 0) #%>%
#dplyr::pull(run_length)

df_run_length_emerge <- df_run_length %>%
  filter(value == 1) #%>%
#dplyr::pull(run_length)

df_run_length_hide <- df_run_length_emerge
#Geometric distribution
# Estimate geometric parameter
p_hat <- 1 / mean(df_run_length_hide$run_length)
N <- nrow(df_run_length_hide)

# X-grid to evaluate CDF on (all integers from min to max)
x_seq <- seq(min(df_run_length_hide$run_length),
             max(df_run_length_hide$run_length))

# ---- Simulate many empirical CDFs under fitted geometric ----
n_sim <- N  # increase for smoother envelopes

sim_cdfs <- map_dfr(1:n_sim, function(i) {
  
  # draw sample from fitted geometric (support 1,2,3,...)
  r <- rgeom(N, p_hat) + 1
  
  # compute ECDF function
  F_emp <- ecdf(r)
  
  tibble(
    x = x_seq,
    cdf = F_emp(x_seq),
    sim = i
  )
})

# ---- Compute 95% envelope at each x ----
df_band <- sim_cdfs %>%
  group_by(x) %>%
  summarise(
    lower = quantile(cdf, 0.025),
    upper = quantile(cdf, 0.975),
    .groups = "drop"
  )

# ---- Empirical CDF of observed data ----
df_cdf <- df_run_length_hide %>%
  count(run_length, name = "n") %>%
  arrange(run_length) %>%
  mutate(cum_prob = cumsum(n) / sum(n)) %>%
  rename(x = run_length)

# ---- Fitted geometric CDF ----
df_geom <- tibble(
  x = x_seq,
  cum_prob = 1 - (1 - p_hat)^x
)

# ---- Plot ----
ggplot() +
  # 95% simulation envelope (stepwise polygon)
  geom_step(data = df_band,
            aes(x = x, y = upper),
            color = "red", linewidth = 0.8) +
  geom_step(data = df_band,
            aes(x = x, y = lower),
            color = "red", linewidth = 0.8) +
  
  # empirical EDF
  geom_step(data = df_cdf,
            aes(x = x, y = cum_prob, color = "Empirical"),
            linewidth = 1) +
  
  # fitted geometric
  geom_step(data = df_geom,
            aes(x = x, y = cum_prob, color = "Fitted geometric"),
            linewidth = 1, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "Empirical" = "blue",
    "Fitted geometric" = "black"
  )) +
  
  labs(
    title = "Emerge run lengths, geometric",
    subtitle = paste0("p̂ = ", round(p_hat, 4)),
    x = "Run length",
    y = "Cumulative probability",
    color = ""
  ) +
  theme_minimal()

# ----- QQ-plot for geometric distribution -----

# Sort observed run lengths
obs <- sort(df_run_length_hide$run_length)

# Expected geometric quantiles:
# For a geometric distribution with support {1,2,3,...}:
#   Q(p) = ceiling( log(1-p) / log(1-p_hat) )
probs <- (1:N) / (N + 1)  # standard plotting positions
geom_q <- ceiling(log(1 - probs) / log(1 - p_hat))

df_qq <- tibble(
  geom_q = geom_q,
  obs_q  = obs
)

# Robust QQ line: through Q1 and Q3
q1_obs  <- quantile(df_qq$obs_q,  0.25)
q3_obs  <- quantile(df_qq$obs_q,  0.75)

q1_geom <- quantile(df_qq$geom_q, 0.25)
q3_geom <- quantile(df_qq$geom_q, 0.75)

slope <- (q3_obs - q1_obs) / (q3_geom - q1_geom)
intercept <- q1_obs - slope * q1_geom

# ----- Plot -----
ggplot(df_qq, aes(x = geom_q, y = obs_q)) +
  geom_point(alpha = 0.7, size = 2) +
  
  # expected line (blue)
  geom_abline(intercept = 0, slope = 1, color = "blue", linewidth = 1) +
  
  # QQ line through quartiles (black)
  geom_abline(intercept = intercept, slope = slope,
              color = "black", linewidth = 1) +
  
  labs(
    title = "QQ-Plot: Observed vs Fitted Geometric Distribution",
    subtitle = paste0("p̂ = ", round(p_hat, 4)),
    x = "Quantiles (Geometric)",
    y = "Quantiles (Observed)"
  ) +
  theme_minimal()

#NB distribution
# Estimate geometric parameter
fit_nb <- fitdistr(df_run_length_hide$run_length, "Negative Binomial")
size_hat <- fit_nb$estimate["size"]
mu_hat   <- fit_nb$estimate["mu"]
cdf_nb <- pnbinom(x_seq, size = size_hat, mu = mu_hat)


# X-grid to evaluate CDF on (all integers from min to max)
x_seq <- seq(min(df_run_length_hide$run_length),
             max(df_run_length_hide$run_length))

# ---- Simulate many empirical CDFs under fitted NB ----
n_sim <- N  # increase for smoother envelopes

sim_cdfs <- map_dfr(1:n_sim, function(i) {
  
  # draw sample from fitted nb (support 1,2,3,...)
  r <- rnbinom(N, size_hat, mu = mu_hat)
  
  # compute ECDF function
  F_emp <- ecdf(r)
  
  tibble(
    x = x_seq,
    cdf = F_emp(x_seq),
    sim = i
  )
})

# ---- Compute 95% envelope at each x ----
df_band <- sim_cdfs %>%
  group_by(x) %>%
  summarise(
    lower = quantile(cdf, 0.025),
    upper = quantile(cdf, 0.975),
    .groups = "drop"
  )

# ---- Empirical CDF of observed data ----
df_cdf <- df_run_length_hide %>%
  count(run_length, name = "n") %>%
  arrange(run_length) %>%
  mutate(cum_prob = cumsum(n) / sum(n)) %>%
  rename(x = run_length)

# ---- Fitted nb CDF ----
df_nb <- tibble(
  x = x_seq,
  cum_prob = pnbinom(x_seq, size = size_hat, mu = mu_hat)
  
)

# ---- Plot ----
ggplot() +
  # 95% simulation envelope (stepwise polygon)
  geom_step(data = df_band,
            aes(x = x, y = upper),
            color = "red", linewidth = 0.8) +
  geom_step(data = df_band,
            aes(x = x, y = lower),
            color = "red", linewidth = 0.8) +
  
  # empirical EDF
  geom_step(data = df_cdf,
            aes(x = x, y = cum_prob, color = "Empirical"),
            linewidth = 1) +
  
  # fitted geometric
  geom_step(data = df_nb,
            aes(x = x, y = cum_prob, color = "Fitted NB"),
            linewidth = 1, linetype = "dashed") +
  
  scale_color_manual(values = c(
    "Empirical" = "blue",
    "Fitted geometric" = "black"
  )) +
  
  labs(
    title = "Emerge run lengths, negative binomial",
    subtitle = paste0("p̂ = ", round(p_hat, 4)),
    x = "Run length",
    y = "Cumulative probability",
    color = ""
  ) +
  theme_minimal()

# ----- QQ-plot for geometric distribution -----

# Sort observed data
obs <- sort(df_run_length_hide$run_length)
N <- length(obs)

# plotting positions
probs <- (1:N) / (N + 1)

# Expected NB quantiles (support 0,1,2,...)
nb_q_raw <- qnbinom(probs, size = size_hat, mu = mu_hat)

# If your data starts at 1, shift NB quantiles
# (remove +1 if your data naturally has zeros)
nb_q <- nb_q_raw + 1

df_qq <- tibble(
  nb_q  = nb_q,
  obs_q = obs
)

# ---- Robust QQ line (through quartiles) ----
q1_obs  <- quantile(df_qq$obs_q,  0.25)
q3_obs  <- quantile(df_qq$obs_q,  0.75)

q1_nb   <- quantile(df_qq$nb_q,   0.25)
q3_nb   <- quantile(df_qq$nb_q,   0.75)

slope <- (q3_obs - q1_obs) / (q3_nb - q1_nb)
intercept <- q1_obs - slope * q1_nb

# ---- Plot ----
ggplot(df_qq, aes(x = nb_q, y = obs_q)) +
  geom_point(alpha = 0.7, size = 2) +
  
  # 45-degree expected line
  geom_abline(intercept = 0, slope = 1, color = "blue", linewidth = 1) +
  
  # robust QQ line
  geom_abline(intercept = intercept, slope = slope,
              color = "black", linewidth = 1) +
  
  labs(
    title = "QQ-Plot: Observed vs Fitted Negative Binomial Distribution",
    subtitle = paste0(
      "size = ", round(size_hat, 3),
      ", mu = ", round(mu_hat, 3)
    ),
    x = "Quantiles (Negative Binomial)",
    y = "Quantiles (Observed)"
  ) +
  theme_minimal()

#log normal
# ----- Fit parameters (example) -----
# Or supply your own meanlog_hat, sdlog_hat
log_vals <- log(df_run_length_hide$run_length)
meanlog_hat <- mean(log_vals)
sdlog_hat   <- sd(log_vals)

N <- nrow(df_run_length_hide)
x_seq <- seq(min(df_run_length_hide$run_length),
             max(df_run_length_hide$run_length))

n_sim <- 2000  # for smooth envelopes

# ----- Monte Carlo CDF simulations -----
sim_cdfs <- map_dfr(1:n_sim, function(i) {
  
  # simulate from log-normal distribution
  r <- rlnorm(N, meanlog = meanlog_hat, sdlog = sdlog_hat)
  
  # empirical CDF
  F_emp <- ecdf(r)
  
  tibble(
    x   = x_seq,
    cdf = F_emp(x_seq),
    sim = i
  )
})

# ---- Simulation envelope ----
df_band <- sim_cdfs %>%
  group_by(x) %>%
  summarise(
    lower = quantile(cdf, 0.025),
    upper = quantile(cdf, 0.975),
    .groups = "drop"
  )

# ---- Empirical CDF ----
df_cdf <- df_run_length_hide %>%
  count(run_length, name = "n") %>%
  arrange(run_length) %>%
  mutate(cum_prob = cumsum(n) / sum(n)) %>%
  rename(x = run_length)

# ---- Fitted lognormal CDF ----
df_lognorm <- tibble(
  x   = x_seq,
  cdf = plnorm(x_seq, meanlog = meanlog_hat, sdlog = sdlog_hat)
)

# ---- Plot ----
ggplot() +
  # Stepwise Monte Carlo 95% envelope
  geom_step(data = df_band, aes(x = x, y = upper),
            color = "red", linewidth = 0.8) +
  geom_step(data = df_band, aes(x = x, y = lower),
            color = "red", linewidth = 0.8) +
  
  # Empirical CDF
  geom_step(data = df_cdf, aes(x = x, y = cum_prob, color = "Empirical"),
            linewidth = 1) +
  
  # Fitted lognormal CDF
  geom_line(data = df_lognorm, aes(x = x, y = cdf, color = "Fitted lognormal"),
            linewidth = 1, linetype = "dashed") +
  
  scale_color_manual(values = c("Empirical" = "blue", "Fitted lognormal" = "black")) +
  
  labs(
    title = "CDF with Stepwise 95% Simulation Envelope",
    subtitle = paste0("Log-normal fit: meanlog = ",
                      round(meanlog_hat,3),
                      ", sdlog = ",
                      round(sdlog_hat,3)),
    x = "Run length",
    y = "Cumulative probability",
    color = ""
  ) +
  theme_minimal()


# ----- Sort observed data -----
obs <- sort(df_run_length_hide$run_length)
N <- length(obs)

# plotting positions
probs <- (1:N) / (N + 1)

# expected lognormal quantiles
ln_q <- qlnorm(probs, meanlog = meanlog_hat, sdlog = sdlog_hat)

df_qq <- tibble(
  ln_q  = ln_q,
  obs_q = obs
)

# ----- Robust QQ line -----
q1_obs <- quantile(df_qq$obs_q, 0.25)
q3_obs <- quantile(df_qq$obs_q, 0.75)

q1_ln <- quantile(df_qq$ln_q, 0.25)
q3_ln <- quantile(df_qq$ln_q, 0.75)

slope <- (q3_obs - q1_obs) / (q3_ln - q1_ln)
intercept <- q1_obs - slope * q1_ln

# ----- Plot -----
ggplot(df_qq, aes(x = ln_q, y = obs_q)) +
  geom_point(alpha = 0.7, size = 2) +
  
  # expected 1:1 reference line
  geom_abline(intercept = 0, slope = 1, color = "blue", linewidth = 1) +
  
  # robust QQ line
  geom_abline(intercept = intercept, slope = slope,
              color = "black", linewidth = 1) +
  
  labs(
    title = "QQ-Plot: Observed vs Fitted Log-normal",
    subtitle = paste0("meanlog = ", round(meanlog_hat,3),
                      ", sdlog = ", round(sdlog_hat,3)),
    x = "Quantiles (Log-normal)",
    y = "Quantiles (Observed)"
  ) +
  theme_minimal()


threshold <- quantile(df_run_length_hide$run_length, 0.05)
forced_runs <- df_run_length_hide %>% filter(run_length <= threshold)
natural_runs <- df_run_length_hide %>% filter(run_length > threshold)

run_bayes <- function(data)

#Design matrix
d <- data |> 
  mutate(intercept = rep(0, n()),
         colony = ifelse(colony == "S5", 0, ifelse(colony == "S9", 1, ifelse(colony == "S15",2, NA))),
         dist_to_ball = (distance_to_ball - mean_dist_to_ball)/SD_dist_to_ball,
         inst_emerged = (inst_emerged - mean_inst_emerged)/SD_inst_emerged,
         prev_hides = (prev_hides - mean_prev_hides)/SD_prev_hides,
         distance_to_ball_X_inst_emerged = inst_emerged * distance_to_ball) |>
  select(binary_response, intercept, colony, dist_to_ball, inst_emerged, prev_hides, distance_to_ball_X_inst_emerged) |>
  filter(!is.na(binary_response)) %>%
  filter(!is.na(dist_to_ball))

bysfitInstxD <- ulam(
  alist(
    binary_response ~ dbern(p),
    logit(p) <- beta_0*intercept+
      beta_1*colony+
      beta_2*inst_emerged+
      beta_3*prev_hides+
      beta_4*distance_to_ball_X_inst_emerged,
    beta_0 ~ dnorm(0,10),
    beta_1 ~ dnorm(0,10),
    beta_2 ~ dnorm(0,10),
    beta_3 ~ dnorm(0,10),
    beta_4 ~ dnorm(0,10)
  ),
  data=d
)

bysfitHxL <- ulam(bysfitInstxD, chains=4, cores=4, warmup=1000, iter=10000)

precis(bysfitHxL, prob=0.95, digits=4)

samples <- extract.samples(bysfitHxL)
class(samples) 
str(samples)
names(samples)

samplesdf <- data.frame(samples)
head(samplesdf)

samplesdf |>
  pivot_longer(cols=everything(), names_to="parameter", values_to="sample_value") |>
  ggplot() +
  geom_histogram(aes(x=sample_value, y=after_stat(density)), bins = 75) +
  facet_wrap(vars(parameter), scales = "free")

traceplot(bysfitHxL)

HPDI(samples$beta_0, prob=0.95)
HPDI(samples$beta_1, prob=0.95)
HPDI(samples$beta_2, prob=0.95)
HPDI(samples$beta_3, prob=0.95)
HPDI(samples$beta_4, prob=0.95)

loo(bysfitHxL)

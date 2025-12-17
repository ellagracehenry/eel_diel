#Packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(MASS) 
library(readxl)
library(lme4)
library(glmmTMB)
library(ggeffects)

#Functions
source("scripts/cross-corr_analysis/R/combine_data_by_time.R")
source("scripts/cross-corr_analysis/R/combine_data_by_individual.R")
source("scripts/cross-corr_analysis/R/each_ind_each_sec.R")
source("scripts/ABM/R/fill_holes.R")

#Initialise
threshold = 5
transitions_path = "/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions"
metadata_path = "/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/diel_cycle_garden_eel/diel_eel_processing.xlsx"

#Summarise data by time
by_time_df <- combine_data_by_time(transitions_path, metadata_path, threshold)

#Summarise data by individual
by_ind_df <- combine_data_by_individual(transitions_path, metadata_path, threshold)

#Each individual, each second
each_ind_each_sec_df <- each_ind_each_sec(transitions_path, metadata_path, threshold)



##### PLOTTING P_EMERGED OVER TIME #####

# Create a new column that combines date and colony
df <- by_time_df %>%
  mutate(date_colony = paste(date, colony, sep = "_"))

# Plot heatmap of p_emerged over time
ggplot(df, aes(x = sec_since_midnight, y = date_colony, fill = prop_emerged)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Time (seconds since midnight)", y = "Date_Colony", fill = "Prop Emerged") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))



##### PLOTTING P_EMERGED PER COLONY #####

df_pc <- df %>% 
  group_by(site, colony, date, trial_ID) %>%
  reframe(per_cap_emerged = mean(prop_emerged), colony_size = mean(colony_size)) 


df_pc$date <- as.factor(df_pc$date)
df_pc %>%
  ggplot(aes(x = colony_size, y = per_cap_emerged, group = date, color = date)) +
  facet_wrap(site~date) +
  geom_smooth(method="lm")+
  geom_point() +
  labs(y = "Average proportion emerged")



##### PLOTTING P_EMERGED PER INDIVIDUAL #####

by_ind_df_f <- by_ind_df %>%
  mutate(individual_ID = as.factor(individual_ID),
         date = as.Date(date)) %>%
         filter(prop_time_emerged > 0.01)

by_ind_df_f %>%
  group_by(colony) %>%
  mutate(ind_color = as.factor(individual_ID)) %>%  # new color factor per colony
  ungroup() %>%
  ggplot(aes(x = date, y = prop_time_emerged,
             group = individual_ID, color = ind_color)) +
  geom_point() +
  geom_line() +
  facet_wrap(~colony) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d()


##### MODELLING SITE, COLONY AND INDIVIDUAL LEVEL CONSISTENCY #####
by_ind_df_f$date_c <- scale(by_ind_df_f$date, scale = FALSE)
m1_centered <- lmer(prop_time_emerged ~ date_c + (1 | site/colony/individual_ID), data = by_ind_df_f)
summary(m1_centered)

##### MODELLING TRANSITION PROBABILITIES #####
persistence_prob(each_ind_each_sec_df)
# --- Prepare predicted data frames ---

# Hidden -> Hidden
pred_hidden_df <- data.frame(
  n_emerged = full_n_emerged,
  predicted = as.numeric(pred_hidden$fit),
  conf.low  = as.numeric(pred_hidden$fit - 1.96*pred_hidden$se.fit),
  conf.high = as.numeric(pred_hidden$fit + 1.96*pred_hidden$se.fit),
  transition = "Hidden → Hidden"
)

# Emerged -> Emerged
pred_emerged_df <- data.frame(
  n_emerged = full_n_emerged,
  predicted = as.numeric(pred_emerged$fit),
  conf.low  = as.numeric(pred_emerged$fit - 1.96*pred_emerged$se.fit),
  conf.high = as.numeric(pred_emerged$fit + 1.96*pred_emerged$se.fit),
  transition = "Emerged → Emerged"
)

# Steady-state probability
pred_steady_df <- data.frame(
  n_emerged = full_n_emerged,
  predicted = (1 - pred_hidden_df$predicted) / (2 - pred_hidden_df$predicted - pred_emerged_df$predicted),
  transition = "Steady-state"
)

# Combine for plotting
pred_combined <- bind_rows(pred_hidden_df, pred_emerged_df, pred_steady_df)

ggplot() +
  # Faint raw points (optional)
  #geom_jitter(
  #  data = raw_combined,
  #  aes(x = n_emerged, y = next_state_prob, color = transition),
  #  height = 0.05, width = 0.2, alpha = 0.1
  #) +
  
  # Predicted lines
  geom_line(
    data = pred_combined,
    aes(x = n_emerged, y = predicted, color = transition),
    size = 1.2
  ) +
  
  # Confidence ribbons (only for Hidden and Emerged, not Steady-state)
  geom_ribbon(
    data = pred_combined %>% filter(transition != "Steady-state"),
    aes(x = n_emerged, ymin = conf.low, ymax = conf.high, fill = transition),
    alpha = 0.1,
    show.legend = FALSE  # <-- hides this ribbon from the legend
  ) +
  
  # Labels & colors
  labs(
    x = "Number emerged in colony",
    y = "Probability",
    color = "Transition"
  ) +
  scale_color_manual(values = c(
    "Hidden → Hidden" = "blue",
    "Emerged → Emerged" = "red",
    "Foraging probability" = "green"
  )) +
  scale_fill_manual(values = c(
    "Hidden → Hidden" = "blue",
    "Emerged → Emerged" = "red"
  )) +
  theme_minimal()

### Run lengths and n emerged
run_lengths <- process_all_transitions(transitions_path, metadata_path, threshold)
run_lengths$run_type <- as.factor(run_lengths$run_type)

run_lengths_f  <- run_lengths %>%
  filter(run_length <10000 & run_type == 1) 

run_lengths_f %>% 
ggplot(aes(x = n_other_emerged, y = run_length, color = run_type)) +
  geom_point(alpha = 0.4, size = 1.5) +   # slightly transparent points
  scale_y_continuous("Run Length (s)") +
  scale_x_continuous("Number of Other Eels Emerged") +
  labs(color = "Run Type") +
  theme_minimal()

mod1 <- lmer(run_length ~ n_other_emerged + (1|date) + (1|site/colony/individual_ID), data = run_lengths_f)

summary(mod1)





#Network using inter-individual distances 

#Group level night time synchrony
## - proportion of time foraging for each individual
by_ind_dataset

## - null distribution, prob of having exactly n hidden
library(gtools)  # for combinations

# Number of individuals
n_inds <- nrow(ind_rest_prob)

# Function to calculate null probability of exactly n hidden individuals
null_prob_n_hidden <- function(n_hidden, p_rest_vec) {
  inds <- 1:length(p_rest_vec)
  combs <- combn(inds, n_hidden, simplify = FALSE)
  
  probs <- sapply(combs, function(idx) {
    # resting individuals
    p_r <- prod(p_rest_vec[idx])
    # active individuals
    p_a <- prod(1 - p_rest_vec[-idx])
    p_r * p_a
  })
  
  sum(probs)
}

## Calculate null probability for all possible numbers of hidden individuals
p_rest_vec <- ind_rest_prob$p_rest
n_range <- 0:n_inds

null_probs <- sapply(n_range, null_prob_n_hidden, p_rest_vec = p_rest_vec)


# - Mulitply together for hiding and foraging individuals
# - Null probability by summing the probabilites of individuals for each second?

#Pairwise synchrony (need spatial)
# - Define null expectation 
# - 



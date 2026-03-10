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
library(stringr)
library(plotly)
library(poibin)
library(broom)

#Functions
source("scripts/cross-corr_analysis/R/combine_data_by_time.R")
source("scripts/cross-corr_analysis/R/combine_data_by_individual.R")
source("scripts/cross-corr_analysis/R/each_ind_each_sec.R")
source("scripts/cross-corr_analysis/R/group_synchrony.R")
source("scripts/cross-corr_analysis/R/pairwise_synchrony.R")
source("scripts/cross-corr_analysis/R/topology.R")
source("scripts/cross-corr_analysis/R/calculate_pairwise_distances.R")
source("scripts/cross-corr_analysis/R/figures/persistence_prob_mem_save.R")
source("scripts/cross-corr_analysis/R/calculate_edge_proximity.R")
source("scripts/ABM/R/fill_holes.R")
source("scripts/ABM/R/fill_holes_NaNsafe.R")

#Initialise
threshold = 3
transitions_path = "/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated"
metadata_path = "/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/diel_cycle_garden_eel/diel_eel_processing.xlsx"

### DATA WRANGLING ### 

#A - Summarise data by time
by_time_df <- combine_data_by_time(transitions_path, metadata_path, threshold)

#B - Summarise data by individual
by_ind_df <- combine_data_by_individual(transitions_path, metadata_path, threshold)

by_ind_df$colony_size[by_ind_df$colony == "D1"] <- 29
by_ind_df$colony_size[by_ind_df$colony == "D2"] <- 94
by_ind_df$colony_size[by_ind_df$colony == "L1"] <- 28
by_ind_df$colony_size[by_ind_df$colony == "L2"] <- 116
by_ind_df$colony_size[by_ind_df$colony == "F1"] <- 60
by_ind_df$colony_size[by_ind_df$colony == "F2"] <- 5

#C - Each individual, each second
each_ind_each_sec_df <- each_ind_each_sec(transitions_path, metadata_path, threshold)

#D - Group-level synchrony
group_sync_df <- group_synchrony(transitions_path, metadata_path, threshold)

#E - Pairwise synchrony
pairwise_sync_df <- pairwise_synchrony(transitions_path, metadata_path, threshold)

#F - topology
topology_df <- topology(metadata_path)

##### 1 - PLOTTING P_EMERGED OVER TIME #####

# Create a new column that combines date and colony
df <- by_time_df %>%
  #filter(colony == "D2" |colony == "D4" | colony == "F2" | colony == "F1") %>%
  mutate(date_colony = paste(date, colony, sep = "_"))

# Plot heatmap of p_emerged over time
#all
proportion_feeding_time_all <- df %>%
  ggplot(aes(x = time_of_day, y = date_colony, fill = prop_emerged)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Time", y = "Date_Colony", fill = "Proportion Feeding") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) 

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/proportion_feeding_time_all.png', proportion_feeding_time_all,
       width = 8, height = 10, units = 'in', dpi = 300)

#filtered for pairs
proportion_feeding_time_pairs = df %>%
  group_by(date,site,time_of_day) %>%
  mutate(n_pairs = n()) %>%
  ungroup() %>%
  filter(n_pairs > 1)%>%
  ggplot(aes(x = time_of_day, y = date_colony, fill = prop_emerged)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Time", y = "Date_Colony", fill = "Proportion Feeding") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/proportion_feeding_time_pairs.png', proportion_feeding_time_pairs,
       width = 8, height = 10, units = 'in', dpi = 300)

#Difference between pairs
diff_feeding_time = df %>%
  group_by(date,site,time_of_day) %>%
  reframe(diff = abs(diff(prop_emerged))) %>%
  mutate(date_site = paste(date, site, sep = "_")) %>%
  ggplot(aes(x=time_of_day, y = date_site, fill = diff)) +
  geom_tile()+
  scale_fill_viridis_c(option = "rocket") +
  labs(x = "Time", y = "Date_Site", fill = "Difference in proportion feeding") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/diff_feeding_time.png', diff_feeding_time,
       width = 8, height = 10, units = 'in', dpi = 300)

##### 2 - PLOTTING P_EMERGED PER COLONY #####
#Means 
feeding_big_small_colony <- by_ind_df %>%
  filter(prop_time_emerged >0.1) %>%
  group_by(date,site) %>%
  mutate(bin_size = if_else(colony_size == min(colony_size), "Small","Big")) %>%
  ungroup() %>%
  group_by(date,site) %>%
  mutate(n_pairs = n_distinct(bin_size), bin_size = factor(bin_size, levels = c("Small", "Big"))) %>%
  ungroup() %>%
  #filter(n_pairs > 1) %>%
  ggplot(aes(x = bin_size, y = prop_time_emerged, group = interaction(site,date), color = site)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", size = 1.5) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 4, alpha = 0.3) +
  #stat_summary(fun.data = mean_cl_normal, geom = "ribbon", alpha = 0.2, aes(fill = site), color = NA)+
  scale_y_continuous(limits=c(0,1),expand=c(0,0))+
  scale_color_brewer(palette="Dark2")+
  theme_classic(base_size = 25) +
  facet_wrap(~date) +
  labs(x = "Colony size", y = "Per capita proportion of time foraging") +
  theme(legend.position = "right", axis.title=element_text(face="bold"))

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/feeding_big_small_colony.png', feeding_big_small_colony,
       width = 8, height = 10, units = 'in', dpi = 300)

#Model
df <- by_ind_df %>%
  filter(prop_time_emerged >0.01) %>%
  filter(prop_time_emerged < 1,!is.na(prop_time_emerged))  %>%
  #filter(date != "2025-05-17")

#%>%
 # group_by(date,colony) %>%
  #reframe(mean = mean(prop_time_emerged)) %>%
  #ungroup()

df$site <- as.factor(df$site)
df$site <- relevel(df$site, ref = "L")

model <- glmmTMB(prop_time_emerged ~ colony_size*site + (1|date) + (1|trial_ID) + (1|colony/individual_ID),
                 family = beta_family(),
                 data = df)
summary(model)
model_simres <- simulateResiduals(model)
plot(model_simres)

model <- glmmTMB(
  cbind(emerged_s, total_s - emerged_s) ~ colony_size*edge_proximity
    (1|date) + (1|site/individual_ID) + (1|trial_ID),
  family = betabinomial(),
  data = by_ind_df
)
summary(model)
model_simres <- simulateResiduals(model)
plot(model_simres)

#Individuals
ind_feeding_big_small_colony <- by_ind_df %>%
  filter(prop_time_emerged >0.1) %>%
  group_by(date,site) %>%
  mutate(bin_size = if_else(colony_size == min(colony_size), "Small","Big")) %>%
  ungroup() %>%
  group_by(date,site) %>%
  mutate(n_pairs = n_distinct(bin_size), bin_size = factor(bin_size, levels = c("Small", "Big"))) %>%
  ungroup() %>%
  filter(n_pairs > 1) %>%
  ggplot(aes(x = bin_size, y = prop_time_emerged, group = interaction(site,date), color = site)) +
  geom_jitter(width = 0.09, size = 2.5, alpha = 0.1) +
  stat_summary(fun = "mean", geom = "line", size = 1.5) +
  stat_summary(fun = "mean", geom = "point", size = 5) +
  scale_y_continuous(limits=c(0,1),expand=c(0,0))+
  scale_color_brewer(palette="Dark2")+
  theme_classic(base_size = 25) +
  #facet_wrap(~date) +
  labs(x = "Colony size", y = "Per capita proportion of time foraging") +
  theme(legend.position = "right", axis.title=element_text(face="bold"))

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/ind_feeding_big_small_colony.png', ind_feeding_big_small_colony,
       width = 8, height = 10, units = 'in', dpi = 300)



##### 3 - PLOTTING P_EMERGED PER INDIVIDUAL #####
#Individual variation
d<-by_ind_df %>%
  filter(prop_time_emerged >0) %>%
  mutate(colony_ind_ID = paste(colony, individual_ID, sep = "_")) %>%
  group_by(colony_ind_ID) %>%
  summarise(mean_prop_emerged = mean(prop_time_emerged, na.rm=TRUE), n_appearances = n(), colony = first(colony), date = first(date), site = first(site), colony_size = first(colony_size)) %>%
  group_by(date,site) %>%
  mutate(n_pairs = n_distinct(bin_size), bin_size = factor(bin_size, levels = c("small", "large"))) %>%
  ungroup() %>%
  filter(n_appearances > 1) %>%
  ggplot(aes(x = mean_prop_emerged))+
  geom_histogram(aes(y = after_stat(density)))+
  facet_wrap(~colony)



topology_df <- topology(metadata_path)

edge_proximity_df <- topology_df %>%
  group_by(colony, individual_ID) %>%
  summarise(colony = first(colony), individual_ID = first(individual_ID), edge_proximity = first(edge_proximity))

by_ind_df_edge <- left_join(by_ind_df, edge_proximity_df)

by_ind_df_edge <- by_ind_df_edge %>%
  mutate(colony = factor(colony,
                         levels = c("D1","D2",
                                    "L1","L2",
                                    "F2","F1")))

means <- by_ind_df_edge %>%
  group_by(colony,individual_ID) %>%
  mutate(n_appear = n()) %>%
  ungroup() %>%
  filter(n_appear > 2) %>%
  filter(prop_time_emerged >0.1) %>%
  filter(colony %in% c("D1","D2","L1","F2","F1","L2")) %>%
  ggplot(aes(x = edge_proximity, y = prop_time_emerged, color = site, group = individual_ID)) +
  #geom_point(size = 2.5, alpha = 0.1) +
  #stat_summary(fun = "mean", geom = "line", linetype = "longdash", size = 1, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 2.5,alpha=0.9) +
  scale_y_continuous(limits=c(0,1),expand=c(0,0))+
  scale_color_brewer(palette="Dark2")+
  #geom_smooth(method = "lm")+
  theme_classic() +
  facet_wrap(~colony, ncol = 2) +
  labs(x = "Edge Proximity", y = "Average proportion of time foraging") +
  theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 22, face = "bold"),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 18))

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/avg_feeding-edge_prox-mean.png', means,
       width = 10, height = 8, units = 'in', dpi = 300)

means_ind <- by_ind_df_edge %>%
  group_by(colony,individual_ID) %>%
  mutate(n_appear = n()) %>%
  ungroup() %>%
  filter(n_appear > 2) %>%
  filter(prop_time_emerged >0.1) %>%
  filter(colony %in% c("D1","D2","L1","F2","F1","L2")) %>%
  ggplot(aes(x = edge_proximity, y = prop_time_emerged, color = site, group = individual_ID)) +
  geom_point(size = 2.5, alpha = 0.2) +
  #stat_summary(fun = "mean", geom = "line", linetype = "longdash", size = 1, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 2.5,alpha=0.9) +
  scale_y_continuous(limits=c(0,1),expand=c(0,0))+
  scale_color_brewer(palette="Dark2")+
  #geom_smooth(method = "lm")+
  theme_classic() +
  facet_wrap(~colony, ncol = 2) +
  labs(x = "Edge Proximity", y = "Proportion of time foraging") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 22, face = "bold"),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18))

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/avg_feeding-edge_prox-mean_ind.png', means_ind,
       width = 10, height = 8, units = 'in', dpi = 300)



df <- by_ind_df_edge %>%
  filter(prop_time_emerged >0.01) %>%
  filter(prop_time_emerged < 1,!is.na(prop_time_emerged)) 
  
df$site <- as.factor(df$site)
df$site <- relevel(df$site, ref = "L")

model <- glmmTMB(cbind(emerged_s, total_s - emerged_s) ~ edge_proximity*colony_size + colony_size*site + edge_proximity*site + (1|date) + (1|colony/individual_ID),
              family = betabinomial(), data = df)
summary(model)
model_simres <- simulateResiduals(model)
plot(model_simres)


# Compute one mean per individual (within colony)

ind_means <- by_ind_df_edge %>%
  filter(prop_time_emerged > 0.001) %>%
  #filter(colony %in% c("D1","D2","L1","F2","F1")) %>%
  group_by(colony, individual_ID) %>%
  summarise(
    mean_edge_proximity = mean(edge_proximity),
    mean_prop_time = mean(prop_time_emerged),
    .groups = "drop",
    n_obs = n(),
  )

# Plot
g <- by_ind_df_edge %>%
  filter(prop_time_emerged > 0.001) %>%
  filter(n_obs>1)%>%
  #filter(colony %in% c("D1","D2","L1","F2","F1")) %>%
  ggplot(aes(x = edge_proximity, y = prop_time_emerged, color = colony)) +
  #geom_point(size = 2.5, alpha = 0.1) +                 # raw points
  geom_point(data = ind_means,                         # individual means
             aes(x = mean_edge_proximity, 
                 y = mean_prop_time),
             size = 3) +
  geom_smooth(method = "lm") +                         # LM on raw data
  scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
  scale_color_brewer(palette="Dark2") +
  theme_classic(base_size = 14) +
  facet_wrap(~colony) +
  labs(x = "Edge Proximity", y = "Average proportion of time feeding") +
  theme(legend.position="right", axis.title=element_text(face="bold"))

ind_summary <- by_ind_df_edge %>%
  filter(prop_time_emerged > 0,
         colony %in% c("D1","D2","L1","F2","F1")) %>%
  group_by(colony, individual_ID, edge_proximity) %>%
  summarise(
    mean_prop = mean(prop_time_emerged, na.rm = TRUE),
    sd_prop = sd(prop_time_emerged, na.rm = TRUE),
    n_obs = n(),
    se_prop   = sd_prop / sqrt(n_obs),
    .groups = "drop"
  )

avg_feedingedge_prox <- ind_summary %>%
  filter(n_obs>2) %>%
ggplot(aes(x = edge_proximity, y = mean_prop, color = colony)) +
  geom_point(size = 3, alpha=0.3) +
  geom_errorbar(aes(ymin = mean_prop - 1.96*se_prop,
                    ymax = mean_prop + 1.96*se_prop),
                width = 0.02,alpha=0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  #scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_color_manual(values = c("chartreuse4","darkgreen","chocolate1","darkorchid","yellow")) +
  theme_classic(base_size = 14) +
  labs(x = "Edge Proximity",
       y = "Average proportion of time feeding") +
  facet_wrap(~colony) +
  theme(legend.position = "right",
        axis.title = element_text(face = "bold"),
        text = element_text(size=20))


ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/avg_feeding-edge_prox-ind.png', g,
       width = 13, height = 8, units = 'in', dpi = 300)



by_ind_df_edge_f <- by_ind_df_edge %>%
  #filter(colony == "D2") %>%
  mutate(individual_ID = as.factor(individual_ID),
         colony = as.factor(colony),
         date = as.Date(date),
         colony_ind_ID = paste(colony, individual_ID, sep = "_")) #%>%

by_ind_df_edge_f <- by_ind_df_edge_f %>%
  mutate(colony = factor(colony,
                         levels = c("D1","L1",
                                    "F2","D2",
                                    "L2","F1")))
line_plot <- by_ind_df_edge_f %>%
  filter(edge_proximity > 0) %>%
  #filter(prop_time_emerged > 0.01) %>%
  filter(colony_ind_ID != "D2_15") %>%
  group_by(colony) %>%
  mutate(n_dates_colony = n_distinct(date)) %>%
  group_by(colony,individual_ID, n_dates_colony) %>%
  #filter(n_distinct(date[!is.na(prop_time_emerged)]) == n_dates_colony) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = prop_time_emerged,
             group = individual_ID, color = edge_proximity)) +
  geom_point(alpha = 0.5, size = 0.3) +
  geom_line(alpha = 0.5, size = 0.3) +
  facet_wrap(~colony) +
  theme_minimal() +
  labs(x = "Date", y = "Proportion of time foraging",color="Edge proximity") +
  scale_color_viridis_c(option = "V",end=0.9)+  
  theme_classic(base_size = 24) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    axis.title = element_text(face = "bold")
  )

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/avg_feeding-edge_prox-ind-date_all.png', line_plot,
       width = 17, height = 8, units = 'in', dpi = 300)


  
#Transition probabilities
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

# Compute stay_hidden / stay_emerged
agg_dt[, stay_hidden := as.numeric(value == 0 & next_state == 0)]
agg_dt[, stay_emerged := as.numeric(value == 1 & next_state == 1)]

agg_dt_bin <- agg_dt %>%
  group_by(date_f, colony) %>%
  filter(!is.na(n_emerged)) %>%
  filter(value == 1) %>%
  mutate(emerged_bin = cut(n_emerged, breaks = 5)) %>%
  count(emerged_bin, next_state) %>%
  extract(emerged_bin,
          into = c("min","max"),
          remove = FALSE,
          regex = "(?:\\(|\\[)(.*),(.*)(?:\\)|\\])",
          convert = TRUE) %>%
  mutate(emerged_bin= (min + max)/2)

agg_dt %>%
  ggplot(aes(x=n_emerged, y=next_state))+
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "glm",
              method.args = list(family = binomial),
              se = TRUE) +
  facet_wrap(~colony)



  

#### 4 - 3D COLONY PLOT ####
# Create the 3D scatter plot
by_ind_df_f <- by_ind_df %>%
  filter(colony == "L1") %>%
  group_by(individual_ID) %>%
  summarise(x = first(x), y = first(y), z = first(z))
fig <- plot_ly(by_ind_df_f, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", text = by_ind_df_f$individual_ID)

# Add axis titles and a main title (optional)
fig <- fig %>% layout(
  title = "Interactive 3D Scatter Plot with Plotly",
  scene = list(
    xaxis = list(title = "X Variable"),
    yaxis = list(title = "Y Variable"),
    zaxis = list(title = "Z Variable")
  )
)

# Display the plot
fig


scatterplot3d(
  by_ind_df_f$x, by_ind_df_f$y, by_ind_df_f$z,
  angle = 0,      # 90° = bird’s-eye view
  pch = 16,
  color = "blue"
)

##### 5 - MODELLING SITE, COLONY AND INDIVIDUAL LEVEL CONSISTENCY #####
by_ind_df_f$date_c <- scale(by_ind_df_f$date, scale = FALSE)
m1_centered <- lmer(prop_time_emerged ~ date_c + (1|colony) + (date_c || colony:colony_ind_ID), data = by_ind_df_f)
summary(m1_centered)

##### MODELLING TRANSITION PROBABILITIES #####
res <- persistence_prob_safe(each_ind_each_sec_df, 30)

pred_hidden_df <- res$pred_hidden
pred_emerged_df <- res$pred_emerged
full_n_emerged <- res$full_n_emerged

# --- Prepare predicted data frames ---

# Hidden -> Hidden
pred_hidden_df <- data.frame(
  n_emerged = res$full_n_emerged,
  predicted = as.numeric(res$pred_hidden$fit),
  conf.low  = as.numeric(res$pred_hidden$fit - 1.96 * res$pred_hidden$se.fit),
  conf.high = as.numeric(res$pred_hidden$fit + 1.96 * res$pred_hidden$se.fit),
  transition = "Hidden → Hidden"
)

# Emerged -> Emerged
pred_emerged_df <- data.frame(
  n_emerged = res$full_n_emerged,
  predicted = as.numeric(res$pred_emerged$fit),
  conf.low  = as.numeric(res$pred_emerged$fit - 1.96*res$pred_emerged$se.fit),
  conf.high = as.numeric(res$pred_emerged$fit + 1.96*res$pred_emerged$se.fit),
  transition = "Emerged → Emerged"
)

# Steady-state probability
pred_steady_df <- data.frame(
  n_emerged = res$full_n_emerged,
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


# These come from the function
full_n_emerged <- res$full_n_emerged
pred_hidden <- res$pred_hidden
pred_emerged <- res$pred_emerged

pred_hidden <- pred_hidden_df
pred_emerged <- pred_emerged_df
# --- If you used time bins in expand.grid, include them ---
# Example: suppose you predicted for 5 time bins
time_bins_seq <- sort(unique(res$pred_hidden$time_bin_s))  # or define same sequence you used before

# Hidden → Hidden
pred_hidden_df <- data.frame(
  n_emerged  = rep(full_n_emerged, times = length(time_bins_seq)),
  time_bin_s = rep(time_bins_seq, each = length(full_n_emerged)),
  predicted  = as.numeric(res$hidden_model$fit),
  conf.low   = as.numeric(res$hidden_model$fit - 1.96 * res$hidden_model$se.fit),
  conf.high  = as.numeric(res$hidden_model$fit + 1.96 * res$hidden_model$se.fit),
  transition = "Hidden → Hidden"
)

# Emerged → Emerged
pred_emerged_df <- data.frame(
  n_emerged  = rep(full_n_emerged, times = length(time_bins_seq)),
  time_bin_s = rep(time_bins_seq, each = length(full_n_emerged)),
  predicted  = as.numeric(pred_emerged$fit),
  conf.low   = as.numeric(pred_emerged$fit - 1.96 * pred_emerged$se.fit),
  conf.high  = as.numeric(pred_emerged$fit + 1.96 * pred_emerged$se.fit),
  transition = "Emerged → Emerged"
)

# Steady-state probability
pred_steady_df <- data.frame(
  n_emerged  = rep(full_n_emerged, times = length(time_bins_seq)),
  time_bin_s = rep(time_bins_seq, each = length(full_n_emerged)),
  predicted  = (1 - pred_hidden_df$predicted) / (2 - pred_hidden_df$predicted - pred_emerged_df$predicted),
  transition = "Steady-state"
)

# Combine all for plotting
pred_combined <- bind_rows(pred_hidden_df, pred_emerged_df, pred_steady_df)

# --- Plotting ---
ggplot(pred_combined, aes(x = n_emerged, y = predicted, color = transition)) +
  geom_line(size = 1.2) +
  geom_ribbon(
    data = pred_combined %>% filter(transition != "Steady-state"),
    aes(ymin = conf.low, ymax = conf.high, fill = transition),
    alpha = 0.1,
    show.legend = FALSE
  ) +
  facet_wrap(~time_bin_s, ncol = 1, scales = "free_y") +  # One plot per time bin
  labs(
    x = "Number emerged in colony",
    y = "Probability",
    color = "Transition"
  ) +
  scale_color_manual(values = c(
    "Hidden → Hidden" = "blue",
    "Emerged → Emerged" = "red",
    "Steady-state" = "green"
  )) +
  scale_fill_manual(values = c(
    "Hidden → Hidden" = "blue",
    "Emerged → Emerged" = "red"
  )) +
  theme_minimal()


pred_emerged_df %>%
  ggplot() +
  geom_point(aes(x=n_emerged, y = predicted))+
  facet_wrap(site~time_bin_s)

### 6 - Run lengths and n emerged
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



#### 7 - group-level synchrony
synchrony_df_long <- pivot_longer(synchrony_df, cols = c(observed,null), values_to = "value")

synchrony_df_long <- pivot_longer(
  synchrony_df, 
  cols = c(observed_prop, null_prob), 
  names_to = "type", 
  values_to = "prob"
)

synchrony_df_long %>%
  filter(colony == "D4") %>%
  #filter(date = "2025-05-17") %>%
  ggplot(aes(x = n_emerged / max(n_emerged), y = prob, fill = type)) +
  geom_col(alpha = 0.6, position = "identity") +
  facet_wrap(~date~colony) +
  labs(
    x = "Proportion of individuals emerged",
    y = "Probability",
    color = "Distribution"
  ) +
  theme_minimal()

#If worth adding analysis to pres - Do one chi square per colony per date
chi_results <- synchrony_df_long %>%
  group_by(colony, date) %>%
  summarise(
    chi_test = list(chisq.test(observed_count, p = null_count / sum(null_count))),
    .groups = "drop"
  ) %>%
  mutate(
    chi_stat = sapply(chi_test, function(x) x$statistic),
    p_value  = sapply(chi_test, function(x) x$p.value)
  )


probs <- D2_sync$null_prob
probs <- probs / sum(probs)
chisq.test(D2_sync$observed_count, p = probs)

##### 8 - Pairwise synchrony
pairwise_sync_df <- pairwise_synchrony(transitions_path, metadata_path, threshold)

pairwise_sync_inv_df <- pairwise_sync_df %>%
  transmute(
    id_i = id_j,
    id_j = id_i,
    synchrony,
    colony,
    date,
    trial_ID,
    site
  )

pairwise_sync_total_df <- bind_rows(pairwise_sync_df,
                                    pairwise_sync_inv_df)

joined <- left_join(pairwise_sync_total_df, topology_df, join_by(id_i==individual_ID, colony == colony, id_j == id_j))

facet_order <- joined %>%
  filter(colony == "D2") %>%
  group_by(id_i) %>%
  summarise(mean_ratio = mean(edge_proximity, na.rm=TRUE)) %>%
  arrange(mean_ratio) %>%
  pull(id_i)

joined %>%
  filter(colony == "D2") %>%
  filter(distance>0)%>%
  #filter(date == "2025-06-09") %>%
  mutate(dist_rank = as.factor(dist_rank), 
         id_i = factor(id_i, levels = facet_order)) %>%
  ggplot(aes(x = distance, y = synchrony, group=date)) +
  geom_point(aes(color = distance, group=date), size = 2) +   # points colored by rank
  geom_smooth(aes(group = 1), method = "lm", se =TRUE, color = "black") +  # one trend per facet
  facet_wrap(~ id_i) +
  theme(legend.position = "right")   # keep legend to show rank colors

# compute average synchrony per pair across dates/trials
pairwise_avg <- joined %>%
  filter(distance > 0) %>%
  group_by(colony, id_i, id_j, distance) %>%  # keep distance constant per pair
  summarise(mean_synchrony = mean(synchrony, na.rm = TRUE),
            n_obs = sum(!is.na(synchrony)),   # optional: how many dates contributed
            .groups = "drop")

facet_order <- pairwise_avg %>%
  filter(colony == "D4") %>%
  group_by(id_i) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE)) %>%
  arrange(mean_distance) %>%
  pull(id_i)

pairwise_avg %>%
  filter(colony == "D4") %>%
  mutate(id_i = factor(id_i, levels = facet_order)) %>%
  ggplot(aes(x = distance, y = mean_synchrony)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ id_i, ncol = 8) +
  theme_bw() +
  labs(
    x = "Distance between individuals",
    y = "Average pairwise synchrony"
  )

##final
library(dplyr)
library(ggplot2)

# ---- 1. Compute pairwise synchrony ----
pairwise_sync_df <- pairwise_synchrony(transitions_path, metadata_path, threshold)

pairwise_sync_sym <- bind_rows(
  pairwise_sync_df,
  pairwise_sync_df %>%
    mutate(temp = id_i) %>%
    transmute(
      id_i = id_j,
      id_j = temp,
      synchrony,
      colony,
      date,
      trial_ID,
      site
    )
)


# ---- 3. Join with distances / topology ----
joined <- left_join(pairwise_sync_sym, topology_df,
                    by = c("id_i" = "individual_ID",
                           "id_j" = "id_j",
                           "colony" = "colony"))

# ---- 4. Order facets by mean edge proximity ----
facet_order <- joined %>%
  filter(colony == "D2") %>%
  group_by(id_i) %>%
  summarise(mean_edge_prox = mean(edge_proximity, na.rm=TRUE), .groups="drop") %>%
  arrange(mean_edge_prox) %>%
  pull(id_i)

joined1 <- joined %>%
  filter(colony == "D2") %>%
  mutate(id_i = factor(id_i, levels = facet_order))

# ---- 5. Plot raw points ----
joined1 %>%
  filter(distance >0) %>%
  ggplot(aes(x = distance, y = synchrony, group = date)) +
  geom_point(aes(color = distance), size = 2, alpha = 0.6) +
  facet_wrap(~ id_i, ncol = 8) +
  theme_bw() +
  labs(
    x = "Distance between individuals",
    y = "Raw pairwise synchrony"
  ) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )



pairwise_avg <- joined %>%
  filter(distance > 0) %>%
  group_by(colony, id_i, id_j, distance) %>%  # keep distance constant per pair
  summarise(mean_synchrony = mean(synchrony, na.rm = TRUE),
            n_obs = sum(!is.na(synchrony)),   # optional: how many dates contributed
            .groups = "drop")

facet_order <- pairwise_avg %>%
  filter(colony == "D2") %>%
  group_by(id_i) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE)) %>%
  arrange(mean_distance) %>%
  pull(id_i)

pairwise_avg %>%
  filter(colony == "D2") %>%
  mutate(id_i = factor(id_i, levels = facet_order)) %>%
  ggplot(aes(x = distance, y = mean_synchrony)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ id_i, ncol = 8) +
  theme_bw() +
  labs(
    x = "Distance between individuals",
    y = "Average pairwise synchrony"
  )

library(dplyr)
library(ggplot2)

#AVERAGES,BINNED
# Define bin width
bin_width <- 0.1

pairwise_binned <- joined %>%
  filter(distance > 0) %>%
  mutate(
    # Compute the bin start
    bin_start = floor(distance / bin_width) * bin_width,
    # Compute the bin midpoint for plotting
    distance_mid = bin_start + bin_width / 2
  ) %>%
  group_by(colony, id_i, distance_mid) %>%
  summarise(
    mean_synchrony = mean(synchrony, na.rm = TRUE),
    n_obs = sum(!is.na(synchrony)),
    .groups = "drop",
    edge_proximity = mean(edge_proximity)
  )


facet_order <- joined %>%
  filter(colony == "D2") %>%
  group_by(id_i) %>%
  summarise(mean_ratio = mean(edge_proximity, na.rm=TRUE)) %>%
  arrange(mean_ratio) %>%
  pull(id_i)

slopes <- pairwise_binned %>%
  filter(colony == "D2") %>%
  group_by(id_i) %>%
  do(tidy(lm(mean_synchrony ~ distance_mid, data = .))) %>%
  filter(term == "distance_mid") %>%
  select(id_i, slope = estimate) %>%
  arrange(desc(slope))

facet_order <- slopes$id_i

# Plot
p<- pairwise_binned %>%
  filter(colony == "D2") %>%
  mutate(id_i = factor(id_i, levels = facet_order)) %>%
  ggplot(aes(x = distance_mid, y = mean_synchrony, color = id_i)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ id_i, ncol = 8) +
  theme_bw() +
  theme(strip.text = element_blank())+
  labs(
    x = "Distance between individuals (binned)",
    y = "Average pairwise synchrony"
  )

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/pairwise_D2.png', p,
       width = 10, height = 8, units = 'in', dpi = 300)





#Network using inter-individual distances 

#Group level night time synchrony
## - proportion of time foraging for each individual
global_by_ind_dataset <- by_ind_df %>% 
  filter(prop_time_emerged > 0.0001) %>%
  group_by(colony, individual_ID) %>%
  summarise(overall_p_time_emerged = mean(prop_time_emerged))

by_ind_df %>%
  mutate(individual_ID = as.factor(individual_ID), date = as.factor(date)) %>% 
  ggplot(aes(x = individual_ID, y = prop_time_emerged, colour = date)) +
  geom_boxplot() +
  facet_wrap(~colony) +
  theme(legend.position = "none")

global_by_ind_dataset %>% 
  mutate(individual_ID = as.factor(individual_ID)) %>% 
  ggplot(aes(x = individual_ID, y = overall_p_time_emerged, colour = individual_ID)) +
  geom_boxplot() +
  facet_wrap(~colony) +
  theme(legend.position = "none")

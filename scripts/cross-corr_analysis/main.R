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

#Functions
source("scripts/cross-corr_analysis/R/combine_data_by_time.R")
source("scripts/cross-corr_analysis/R/combine_data_by_individual.R")
source("scripts/cross-corr_analysis/R/each_ind_each_sec.R")
source("scripts/cross-corr_analysis/R/group_synchrony.R")
source("scripts/cross-corr_analysis/R/pairwise_synchrony.R")
source("scripts/cross-corr_analysis/R/topology.R")
source("scripts/cross-corr_analysis/R/figures/persistence_prob_mem_save.R")
source("scripts/ABM/R/fill_holes.R")

#Initialise
threshold = 5
transitions_path = "/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated"
metadata_path = "/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/diel_cycle_garden_eel/diel_eel_processing.xlsx"


### DATA WRANGLING ### 

#A - Summarise data by time
by_time_df <- combine_data_by_time(transitions_path, metadata_path, threshold)

by_time_df$colony_size[by_time_df$colony == "D2"] <- 29
by_time_df$colony_size[by_time_df$colony == "D4"] <- 94
by_time_df$colony_size[by_time_df$colony == "L1"] <- 28
by_time_df$colony_size[by_time_df$colony == "L4"] <- 116
by_time_df$colony_size[by_time_df$colony == "F1"] <- 60
by_time_df$colony_size[by_time_df$colony == "F2"] <- 5

by_time_df$time_of_day <- as.POSIXct(by_time_df$sec_since_midnight,
                             origin = "1970-01-01",
                             tz = "UTC")

#B - Summarise data by individual
by_ind_df <- combine_data_by_individual(transitions_path, metadata_path, threshold)

by_ind_df$colony_size[by_ind_df$colony == "D2"] <- 29
by_ind_df$colony_size[by_ind_df$colony == "D4"] <- 94
by_ind_df$colony_size[by_ind_df$colony == "L1"] <- 28
by_ind_df$colony_size[by_ind_df$colony == "L4"] <- 116
by_ind_df$colony_size[by_ind_df$colony == "F1"] <- 60
by_ind_df$colony_size[by_ind_df$colony == "F2"] <- 5


#C - Each individual, each second
each_ind_each_sec_df <- each_ind_each_sec(transitions_path, metadata_path, threshold)

each_ind_each_sec_df$individual_ID <- as.factor(each_ind_each_sec_df$individual_ID)
each_ind_each_sec_df$colony <- as.factor(each_ind_each_sec_df$colony)
each_ind_each_sec_df$site <- as.factor(each_ind_each_sec_df$site)
each_ind_each_sec_df$site <- as.factor(each_ind_each_sec_df$site)
each_ind_each_sec_df$date_f <- as.factor(each_ind_each_sec_df$date)

each_ind_each_sec_df$colony_size[each_ind_each_sec_df$colony == "D2"] <- 29
each_ind_each_sec_df$colony_size[each_ind_each_sec_df$colony == "D4"] <- 94
each_ind_each_sec_df$colony_size[each_ind_each_sec_df$colony == "L1"] <- 28
each_ind_each_sec_df$colony_size[each_ind_each_sec_df$colony == "L4"] <- 116
each_ind_each_sec_df$colony_size[each_ind_each_sec_df$colony == "F1"] <- 60
each_ind_each_sec_df$colony_size[each_ind_each_sec_df$colony == "F2"] <- 5

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
  filter(prop_time_emerged >0) %>%
  group_by(date,site) %>%
  mutate(bin_size = if_else(colony_size == min(colony_size), "small","large")) %>%
  ungroup() %>%
  group_by(date,site) %>%
  mutate(n_pairs = n_distinct(bin_size), bin_size = factor(bin_size, levels = c("small", "large"))) %>%
  ungroup() %>%
  filter(n_pairs > 1) %>%
  ggplot(aes(x = bin_size, y = prop_time_emerged, group = interaction(site,date), color = site)) +
  stat_summary(fun = "mean", geom = "line", size = 1) +
  stat_summary(fun = "mean", geom = "point", size = 2.5) +
  scale_y_continuous(limits=c(0,1),expand=c(0,0))+
  scale_color_brewer(palette="Dark2")+
  theme_classic(base_size = 14) +
  #facet_wrap(~date) +
  labs(x = "Colony size", y = "Average proportion of time feeding") +
  theme(legend.position = "right", axis.title=element_text(face="bold"))

ggsave('~/Desktop/PhD/academic_projects/eel_diel/presentations/benthics_25/figures/feeding_big_small_colony.png', feeding_big_small_colony,
       width = 8, height = 10, units = 'in', dpi = 300)


#Individuals
ind_feeding_big_small_colony <- by_ind_df %>%
  filter(prop_time_emerged >0) %>%
  group_by(date,site) %>%
  mutate(bin_size = if_else(colony_size == min(colony_size), "small","large")) %>%
  ungroup() %>%
  group_by(date,site) %>%
  mutate(n_pairs = n_distinct(bin_size), bin_size = factor(bin_size, levels = c("small", "large"))) %>%
  ungroup() %>%
  filter(n_pairs > 1) %>%
  ggplot(aes(x = bin_size, y = prop_time_emerged, group = interaction(site,date), color = site)) +
  geom_jitter(width = 0.09, size = 2.5, alpha = 0.1) +
  stat_summary(fun = "mean", geom = "line", size = 1) +
  stat_summary(fun = "mean", geom = "point", size = 2.5) +
  scale_y_continuous(limits=c(0,1),expand=c(0,0))+
  scale_color_brewer(palette="Dark2")+
  theme_classic(base_size = 14) +
  #facet_wrap(~date) +
  labs(x = "Colony size", y = "Average proportion of time feeding") +
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

by_ind_df_edge %>%
  filter(prop_time_emerged >0) %>%
  filter(colony %in% c("D2","D4","L1","F2")) %>%
  ggplot(aes(x = edge_proximity, y = prop_time_emerged)) +
  geom_point(size = 2.5, alpha = 0.1) +
  #stat_summary(fun = "mean", geom = "line", linetype = "longdash", size = 1, alpha = 0.5) +
  #stat_summary(fun = "mean", geom = "point", size = 2.5) +
  scale_y_continuous(limits=c(0,1),expand=c(0,0))+
  scale_color_brewer(palette="Dark2")+
  geom_smooth(method = "lm")+
  theme_classic(base_size = 14) +
  #facet_wrap(~colony) +
  labs(x = "Edge Proximity", y = "Proportion of time feeding") +
  theme(legend.position = "right", axis.title=element_text(face="bold"))


by_ind_df_edge_f <- by_ind_df_edge %>%
  #filter(colony == "D2") %>%
  mutate(individual_ID = as.factor(individual_ID),
         colony = as.factor(colony),
         date = as.Date(date),
         colony_ind_ID = paste(colony, individual_ID, sep = "_")) #%>%

by_ind_df_edge_f %>%
  filter(edge_proximity > 0) %>%
  filter(prop_time_emerged > 0) %>%
  filter(colony_ind_ID != "D4_15") %>%
  group_by(colony) %>%
  mutate(n_dates_colony = n_distinct(date)) %>%
  group_by(colony,individual_ID, n_dates_colony) %>%
  filter(n_distinct(date[!is.na(prop_time_emerged)]) == n_dates_colony) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = prop_time_emerged,
             group = individual_ID, color = edge_proximity)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(~colony) +
  theme_minimal() +
  scale_color_brewer(palette="Dark2")+
  theme(legend.position = "right", axis.title=element_text(face="bold")) +
  labs(x = "Date", y = "Proportion of time feeding",color="Edge proximity") +
  scale_color_viridis_c(option = "V",end=0.9)+  
  theme_classic(base_size = 14)

  
  

#### 4 - 3D COLONY PLOT ####
# Create the 3D scatter plot
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
pairwise_sync_inv_df <- pairwise_sync_df

pairwise_sync_inv_df <- pairwise_sync_inv_df %>%
  rename(
    id_i = id_j,
    id_j = id_i
  )

pairwise_sync_total_df <- rbind(pairwise_sync_df, pairwise_sync_inv_df)


joined <- left_join(pairwise_sync_total_df, topology_df, join_by(id_i==individual_ID, colony == Colony, id_j == id_j))

facet_order <- joined %>%
  filter(colony == "D4") %>%
  group_by(id_i) %>%
  summarise(mean_ratio = mean(ratio_edge_to_centre, na.rm=TRUE)) %>%
  arrange(mean_ratio) %>%
  pull(id_i)


joined %>%
  filter(colony == "D2") %>%
  #filter(date == "2025-06-09") %>%
  mutate(dist_rank = as.factor(dist_rank), 
         id_i = factor(id_i, levels = facet_order)) %>%
  ggplot(aes(x = distance, y = synchrony, group=date)) +
  geom_point(aes(color = distance, group=date), size = 2) +   # points colored by rank
  geom_smooth(aes(group = 1), method = "lm", se =TRUE, color = "black") +  # one trend per facet
  facet_wrap(~ id_i) +
  theme(legend.position = "right")   # keep legend to show rank colors




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







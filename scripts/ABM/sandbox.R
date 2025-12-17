#Sandbox
#Histogram of run lengths
results_run_length <- results %>%
  group_by(individual_ID) %>%
  arrange(variable, .by_group = TRUE) %>%
  reframe({
    r <- rle(value)
    tibble(value = r$values,
           run_length = r$lengths)
  }) %>%
  filter(!individual_ID %in% c(1,2)) %>%
  ungroup() %>%
  group_by(individual_ID) %>%
  mutate(cum_val = cumsum(run_length), start_time = cumsum(run_length) - run_length, seg = ceiling(start_time/512), seg_ID = (seg-1) + 7, second = start_time-(seg-1)*512) %>%
  ungroup()

results_run_length_hide <- results_run_length %>%
  filter(value == 0) #%>%
#dplyr::pull(run_length)

results_run_length_emerge <- results_run_length %>%
  filter(value == 1) #%>%
#dplyr::pull(run_length)

#Plotting raw data or sim
df <- data.frame(value = c(results_run_length_emerge$run_length, df_run_length_emerge$run_length),
                 group = factor(rep(c("sim","empirical"),
                                    times = c(length(results_run_length_emerge$run_length),length(df_run_length_emerge$run_length))))
)

df %>%
  ggplot(aes(x = value, fill = group))+
  geom_histogram(alpha = 0.5, position = "identity", bins = 30)+
  scale_fill_manual(values = c("red","blue"))+
  theme_minimal()+
  labs(title = "Nbinomial, emerge run lengths")

df_long %>%
  ggplot(aes(x = variable, y = individual_ID, fill = factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "black", "1" = "white")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Simulation with a switching prob calculated from nb distr")

#Isolating perturbed hides
peturb_only <- isolate_perturbed_hides(df_run_length)

inverse_df <- anti_join(df_run_length_hide, peturb_only)

mean(df_run_length$run_length)


#Simulate random positions dataset 
positions <- data.frame(rnorm(26, 2,1),rnorm(26, 2,1),1:26)
names(positions) <- c("x","y","individual_ID")
df_long_positions <- left_join(df_long,positions,by="individual_ID")

#Testing activation function
df_long <- calculate_state_changes(df_long)
df_long <- activation_function(df_long)

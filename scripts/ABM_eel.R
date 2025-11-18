#Path of external forcing moving across group
#All up, when force comes within threshold distance hide 
#Neighbourhood rules if neighbour x distance hides this changes your likelihood of staying emerged

library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(MASS) 

setwd("/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Curacao 2023/Gil Lab field research/ella_2023/garden eel/Analysis/Data/transition_matricies/latest")
abs_distances = read.csv("D4_abs_dist.csv", header = FALSE)
abs_distances = abs_distances[1:28,]
plot(abs_distances[,1], abs_distances[,2])
abs_distances[,3] <- 1:nrow(abs_distances)

forage_function <- function(abs_distances, ts, f_swimover, internal_energetic_state, feeding_gain, energy_loss_hidden, energy_loss_emerged) {
  
  #
  n <- nrow(abs_distances)
  #get IDs
  id <- 1:n
  #Initialize matrix of the state of each individual for each time step
  state_matrix <- matrix(NA, nrow = n, ncol = ts)
  state_matrix <- cbind(id, state_matrix)
  colnames(state_matrix) <- c("ID", paste0("T", 1:ts))
  #Initialize first time step states as 0
  state_matrix[,2] <- rep(0, n)
  #Initial foraging need (after fasting the night)
  fish_pos <- matrix(NA, 30, 2)
  fish_present <- 0 
  maxX <- ceiling(max(abs_distances[,1])) + 5
  minX <- floor(min(abs_distances[,1])) - 5
  maxY <- ceiling(max(abs_distances[,2])) + 5
  minY <- floor(min(abs_distances[,2])) - 5
  
  energetic_state <- matrix(NA, nrow = n, ncol = ts)
  energetic_state <- cbind(id, energetic_state)
  energetic_state[,2] <- rep(internal_energetic_state, n)
  
  for (f in 3:max(ts-1)) {
    prop_emerged <- sum(state_matrix[,f-1])/n #calculate the proportion emerged in the previous second
    for (i in 1:length(id)) {
      #If hidden in previous second, determine whether stays or emerges based on internal energetic state
      if (state_matrix[i, f-1] == 0) {
        p_swim <- max(0, min(1, energetic_state[i,f-1]))
        state_matrix[i,f] <- rbinom(1,1, p_swim)
      }
      #If emerged in previous second
      else {
          #If there is a fish in present, determine distance from fish, internal energetic state and neighbour infuence
          if (fish_present == 1) {
            fish_x <- fish_pos[1,f]
            fish_y <- fish_pos[1,f]
            eel_x <- abs_distances$V1[i]
            eel_y <- abs_distances$V2[i]
            dist_fish <-  sqrt((fish_x - eel_x)^2 + (fish_y - eel_y)^2)
            neighbour_influence <- 0.1
              #determine states of neighbours
              #compute influence term based on distance and number of neighbours 
              #Start with metric fixed 
            p_swim <- max(0, min(1, energetic_state[i,f-1] + neighbour_influence + dist_fish))
            state_matrix[i,f] <- rbinom(1,1, p_swim)
          }
          #if there is no fish present, just determine internal energetic state and neighbour influence
          else {
            neighbour_influence <- 0.1
            p_swim <- max(0, min(1, energetic_state[i,f-1] + neighbour_influence))
            state_matrix[i,f] <- rbinom(1,1, p_swim)
          }
      }
      
      #if chose to emerge, internal energetic state increase with feeding gain but decreases with energy lost from swimming 
      if (state_matrix[i,f] == 1) {
        energetic_state[i,f] <- energetic_state[i,f-1] + feeding_gain + energy_loss_emerged
      } 
      #if chose to hide, internal energetic state decreases with energy lost from swimming
      else {
        energetic_state[i,f] <- energetic_state[i,f-1] + energy_loss_hidden
      }

    #If fish not present, determine whether it enters scene or not. 
    if (fish_present == 0) {

      fish_enter <- rbinom(1, 1, f_swimover)
      
      if (fish_enter == 1) {
        
        #determine side
        side <- sample(1:4,1)
        if (side == 1) {
          fish_pos[1,1] <- minX
          fish_pos[1,2] <- runif(1, minY, maxY)
        }
        if (side == 2) {
          fish_pos[1,1] <- runif(1,minX, maxX)
          fish_pos[1,2] <- minY
        }
        if (side == 3) {
          fish_pos[1,1] <- maxX
          fish_pos[1,2] <- runif(1,minY,maxY)
        }
        if (side == 4) {
          fish_pos[1,1] <- runif(1,minX,maxX)
          fish_pos[1,2] <- maxY
        }
        
        #determine angle
        theta <- runif(1, 0, pi)
        
        #determine direction
        dx = cos(theta)
        dy = sin(theta)
        
        #compute candidate intersection points with the bounding box
        t_values <- c()
        if (dx != 0){
          t_values <- c(t_values, (minX - fish_pos[1,1])/dx,(maxX - fish_pos[1,1])/dx)
        }
        if (dy != 0){
          t_values <- c(t_values, (minY - fish_pos[1,2])/dy,(maxY - fish_pos[1,2])/dy)
        }
          
        points <- list()
        
        for (t in t_values) {
          x <- fish_pos[1,1] + t*dx
          y <- fish_pos[1,2] + t*dy
          if (x >= minX && x <= maxX && y >= minY && y<= maxY) {
            points <- append(points, list(c(x,y)))
          }
        }
        
        if (length(points) >= 2) { 
          p1 <- points[[1]]
          p2 <- points[[2]]
        }
        
        N <- 30
        t <- seq(from = 0, to = 1, length.out = N)
        fish_pos[,1] <- p1[1] + t * (p2[1] - p1[1])
        fish_pos[,2] <- p1[2] + t * (p2[2] - p1[2])
        
      }
    } 
      else {
      if (fish_pos[f,1] == fish_pos[N,1]) {
        fish_present <- 0
      }
    }
    }
  }
  results <- energetic_state
}

ts <- 60
f_swimover <- 0.1
internal_energetic_state <- 0.8
feeding_gain <- 0.1
energy_loss_hidden <- 0.1
energy_loss_emerged <- 0.1

results <- forage_function(abs_distances, ts, f_swimover, internal_energetic_state, feeding_gain, energy_loss_hidden, energy_loss_emerged)

non_state_dependent_submodel <- function(abs_distances, ts, distr_emerge, distr_hide) {
  
  #
  n <- nrow(abs_distances)
  #get IDs
  id <- 1:n
  #Initialize matrix of the state of each individual for each time step
  state_matrix <- matrix(NA, nrow = n, ncol = ts)
  state_matrix <- cbind(id, state_matrix)
  colnames(state_matrix) <- c("ID", paste0("T", 1:ts))
  #Initialize first time step states as 0
  state_matrix[,2] <- rep(0, n)
  #Initial foraging need (after fasting the night)
  fish_pos <- matrix(NA, 30, 2)
  fish_present <- 0 
  maxX <- ceiling(max(abs_distances[,1])) + 5
  minX <- floor(min(abs_distances[,1])) - 5
  maxY <- ceiling(max(abs_distances[,2])) + 5
  minY <- floor(min(abs_distances[,2])) - 5
  
  for (f in 3:max(ts-1)) {

      for (i in 1:length(id)) {
        state_matrix[i,f] <- rbinom(1,1, p_swim)
      }
    
  }
  
  results <- state_matrix
    
}

results <- non_state_dependent_submodel(abs_distances, 29184, avg_prop_up)

#Loading in empirical data
setwd("~/Desktop/PhD/academic_projects/eel_diel/dataframes")
data <- read.csv("transitions_D2_31_05_25_complete.csv", header =FALSE)

fill_holes <- function(vec, threshold = 10) {
  rle_vec <- rle(vec)  # Run length encoding of the vector
  values <- rle_vec$values
  lengths <- rle_vec$lengths
  
  # Find indices of runs smaller than the threshold
  small_runs <- which(lengths < threshold)
  
  for (i in small_runs) {
    if (i > 1 && i < length(values)) {
      values[i] <- values[i-1]
    }
  }
  
  # Recreate the original vector with the modified runs
  inverse_rle <- inverse.rle(list(lengths = lengths, values = values))
  return(inverse_rle)
}



current_colnames_T <- colnames(data) #get colnames
current_colnames_T[1] <- "individual_ID" #add individual_ID as colname
colnames(data) <- current_colnames_T #re add columns to transitions
colnames(data)[2:ncol(data)] <- seq(1, ncol(data) - 1) #Add a number column name
data[,-1] <- lapply(data[,-1], as.numeric) #Convert to numeric
data[, 2:ncol(data)] <- t(apply(data[, 2:ncol(data)], 1, fill_holes)) # Apply the function to each row, starting from the 2nd column
  
results_data <- rowSums(data[2:29185])
results_data <- results_data[2:28]
results_data_d <- results_data/29184
avg_prop_up <- mean(results_data_d)
    
df_long <- melt(data, id.vars = "individual_ID")

df_long$variable <- as.numeric(df_long$variable)

ggplot(df_long, aes(x = variable, y = individual_ID, fill = factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "black", "1" = "white")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Binary Barcode Plot")

sim_long <- melt(results, id.vars = "ID")
ggplot(sim_long, aes(x = Var2, y = Var1, fill = factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "black", "1" = "white")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Binary Barcode Plot")


df_sum <- df_long %>%
  group_by(X1) %>%
  summarise(prop_emerged = sum(value)/max(variable))

hist(df_sum$prop_emerged)

df_long$X1 <- as.factor(df_long$X1)

df_run_length <- df_long %>%
  group_by(individual_ID) %>%
  arrange(variable, .by_group = TRUE) %>%
  reframe({
    r <- rle(value)
    tibble(value = r$values,
           run_length = r$lengths)
  }) %>%
  filter(individual_ID != c(1)) %>%
  ungroup()

df_run_length_down <- df_run_length %>%
  filter(run_length > 10 & value == 0)

hist(df_run_length_down$run_length)

p_hat <- 1 / mean(df_run_length_down$run_length)
N <- nrow(df_run_length_down)
# Range of x-values (run lengths)
x_seq <- seq(min(df_run_length_down$run_length), max(df_run_length_down$run_length))

# Simulate many samples under fitted geometric
n_sim <- 1000  # number of Monte Carlo simulations

sim_cdfs <- map_dfr(1:n_sim, function(i) {
  r <- rgeom(N, p_hat) + 1  # geometric counts from 1 upward
  tibble(
    run_length = sort(unique(x_seq)),
    cum_prob = ecdf(r)(x_seq),
    sim = i
  )
})

df_band <- sim_cdfs %>%
  group_by(run_length) %>%
  summarise(
    lower = quantile(cum_prob, 0.025),
    upper = quantile(cum_prob, 0.975),
    .groups = "drop"
  )

df_cdf <- df_run_length_down %>%
  count(run_length, name = "n") %>%
  arrange(run_length) %>%
  mutate(cum_prob = cumsum(n) / sum(n))

df_geom <- tibble(
  run_length = x_seq,
  cum_prob = 1 - (1 - p_hat)^x_seq
)

ggplot() +
  # 95% range (shaded area)
  geom_ribbon(
    data = df_band,
    aes(x = run_length, ymin = lower, ymax = upper),
    fill = "grey80", alpha = 0.6
  ) +
  # Empirical CDF
  geom_step(
    data = df_cdf,
    aes(x = run_length, y = cum_prob, color = "Empirical"),
    linewidth = 1
  ) +
  # Fitted geometric CDF
  geom_line(
    data = df_geom,
    aes(x = run_length, y = cum_prob, color = "Fitted geometric"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_color_manual(values = c("Empirical" = "blue", "Fitted geometric" = "red")) +
  labs(
    title = "Empirical CDF of Run Lengths with Fitted Geometric Distribution",
    subtitle = paste0("p̂ = ", round(p_hat, 3), ", 95% simulation band (N = ", N, ")"),
    x = "Run Length",
    y = "Cumulative Probability",
    color = ""
  ) +
  theme_minimal()


# -------------------------
# 1. Compute run lengths
# -------------------------
df_run_length <- df_long %>%
  filter(individual_ID != 1) %>%
  group_by(individual_ID) %>%
  arrange(variable, .by_group = TRUE) %>%
  reframe({
    r <- rle(value)
    tibble(
      value = r$values,
      run_length = r$lengths
    )
  }) %>%
  filter(value == 1 & run_length > 10) %>%
  ungroup()

# -------------------------
# 2. Prepare empirical CDF
# -------------------------
df_cdf <- df_run_length %>%
  count(run_length, name = "n") %>%
  arrange(run_length) %>%
  mutate(cum_prob = cumsum(n) / sum(n))

x_seq <- seq(min(df_run_length$run_length), max(df_run_length$run_length))

# -------------------------
# 3. Fit candidate distributions
# -------------------------
# Geometric: p_hat = 1 / mean
p_hat <- 1 / mean(df_run_length$run_length)
cdf_geom <- 1 - (1 - p_hat)^x_seq

# Poisson fit
fit_pois <- fitdistr(df_run_length$run_length, "Poisson")
lambda_hat <- fit_pois$estimate
cdf_pois <- ppois(x_seq, lambda_hat)

# Negative binomial fit
fit_nb <- fitdistr(df_run_length$run_length, "Negative Binomial")
size_hat <- fit_nb$estimate["size"]
mu_hat   <- fit_nb$estimate["mu"]
cdf_nb <- pnbinom(x_seq, size = size_hat, mu = mu_hat)

# -------------------------
# 4. Plot empirical and fitted CDFs
# -------------------------
df_plot <- tibble(
  run_length = x_seq,
  geom = cdf_geom,
  poisson = cdf_pois,
  negbin = cdf_nb
) %>%
  pivot_longer(-run_length, names_to = "dist", values_to = "cum_prob")

ggplot() +
  geom_step(data = df_cdf, aes(x = run_length, y = cum_prob, color = "Empirical"), linewidth = 1) +
  geom_line(data = df_plot, aes(x = run_length, y = cum_prob, color = dist), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Empirical" = "blue", "geom" = "red", "poisson" = "green", "negbin" = "purple"),
                     labels = c("Empirical", "Geometric", "Poisson", "Neg. Binomial")) +
  labs(
    title = "Empirical CDF of Emerged Run Lengths with Fitted Distributions",
    x = "Run Length",
    y = "Cumulative Probability",
    color = ""
  ) +
  theme_minimal()


library(dplyr)
library(ggplot2)
library(MASS)
library(purrr)
library(tidyr)

# -------------------------
# 1. Prepare run lengths
# -------------------------
df_run_length <- df_long %>%
  filter(individual_ID != 1) %>%
  group_by(individual_ID) %>%
  arrange(variable, .by_group = TRUE) %>%
  reframe({
    r <- rle(value)
    tibble(
      value = r$values,
      run_length = r$lengths
    )
  }) %>%
  filter(value == 0 & run_length > 10) %>%
  ungroup()

x_seq <- seq(min(df_run_length$run_length), max(df_run_length$run_length))
N <- nrow(df_run_length)

# -------------------------
# 2. Empirical CDF
# -------------------------
df_cdf <- df_run_length %>%
  count(run_length, name = "n") %>%
  arrange(run_length) %>%
  mutate(cum_prob = cumsum(n) / sum(n))

# -------------------------
# 3. Fit negative binomial
# -------------------------
fit_nb <- fitdistr(df_run_length$run_length, "Negative Binomial")
size_hat <- fit_nb$estimate["size"]
mu_hat   <- fit_nb$estimate["mu"]

# Fitted CDF
cdf_nb <- pnbinom(x_seq, size = size_hat, mu = mu_hat)

# -------------------------
# 4. Simulate 95% confidence band
# -------------------------
n_sim <- 1000
sim_cdfs <- map_dfr(1:n_sim, function(i) {
  r <- rnbinom(N, size = size_hat, mu = mu_hat)
  tibble(
    run_length = x_seq,
    cum_prob = ecdf(r)(x_seq),
    sim = i
  )
})

df_band <- sim_cdfs %>%
  group_by(run_length) %>%
  summarise(
    lower = quantile(cum_prob, 0.025),
    upper = quantile(cum_prob, 0.975),
    .groups = "drop"
  )

# -------------------------
# 5. Plot
# -------------------------
ggplot() +
  # 95% band
  geom_ribbon(
    data = df_band,
    aes(x = run_length, ymin = lower, ymax = upper),
    fill = "purple", alpha = 0.2
  ) +
  # Empirical CDF
  geom_step(
    data = df_cdf,
    aes(x = run_length, y = cum_prob, color = "Empirical"),
    linewidth = 1
  ) +
  # Fitted negative binomial CDF
  geom_line(
    data = tibble(run_length = x_seq, cum_prob = cdf_nb),
    aes(x = run_length, y = cum_prob, color = "Neg. Binomial fit"),
    linewidth = 1, linetype = "dashed"
  ) +
  scale_color_manual(values = c("Empirical" = "blue", "Neg. Binomial fit" = "purple")) +
  labs(
    title = "Run Length CDF with Fitted Negative Binomial",
    subtitle = paste0("mu = ", round(mu_hat, 2), ", size = ", round(size_hat, 2), 
                      ", 95% simulation band (N = ", N, ")"),
    x = "Run Length",
    y = "Cumulative Probability",
    color = ""
  ) +
  theme_minimal()

#very overdispersed - short and long values

#compute the times of ch
df_run_length <- df_run_length %>%
  group_by(individual_ID) %>%
  mutate(cum_val = cumsum(run_length), seg = ceiling(cumsum(run_length)/512), seg_ID = (seg-1) + 7, second = cum_val-(seg-1)*512)

df_run_length_hide_time <- df_run_length %>%
  filter(value == 1)

#pull out emerges ended naturally and the associated hides
df_run_length_hide_time_nofish <- df_run_length %>%
  filter(individual_ID == 14) %>%
  arrange(cum_val) %>%
  mutate(row_id = row_number()) %>%
  {
    rows <- which(.$second %in% c(238,253,345,98,211,97,95,39,411))
    slice(., sort(c(rows, rows + 1)))
  }

df_run_length_hide_time_nofish %>%
  filter(value ==0) %>%
  ggplot(aes(run_length)) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  labs(
    title = "CDF of Run Lengths (value == 1)",
    x = "Run length of 1s",
    y = "Cumulative probability"
  ) +
  theme_minimal()

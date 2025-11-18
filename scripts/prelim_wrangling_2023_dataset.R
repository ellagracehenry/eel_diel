################## PACKAGES ##########################
library(tidyr)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(patchwork)
library(spdep)
library(sp)
library(stringr)
library(boot)
library(forcats)
library(ggridges)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

################### FUNCTIONS #########################
fill_holes <- function(vec, threshold = 5) {
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

calculate_edge_to_centre_ratio <- function(df) {
  # Calculate the centroid (mean of x and y coordinates)
  centroid <- colMeans(df[, c("x", "y")])
  
  # Identify the convex hull points
  hull_indices <- chull(df[, c("x", "y")])
  hull_points <- df[hull_indices, c("x", "y")]
  
  # Helper function to calculate the distance from a point to a line segment
  point_to_segment_distance <- function(p, v, w) {
    l2 <- sum((w - v)^2)  # Squared length of the segment
    if (l2 == 0) return(sqrt(sum((p - v)^2)))  # Segment is a single point
    
    t <- max(0, min(1, sum((p - v) * (w - v)) / l2))  # Projection onto segment
    projection <- v + t * (w - v)
    return(sqrt(sum((p - projection)^2)))
  }
  
  # Initialize a vector to store the ratios
  ratios <- numeric(nrow(df))
  
  # Calculate the ratio for each point
  for (i in seq_len(nrow(df))) {
    focal_point <- as.numeric(df[i, c("x", "y")])
    
    # Distance to the centroid
    dist_to_centre <- sqrt(sum((focal_point - centroid)^2))
    
    # Distance to the edge (minimum distance to any segment of the hull)
    dist_to_edge <- Inf
    for (j in seq_len(nrow(hull_points))) {
      v <- hull_points[j, ]
      w <- hull_points[ifelse(j == nrow(hull_points), 1, j + 1), ]  # Wrap around
      dist_to_edge <- min(dist_to_edge, point_to_segment_distance(focal_point, v, w))
    }
    
    # Calculate the ratio
    ratios[i] <- dist_to_edge / dist_to_centre
  }
  
  # Add the ratios to the data frame as a new column
  df$ratio_edge_to_centre <- ratios
  return(df)
}

################ WORKING DIRECTORY ############################
setwd("H:/Shared drives/Field Research Videos/Gil Lab/Curacao 2023/Gil Lab field research/ella_2023/garden eel/Analysis/Data/transition_matricies/latest")

################ LOAD IN ######################################
transitions = read.csv("transitions_D2_31_05_25_complete.csv", header = FALSE) #load in transitions
abs_distances = read.csv("D3_abs_dist.csv",header = FALSE) #load in absolute distances
state_matrix =  read.csv("state_matrix_2202.csv")
write.csv(state_matrix, "state_matrix_2202.csv")


current_colnames_T <- colnames(transitions) #get colnames
current_colnames_T[1] <- "individual_ID" #add individual_ID as colname
colnames(transitions) <- current_colnames_T #re add columns to transitions
colnames(transitions)[2:ncol(transitions)] <- seq(1, ncol(transitions) - 1) #Add a number column name
transitions[,-1] <- lapply(transitions[,-1], as.numeric) #Convert to numeric
transitions[, 2:ncol(transitions)] <- t(apply(transitions[, 2:ncol(transitions)], 1, fill_holes)) # Apply the function to each row, starting from the 2nd column

current_colnames_D <- colnames(abs_distances) #get colnames from abs distance
current_colnames_D[3] <- "individual_ID" #rename
current_colnames_D[1] <- "x"
current_colnames_D[2] <- "y"
colnames(abs_distances) <- current_colnames_D #re add columns to abs distance

#Row for each individual, for each second
long_format <- transitions %>%
  #filter(!individual_ID %in% c(1,2)) %>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "second",
               values_to = "state") %>%
  drop_na(state)

long_format$trial_id <- rep(1,nrow(long_format)) #assign trial ID

long_format$second <- as.numeric(long_format$second) #make numeric

long_format <- long_format %>%
  left_join(abs_distances %>% select(individual_ID, x, y), by = "individual_ID")

long_format$x <- NA
long_format$y <- NA

#add in state change and time in state columns
long_format <- long_format %>%
  group_by(individual_ID) %>%
  arrange(second) %>%
  mutate(state_change = case_when(
    is.na(state) ~ NA_real_,
    state == 1 & lag(state, default = state[1]) == 0 ~ 1,  # 0 -> 1 transition
    state == 0 & lag(state, default = state[1]) == 1 ~ -1, # 1 -> 0 transition
    TRUE ~ 0  # No change or other transitions
  ),
  time_in_state = ifelse(is.na(state), NA_real_, ave(second, cumsum(c(1, diff(state, na.rm = TRUE ) != 0)), FUN = seq_along) -1)
  )

long_format <- long_format %>%
  group_by(second) %>%
  arrange(individual_ID) %>%
  mutate(inst_emerged = sum(state == 1, na.rm = TRUE),
         total_on_screen = n_distinct(individual_ID))

#Add in meta
long_format <- long_format %>%
  mutate(
    colony = "S4",
    individual_ID = str_c(colony, "_", individual_ID),
    site = "Santa",
    time = format(strptime("12:00:00", "%H:%M:%S"), "%H:%M:%S"),
    date = as.Date("2023-07-19"),
    total_group_size = 20
  )

# Check for duplicate rows
duplicates <- duplicated(state_matrix)

# Print if there are any duplicates
if (any(duplicates)) {
  cat("There are identical rows in the data frame.\n")
  print(df[duplicates, ])
} else {
  cat("No identical rows found.\n")
}

#Time calculation
# Convert to time to POSIXct and seconds since midnight
long_format$date_time_POSIXct <-  as.POSIXct(paste(long_format$date, long_format$time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

long_format$seconds_since_midnight <- as.numeric(as.difftime(long_format$time, format = "%H:%M:%S", units = "secs")) + long_format$second

long_format$date <- as.Date(long_format$date)

#Recalculating inst emerged, inst hidden. total on screen
long_format <- long_format %>%
  # Group by trial and second
  group_by(trial_id, second) %>%
  mutate(
    # Count hidden individuals (state == 0)
    inst_hidden = sum(state == 0, na.rm = TRUE),
    
    # Calculate proportion of emerged individuals
    proportion_up = inst_emerged / total_on_screen,
    
    # Calculate proportion of hidden individuals
    proportion_down = inst_hidden / total_on_screen
  ) %>%
  ungroup() # Ungroup to return a flat structure

state_matrix$X <- NULL
class(state_matrix$trial_id)

state_matrix = rbind(state_matrix, long_format)

state_matrix$date <- as.Date(state_matrix$date)
state_matrix$trial_id <- as.integer(state_matrix$trial_id)
state_matrix$date_time_POSIXct <- as.POSIXct(state_matrix$date_time_POSIXct)

#EMERGENCE OVER TIME
state_matrix %>%
  mutate(colony = fct_reorder(factor(colony), total_group_size)) %>%
  ggplot(aes(x = seconds_since_midnight, y = proportion_up, group = trial_id, color = factor(colony))) +
  geom_smooth(size = 0.7) +
  labs(
    title = "Proportion Emerged (Facet By Colony)",
    x = "Time (Hours)",
    y = "Proportion of Individuals Emerged",
    color = "Trial"
  ) +
  scale_color_manual(
    values = c("L3" = "purple",
               "D1" = "red",
               "D3" = "#72C02E",
               "L2" = "#44C7ED",
               "L4" = "#FA70DA",
               "D4" = "#1EBB98",
               "D2" = "yellow",
               "S1" = "brown",
               "L1" = "orange",
               "S2" = "black",
               "S4" = "darkblue")
  )+
  theme_minimal() +
  scale_x_continuous(
    name = "Time (hours)",
    breaks = seq(0, 86400, by = 3600),
    labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%I %p")  # Format to time (e.g., 11 AM)
  ) +
  facet_wrap(~ colony) +
  ylim(0, 1) +
  theme(legend.position = "none",
        axis.text = element_text(size = 8))

#BAR CODE PLOT
state_matrix %>%
  filter(!trial_id %in% c(8, 18))%>%
  ggplot(aes(x = seconds_since_midnight, y = factor(individual_ID), fill = factor(state))) +
  geom_tile() +  # Optional: adds a border around tiles
  scale_fill_manual(values = c("1" = "white", "0" = "black")) +
  facet_wrap(~ trial_id, scales = "free") +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 4)  # Ensures consistent intervals (5 breaks here, adjust as needed)
  ) +# Ensures consistent x-scale across facets
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "none"
  ) +
  scale_x_continuous(
    name = "Time (hours)",
    breaks = seq(0, 86400, by = 3600),
    labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%I %p")  # Format to time (e.g., 11 AM)
  ) +
  labs(
    x = "Time (seconds)",
    y = "Individuals",
    fill = "State"
  )
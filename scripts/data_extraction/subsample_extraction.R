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

transitions <- read.csv("/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated/transitions_D4_05_06_25_complete.csv", header =FALSE)

current_colnames_T <- colnames(transitions) #get colnames
current_colnames_T[1] <- "individual_ID" #add individual_ID as colname
colnames(transitions) <- current_colnames_T #re add columns to transitions
colnames(transitions)[2:ncol(transitions)] <- seq(1, ncol(transitions) - 1) #Add a number column name
transitions[,-1] <- lapply(transitions[,-1], as.numeric) #Convert to numeric
transitions[, 2:ncol(transitions)] <- t(apply(transitions[, 2:ncol(transitions)], 1, fill_holes)) # Apply the function to each row, starting from the 2nd column

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

long_format_by_sec <- long_format %>%
  group_by(second) %>%
  summarise(hide = as.integer(any(state_change == -1)),
            .groups = "drop"
  )

set.seed(123)

hide_sample <- long_format_by_sec %>%
  filter(hide == 1) %>%
  slice_sample(prop = 0.1)

hide_sample <- hide_sample %>%
  mutate(
    segment = floor((second - 1) / 512) + 1,
    frame   = ((second - 1) %% 512) + 1
  )

start_file <- "GH049672.MP4"

start_code <- as.integer(sub("^GH(\\d{2}).*", "\\1", start_file))

hide_sample <- hide_sample %>%
  mutate(
    segment = floor((second - 1) / 512) + 1,
    frame   = ((second - 1) %% 512) + 1,
    
    video_code = start_code + (segment - 1),
    video_file = sprintf("frames_GH%02d9672", video_code)
  )

library(dplyr)

source_root <- "/Volumes/eel_diel_frames/garden_eel_diel-050625-D4-cam1"
dest_root   <- "/Volumes/GILLAB_AP/garden_eel_diel-050625-D4-cam1"

for (i in seq_len(nrow(hide_sample))) {
  
  row <- hide_sample[i, ]
  
  # Folder name (e.g. frames_GH070191)
  video_folder <- row$video_file
  
  # Frame file name (e.g. 00510.jpg)
  frame_file <- sprintf("%05d.png", row$frame)
  
  # Full source path
  src <- file.path(
    source_root,
    video_folder,
    frame_file
  )
  
  # Destination folder (named by original video)
  dest_dir <- file.path(
    dest_root,
    video_folder
  )
  
  # Create destination folder if needed
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Full destination path
  dest <- file.path(dest_dir, frame_file)
  
  # Copy if source exists
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
  } else {
    warning("Missing file: ", src)
  }
}

trial_name <- basename(source_root)

export_df <- hide_sample %>%
  arrange(video_code, frame) %>%
  transmute(
    trial_name = trial_name,
    second,
    segment,
    frame,
    video_code,
    video_file
  )


write.csv(
  export_df,
  file = file.path(dest_root, "hide_frames_index.csv"),
  row.names = FALSE
)






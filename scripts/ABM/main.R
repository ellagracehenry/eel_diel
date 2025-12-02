#Packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(MASS) 

#Functions
source("scripts/ABM/R/fill_holes.R")
source("scripts/ABM/R/isolate_perturbed_hides.R")

#Submodels
source("scripts/ABM/R/foraging_nonstatedependent.R")
source("scripts/ABM/R/geometric_submodel.R")
source("scripts/ABM/R/negativebinomial_submodel.R")

#Loading in empirical data
data <- read.csv(
  "~/Desktop/PhD/academic_projects/eel_diel/data/transitions/transitions_D2_31_05_25_complete.csv",
  header = FALSE
)

threshold = 5

current_colnames_T <- colnames(data) #get colnames
current_colnames_T[1] <- "individual_ID" #add individual_ID as colname
colnames(data) <- current_colnames_T #re add columns to transitions
colnames(data)[2:ncol(data)] <- seq(1, ncol(data) - 1) #Add a number column name
data[,-1] <- lapply(data[,-1], as.numeric) #Convert to numeric
data[, 2:ncol(data)] <- t(apply(data[, 2:ncol(data)], 1, function(row) fill_holes(row, threshold))) # Apply the function to each row, starting from the 2nd column

df_long <- melt(data, id.vars = "individual_ID")
df_long$variable <- as.numeric(df_long$variable)

df_run_length <- df_long %>%
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
  
df_run_length_hide <- df_run_length %>%
  filter(value == 0) #%>%
  #dplyr::pull(run_length)

df_run_length_emerge <- df_run_length %>%
  filter(value == 1) #%>%
  #dplyr::pull(run_length)

#non state dependent - parameterized by per cap proportion of time emerged 
results <- non_state_dependent_submodel(data, 29184, avg_prop_up)

results <- geometric_submodel(data)

results <- nbinomial_submodel(data)

results <- SIR_submodel(data)

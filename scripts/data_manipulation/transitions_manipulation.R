setwd("~/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated")

data <- read.csv("/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions/transitions_L1_28_05_25_complete.csv", header = FALSE)

data[c(1),] <- NA

data[c(22,25,24),27649:32769] <- NA

cut_20 <- data[c(22,25,24),5120:5632]

write.csv(data, "transitions_L1_28_05_25_complete_updated.csv")

added512 <- matrix(rep(rep(NA,nrow(data))),512)

matrix(1:nrow(data),1:512) <- NA

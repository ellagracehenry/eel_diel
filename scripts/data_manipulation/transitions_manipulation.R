setwd("~/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated")

data <- read.csv("/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated/transitions_D1_23_05_25_complete.csv", header = FALSE)

data[c(1),] <- NA

data[c(22,25,24),27649:32769] <- NA

data1 <- data[,2:9890]

write.csv(data1, "/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated/transitions_D1_23_05_25_complete.csv")

added512 <- matrix(rep(rep(NA,nrow(data))),512)

matrix(1:nrow(data),1:512) <- NA

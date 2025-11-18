# Social information evolution model
rm(list = ls())
#ptm <- proc.time()
require(ggplot2)
require(gridExtra)
library(truncnorm)

# # assumptions (parameters in function below):
# Ni = 500, # number of individuals
# tf = 30, #300 # time steps for a given generation
# e_gain = 1, # energy units gained each time step in which the ith individual does not flee
# coef_false = 0.20, # coefficient that determines the prob of a false alarm (a smaller value than f_pred)
# # we start with X individuals, whose evolvable traits of interest ('jumpiness', social faith, density dependence in social faith) are determined by random draws from Gaussain distributions
# # the jumpiness trait determines an individual's probability of: randomly fleeing (NOT due to a real threat, i.e., a false alarm) AND an individual's probability of detecting an attacking predator. As a first pass, let's simply make the false alarm probability much lower than the response to predators probability. Perhaps 10-20% of the response to predators probability:
# ######################### CAN CUT THIS (repeated in code below)
# coef_false = 0.20,
# f_pred = runif(Ni, min=0, max=1), # using uniform distribution to constrain sampling bewteen 0 and 1
# # the 'social faith' parm is an intercept term that represents the probability that a given individual will flee in a time step, given that another individual in the group fled. We can make the ordering of individuals random, so that we don't make it consistently less or more likely that certain individuals will flee
# s_faith = runif(Ni, min=0, max=0.5),
# s_dd = runif(Ni, min=-2, max=2),
# #########################
# fit = matrix(NA, ncol=tf,nrow=Ni),
# fit[ , 1] = 1, # setting fitness of all individuals at the start at 1
# maxf = 100 #100 # number of generations to run the model through

Ni = 100
tf = 30
e_gain = 1
coef_false = 0.20
maxf = 200
prob_pred = 0.2
max_group_size = 1

evo.fun1 <- function(Ni = 200, tf = 30, e_gain = 1, coef_false = 0.20, maxf = 200, prob_pred = 0.4, max_group_size = 25, distribution = 1, vari = 1) {
	
	f_pred = runif(Ni, min = 0, max = 1)
	s_faith = runif(Ni, min = 0, max = 0.5)
	s_dd = runif(Ni, min = -2, max = 2)
	fit = matrix(NA, ncol = tf, nrow = Ni)
	fit[ , 1] = 1
	lambda_groupsize = mean(max_group_size)
	
	# f_false = coef_false*f_pred
	flights <- list() # collecting all flight info for a generation
	attacks_all <- list()
	eaten_detect_all <- list()
	eaten_nodetect_all <- list()
	fit_traits_gen0 <- list() # list to store final fitness and traits of all individuals (alive and dead) in each generation
	fit_traits_gen <- list() # list to store final fitness and traits of SURVIVING individuals in each generation
	trait_mean <- matrix(NA, nrow = maxf, ncol = 3) # to store mean values of survivor traits for each generation
	trait_sd <- matrix(NA, nrow = maxf, ncol = 3) # to store sd of survivor traits for each generation
	f_all <- list() # this list stores the new set of traits for each generation (draw from parents in prev gen). This will end up with maxf-1 items (since there was not one for the first gen).
	flights_master <- list()
	eaten_detect_master <- list()
	eaten_nodetect_master <- list()
	
	for (f in 1:maxf){
		if (f == 1){ #first gen traits are randomly determined
			f_pred <- runif(Ni, min = 0, max = 1) # using uniform distribution to constrain sampling bewteen 0 and 1
			f_false <- coef_false*f_pred
			s_faith <- runif(Ni, min = 0, max = 0.5)
			s_dd <- runif(Ni, min = -2, max = 2)
		} else{ # from the 2nd gen onward, traits are inherited based on parent fitness
			f_pred <- f_all[[f]][ , 1]
			f_false <- coef_false*f_pred
			s_faith <- f_all[[f]][ , 2]
			s_dd <- f_all[[f]][ , 3]
		}
		
		# For each time step, the population gets reassembled into groups based on a uniform distribution of group sizes. Then, each group is potentially subjected to a predator attack (based on a background predation level, probability set to 0.2 by default below). 
				
		for (t in 1:(tf - 1)){ # running through all the time steps of a generation
		  if (distribution == 1){
		    group_sizes = seq(1:max_group_size)}
			else {
			  if (distribution == 2){
			  group_sizes = rtruncnorm(1000, a = 1, b = max_group_size, mean = max_group_size/2, sd = vari)
			  group_sizes = round(group_sizes, digits = 0)
		  }}
			groups <- list()
			j <- 1
			while (sum(unlist(groups)) < Ni){
				j <- j + 1
				groups[j] = sample(group_sizes, size = 1, replace = FALSE)
			}
			groups = unlist(groups)
			remainder = sum(groups) - Ni
			groups[length(groups)] <- groups[length(groups)] - remainder
			group_vec <- list()
			for (k in 1:length(groups)){
				vec <- rep(k, groups[k])
				group_vec[[k]] <- vec
			}	
			group_vec <- unlist(group_vec) # expanding group to list of individuals with group assignments
			seq0 <- sample(seq(1:Ni), size = Ni, replace = FALSE) # setting a random sequence of individuals that can each respond to the predator or to non threats (false alarm))
			groups_df <- data.frame(individual = seq0, groupID = group_vec)
			flights0 <- matrix(NA, nrow=length(unique(group_vec)), ncol = 4) # collecting flight info for each group in a time step
			eaten_detect0 <- matrix(NA, nrow=length(unique(group_vec)), ncol = 2)
			eaten_nodetect0 <- matrix(NA, nrow=length(unique(group_vec)), ncol = 2)
			attacks_vec <- rep(NA, length(unique(group_vec)))
			for (g in 1:length(unique(group_vec))){
				prev_flee <- 0 # setting the initial number of observable neighbor flights for a given group in a given time step
				subgroup <- subset(groups_df, groups_df$groupID == g)
				ddensity <- sum(as.numeric((fit[subgroup$individual, t] > 0))) # determining the density of LIVING individuals in the group
				pred <- rbinom(1, 1, prob_pred-1/(length(subgroup)*100)) #0.1 # determining if a predator attacks the group for the given time step
				attacks_vec[g] <- pred
				prev_detect <- 0 # setting the initial number of observable neighbor flights from a predator for a given group in a given time step in which a predator attacks
				eaten_detect_vec <- rep(0, nrow(subgroup))
				eaten_nodetect_vec <- rep(0, nrow(subgroup))
				for (i in 1:length(subgroup$individual)){
					ii <- subgroup$individual[i] # determining the order of prey decisions randomly for each time step
					#density <- length(subgroup$individual)
					eaten_detect <- 0; eaten_nodetect <- 0 # setting defaults/placeholders
					if (fit[ii, t] == 0){ # if prey were EATEN in a previous time step, their fitness remains at zero, otherwise their fitness is >0 (they start out with a fitness value of 1)
						fit[ii, t + 1] <- 0
					} else { # if focal was NOT eaten in a previous time step
						if (prev_flee > 0) { # if one or more neighbors has already fled in this time step
							p_flee_s <- prev_flee*s_faith[ii] + (ddensity - prev_flee)*s_dd[ii] # I think this density should be the density of REMAINING individuals
							if (p_flee_s > 1){
								p_flee_s <- 1
							}
							if (p_flee_s < 0){
								p_flee_s <- 0
							}
							flee0 <- rbinom(1, 1, f_false[ii]) + rbinom(1, 1, p_flee_s) # flight of focal depends on both random flight and flight of neighbors
							flee <- as.numeric(flee0 >= 1)
						}else { # if no neighbors have fled yet in this time step
							flee <- rbinom(1, 1, f_false[ii])
						}
						prev_flee <- prev_flee + flee # updating flights from false alarms
						if (flee == 1 | pred == 1){ # if focal flees OR the predator attacks, the focal does not gain energy (eat) in this time step
							fit[ii, t + 1] <- fit[ii, t] 
						}
						if (pred==1 & flee==0){ # if a predator attacks AND the focal did NOT flee on this time step...
							p_detect_s <- sum(prev_detect)*s_faith[ii] + (ddensity-prev_flee-prev_detect)*s_dd[ii]
							if (p_detect_s > 1) {
								p_detect_s <- 1
							}
							if (p_detect_s < 0) {
								p_detect_s <- 0
							}
							detect0 <- rbinom(1, 1, f_pred[ii]) + rbinom(1, 1, p_detect_s) # move to prob!?!!!!!!!!!!!!!
							detect <- as.numeric(detect0 >= 1)
							prev_detect <- prev_detect + detect # updating flights from predator
							if (detect == 1){ # if the focal detects the predator
								peaten_detect <- 1/((length(subgroup$individual) - prev_flee) + 10) # probability of life or death (the +10 is a bonus for early detection)
								eaten_detect <- rbinom(1, 1, peaten_detect)
								eaten_detect_vec[i] <- eaten_detect
							} else { # if the focal does not detect the predator
								peaten_nodetect <- 1/((length(subgroup$individual) - prev_flee)) # LOWER prob life/death (since pred was not detected)
								eaten_nodetect <- rbinom(1, 1, peaten_nodetect)
								eaten_nodetect_vec[i] <- eaten_nodetect
								# add up each eaten for each group and store in a list at each time point
							}
							if (eaten_detect == 1 | eaten_nodetect == 1){
								fit[ii, t + 1] <- 0 # if eaten, the focal's fitness goes to 0				
							}
						}
						if (pred == 0 & flee == 0){ # if predator does NOT attack AND focal does NOT flee (from a false alarm)
							fit[ii, t + 1] <- fit[ii, t] + e_gain
						}
					}
					#print(c(t,g,i))
				} # end of i loop
				# make a matrix that is col=var, row=# groups, and all this matrix to a list each time step
				flights0[g, ] <- c(t, ddensity, prev_flee, prev_detect) # recording all flights, from false alarms AND predators, for a given group and time step (groups change each time step)
				eaten_detect0[g, ] <- c(t, sum(eaten_detect_vec))
				eaten_nodetect0[g, ] <- c(t, sum(eaten_nodetect_vec))
			} # end of g loop
			attacks_all[[t]] <- attacks_vec
			eaten_detect_all[[t]] <- eaten_detect0
			eaten_nodetect_all[[t]] <- eaten_nodetect0
			flights[[t]] <- flights0
		} # end of t loop
		flights_all <- do.call(rbind, flights)
		flights_master[[f]] <- flights_all
		eaten_detect_mat <- do.call(rbind, eaten_detect_all)
		eaten_detect_master[[f]] <- eaten_detect_mat
		eaten_nodetect_mat <- do.call(rbind, eaten_nodetect_all)
		eaten_nodetect_master[[f]] <- eaten_nodetect_mat	
		
		survive0 <- cbind(fit[,tf], f_pred, s_faith, s_dd) # combining final fitness and traits for this generation
		fit_traits_gen0[[f]] <- survive0
		survive <- survive0[survive0[,1]>0,] # subsetting only the survivors (those not eaten by predators)
		fit_traits_gen[[f]] <- survive
		trait_mean[f,] <- c(mean(survive[,2]), mean(survive[,3]), mean(survive[,4]))
		trait_sd[f,] <- c(sd(survive[,2]), sd(survive[,3]), sd(survive[,4]))
		survive[,1] <- survive[,1]/sum(survive[,1]) # calculating proportional fitness (fitness of population sums to zero)
		surv_df <- data.frame(survive) 
		surv_df$index <- seq(1:length(surv_df$V1)) # creating a column of row numbers to sample from for the traits of next gen
		f_index <- sample(surv_df$index, Ni, prob=surv_df$V1, replace=TRUE) # sampling next gen traits weighted by parent fitness
		f_all[[f + 1]] <- survive[f_index,2:4] # traits of all Ni individuals for next gen	
	} # end of f loop

	# to select only survivors from the complete 'fit_traits_gen' list (which contains fitnesses for all individuals in every generation), I'd modify this line:
	#dev.new()
	#par(mfrow=c(1,3))
	traits <- c("jumpiness", "sociality", "density dependence in sociality")
	gg_trait_ls <- list()
	out_ls <- list()
	mean_ff <- round(mean(fit_traits_gen[[maxf]][,1]), 2) # mean final fitness for last generation
	sd_ff <- round(sd(fit_traits_gen[[maxf]][,1]), 2) # sd of final fitness for last generation
 for (p in 1:length(traits)){
		dff <- data.frame(generation = seq(1:nrow(trait_mean)), trait_mean = trait_mean[ , p], lb = trait_mean[ , p] - trait_sd[ , p], ub = trait_mean[ , p] + trait_sd[ , p])
		out_ls[[p]] <- dff
		gg_trait_ls[[p]] <- ggplot(dff, aes(generation)) + 
		geom_line(aes(y=trait_mean), colour="black") + 
		geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.2) +
		labs(y = traits[p])
	}
	#proc.time() - ptm
	#dev.new()
	grid.arrange(gg_trait_ls[[1]] + ggtitle(paste("Fitness=",mean_ff,"+/-",sd_ff,"SD")), gg_trait_ls[[2]], gg_trait_ls[[3]], ncol = 1) #PLOTS
	
	#multiplot(gg_trait_ls[[1]] + ggtitle(paste("Fitness=",mean_ff,"+/-",sd_ff,"SD")),gg_trait_ls[[2]],gg_trait_ls[[3]])
	
	return(out_ls)
}

test <- evo.fun1()
test

#Max group size
setwd("C:/Users/ellag/Desktop/ACADEMIC PROJECTS/SOCIALITY MODELLING")
sub1 <- read.csv("sub1.csv")
sub2 <- read.csv("sub2.csv")
sub3 <- read.csv("sub3.csv")

max_groups = c(1,5,10,15,20,25)
maxf = 200
repeats = 1:50
h=2
trait = 3
total = list()
sub3 <- matrix(NA, nrow = length(max_groups), ncol = 4, dimnames = list(c(1:length(max_groups)), c("max_groups","trait_mean","lb","ub"))) #change for each trait ! 1,2,3
sub0 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("max_groups","trait_mean","lb","ub")))

for (i in 1:length(max_groups)){
  if (h == 1){
    g <- evo.fun1(max_group_size = max_groups[i], maxf = 200)
    mg = rep(c(max_groups[i]), each= 200)
    
    trait = rep(c(1), each = 200)
    g[[1]] <- cbind(g[[1]],mg,trait)
    
    trait = rep(c(2), each = 200)
    g[[2]] <- cbind(g[[2]],mg,trait)
    
    trait = rep(c(3), each = 200)
    g[[3]] <- cbind(g[[3]],mg,trait)
    
    total <- rbind(total, g[[1]], g[[2]], g[[3]])

  }
  if (h == 2){
    
  for (ii in 1:length(repeats)){
    
g <- evo.fun1(max_group_size = max_groups[i], maxf = 200)

sub0[ii,1] <- max_groups[i]
sub0[ii,2] <- mean(g[[trait]][which(g[[trait]][ , "generation"] == maxf), "trait_mean"])
sub0[ii,3] <- mean(g[[trait]][which(g[[trait]][ , "generation"] == maxf), "lb"])
sub0[ii,4] <- mean(g[[trait]][which(g[[trait]][ , "generation"] == maxf), "ub"])
  }
  
sub0 <- as.data.frame(sub0)
sub3[i,1] <- max_groups[i]
sub3[i,2] <- mean(sub0$trait_mean)
sub3[i,3] <- mean(sub0$trait_mean) - sd(sub0$trait_mean)
sub3[i,4] <- mean(sub0$ub) + sd(sub0$trait_mean)
}
} 

#h = 1: plotting over generation steps
total$mg <- as.numeric(as.character((total$mg)))
total$trait <- as.factor(total$trait)

trait1_plot = total |>
  dplyr::filter(trait == 3) |>
ggplot(aes(generation, trait_mean, colour = mg, group = mg))+
    geom_line()+
  geom_point()+
  xlab("Generation") +
  ylab("Trait value") + 
  theme_bw() 
trait1_plot

#plotting final values
cbPalette <- c("#D55E00", "#009E73", "#56B4E9")
sub1 <- as.data.frame(sub1)
sub2 <- as.data.frame(sub2)
sub3 <- as.data.frame(sub3)

trait <- c(1,1,1,1,1,1)
sub1 <- cbind(sub1, trait)
trait <- c(2,2,2,2,2,2)
sub2 <- cbind(sub2, trait)
trait <- c(3,3,3,3,3,3)
sub3 <- cbind(sub3, trait)

write.csv(sub1, "sub1mgs.csv")
write.csv(sub2, "sub2mgs.csv")
write.csv(sub3, "sub3mgs.csv")

sub <- rbind(sub1, sub2)
sub <- rbind(sub, sub3)

sub$trait <- as.factor(sub$trait)

levels(sub$trait)<- list("Jumpiness"="1",
                             "Sociality"="2",
                             "Density dependence in sociality"="3")

ggplot(sub)+
geom_point(data = sub, aes(max_groups, trait_mean, colour = trait), shape = 16, size=2)+
  geom_ribbon(data = sub, aes(x=max_groups,
                             ymin=lb,
                             ymax=ub, 
                             fill = trait,
                             colour = trait),
              alpha = 0.4)+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  xlab("Maximum group size") +
  ylab("Trait value") + 
  theme_bw() 
ggsave("mgs.png", width = 7, height = 5, dpi = 600, units = "in")

##Pred pressure
pred = c(0.2, 0.4, 0.6, 0.8, 1)
maxf = 200
repeats = 1:50
h=2
trait = 2
total = list()
sub1 <- matrix(NA, nrow = length(pred), ncol = 4, dimnames = list(c(1:5), c("pred","trait_mean","lb","ub"))) #change for each trait ! 1,2,3
sub2 <- matrix(NA, nrow = length(pred), ncol = 4, dimnames = list(c(1:5), c("pred","trait_mean","lb","ub"))) #change for each trait ! 1,2,3
sub3 <- matrix(NA, nrow = length(pred), ncol = 4, dimnames = list(c(1:5), c("pred","trait_mean","lb","ub"))) #change for each trait ! 1,2,3
sub01 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("pred","trait_mean","lb","ub")))
sub02 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("pred","trait_mean","lb","ub")))
sub03 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("pred","trait_mean","lb","ub")))

for (i in 1:length(pred)){
  if (h == 1){
    g <- evo.fun1(prob_pred = pred[i], maxf = 100)
    pp = rep(c(pred[i]), each= 100)
    
    trait = rep(c(1), each = 100)
    g[[1]] <- cbind(g[[1]],pp,trait)
    
    trait = rep(c(2), each = 100)
    g[[2]] <- cbind(g[[2]],pp,trait)
    
    trait = rep(c(3), each = 100)
    g[[3]] <- cbind(g[[3]],pp,trait)
    
    total <- rbind(total, g[[1]], g[[2]], g[[3]])
    
  }
  if (h == 2){
    
    for (ii in 1:length(repeats)){
      
      g <- evo.fun1(prob_pred = pred[i], maxf = 200, max_group_size = 25, distribution = 1)
      
      sub01[ii,1] <- pred[i]
      sub01[ii,2] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "trait_mean"])
      sub01[ii,3] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "lb"])
      sub01[ii,4] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "ub"])
      
      sub02[ii,1] <- pred[i]
      sub02[ii,2] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "trait_mean"])
      sub02[ii,3] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "lb"])
      sub02[ii,4] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "ub"])
      
      sub03[ii,1] <- pred[i]
      sub03[ii,2] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "trait_mean"])
      sub03[ii,3] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "lb"])
      sub03[ii,4] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "ub"])
    }
    
    sub01 <- as.data.frame(sub01)
    sub02 <- as.data.frame(sub02)
    sub03 <- as.data.frame(sub03)
    
    sub1[i,1] <- pred[i]
    sub1[i,2] <- mean(sub01$trait_mean)
    sub1[i,3] <- mean(sub01$trait_mean) - sd(sub01$trait_mean)
    sub1[i,4] <- mean(sub01$ub) + sd(sub01$trait_mean)
    
    sub2[i,1] <- pred[i]
    sub2[i,2] <- mean(sub02$trait_mean)
    sub2[i,3] <- mean(sub02$trait_mean) - sd(sub02$trait_mean)
    sub2[i,4] <- mean(sub02$ub) + sd(sub02$trait_mean)
    
    sub3[i,1] <- pred[i]
    sub3[i,2] <- mean(sub03$trait_mean)
    sub3[i,3] <- mean(sub03$trait_mean) - sd(sub03$trait_mean)
    sub3[i,4] <- mean(sub03$ub) + sd(sub03$trait_mean)
  }
}

#h=1 plotting across generations
total$pp <- as.numeric(as.character((total$pp)))
total$trait <- as.factor(total$trait)

write.csv(total, "total_pred.csv")

trait1_plot = total |>
  dplyr::filter(trait == 1) |>
  ggplot(aes(generation, trait_mean, colour = pp, group = pp))+
  geom_line()+
  geom_point()+
  xlab("Generation") +
  ylab("Trait value") + 
  theme_bw() 
trait1_plot


#h=2 plotting final values
cbPalette <- c("#D55E00", "#009E73", "#56B4E9")
sub1pred <- as.data.frame(sub1)
write.csv(sub1pred, "sub1pred.csv")
sub2pred <- as.data.frame(sub2)
write.csv(sub2pred, "sub2pred.csv")
sub3pred <- as.data.frame(sub3)
write.csv(sub3pred, "sub3pred.csv")

sub1pred <- na.omit(sub1pred)
sub2pred <- na.omit(sub2pred)
sub3pred <- na.omit(sub3pred)

trait <- c(1,1,1,1,1)
sub1pred <- cbind(sub1pred, trait)
trait <- c(2,2,2,2,2)
sub2pred <- cbind(sub2pred, trait)
trait <- c(3,3,3,3,3)
sub3pred <- cbind(sub3pred, trait)

subpred <- rbind(sub1pred, sub2pred)
subpred <- rbind(subpred, sub3pred)

write.csv(subpred, "subpred.csv")

subpred$trait <- as.factor(subpred$trait)

levels(subpred$trait)<- list("Jumpiness"="1",
                         "Social faithfulness"="2",
                         "Density dependence"="3")

ggplot(subpred)+
  geom_point(data = subpred, aes(pred, trait_mean, colour = trait), shape = 16, size=2)+
  geom_ribbon(data = subpred, aes(x=pred,
                              ymin=lb,
                              ymax=ub, 
                              fill = trait,
                              colour = trait),
              alpha = 0.4)+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  xlab("Predation pressure") +
  ylab("Trait value") + 
  theme_bw()
ggsave("predpress.png", width = 7, height = 5, dpi = 600, units = "in")


#DISTRIBUTIONS
distributions = 1
h=2
varia = c(0,2,4,6)
maxf = 200
repeats = 1:50
trait = 3
total = list()
total1 = list()
total2 = list()
sub1 <- matrix(NA, nrow = 1, ncol = 4, dimnames = list(c(1), c("dist","trait_mean","lb","ub"))) #change for each trait ! 1,2,3
sub2 <- matrix(NA, nrow = 1, ncol = 4, dimnames = list(c(1), c("dist","trait_mean","lb","ub"))) #change for each trait ! 1,2,3
sub3 <- matrix(NA, nrow = 1, ncol = 4, dimnames = list(c(1), c("dist","trait_mean","lb","ub"))) #change for each trait ! 1,2,3
sub011<- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("dist","trait_mean","lb","ub")))
sub022 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("dist","trait_mean","lb","ub")))
sub033 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("dist","trait_mean","lb","ub")))
sub01<- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("dist","trait_mean","lb","ub")))
sub02 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("dist","trait_mean","lb","ub")))
sub03 <- matrix(NA, nrow = length(repeats), ncol = 4, dimnames = list(c(1:50), c("dist","trait_mean","lb","ub")))


  if (h == 1){
    if (distributions == 1){
      g <- evo.fun1(distribution = distributions, maxf = 200)
      
      dist = rep(c("uniform"), each= 100)
      
      trait = rep(c(1), each = 100)
      g[[1]] <- cbind(g[[1]],dist,trait)
      
      trait = rep(c(2), each = 100)
      g[[2]] <- cbind(g[[2]],dist,trait)
      
      trait = rep(c(3), each = 100)
      g[[3]] <- cbind(g[[3]],dist,trait)
      
      total1 <- rbind(total1, g[[1]], g[[2]], g[[3]])
      
      
    }
    else if (distributions == 2){
      for (i in 1:length(varia)){
      g <- evo.fun1(vari = varia[i], maxf = 200)
      
      dist = rep(c(varia[i]), each= 100)
      
      trait = rep(c(1), each = 100)
      g[[1]] <- cbind(g[[1]],dist,trait)
      
      trait = rep(c(2), each = 100)
      g[[2]] <- cbind(g[[2]],dist,trait)
      
      trait = rep(c(3), each = 100)
      g[[3]] <- cbind(g[[3]],dist,trait)
      
      total2 <- rbind(total2, g[[1]], g[[2]], g[[3]])
      
    }

  }
  if (h == 2){
  
      if (distributions == 1){
        for (ii in 1:length(repeats)){
        g <- evo.fun1(distribution = distributions, maxf = 200)
        
        sub01[ii,1] <- "uniform"
        sub01[ii,2] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "trait_mean"])
        sub01[ii,3] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "lb"])
        sub01[ii,4] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "ub"])
      
        sub02[ii,1] <- "uniform"
        sub02[ii,2] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "trait_mean"])
        sub02[ii,3] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "lb"])
        sub02[ii,4] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "ub"])
        
        sub03[ii,1] <- "uniform"
        sub03[ii,2] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "trait_mean"])
        sub03[ii,3] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "lb"])
        sub03[ii,4] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "ub"])
        }}
      if (distributions == 2){
        
      for (i in 1:length(varia)){
        for (ii in 1:length(repeats)){
      g <- evo.fun1(vari = varia[i], maxf = 200, distribution = distributions)
      
      sub011[ii,1] <- varia[i]
      sub011[ii,2] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "trait_mean"])
      sub011[ii,3] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "lb"])
      sub011[ii,4] <- mean(g[[1]][which(g[[1]][ , "generation"] == maxf), "ub"])
      
      sub022[ii,1] <- varia[i]
      sub022[ii,2] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "trait_mean"])
      sub022[ii,3] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "lb"])
      sub022[ii,4] <- mean(g[[2]][which(g[[2]][ , "generation"] == maxf), "ub"])
      
      sub033[ii,1] <- varia[i]
      sub033[ii,2] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "trait_mean"])
      sub033[ii,3] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "lb"])
      sub033[ii,4] <- mean(g[[3]][which(g[[3]][ , "generation"] == maxf), "ub"])
      }
    
    sub01 <- as.data.frame(sub01)
    sub01$trait_mean <- as.numeric(sub01$trait_mean)
    sub01$ub <- as.numeric(sub01$ub)
    sub01$lb <- as.numeric(sub01$lb)
    sub1[i,1] <- "uniform"
    sub1[i,2] <- mean(sub01$trait_mean)
    sub1[i,3] <- mean(sub01$trait_mean) - sd(sub01$trait_mean)
    sub1[i,4] <- mean(sub01$ub) + sd(sub01$trait_mean)
    
    sub02 <- as.data.frame(sub02)
    sub02$trait_mean <- as.numeric(sub02$trait_mean)
    sub02$ub <- as.numeric(sub02$ub)
    sub02$lb <- as.numeric(sub02$lb)
    sub2[i,1] <- "uniform"
    sub2[i,2] <- mean(sub02$trait_mean)
    sub2[i,3] <- mean(sub02$trait_mean) - sd(sub02$trait_mean)
    sub2[i,4] <- mean(sub02$ub) + sd(sub02$trait_mean)
    
    sub03 <- as.data.frame(sub03)
    sub03$trait_mean <- as.numeric(sub03$trait_mean)
    sub03$ub <- as.numeric(sub03$ub)
    sub03$lb <- as.numeric(sub03$lb)
    sub3[i,1] <- "uniform"
    sub3[i,2] <- mean(sub03$trait_mean)
    sub3[i,3] <- mean(sub03$trait_mean) - sd(sub03$trait_mean)
    sub3[i,4] <- mean(sub03$ub) + sd(sub03$trait_mean)
  }
      }}
  }

#h=1 plotting across generations
total2$dist <- as.character(total2$dist)
total$dist <- as.factor(total$dist)
total <- rbind(total1, total2)
total$trait <- as.factor(total$trait)

levels(total$dist) <- list(
                         "Sd = 0"="0",
                         "Sd = 1"="1",
                         "Sd = 2"="2",
                         "Sd = 3"="3",
                         "Uniform"="uniform")

write.csv(total, "total_distrib.csv")

trait1_plot = total |>
  dplyr::filter(trait == 3) |>
  ggplot(aes(generation, trait_mean, colour = dist, group = dist))+
  geom_line()+
  geom_point()+
  xlab("Generation") +
  ylab("Trait value") + 
  theme_bw() 
trait1_plot


#h=2 plotting final values
sub1uni <- as.data.frame(sub1)
write.csv(sub1uni, "sub1uni.csv")
sub2uni <- as.data.frame(sub2)
write.csv(sub2uni, "sub2uni.csv")
sub3uni <- as.data.frame(sub3)
write.csv(sub3uni, "sub3uni.csv")



trait <- c(1)
sub1uni <- cbind(sub1uni, trait)
trait <- c(2)
sub2uni <- cbind(sub2uni, trait)
trait <- c(3)
sub3uni <- cbind(sub3uni, trait)

sub1dist <- as.data.frame(sub1)
sub2dist <- as.data.frame(sub2)
sub3dist <- as.data.frame(sub3)
trait <- c(1,1,1,1)
sub1dist <- cbind(sub1dist, trait)
trait <- c(2,2,2,2)
sub2dist <- cbind(sub2dist, trait)
trait <- c(3,3,3,3)
sub3dist <- cbind(sub3dist, trait)
write.csv(sub1dist, "sub1dist.csv")
write.csv(sub2dist, "sub2dist.csv")
write.csv(sub3dist, "sub3dist.csv")

subnormdist <- rbind(sub1uni, sub2uni, sub3uni, sub1dist, sub2dist, sub3dist)

write.csv(subnormdist, "subnorm.csv")
subnormdist$dist <- as.numeric(as.character(subnormdist$dist))
subnormdist$trait <- as.factor(subnormdist$trait)
subnormdist$trait_mean <- as.numeric(subnormdist$trait_mean)
subnormdist$ub <- as.numeric(subnormdist$ub)
subnormdist$lb <- as.numeric(subnormdist$lb)

levels(subnormdist$trait)<- list("Jumpiness"="1",
                             "Social faithfulness"="2",
                             "Density dependence"="3")
levels(subnormdist$dist) <- list(
  "0"="0",
  "2"="2",
  "4"="4",
  "6"="6",
  "8"="uniform")

cbPalette <- c("#D55E00", "#009E73", "#56B4E9")
ggplot(subnormdist)+
  geom_point(data = subnormdist, aes(dist, trait_mean, colour = trait), shape = 16, size=2)+
  geom_ribbon(data = subnormdist, aes(x=dist,
                                  ymin=lb,
                                  ymax=ub, 
                                  fill = trait,
                                  colour = trait),
              alpha = 0.4)+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  xlab("Distribution") +
  ylab("Trait value") + 
  theme_bw()
ggsave("groupdist.png", width = 7, height = 5, dpi = 600, units = "in")






# 
# 
# ptm <- proc.time()
# 
# lopred1 <- evo.fun1(prob_pred = 0.2, maxf = 1000)
# lopred2 <- evo.fun1(prob_pred = 0.2, maxf = 1000)
# lopred3 <- evo.fun1(prob_pred = 0.2, maxf = 1000)
# lopred4 <- evo.fun1(prob_pred = 0.2, maxf = 1000)
# lopred5 <- evo.fun1(prob_pred = 0.2, maxf = 1000)
# 
# mdpred1 <- evo.fun1(prob_pred = 0.4, maxf = 1000)
# mdpred2 <- evo.fun1(prob_pred = 0.4, maxf = 1000)
# mdpred3 <- evo.fun1(prob_pred = 0.4, maxf = 1000)
# mdpred4 <- evo.fun1(prob_pred = 0.4, maxf = 1000)
# mdpred5 <- evo.fun1(prob_pred = 0.4, maxf = 1000)
# 
# proc.time() - ptm
# 
# traits <- c("jumpiness", "sociality", "density dependence in sociality")
# gg_trait_ls <- list()
# out_ls <- list()
# mean_ff <- round(mean(lopred1[[maxf]][,1]), 2) # mean final fitness for last generation
# sd_ff <- round(sd(lopred1[[maxf]][,1]), 2) # sd of final fitness for last generation
# for (p in 1:length(traits)){
#   dff <- data.frame(generation = seq(1:nrow(trait_mean)), trait_mean = trait_mean[ , p], lb = trait_mean[ , p] - trait_sd[ , p], ub = trait_mean[ , p] + trait_sd[ , p])
#   out_ls[[p]] <- dff
#   gg_trait_ls[[p]] <- ggplot(dff, aes(generation)) + 
#     geom_line(aes(y=trait_mean), colour="black") + 
#     geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.2) +
#     labs(y = traits[p])
# }
# #proc.time() - ptm
# dev.new()
# grid.arrange(gg_trait_ls[[1]] + ggtitle(paste("Fitness=",mean_ff,"+/-",sd_ff,"SD")), gg_trait_ls[[2]], gg_trait_ls[[3]], ncol = 1) #PLOTS
# 
# gg_trait_ls

# if (trait == 1){
#   ymean <- jumpiness_mean
#   ylb = jumpiness_lb
#   yup = jumpiness_ub
# }
# 
# if (trait == 2){
#   ymean = sociality_mean
#   ylb = sociality_lb
#   yup = sociality_ub
# }
# 
# if (trait == 3){
#   ymean = socdensdep_mean
#   ylb = socdensdep_lb
#   yup = socdensdep_ub
# }

# if (paramsweep == 1){
#   x = max_group_size
#   param = max_groups
# }
# if (paramsweep == 2){
#   x = distribution
#   param = distrib
# }
# if (paramsweep == 3){
#   x = prob_pred
#   param = pred
# }

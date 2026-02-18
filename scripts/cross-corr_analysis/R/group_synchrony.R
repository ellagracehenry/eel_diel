#for each individual, the probability of resting as the proportion of time spent resting

##group-level synchrony: null distribution of n individuals resting
#At each time point, count how many individuals are resting
#Null distribution assuming independence 
#Observed to null
#the probability of unique combo of resting and active by multiplying the prob of resting for the resting individuals with the probabilites of being active 1-pR for the active individuals
#null probability of n individuals resting by summing all the probabilities of having a group combination where n individuals are resting


#Pairwise synchrony
#How many times observed together
#Null expectation is probability both are resting plus probability both are active
#Dividing observed by expected

group_synchrony <- function(transitions_path, metadata_path, threshold){
  
  
  #load in anything ending with csv
  files <- list.files(
    path = transitions_path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  metadata <- read_excel(metadata_path, sheet = "identities")
  
  out_list <- vector("list", length(files))
  
  for (i in seq_along(files)) {
    f <- files[i]
    temp <- read.csv(f, header = TRUE, na.string = c("NaN","NA")) #read data
    
    if (temp[1,1]>1){
      temp <- read.csv(f, header = FALSE, na.string = c("NaN","NA")) #read data
    }
    if (!is.na(temp[2,2]) && temp[2,2] > 1){
      temp[,2] <- NULL
    }
    
    current_colnames_T <- colnames(temp) #get colnames
    current_colnames_T[1] <- "individual_ID" #add individual_ID as colname
    colnames(temp) <- current_colnames_T #re add columns to transitions
    #temp$individual_ID <- as.character(temp$individual_ID)
    colnames(temp)[2:ncol(temp)] <- seq(1, ncol(temp) - 1) #Add a number column name
    temp[,-1] <- lapply(temp[,-1], as.numeric) #Convert to numeric
    temp[, 2:ncol(temp)] <- t(apply(
      temp[, 2:ncol(temp)], 1, 
      function(row) {
        if (all(is.na(row))) {
          return(rep(NaN, length(row)))
        }
        fill_holes(row, threshold) # Apply the function to each row, starting from the 2nd column
      }
    ))
    
    temp_long <- reshape2::melt(temp, id.vars = "individual_ID") #convert to long
    temp_long$variable <- as.factor(temp_long$variable) #time as a factor
    n_ind <- length(unique(temp_long$individual_ID)) #count up how many individuals
    
    ## STEP 1: PROPORTION EMERGED
    temp_long_s <- temp_long %>% #summarise for p_emerged
      group_by(individual_ID) %>%
      reframe(prop_time_emerged = mean(value, na.rm=TRUE)) %>%
      filter(prop_time_emerged > 0)
    
    # STEP 2: OBSERVED GROUP LEVEL SYNCHRONY
    n_emerge_obs <- colSums(temp[,-1], na.rm=TRUE) #count up how many observed in each second
    max_obs <- max(n_emerge_obs) #calculate max number emerged
    obs_dist <- table(n_emerge_obs)/length(n_emerge_obs) #proportion of time spent with each number of individuals emerged
    obs_count <- table(n_emerge_obs)
    
    # STEP 3: GROUP LEVEL NULL DISTRIBUTION ASSUMING INDEPENDENCE
    p_emerged <- temp_long_s$prop_time_emerged #proportion of time each individual is emerged
    p_emerged <- na.omit(p_emerged) #remove NAs
    N <- length(p_emerged) #how many individuals there are 

    #Probability of 0 individuals to N individuals emerged, all combinations
    null_dist <- dpoibin(0:N, p_emerged) #calculates the probability of obtaining a specific number of k successes in n independent non-indentical bernoulli trials, each with its own probability of success
    names(null_dist) <- 0:N #add column names 
    
    # STEP 4: full comparison table
    #initially add zeros for all
    comparison <- data.frame(
      n_emerged = 0:N,
      observed_prop = 0,
      null_prob = null_dist,
      observed_count = 0,
      null_count = null_dist*length(n_emerge_obs)
    )
    
    
    # fill observed where it exists
    comparison$observed_prop[match(
      as.numeric(names(obs_dist)),
      comparison$n_emerged
    )] <- as.numeric(obs_dist)
    
    comparison$observed_count[match(as.numeric(names(obs_count)), comparison$n_emerged)] <- as.numeric(obs_count)
    
    
    #Add in meta
    fname <- basename(f) #file name
    colony <- sub("^transitions_([^_]+)_.*$", "\\1", fname) #extract colony
    date <- as.Date(sub("^transitions_[^_]+_([0-9_]+)_.*$", "\\1", fname), format = "%d_%m_%y") #extract date
    site <- sub(".*_([DFL])[0-9]+_.*", "\\1", fname) #extract site
    
    comparison$colony <- colony # add colony
    comparison$date <- date # add date
    comparison$trial_ID <- i
    comparison$site <- site
    
  #assign to list  
  out_list[[i]] <- comparison

  }
  
  # combine everything
  group_level_synchrony <- dplyr::bind_rows(out_list)
  return(group_level_synchrony)

  
}

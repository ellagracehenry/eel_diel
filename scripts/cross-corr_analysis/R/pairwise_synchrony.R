pairwise_synchrony <- 
  function(transitions_path, metadata_path, threshold){
  
  
  #load in anything ending with csv
  files <- list.files(
    path = transitions_path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  metadata <- read_excel(metadata_path, sheet = "identities")
  
  out_list <- vector("list", length(files))
  
  for (i in seq_along(files)) {
    f <- files[1]
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
    
    temp <- temp %>%
      filter(if_all(everything(), ~!is.na(.)) & 
            if_any(-1, ~ . != 0)) 
    
    temp_long <- reshape2::melt(temp, id.vars = "individual_ID") #convert to long
    temp_long$variable <- as.factor(temp_long$variable) #time as a factor

    ## STEP 1: PROPORTION EMERGED
    temp_long_s <- temp_long %>% #summarise for p_emerged
      group_by(individual_ID) %>%
      reframe(prop_time_emerged = mean(value, na.rm=TRUE), prop_time_hidden = 1-prop_time_emerged) %>%
      filter(prop_time_emerged > 0)
    
    n_ind <- length(unique(temp_long_s$individual_ID)) #count up how many individuals
    
    temp <- temp %>%
      filter(individual_ID %in% valid_ids)
    
    ## STEP 2: NULL EXPECTATION PER PAIR
    p_prop_time_emerged <- outer(temp_long_s$prop_time_emerged, temp_long_s$prop_time_emerged, FUN = "*")
    diag(p_prop_time_emerged) <- NA
    
    p_prop_time_hidden <- outer(temp_long_s$prop_time_hidden, temp_long_s$prop_time_hidden, FUN = "*")
    diag(p_prop_time_hidden) <- NA

    pairs <- combn(n_ind, 2)
    
    results <- numeric(ncol(pairs))
    
    pairs <- rbind(pairs, rep(0, ncol(pairs)))
    
    for (k in 1:ncol(pairs)) {
      ii <- pairs[1,k]
      jj <- pairs[2,k]
      
      p_emerge_ij <- p_prop_time_emerged[ii,jj]
      
      p_hide_ij <- p_prop_time_hidden[ii,jj]
      
      null_ij <- p_emerge_ij # CAN ALSO BE + p_emerge_ij. Decide whether we want this to be from both or not
      
      pairs[3,k] <- null_ij
      
    }
  
    ## STEP 3: OBSERVED SYNCHRONY PER PAIR
    X <- as.matrix(temp[,-1])
    storage.mode(X) <- "numeric"
    T <- ncol(X)
    co_emerge_matrix <- (X %*% t(X)) / T
    
    # observed / null synchrony per pair
    obs_sync <- mapply(function(i, j, k) {
      if (pairs[3,k] <= 0) return(NA_real_)
      co_emerge_matrix[i, j] / pairs[3,k]
    }, pairs[1, ], pairs[2, ], seq_len(ncol(pairs)))
    
    # add to pairs matrix
    pairs <- rbind(pairs, obs_sync) 
    
    pairwise_sync_long <- data.frame(id_i = pairs[1,], id_j = pairs[2,], synchrony = pairs[4,])

    #Add in meta
    fname <- basename(f) #file name
    colony <- sub("^transitions_([^_]+)_.*$", "\\1", fname) #extract colony
    date <- as.Date(sub("^transitions_[^_]+_([0-9_]+)_.*$", "\\1", fname), format = "%d_%m_%y") #extract date
    site <- sub(".*_([DFL])[0-9]+_.*", "\\1", fname) #extract site
    
    pairwise_sync_long$colony <- colony # add colony
    pairwise_sync_long$date <- date # add date
    pairwise_sync_long$trial_ID <- i
    pairwise_sync_long$site <- site
    
    #assign to list  
    out_list[[i]] <-pairwise_sync_long
    
  }
  
  # combine everything
  pairwsie_synchrony <- dplyr::bind_rows(out_list)
  return(pairwsie_synchrony)
  
  
}

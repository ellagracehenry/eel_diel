combine_data_by_individual <- function(transitions_path, metadata_path, threshold){
  
  #load in anything ending with csv
  files <- list.files(
    path = transitions_path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  metadata <- read_excel(metadata_path)
  
  
  
  out_list <- vector("list", length(files))
  
  for (i in seq_along(files)) {
    f <- files[i]
    temp <- read.csv(f, header = FALSE) #read data
    current_colnames_T <- colnames(temp) #get colnames
    current_colnames_T[1] <- "individual_ID" #add individual_ID as colname
    colnames(temp) <- current_colnames_T #re add columns to transitions
    colnames(temp)[2:ncol(temp)] <- seq(1, ncol(temp) - 1) #Add a number column name
    temp[,-1] <- lapply(temp[,-1], as.numeric) #Convert to numeric
    temp[, 2:ncol(temp)] <- t(apply(temp[, 2:ncol(temp)], 1, function(row) fill_holes(row, threshold))) # Apply the function to each row, starting from the 2nd column
    
    temp_long <- melt(temp, id.vars = "individual_ID") #convert to long
    temp_long$variable <- as.factor(temp_long$variable) #time as a factor
    n_ind <- length(unique(temp_long$individual_ID)) #count up how many individuals
    
    temp_long_s <- temp_long %>% #summarise for p_emerged
      group_by(individual_ID) %>%
      reframe(prop_time_emerged = mean(value))
    
    fname <- basename(f) #file name
    colony <- sub("^transitions_([^_]+)_.*$", "\\1", fname) #extract colony
    date <- as.Date(sub("^transitions_[^_]+_([0-9_]+)_.*$", "\\1", fname), format = "%d_%m_%y") #extract date
    site <- sub(".*_([DFL])[0-9]+_.*", "\\1", fname) #extract site
    
    temp_long_s$colony <- colony # add colony
    temp_long_s$date <- date # add date
    temp_long_s$trial_ID <- i
    temp_long_s$site <- site
    
    out_list[[i]] <- temp_long_s # store result
    
    
  }
  
  # combine everything
  combined_data <- dplyr::bind_rows(out_list)
  
  return(combined_data)
  
}

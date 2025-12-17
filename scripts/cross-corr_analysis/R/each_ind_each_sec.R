each_ind_each_sec <- function(transitions_path, metadata_path, threshold){
  
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
    
    
    fname <- basename(f) #file name
    colony <- sub("^transitions_([^_]+)_.*$", "\\1", fname) #extract colony
    date <- as.Date(sub("^transitions_[^_]+_([0-9_]+)_.*$", "\\1", fname), format = "%d_%m_%y") #extract date
    site <- sub(".*_([DFL])[0-9]+_.*", "\\1", fname) #extract site
    
    temp_long$colony <- colony # add colony
    temp_long$date <- date # add date
    temp_long$trial_ID <- i
    temp_long$site <- site
    
    temp_long <- temp_long %>%
      group_by(variable) %>%
      mutate(n_emerged = sum(value))
    
    
    idx <- match(fname, metadata$trial_file_name)
    if (is.na(idx)) {
      stop("Filename not found in metadata: ", fname)
    }
    
    time_begin <- metadata$time_extraction_begin[idx]
    
    # FORCE POSIXct just to be safe
    time_begin <- as.POSIXct(time_begin, tz = "UTC")
    
    # seconds since midnight at trial start
    start_sec <- as.numeric(time_begin) %% 86400
    
    # convert variable safely to numeric seconds
    t_sec <- as.numeric(as.character(temp_long$variable))
    
    # final seconds since midnight (handle midnight wrap)
    temp_long$sec_since_midnight <- (start_sec + t_sec) %% 86400
    
    temp_long <- temp_long %>%
      group_by(individual_ID) %>%
      arrange(sec_since_midnight) %>%
      mutate(
        next_state = lead(value),
        
        # directional transitions
        emerge = if_else(value == 0 & next_state == 1, 1, 0),
        hide   = if_else(value == 1 & next_state == 0, 1, 0),
        
        stay_hidden  = if_else(value == 0 & next_state == 0, 1, 0),
        stay_emerged = if_else(value == 1 & next_state == 1, 1, 0)
      ) 
    
    
    out_list[[i]] <- temp_long # store result
    
    
  }
  
  # combine everything
  combined_data <- dplyr::bind_rows(out_list)
  
  return(combined_data)
  
}

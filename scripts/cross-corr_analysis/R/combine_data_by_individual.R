combine_data_by_individual <- function(transitions_path, metadata_path, threshold){
  
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
      return(rep(NA, length(row)))
    }
      fill_holes_all_segments(row, threshold) # Apply the function to each row, starting from the 2nd column
    }
    ))
    
    time_cols <- colnames(temp)[-1]
    all_na_cols <- time_cols[colSums(!is.na(temp[,time_cols])) == 0]
    cols_to_check <- setdiff(time_cols, all_na_cols)
    temp <- temp[
      rowSums(is.na(temp[, cols_to_check])) == 0,
      colnames(temp)
    ]

    
    temp_long <- reshape2::melt(temp, id.vars = "individual_ID") #convert to long
    temp_long$variable <- as.numeric(temp_long$variable) #time as a factor
    
    temp_long_s <- temp_long %>% #summarise for p_emerged
      group_by(individual_ID) %>%
      reframe(prop_time_emerged = mean(value, na.rm=TRUE), emerged_s = sum(value), total_s = max(variable))
    
    fname <- basename(f) #file name
    colony <- sub("^transitions_([^_]+)_.*$", "\\1", fname) #extract colony
    date <- as.Date(sub("^transitions_[^_]+_([0-9_]+)_.*$", "\\1", fname), format = "%d_%m_%y") #extract date
    site <- sub(".*_([DFL])[0-9]+_.*", "\\1", fname) #extract site
    
    temp_long_s$colony <- colony # add colony
    temp_long_s$date <- date # add date
    temp_long_s$trial_ID <- i
    temp_long_s$site <- site
    
    temp_long_s <- temp_long_s %>%
      left_join(dplyr::select(metadata, individual_ID, colony, x, y, z), by = c("colony","individual_ID"))
    

    out_list[[i]] <- temp_long_s # store result
    
    
  }
  
  # combine everything
  combined_data <- dplyr::bind_rows(out_list)
  
  return(combined_data)
  
}


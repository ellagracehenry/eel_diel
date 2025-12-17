
process_all_transitions <- function(transitions_path, metadata_path, threshold = 0) {
  
  files <- list.files(path = transitions_path, pattern = "\\.csv$", full.names = TRUE)
  metadata <- read_excel(metadata_path)
  out_list <- vector("list", length(files))
  
  for (i in seq_along(files)) {
    f <- files[i]
    temp <- read.csv(f, header = FALSE)
    
    colnames(temp)[1] <- "individual_ID"
    colnames(temp)[2:ncol(temp)] <- seq(1, ncol(temp) - 1)
    temp[,-1] <- lapply(temp[,-1], as.numeric)
    
    # apply fill_holes if needed
    temp[, 2:ncol(temp)] <- t(apply(temp[, 2:ncol(temp)], 1, function(row) fill_holes(row, threshold)))
    
    # convert to long
    temp_long <- melt(temp, id.vars = "individual_ID")
    temp_long$variable <- as.numeric(as.character(temp_long$variable))
    
    # add metadata
    fname <- basename(f)
    colony <- sub("^transitions_([^_]+)_.*$", "\\1", fname)
    date <- as.Date(sub("^transitions_[^_]+_([0-9_]+)_.*$", "\\1", fname), format = "%d_%m_%y")
    site <- sub(".*_([DFL])[0-9]+_.*", "\\1", fname) #extract site
    
    temp_long$colony <- colony
    temp_long$date <- date
    temp_long$trial_ID <- i
    temp_long$site <- site
    
    # compute number emerged at each time step
    temp_long <- temp_long %>%
      group_by(variable) %>%
      mutate(n_emerged = sum(value)) %>%
      ungroup()
    
    # compute run lengths with social context
    df_rl <- compute_run_lengths(temp_long)
    
    # attach metadata
    df_rl <- compute_run_lengths(temp_long) %>%
      mutate(
        trial_ID = i,
        colony = colony,
        date = date,
        site = site
      ) %>%
      dplyr::select(trial_ID, site, colony, date, individual_ID, run_type, run_length, n_other_emerged)
    out_list[[i]] <- df_rl
  }
  
  combined_run_lengths <- bind_rows(out_list)
  return(combined_run_lengths)
}

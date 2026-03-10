pairwise_synchrony <- function(transitions_path, metadata_path, threshold){
  
  # load files
  files <- list.files(
    path = transitions_path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  metadata <- readxl::read_excel(metadata_path, sheet = "identities")
  
  out_list <- vector("list", length(files))
  
  for (i in seq_along(files)) {
    
    f <- files[i]
    
    # -----------------------------
    # 1. READ + CLEAN DATA
    # -----------------------------
    temp <- read.csv(f, header = TRUE, na.strings = c("NaN","NA"))
    
    if (temp[1,1] > 1) {
      temp <- read.csv(f, header = FALSE, na.strings = c("NaN","NA"))
    }
    
    if (!is.na(temp[2,2]) && temp[2,2] > 1) {
      temp[,2] <- NULL
    }
    
    colnames(temp)[1] <- "individual_ID"
    colnames(temp)[2:ncol(temp)] <- seq_len(ncol(temp) - 1)
    
    temp[,-1] <- lapply(temp[,-1], as.numeric)
    
    # fill holes row-wise
    temp[, 2:ncol(temp)] <- t(apply(
      temp[, 2:ncol(temp)], 1,
      function(row) {
        if (all(is.na(row))) {
          return(rep(NA_real_, length(row)))
        }
        fill_holes(row, threshold)
      }
    ))
    
    # remove individuals never observed emerging
    temp_long <- reshape2::melt(temp, id.vars = "individual_ID")
    
    prop_df <- temp_long %>%
      dplyr::group_by(individual_ID) %>%
      dplyr::summarise(prop_time_emerged = mean(value, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::filter(prop_time_emerged > 0.1) %>%
      dplyr::filter(prop_time_emerged < 1) 
    
    temp <- temp %>%
      dplyr::filter(individual_ID %in% prop_df$individual_ID)
    
    # -----------------------------
    # 2. BUILD MATRIX
    # -----------------------------
    X <- as.matrix(temp[,-1])
    storage.mode(X) <- "numeric"
    
    ids <- temp$individual_ID
    n_ind <- nrow(X)
    
    if (n_ind < 2) {
      out_list[[i]] <- NULL
      next
    }
    
    pairs <- combn(n_ind, 2)
    
    sync_vals <- numeric(ncol(pairs))
    
    # -----------------------------
    # 3. PAIRWISE SYNCHRONY
    # -----------------------------
    for (k in seq_len(ncol(pairs))) {
      
      ii <- pairs[1, k]
      jj <- pairs[2, k]
      
      xi <- X[ii, ]
      xj <- X[jj, ]
      
      # overlapping observed periods
      valid <- !is.na(xi) & !is.na(xj)
      
      if (sum(valid) < 10) {
        sync_vals[k] <- NA_real_
        next
      }
      
      xi_v <- xi[valid]
      xj_v <- xj[valid]
      
      
      # observed co-emergence
      obs_same <- mean(xi_v == xj_v)
      
      # expected under independence (conditional on overlap)
      pi <- mean(xi_v)
      pj <- mean(xj_v)
      
      expected_same <- pi * pj + (1-pi)*(1-pj)
      
      if (expected_same > 0) {
        sync_vals[k] <- obs_same / expected_same
      } else {
        sync_vals[k] <- NA_real_
      }
    }
    
    pairwise_sync_long <- data.frame(
      id_i = ids[pairs[1,]],
      id_j = ids[pairs[2,]],
      synchrony = sync_vals
    )
    
    # -----------------------------
    # 4. ADD METADATA
    # -----------------------------
    fname <- basename(f)
    
    colony <- sub("^transitions_([^_]+)_.*$", "\\1", fname)
    date <- as.Date(sub("^transitions_[^_]+_([0-9_]+)_.*$", "\\1", fname),
                    format = "%d_%m_%y")
    site <- sub(".*_([DFL])[0-9]+_.*", "\\1", fname)
    
    pairwise_sync_long$colony <- colony
    pairwise_sync_long$date <- date
    pairwise_sync_long$trial_ID <- fname
    pairwise_sync_long$site <- site
    
    out_list[[i]] <- pairwise_sync_long
  }
  
  pairwise_synchrony_df <- dplyr::bind_rows(out_list)
  
  return(pairwise_synchrony_df)
}

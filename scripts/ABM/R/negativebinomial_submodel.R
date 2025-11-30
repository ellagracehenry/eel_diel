nbinomial_submodel <- function(data) {
  current_colnames_T <- colnames(data) #get colnames
  current_colnames_T[1] <- "individual_ID" #add individual_ID as colname
  colnames(data) <- current_colnames_T #re add columns to transitions
  colnames(data)[2:ncol(data)] <- seq(1, ncol(data) - 1) #Add a number column name
  colnames(data)[2:ncol(data)] <- as.character(seq(1, ncol(data) - 1))
  data[, 2:ncol(data)] <- t(apply(data[, 2:ncol(data)], 1, function(row) fill_holes(row, threshold))) # Apply the function to each row, starting from the 2nd column
  
  df_long <- melt(data, id.vars = "individual_ID")
  df_long$variable <- as.numeric(df_long$variable)
  
  df_run_length <- df_long %>%
    group_by(individual_ID) %>%
    arrange(variable, .by_group = TRUE) %>%
    reframe({
      r <- rle(value)
      tibble(value = r$values,
             run_length = r$lengths)
    }) %>%
    filter(!individual_ID %in% c(1,2)) %>%
    ungroup() %>%
    group_by(individual_ID) %>%
    mutate(cum_val = cumsum(run_length), start_time = cumsum(run_length) - run_length, seg = ceiling(start_time/512), seg_ID = (seg-1) + 7, second = start_time-(seg-1)*512) %>%
    ungroup()
  
  df_run_length_hide <- df_run_length %>%
    filter(value == 0) #%>%
  #dplyr::pull(run_length)
  
  df_run_length_emerge <- df_run_length %>%
    filter(value == 1) #%>%
  #dplyr::pull(run_length)
  
  N <- nrow(data)
  T <- ncol(data) - 1
  
  fit_nb_hide <- fitdistr(df_run_length_hide$run_length, "Negative Binomial")
  hide_size_hat <- fit_nb_hide$estimate["size"]
  hide_mu_hat   <- fit_nb_hide$estimate["mu"]
  
  p_hide <-1 / (1 + hide_mu_hat / hide_size_hat)
  
  fit_nb_emerge <- fitdistr(df_run_length_emerge$run_length, "Negative Binomial")
  emerge_size_hat <- fit_nb_emerge$estimate["size"]
  emerge_mu_hat   <- fit_nb_emerge$estimate["mu"]
  
  p_emerge <-1 / (1 + emerge_mu_hat / emerge_size_hat)
  
  state_record = matrix(NA, nrow = N, ncol = T)
  
  states = sample(c(0,1), N, replace = TRUE)
  
  state_record[,1]  <- states
  
  for(t in 1:(T-1)) {
    for (i in 1:N){
      if (state_record[i,t] == 1) {
        if (rnbinom(1,1,p_hide) == 1) {
          state_record[i,t+1] <- 0
        } else {
          state_record[i,t+1] <- 1
        }
      } else {
        if (rbinom(1,1,p_emerge) == 1) {
          state_record[i,t+1] <- 1
        } else {
          state_record[i,t+1] <- 0
        }
      }
    }
  }
  
  state_record <- cbind(individual_ID = data$individual_ID, state_record)
  
  df_state <- as.data.frame(state_record)
  
  df_long <- melt(df_state, id.vars = "individual_ID")
  df_long$variable <- as.numeric(df_long$variable)
  
  
  return(df_long)
  
}

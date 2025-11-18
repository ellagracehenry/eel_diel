
non_state_dependent_submodel <- function(data, emerge_run_lengths, hide_run_lengths) {
  
  #Import raw data
  n <- nrow(data)
  #Get distribution
  #get IDs
  id <- 1:n
  #Get time steps
  ts <- ncol(data)-1
  #Initialize matrix of the state of each individual for each time step
  state_matrix <- matrix(NA, nrow = n, ncol = ts)
  state_matrix <- cbind(id, state_matrix)
  colnames(state_matrix) <- c("ID", paste0("T", 1:ts))
  #Initialize first time step states as 0
  state_matrix[,2] <- rep(0, n)
  #Initial foraging need (after fasting the night)
  fish_pos <- matrix(NA, 30, 2)
  fish_present <- 0 
  maxX <- ceiling(max(abs_distances[,1])) + 5
  minX <- floor(min(abs_distances[,1])) - 5
  maxY <- ceiling(max(abs_distances[,2])) + 5
  minY <- floor(min(abs_distances[,2])) - 5
  
    for (i in 1:length(id)) {
      traj_df <- generate_time_step_trajectory(emerge_run_lengths, hide_run_lengths, total_steps = ts)
      
      sample(df_run_length_emerge,1)
      sample(df_run_length_hide,1)
      
      
      state_matrix[i,f] <- rbinom(1,1, p_swim)
    
  }
  
  results <- state_matrix
  
}
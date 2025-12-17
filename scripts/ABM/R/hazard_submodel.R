hazard_submodel <- function(data, lambda_emerge, lambda_hide){
  individual_ID <- data$individual_ID
  N <- length(individual_ID)
  T <- ncol(data)-1
  
  a_threshold <- rep(1,N)
  tm <- 5
  positions <- data.frame("x" = rnorm(N,2,1),"y"=rnorm(N,2,1))
  df_agents <- data.frame(individual_ID, a_threshold, positions)
  
  state_record = matrix(NA, nrow = N, ncol = T)
  states = sample(c(0,1), N, replace = TRUE)
  state_record[,1]  <- states
  
  time_in_state_matrix <- matrix(1, nrow = N, ncol = 1)
  
  
  for(t in 1:(T-1)) {
    for (i in 1:N){
      current_state <- state_record[i,t] #current state
      alternate_state <- ifelse((1-current_state == 1), 1, 0) #alternate state
      
      # #time in current state 
      # count <- 0 
      # for (k in t:1) {
      #   if (state_record[i,t] == current_state) {
      #     time_in_current_state <- count + 1
      #   } else {
      #     break
      #   }
      # }
      
      K <- sum(state_record[,t]) #number of emerged individuals
      
      if (current_state == 1) {
          p_hide <- exp_hazard_function(lambda_hide, time_in_current_state)
          if (rbinom(1,1,p_hide) == 1) {
            new_state <- 0
            time_in_state[i,1] <- 0
          } else {
            new_state <- 1
            time_in_state[i,1] <- time_in_state[i,1] + 1
          }
        } else {
        p_emerge <- exp_hazard_function(lambda_emerge, time_in_current_state)
        if (rbinom(1,1,p_emerge) == 1) {
          new_state <- 1
          time_in_state[i,1] <- 0
        } else {
          new_state <- 0
          time_in_state[i,1] <- time_in_state[i,1] + 1
        }
        
        state_record[i,t+1] <- new_state
        
        }
      }
    }
  
  state_record <- cbind(individual_ID = data$individual_ID, state_record)
  
  df_state <- as.data.frame(state_record)
  
  df_long <- melt(df_state, id.vars = "individual_ID")
  df_long$variable <- as.numeric(df_long$variable)
  
  
  return(df_long)
  
}

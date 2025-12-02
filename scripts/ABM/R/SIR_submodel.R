SIR_submodel <- function(data){
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

startled_matrix <- matrix(FALSE, nrow=N, ncol=T)

w <- matrix(1, nrow=N, ncol=N)

max_logs <- N * T
log_i     <- integer(max_logs)
log_time  <- integer(max_logs)
log_change<- integer(max_logs)
log_idx <- 1
  
for(t in 1:(T-1)) {
    for (i in 1:N){
      current_state <- state_record[i,t]
      K <- sum(state_record[,t]) #number of emerged individuals
      
      if (current_state == 1) {
        new_state <- startle_activation(i, startled_matrix, w, t, tm, df_agents$a_threshold[i],K)
        if (new_state == 1) {
          #Spontaneous hide
          if (rbinom(1,1,0.1) == 1) {
            new_state <- 0
           } else {
             new_state <- 1
           }
        } else {
          new_state <- 0
        }
        } else {
      #Emerge
      new_state <- rbinom(1,1,0.3)
      }

      state_record[i,t+1] <- new_state
      
      if (new_state != current_state) {
        if (new_state != current_state && new_state == 0) {
          startled_matrix[i, t+1] <- TRUE
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
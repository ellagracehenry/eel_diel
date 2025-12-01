SIR_submodel <- function(data){
individual_ID <- data$individual_ID
N <- length(individual_ID)
T <- ncol(data)-1

a_threshold <- rep(1,1,26)
positions <- data.frame("x" = rnorm(26, 2,1),"y"=rnorm(26, 2,1))
df_agents <- data.frame(individual_ID, a_threshold, positions)
  
state_record <- expand.grid(
  individual_ID = individual_ID,
  time = 1:T
)
state_record$state <- NA
initial_states = sample(c(0,1), N, replace = TRUE)
state_record$state[state_record$time == 1] <- initial_states


  for(t in 1:(T-1)) {
    for (i in 1:N){
      if (state_record[i,t] == 1) {
      state_record[i,t+1] <- activation_function()
      } else {
      state_record[i,t+1] <- rbinom(1,1,0.4)
      }
    }
  }
  
  state_record <- cbind(individual_ID = data$individual_ID, state_record)
  
  df_state <- as.data.frame(state_record)
  
  df_long <- melt(df_state, id.vars = "individual_ID")
  df_long$variable <- as.numeric(df_long$variable)
  
  
  return(df_long)
  
}
startle_activation <- function(i, startled_matrix, w, tc, tm, a_threshold,K) {
  
  activation_score <- 0 #initialise activation score 
  da <- 1 #cue size
  
  if (tc < tm) {
    tm <- tc - 1
  } 
  
  #For each time step in memory
  for (t in (tc-tm):(tc-1)) {
    
    #Calculate who is active (startled in that time step)
    startled_ind <- which(startled_matrix[, t])
    #Calculate link strength between active individuals and focal individual
    #startled_ind$w <- rep(1,nrow(startled_ind)) #all get 1 for now
    #Filter out individuals they are not linked to (w<0)
    #startled_ind_linked <- startled_ind[startled_ind$w > 0,]
    
    if (length(startled_ind) > 0) {

      for (a in seq_along(startled_ind)) {
        activation_score <- activation_score + da
      }
      
    } else {
      
    }
  }
  
  if (K == 0) {
    K <- 0.001
  }
  
  activation_score_fc <- activation_score/K #add in fractional contagion
  
  if (activation_score_fc > a_threshold) {
    state <- 0
  }
  else {
    state <- 1
  }
  
  return(state)
}

# 
# #FOR BALL DROP, WHEN CUE IS RECEIVED OVER TIME.
# #rate of cue arrival
# r_ij <- rho_max * w_ij
# #Calculate number of cues over time
# n_cues_ij <- r_ij * dt
# #stochastic realization
# actual_cues_ij <- rpois(1, lambda_n = cues_ij)
# #Cue score (da is fixed)
# cue_score <- actual_cues_ij * da






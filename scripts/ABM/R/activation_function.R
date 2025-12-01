activation_func <- function(ind, df_long_positions, t, tm){
  tm <- #memory
  t <- #current time
  df_long_positions_changes <- calculate_state_changes()
  for (i in (t-tm):(t-1)) {
  df_long_positions_active <- df_long_positions[df_long_positions$value == 1 & df_long_positions$variable == i,]
      for (j in 1:active_individuals) {
        #filter for individuals within interaction range 
        cum_cues <- cue_values[active_individual[1]]
      }
  }
  /K #to add in fractional contagion or not
}




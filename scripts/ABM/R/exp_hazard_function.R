exp_hazard_function <- function(lambda, time_in_current_state) {
  h_t <- lambda*time_in_current_state
  s_t <- exp(-h_t)
  p_switch <- 1 - s_t
  return(p_switch)
}
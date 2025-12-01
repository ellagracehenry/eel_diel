calculate_state_changes <- function(df_long){
  df_long<-df_long %>%
    group_by(individual_ID) %>%
    arrange(variable, .by_group=TRUE) %>%
    mutate(state_change = value - dplyr::lag(value))
  return(df_long)
}

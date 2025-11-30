 isolate_perturbed_hides <- function(df_run_length) {
  df_run_length_hides <- df_run_length[df_run_length$value == 0,]
  start_times <- df_run_length_hides$start_time
  n <- length(start_times)
  
  results <- list()
  k <- 1

  for (i in seq_len(n)){
    idx <- which(
      abs(start_times-start_times[i]) <= 3 & 
      seq_len(n) != i & 
        start_times != 0 #& 
        #df_run_length_hides$run_length[i] <10
      )
    
    if (!identical(idx,integer(0))) {
    results[[k]]  <- df_run_length_hides[i,]
    k <- k+1
    }
    else {
      
    }
  }
  
  perturb_only <- do.call(rbind,results)
  
  return(perturb_only)
  
}

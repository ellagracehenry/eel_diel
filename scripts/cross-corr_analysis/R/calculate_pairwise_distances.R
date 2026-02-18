calculate_pairwise_distances <- function(df) {
  
  coords <- as.matrix(df[, c("x", "y", "z")])
  
  # Distance matrix
  dist_mat <- as.matrix(dist(coords))
  
  # Name columns based on individual_ID
  colnames(dist_mat) <- paste0(df$individual_ID)
  
  # Bind to original df
  df_out <- bind_cols(df, as.data.frame(dist_mat))
  
  return(df_out)
}

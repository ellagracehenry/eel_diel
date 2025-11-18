fill_holes <- function(vec, threshold) {
  rle_vec <- rle(vec)  # Run length encoding of the vector
  values <- rle_vec$values
  lengths <- rle_vec$lengths
  
  # Find indices of runs smaller than the threshold
  small_runs <- which(lengths < threshold)
  
  for (i in small_runs) {
    if (i > 1 && i < length(values)) {
      values[i] <- values[i-1]
    }
  }
  
  # Recreate the original vector with the modified runs
  inverse_rle <- inverse.rle(list(lengths = lengths, values = values))
  return(inverse_rle)
}
fill_holes_all_segments <- function(vec, threshold) {
  # Identify runs of NA and non-NA
  is_na <- is.na(vec)
  r <- rle(is_na)
  
  filled <- c()
  start <- 1
  for (i in seq_along(r$values)) {
    segment_length <- r$lengths[i]
    segment <- vec[start:(start + segment_length - 1)]
    
    if (r$values[i]) {
      # This is an NA segment, keep as is
      filled <- c(filled, segment)
    } else {
      # Non-NA segment, fill holes normally
      filled <- c(filled, fill_holes(segment, threshold))
    }
    
    start <- start + segment_length
  }
  
  return(filled)
}
calculate_edge_proximity <- function(df) {
  
  df <- na.omit(df)
  # Calculate the centroid (mean of x and y coordinates)
  centroid <- colMeans(df[, c("x", "y")])
  
  # Identify the convex hull points
  hull_indices <- chull(df[, c("x", "y")])
  hull_points <- df[hull_indices, c("x", "y")]
  
  # Helper function to calculate the distance from a point to a line segment
  point_to_segment_distance <- function(p, v, w) {
    l2 <- sum((w - v)^2)  # Squared length of the segment
    if (l2 == 0) return(sqrt(sum((p - v)^2)))  # Segment is a single point
    
    t <- max(0, min(1, sum((p - v) * (w - v)) / l2))  # Projection onto segment
    projection <- v + t * (w - v)
    return(sqrt(sum((p - projection)^2)))
  }
  
  # Initialize a vector to store the ratios
  ratios <- numeric(nrow(df))
  
  # Calculate the ratio for each point
  for (i in seq_len(nrow(df))) {
    focal_point <- as.numeric(df[i, c("x", "y")])
    
    # Distance to the centroid
    dist_to_centre <- sqrt(sum((focal_point - centroid)^2))
    
    # Distance to the edge (minimum distance to any segment of the hull)
    dist_to_edge <- Inf
    for (j in seq_len(nrow(hull_points))) {
      v <- hull_points[j, ]
      w <- hull_points[ifelse(j == nrow(hull_points), 1, j + 1), ]  # Wrap around
      dist_to_edge <- min(dist_to_edge, point_to_segment_distance(focal_point, v, w))
    }
    
    # Calculate the ratio
    ratios[i] <- dist_to_centre / (dist_to_edge + dist_to_centre)
  }
  
  # Add the ratios to the data frame as a new column
  df$edge_proximity <- ratios
  return(df)
}

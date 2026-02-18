topology <- function(metadata_path) {
  #Load in metadata
  metadata <- read_excel(metadata_path, sheet = "identities")
  
  metadata_reduced <- metadata %>%
    select(colony, individual_ID, x, y, z)
  
  metadata_reduced <- metadata_reduced %>%
    group_by(colony) %>%
    group_modify(~calculate_edge_proximity(.x)) %>%
    group_modify(~calculate_pairwise_distances(.x))
  
  metadata_reduced_long <- metadata_reduced %>%
    pivot_longer(
      cols = -c(individual_ID, colony, x, y, z, edge_proximity),
      names_to = "id_j",
      values_to = "distance"
    ) %>%
    mutate(id_j = as.numeric(id_j))
  
  metadata_reduced_long_ordered <- metadata_reduced_long %>%
    group_by(colony, individual_ID) %>%
    mutate(dist_rank = rank(distance))
  
  return(metadata_reduced_long_ordered)

}

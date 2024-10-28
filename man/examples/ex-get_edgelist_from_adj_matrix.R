adj_matrix <- data.frame(
  "A" = c(0, 0, 0.3, 0),
  "B" = c(0.5, 0, 0, 0.6),
  "C" = c(0, 0, 0, 0),
  "D" = c(1, 0, 1, 0)
)
get_edgelist_from_adj_matrix(adj_matrix)

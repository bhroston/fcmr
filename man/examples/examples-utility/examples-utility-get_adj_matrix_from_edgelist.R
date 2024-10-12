edgelist <- data.frame(
  "source" = c("C", "A", "D", "A", "C"),
  "target" = c("A", "B", "B", "D", "D"),
  "weight" = c(0.3, 0.5, 0.6, 1.0, 1.0)
)
get_adj_matrix_from_edgelist(edgelist)

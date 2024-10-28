adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(1, 0)
)
adj_matrix_2 <- data.frame(
  "A" = c(0, 0, 0),
  "B" = c(0.25, 0, 1),
  "C" = c(0, 0.7, 0)
)
adj_matrix_3 <- data.frame(
  "B" = c(0, 0),
  "D" = c(0.75, 0)
)
adj_matrix_4 <- data.frame(
  "A" = c(0, 0, 0.3, 0),
  "B" = c(0.5, 0, 0, 0.6),
  "E" = c(0, 0, 0, 0),
  "F" = c(1, 0, 1, 0)
)
adj_matrices_w_different_concepts <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
standardize_adj_matrices(adj_matrices_w_different_concepts)


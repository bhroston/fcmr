make_adj_matrix_w_ivfns(
  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
)

lower_adj_matrix <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.4, 0)
)
upper_adj_matrix <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.6, 0)
)
make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)

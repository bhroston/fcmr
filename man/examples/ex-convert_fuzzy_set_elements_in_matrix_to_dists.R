lower_adj_matrix <- data.frame(
  C1 = c(0, 0, 0, 0, 0, 0),
  C2 = c(-0.85, 0, 0, 0.35, 0, 0),
  C3 = c(0, 0, 0, 0, 0, 0),
  C4 = c(-0.7, 0.6, -1, 0, -1, 0),
  C5 = c(0.1, 0, 0, -0.8, 0, 0),
  C6 = c(0, -0.95, 0, 0, -0.95, 0)
)
mode_adj_matrix <- data.frame(
  C1 = c(0, 0, 0, 0, 0, 0),
  C2 = c(-0.65, 0, 0, 0.80, 0, 0),
  C3 = c(0, 0, 0, 0, 0, 0),
  C4 = c(-0.6, 0.65, -0.85, 0, -0.95, 0),
  C5 = c(0.6, 0, 0, -0.2, 0, 0),
  C6 = c(0, -0.85, 0.1, 0, -0.75, 0)
)
upper_adj_matrix <- data.frame(
  C1 = c(0, 0, 0, 0, 0, 0),
  C2 = c(-0.1, 0, 0, 1, 0, 0),
  C3 = c(0, 0, 0, 0, 0, 0),
  C4 = c(-0.4, 0.7, -0.7, 0, -0.1, 0),
  C5 = c(0.7, 0, 0, -0.1, 0, 0),
  C6 = c(0, -0.2, 0.25, 0, -0.7, 0)
)
ex_tfn_adj_matrix <- make_adj_matrix_w_tfns(
  lower_adj_matrix, mode_adj_matrix, upper_adj_matrix
)

convert_fuzzy_set_elements_in_matrix_to_distributions(
  ex_tfn_adj_matrix, "tfn", 1000
)


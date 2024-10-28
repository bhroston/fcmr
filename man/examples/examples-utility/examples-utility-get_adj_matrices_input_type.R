# Individual Conventional Adj. Matrix ----
adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(1, 0)
)
get_adj_matrices_input_type(adj_matrix_1)


# List of Multiple Conventional FCM Adj. Matrices ----
adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(1, 0)
)
adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.25, 0)
)
adj_matrix_3 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.75, 0)
)
adj_matrix_4 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.5, 0)
)
fcms <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
get_adj_matrices_input_type(fcms)

# List of Multiple IVFN FCM Adj. Matrices ----
lower_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.4, 0)
)
upper_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.6, 0)
)
adj_matrix_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)
lower_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.6, 0)
)
upper_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(1, 0)
)
adj_matrix_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)
lower_adj_matrix_3 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.2, 0)
)
upper_adj_matrix_3 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.6, 0)
)
adj_matrix_3 <- make_adj_matrix_w_ivfns(lower_adj_matrix_3, upper_adj_matrix_3)
lower_adj_matrix_4 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.0, 0)
)
upper_adj_matrix_4 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.4, 0)
)
adj_matrix_4 <- make_adj_matrix_w_ivfns(lower_adj_matrix_4, upper_adj_matrix_4)
fcms_w_ivfns <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
get_adj_matrices_input_type(fcms_w_ivfns)


# List of Multiple TFN FCM Adj. Matrices ----
lower_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.4, 0)
)
mode_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.4, 0)
)
upper_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.6, 0)
)
adj_matrix_1 <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)
lower_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.6, 0)
)
mode_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.7, 0)
)
upper_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(1, 0)
)
adj_matrix_2 <- make_adj_matrix_w_tfns(lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2)
lower_adj_matrix_3 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.2, 0)
)
mode_adj_matrix_3 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.4, 0)
)
upper_adj_matrix_3 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.6, 0)
)
adj_matrix_3 <- make_adj_matrix_w_tfns(lower_adj_matrix_3, mode_adj_matrix_3, upper_adj_matrix_3)
lower_adj_matrix_4 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.0, 0)
)
mode_adj_matrix_4 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0, 0)
)
upper_adj_matrix_4 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.4, 0)
)
adj_matrix_4 <- make_adj_matrix_w_tfns(lower_adj_matrix_4, mode_adj_matrix_4, upper_adj_matrix_4)
fcms_w_tfns <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
get_adj_matrices_input_type(fcms_w_tfns)

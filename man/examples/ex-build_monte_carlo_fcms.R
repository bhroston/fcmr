# Conventional FCMs
ex_adj_matrix_1 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(0, 0, 0, 1),
  "C" = c(0, 1, 0, 0),
  "D" = c(0, 0, 1, 0)
)
ex_adj_matrix_2 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(0.25, 0, 0, 0.25),
  "C" = c(0, 0.25, 0, 0),
  "D" = c(0, 0, 0.25, 0)
)
ex_adj_matrix_3 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(0.75, 0, 0, 0.75),
  "C" = c(0, 0.75, 0, 0),
  "D" = c(0, 0, 0.75, 0)
)
ex_adj_matrix_4 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(0.5, 0, 0, 0.5),
  "C" = c(0, 0.5, 0, 0),
  "D" = c(0, 0, 0.5, 0)
)
ex_fcms <- list(
  ex_adj_matrix_1, ex_adj_matrix_2, ex_adj_matrix_3, ex_adj_matrix_4
)

mc_conventional_fcms <- build_monte_carlo_fcms(
  ex_fcms, N_samples = 1000, include_zeroes = FALSE, show_progress = TRUE
)



# For Interval-Valued Fuzzy Numbers (IVFNs)
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

mc_ivfn_fcms <- build_monte_carlo_fcms(
  fcms_w_ivfns, N_samples = 1000, include_zeroes = FALSE
)


# For Triangular Fuzzy Numbers (TFNs)
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
adj_matrix_1 <- make_adj_matrix_w_tfns(
  lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1
)
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
adj_matrix_2 <- make_adj_matrix_w_tfns(
  lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2
)
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
adj_matrix_3 <- make_adj_matrix_w_tfns(
  lower_adj_matrix_3, mode_adj_matrix_3, upper_adj_matrix_3
)
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
adj_matrix_4 <- make_adj_matrix_w_tfns(
  lower_adj_matrix_4, mode_adj_matrix_4, upper_adj_matrix_4
)

fcms_w_tfns <- list(
  adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4
)

mc_tfn_fcms <- build_monte_carlo_fcms(
  fcms_w_tfns, N_samples = 1000, include_zeroes = FALSE
)



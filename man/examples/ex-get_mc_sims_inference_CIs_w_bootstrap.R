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

mc_fcms <- build_monte_carlo_fcms_from_conventional_adj_matrices(
  ex_fcms, N_samples = 100, include_zeroes = FALSE, show_progress = TRUE
)

mc_fcms_inferences <- infer_monte_carlo_fcm_set(
  mc_adj_matrices = mc_fcms,
  initial_state_vector <- c(1, 1, 1, 1),
  clamping_vector <- c(1, 0, 0, 0),
  activation = "kosko",
  squashing = "sigmoid",
  lambda = 1,
  point_of_inference = "final",
  max_iter = 1000,
  min_error = 1e-5,
  parallel = FALSE,
  show_progress = TRUE
)

mc_sims_inferences <- get_mc_simulations_inference_CIs_w_bootstrap(
  mc_fcms_inferences$inference, "median", 0.95, parallel = FALSE
)


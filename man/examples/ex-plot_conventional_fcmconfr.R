test_adj_matrix_1 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(1, 0, 0, 1),
  "C" = c(0, 1, 0, 0),
  "D" = c(0, 0, 1, 0)
)
test_adj_matrix_2 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(0.25, 0, 0, 0.25),
  "C" = c(0, 0.25, 0, 0),
  "D" = c(0, 0, 0.25, 0)
)
test_adj_matrix_3 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(0.75, 0, 0, 0.75),
  "C" = c(0, 0.75, 0, 0),
  "D" = c(0, 0, 0.75, 0)
)
test_adj_matrix_4 <- data.frame(
  "A" = c(0, 0, 0, 0),
  "B" = c(0.5, 0, 0, 0.5),
  "C" = c(0, 0.5, 0, 0),
  "D" = c(0, 0, 0.5, 0)
)
ex_conventional_fcms <- list(
  test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4
)
ex_conventional_fcmconfr <- fcmconfr(
  adj_matrices = ex_conventional_fcms,
  # Aggregation and Monte Carlo Sampling
  aggregation_function = 'mean',
  monte_carlo_sampling_draws = 100,
  # Simulation
  initial_state_vector = c(1, 1, 1, 1),
  clamping_vector = c(0, 1, 0, 0),
  activation = 'kosko',
  squashing = 'sigmoid',
  lambda = 1,
  max_iter = 100,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  inference_estimation_function = "median",
  inference_estimation_CI = 0.95,
  inference_estimation_bootstrap_reps = 1000,
  # Runtime Options
  show_progress = TRUE,
  parallel = TRUE,
  n_cores = 2,
  # Additional Options
  perform_aggregate_analysis = TRUE,
  perform_monte_carlo_analysis = TRUE,
  perform_monte_carlo_inference_bootstrap_analysis = TRUE,
  include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
  include_monte_carlo_FCM_simulations_in_output = TRUE
)


fcmconfr_plot(ex_conventional_fcmconfr)

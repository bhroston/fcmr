# Conventional FCMs
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
  agg_function = 'mean',
  num_mc_fcms = 100,
  # Simulation
  initial_state_vector = c(1, 1, 1, 1),
  clamping_vector = c(0, 1, 0, 0),
  activation = 'kosko',
  squashing = 'sigmoid',
  lambda = 1,
  point_of_inference = "final",
  max_iter = 100,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  ci_centering_function = "median",
  confidence_interval = 0.95,
  num_ci_bootstraps = 1000,
  # Runtime Options
  show_progress = TRUE,
  parallel = TRUE,
  n_cores = 2,
  # Additional Options
  run_agg_calcs = TRUE,
  run_mc_calcs = TRUE,
  run_ci_calcs = TRUE,
  include_zeroes_in_sampling = FALSE,
  mc_sims_in_output = TRUE
)


# IVFN FCM fcmconfr
lower_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.25, 0)
)
upper_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.75, 0)
)
adj_matrix_w_ivfns_1 <- make_adj_matrix_w_ivfns(
  lower_adj_matrix_1, upper_adj_matrix_1
)
lower_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.15, 0)
)
upper_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.85, 0)
)
adj_matrix_w_ivfns_2 <- make_adj_matrix_w_ivfns(
  lower_adj_matrix_2, upper_adj_matrix_2
)
ex_ivfn_fcms <- list(adj_matrix_w_ivfns_1, adj_matrix_w_ivfns_2)
ex_ivfn_fcmconfr <- fcmconfr(
  adj_matrices = ex_ivfn_fcms,
  # Aggregation and Monte Carlo Sampling
  agg_function = 'mean',
  num_mc_fcms = 100,
  # Simulation
  initial_state_vector = c(1, 1),
  clamping_vector = c(0, 1),
  activation = 'kosko',
  squashing = 'sigmoid',
  lambda = 1,
  point_of_inference = "final",
  max_iter = 100,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  ci_centering_function = "mean",
  confidence_interval = 0.95,
  num_ci_bootstraps = 1000,
  # Runtime Options
  show_progress = TRUE,
  parallel = TRUE,
  n_cores = 2,
  # Additional Options
  run_agg_calcs = TRUE,
  run_mc_calcs = TRUE,
  run_ci_calcs = TRUE,
  include_zeroes_in_sampling = FALSE,
  mc_sims_in_output = TRUE
)


# TFN FCM fcmconfr
lower_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.25, 0)
)
mode_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.5, 0)
)
upper_adj_matrix_1 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.75, 0)
)
adj_matrix_w_tfns_1 <- make_adj_matrix_w_tfns(
  lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1
)

lower_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.15, 0)
)
mode_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.65, 0)
)
upper_adj_matrix_2 <- data.frame(
  "A" = c(0, 0),
  "B" = c(0.85, 0)
)
adj_matrix_w_tfns_2 <- make_adj_matrix_w_tfns(
  lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2
)

ex_tfn_fcms <- list(adj_matrix_w_tfns_1, adj_matrix_w_tfns_2)
ex_tfn_fcmconfr <- fcmconfr(
  adj_matrices = ex_tfn_fcms,
  # Aggregation and Monte Carlo Sampling
  agg_function = 'mean',
  num_mc_fcms = 100,
  # Simulation
  initial_state_vector = c(1, 1),
  clamping_vector = c(0, 1),
  activation = 'kosko',
  squashing = 'sigmoid',
  lambda = 1,
  point_of_inference = "final",
  max_iter = 100,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  ci_centering_function = "mean",
  confidence_interval = 0.95,
  num_ci_bootstraps = 1000,
  # Runtime Options
  show_progress = TRUE,
  parallel = FALSE,
  # Additional Options
  run_agg_calcs = TRUE,
  run_mc_calcs = TRUE,
  run_ci_calcs = TRUE,
  include_zeroes_in_sampling = FALSE,
  mc_sims_in_output = TRUE
)



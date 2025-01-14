ex_conventional_fcmconfr <- fcmconfr(
  adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
  # adj_matrices = group_conventional_fcms,
  # Aggregation and Monte Carlo Sampling
  aggregation_function = 'mean',
  monte_carlo_sampling_draws = 100,
  # Simulation
  # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  initial_state_vector = test_initial_state_vector,
  clamping_vector = test_clamping_vector,
  activation = 'kosko',
  squashing = 'tanh',
  lambda = 0.5,
  point_of_inference = "final",
  max_iter = 10000,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  inference_estimation_function = "mean",
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
  include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
  include_monte_carlo_FCM_simulations_in_output = TRUE
)
get_inferences(ex_conventional_fcmconfr, analysis = c("input"))
get_inferences(ex_conventional_fcmconfr, analysis = c("input", "aggregate"))
get_inferences(ex_conventional_fcmconfr, analysis = c("input", "aggregate", "mc"))
get_inferences(ex_conventional_fcmconfr)


ex_ivfn_fcmconfr <- fcmconfr(
  adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
  # adj_matrices = group_ivfn_fcms,
  # Aggregation and Monte Carlo Sampling
  aggregation_function = 'mean',
  monte_carlo_sampling_draws = 100,
  # Simulation
  # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  initial_state_vector = test_initial_state_vector,
  clamping_vector = test_clamping_vector,
  activation = 'rescale',
  squashing = 'sigmoid',
  lambda = 1,
  point_of_inference = "final",
  max_iter = 1000,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  inference_estimation_function = mean,
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
  include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
  include_monte_carlo_FCM_simulations_in_output = TRUE
)
get_inferences(ex_ivfn_fcmconfr, analysis = c("input"))
get_inferences(ex_ivfn_fcmconfr, analysis = c("input", "aggregate"))
get_inferences(ex_ivfn_fcmconfr, analysis = c("input", "aggregate", "mc"))
get_inferences(ex_ivfn_fcmconfr)


ex_tfn_fcmconfr <- fcmconfr(
  adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
  # adj_matrices = group_tfn_fcms,
  # Aggregation and Monte Carlo Sampling
  aggregation_function = 'mean',
  monte_carlo_sampling_draws = 100,
  # Simulation
  initial_state_vector = c(1, 1, 1, 1, 1, 1, 1),
  clamping_vector = c(1, 0, 0, 0, 0, 0, 0),
  activation = 'rescale',
  squashing = 'sigmoid',
  lambda = 1,
  point_of_inference = "final",
  max_iter = 1000,
  min_error = 1e-05,
  # Inference Estimation (bootstrap)
  inference_estimation_function = mean,
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
  include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
  include_monte_carlo_FCM_simulations_in_output = TRUE
)
get_inferences(ex_tfn_fcmconfr, analysis = c("input"))
get_inferences(ex_tfn_fcmconfr, analysis = c("input", "aggregate"))
get_inferences(ex_tfn_fcmconfr, analysis = c("input", "aggregate", "mc"))
get_inferences(ex_tfn_fcmconfr)

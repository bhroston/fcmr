
test_that("fcmconfr_plot works with Conventional FCMs", {
  # Clamping: Inputs Only
  conventional_clamping_inputs_only <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # Simulation
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 2,
    # Additional Options
    perform_aggregate_analysis = FALSE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(conventional_clamping_inputs_only))

  # Pulse: Inputs Only
  conventional_pulse_inputs_only <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # Simulation
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 2,
    # Additional Options
    perform_aggregate_analysis = FALSE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(conventional_clamping_inputs_only))


  # test <- fcm::fcm.infer(
  #   activation_vec = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  #   weight_mat = salinization_conventional_fcms[[1]],
  #   iter = 6, infer = "k",
  #   transform = "t",
  #   lambda = 1
  # )


  # Clamping: Inputs and Agg
  conventional_clamping_inputs_and_agg <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # Aggregation
    aggregation_function = 'mean',
    # Simulation
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(conventional_clamping_inputs_and_agg))

  # Pulse: Inputs and Agg
  conventional_pulse_inputs_and_agg <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # Aggregation
    aggregation_function = 'mean',
    # Simulation
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(conventional_pulse_inputs_and_agg))

  # Clamping: Inputs, Agg, and MC (NO Bootstrap)
  conventional_clamping_inputs_agg_and_mc_no_bs <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # adj_matrices = group_conventional_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(plot(conventional_clamping_inputs_agg_and_mc_no_bs))

  # Pulse: Inputs, Agg, and MC (NO Bootstrap)
  conventional_pulse_inputs_agg_and_mc_no_bs <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # adj_matrices = group_conventional_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(plot(conventional_pulse_inputs_agg_and_mc_no_bs))


  # Clamping: Inputs, Agg, and MC (w/ Bootstrap)
  conventional_clamping_inputs_agg_and_mc_w_bs <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # adj_matrices = group_conventional_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Inference Estimation (bootstrap)
    inference_estimation_function = mean,
    inference_estimation_CI = 0.95,
    inference_estimation_bootstrap_reps = 1000,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = TRUE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(plot(conventional_clamping_inputs_agg_and_mc_w_bs))

  # Pulse: Inputs, Agg, and MC (w/ Bootstrap)
  conventional_pulse_inputs_agg_and_mc_w_bs <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # adj_matrices = group_conventional_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Inference Estimation (bootstrap)
    inference_estimation_function = mean,
    inference_estimation_CI = 0.95,
    inference_estimation_bootstrap_reps = 1000,
    # Runtime Options
    show_progress = TRUE,
    parallel = FALSE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = TRUE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(plot(conventional_pulse_inputs_agg_and_mc_w_bs))

})


# Not finished yet
test_that("fcmconfr_plot works with IVFN FCMs", {
  # Clamping: Inputs Only
  ivfn_clamping_inputs_only <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # Simulation
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 2,
    # Additional Options
    perform_aggregate_analysis = FALSE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(ivfn_clamping_inputs_only))

  # Pulse: Inputs Only
  ivfn_pulse_inputs_only <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # Simulation
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 2,
    # Additional Options
    perform_aggregate_analysis = FALSE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(ivfn_clamping_inputs_only))


  # Clamping: Inputs and Agg
  ivfn_clamping_inputs_and_agg <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # Aggregation
    aggregation_function = 'mean',
    # Simulation
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(ivfn_clamping_inputs_and_agg))

  # Pulse: Inputs and Agg
  ivfn_pulse_inputs_and_agg <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # Aggregation
    aggregation_function = 'mean',
    # Simulation
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = FALSE
  )
  expect_snapshot(plot(ivfn_pulse_inputs_and_agg))

  # Clamping: Inputs, Agg, and MC (NO Bootstrap)
  ivfn_clamping_inputs_agg_and_mc_no_bs <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # adj_matrices = group_ivfn_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )

  # Pulse: Inputs, Agg, and MC (NO Bootstrap)
  ivfn_pulse_inputs_agg_and_mc_no_bs <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # adj_matrices = group_ivfn_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )

  # Clamping: Inputs, Agg, and MC (w/ Bootstrap)
  ivfn_clamping_inputs_agg_and_mc_w_bs <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # adj_matrices = group_ivfn_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Inference Estimation (bootstrap)
    inference_estimation_function = mean,
    inference_estimation_CI = 0.95,
    inference_estimation_bootstrap_reps = 1000,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = TRUE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )

  # Pulse: Inputs, Agg, and MC (w/ Bootstrap)
  ivfn_pulse_inputs_agg_and_mc_w_bs <- fcmconfr(
    adj_matrices = salinization_ivfn_fcms,
    # adj_matrices = group_ivfn_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Inference Estimation (bootstrap)
    inference_estimation_function = mean,
    inference_estimation_CI = 0.95,
    inference_estimation_bootstrap_reps = 1000,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = TRUE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
})




test_that("fcmconfr_plot works with example datasets", {

  test_conventional <- fcmconfr(
    adj_matrices = salinization_conventional_fcms,
    # adj_matrices = group_conventional_fcms,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
    initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
    activation = 'modified-kosko',
    squashing = 'sigmoid',
    lambda = 0.5,
    max_iter = 1000,
    min_error = 1e-05,
    # Inference Estimation (bootstrap)
    inference_estimation_function = mean,
    inference_estimation_CI = 0.95,
    inference_estimation_bootstrap_reps = 1000,
    # Runtime Options
    show_progress = TRUE,
    parallel = TRUE,
    n_cores = 10,
    # Additional Options
    perform_aggregate_analysis = TRUE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = TRUE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
})

test_that("fcmconfr_plot works", {
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
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)

  expect_no_error(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
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
      ),
      fcmconfr_plot(test)
    ))
  )


  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  adj_matrix_w_ivfns_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)
  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.15, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.85, 0)
  )
  adj_matrix_w_ivfns_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)
  test_fcms <- list(adj_matrix_w_ivfns_1, adj_matrix_w_ivfns_2)

  expect_no_error(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 1000,
        # Simulation
        initial_state_vector = c(1, 1),
        clamping_vector = c(0, 1),
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        max_iter = 100,
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
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      ),
      fcmconfr_plot(test)
    ))
  )


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
  adj_matrix_w_tfns_1 <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)

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
  adj_matrix_w_tfns_2 <- make_adj_matrix_w_tfns(lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2)

  test_fcms <- list(adj_matrix_w_tfns_1, adj_matrix_w_tfns_2)
  expect_no_error(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 100,
        # Simulation
        initial_state_vector = c(1, 1),
        clamping_vector = c(0, 1),
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        max_iter = 100,
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
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      ),
      fcmconfr_plot(test)
    )
    )
  )


})

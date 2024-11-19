
test_that("streamlined fcmconfr works", {

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

  # aggregate_fcms(test_fcms, "mean")

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
      )
    ))
  )

  expect_snapshot(test)


  # ggplot() +
  #   geom_jitter(data = test$inferences$input_fcms$inferences, aes(x = node, y = value)) +
  #   geom_crossbar(data = bootstrapped_means, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), fill = "red", color = "red", size = 0.1)


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
        parallel = FALSE,
        # Additional Options
        perform_aggregate_analysis = TRUE,
        perform_monte_carlo_analysis = TRUE,
        perform_monte_carlo_inference_bootstrap_analysis = TRUE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )
    ))
  )

  expect_snapshot(test)


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

  # test_aggregate <- aggregate_fcms(test_fcms, "mean")

  expect_no_error(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = adj_matrix_w_tfns_1,
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
        inference_estimation_CI = 0.95,
        inference_estimation_bootstrap_reps = 1000,
        # Runtime Options
        show_progress = TRUE,
        parallel = FALSE,
        # Additional Options
        perform_aggregate_analysis = FALSE,
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
        include_monte_carlo_FCM_simulations_in_output = FALSE
      )
    ))
  )

  expect_snapshot(test)




  # adj_matrices = test_fcms
  # # Aggregation and Monte Carlo Sampling
  # aggregation_function = 'mean'
  # monte_carlo_sampling_draws = 1000
  # # Simulation
  # initial_state_vector = c(1, 1, 1, 1)
  # clamping_vector = c(0, 0, 0, 0)
  # activation = 'kosko'
  # squashing = 'sigmoid'
  # lambda = 1
  # max_iter = 100
  # min_error = 1e-05
  # fuzzy_set_samples = 1000
  # # Inference Estimation (bootstrap)
  # inference_estimation_CI = 0.95
  # inference_estimation_bootstrap_reps = 1000
  # # Runtime Options
  # show_progress = TRUE
  # parallel = FALSE
  # n_cores = 10
  # # Additional Options
  # include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE
  # include_monte_carlo_FCM_simulations_in_output = TRUE
  # estimate_inference_CI_w_bootstrap = TRUE

  # ggplot() +
  #   geom_jitter(data = test$inference_for_plotting, aes(x = node, y = value)) +
  #   geom_crossbar(data = bootstrapped_means, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), fill = "red", color = "red", size = 0.1)
})


test_that("pulse only fcmconfr works", {
  salinization_conventional_fcms <- salinization_conventional_fcms

  expect_no_error(
    invisible(capture.output(
      test_fcmconfr_conventional_sigmoid <- fcmconfr(
        adj_matrices = salinization_conventional_fcms[[1]],
        # Simulation
        initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
        clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
        activation = 'modified-kosko',
        squashing = 'sigmoid',
        lambda = 1,
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
        perform_aggregate_analysis = FALSE,
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )
    ))
  )
  test_inferences <- test_fcmconfr_conventional_sigmoid$inferences$input_fcms$inferences[, -1]

  expected_inferences <- c(0.659, 0.659, 0.491, 0.659, 0.58, 0.659, 0.756, 0.611, 0.705)
  avg_error <- sum(abs(test_inferences - expected_inferences))/(length(test_inferences))
  max_allowable_avg_error <- 10e-4
  expect_lt(avg_error, max_allowable_avg_error)

  expect_no_error(
    invisible(capture.output(
      test_fcmconfr_conventional_tanh <- fcmconfr(
        adj_matrices = salinization_conventional_fcms[[1]],
        # Simulation
        initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
        clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
        activation = 'modified-kosko',
        squashing = 'tanh',
        lambda = 1,
        max_iter = 100,
        min_error = 1e-03,
        # Inference Estimation (bootstrap)
        inference_estimation_function = mean,
        inference_estimation_CI = 0.95,
        inference_estimation_bootstrap_reps = 1000,
        # Runtime Options
        show_progress = TRUE,
        parallel = TRUE,
        n_cores = 2,
        # Additional Options
        perform_aggregate_analysis = FALSE,
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )
    ))
  )
  test_inferences <- test_fcmconfr_conventional_tanh$inferences$input_fcms$inferences[, -1]

  expected_inferences <- c(0, 0, 0.131, 0, 0.804, 0, 0.882, 0.612, 0)
  avg_error <- sum(abs(test_inferences - expected_inferences))/(length(test_inferences))
  max_allowable_avg_error <- 10e-4
  expect_lt(avg_error, max_allowable_avg_error)
})


test_that("fcmconfr error checks work", {

  # Expect error w/ different sized adj. matrices
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
    "A" = c(0, 0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75, 0),
    "C" = c(0, 0.75, 0, 0, 0),
    "D" = c(0, 0, 0.75, 0, 0),
    "E" = c(0, 0, 0, 0, 0)
  )
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3)

  expect_error(
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
      )
    ))
  )


  # Expect warning w/ blank initial_state_vector, clamping_vector, activation, or squashin
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
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3)

  # Blank initial_state_vector
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 100,
        # Simulation
        initial_state_vector = c(),
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
    ))
  )

  # Blank clamping_vector
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 100,
        # Simulation
        initial_state_vector = c(1, 1, 1, 1),
        clamping_vector = c(),
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
    ))
  )

  # Blank activation
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 100,
        # Simulation
        initial_state_vector = c(1, 1, 1, 1),
        clamping_vector = c(0, 1, 0, 0),
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
    ))
  )

  # Blank clamping
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 100,
        # Simulation
        initial_state_vector = c(1, 1, 1, 1),
        clamping_vector = c(0, 1, 0, 0),
        activation = "kosko",
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
    ))
  )

  # Expect error when any clamping_vector != 0 and !all initial_state_vector = 1
  expect_error(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 100,
        # Simulation
        initial_state_vector = c(1, 0, 1, 1),
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
    ))
  )

  # Expect error with different concepts across adj. matrices
  test_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  test_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "C" = c(0.25, 0, 0, 0.25),
    "E" = c(0, 0.25, 0, 0),
    "F" = c(0, 0, 0.25, 0)
  )
  test_adj_matrix_3 <- data.frame(
    "K" = c(0, 0, 0, 0),
    "J" = c(0.75, 0, 0, 0.75),
    "Q" = c(0, 0.75, 0, 0),
    "T" = c(0, 0, 0.75, 0)
  )
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3)

  # Blank initial_state_vector
  expect_error(
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
      )
    ))
  )


  # Check cannot aggregate an individual, conventional FCM
  test_adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_adj_matrix,
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
        inference_estimation_bootstrap_reps = 1000,
        # Runtime Options
        show_progress = TRUE,
        parallel = TRUE,
        n_cores = 2,
        # Additional Options
        perform_aggregate_analysis = TRUE,
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )
    ))
  )


  # Check cannot aggregate individual ivfn/tfn FCM
  lower_adj_matrix <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  upper_adj_matrix <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  adj_matrix_w_ivfns <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = adj_matrix_w_ivfns,
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
    ))
  )


  # Expect warning when trying to bootstrap w/o performing monte carlo analysis
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
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2)

  expect_warning(
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
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = TRUE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )
    ))
  )

  # Expect warning if no aggregate_function defined
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
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
    ))
  )


  # Expect warning if no inference_estimation_function defined
  expect_warning(
    invisible(capture.output(
      test <- fcmconfr(
        adj_matrices = test_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = "mean",
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
    ))
  )

})


test_that("print.fcmconfr works", {
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

  # Perform aggregate & Perform monte carlo & Perform bootstrap
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
  )
  expect_snapshot(print(test))

  # Perform aggregate & Perform monte carlo & !Perform bootstrap
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
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(print(test))

  # !Perform aggregate & Perform monte carlo & Perform bootstrap
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
    perform_aggregate_analysis = FALSE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = TRUE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )

  # !Perform aggregate & Perform monte carlo & !Perform bootstrap
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
    perform_aggregate_analysis = FALSE,
    perform_monte_carlo_analysis = TRUE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(test)

  # Perform aggregate & !Perform monte carlo
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
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(test)

  # !Perform aggregate & !Perform monte carlo
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
    perform_aggregate_analysis = FALSE,
    perform_monte_carlo_analysis = FALSE,
    perform_monte_carlo_inference_bootstrap_analysis = FALSE,
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE
  )
  expect_snapshot(test)

})

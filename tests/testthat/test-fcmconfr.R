
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
        fuzzy_set_samples = 100,
        # Inference Estimation (bootstrap)
        inference_estimation_function = "median",
        inference_estimation_CI = 0.95,
        inference_estimation_bootstrap_reps = 1000,
        inference_estimation_bootstrap_draws_per_rep = 1000,
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
        fuzzy_set_samples = 100,
        # Inference Estimation (bootstrap)
        inference_estimation_function = "mean",
        inference_estimation_CI = 0.95,
        inference_estimation_bootstrap_reps = 1000,
        inference_estimation_bootstrap_draws_per_rep = 1000,
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
    ))
  )

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
        fuzzy_set_samples = 100,
        # Inference Estimation (bootstrap)
        inference_estimation_CI = 0.95,
        inference_estimation_bootstrap_reps = 1000,
        inference_estimation_bootstrap_draws_per_rep = 1000,
        # Runtime Options
        show_progress = TRUE,
        parallel = FALSE,
        n_cores = 10,
        # Additional Options
        perform_aggregate_analysis = FALSE,
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
        include_monte_carlo_FCM_simulations_in_output = FALSE
      )
    ))
  )




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
  # inference_estimation_bootstrap_draws_per_rep = 1000
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






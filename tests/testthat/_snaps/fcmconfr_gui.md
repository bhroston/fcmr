# fcmconfr_gui console output works

    Code
      agg_and_no_mc_and_no_bs
    Output
      fcmconfr(
        adj_matrices = salinization_ses_fcms,
        # Aggregation
        aggregation_function = 'mean',
        # Simulation
        initial_state_vector = 1111111111111111111111111111111111111111111111,
        clamping_vector = 0000100000000000000000000000000000000000000000,
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        point_of_inference = '',
        max_iter = 100,
        min_error = 1e-05,
        # Runtime Options
        show_progress = TRUE,
        # Additional Options
        perform_aggregate_analysis = TRUE,
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE
      )

---

    Code
      agg_and_mc_and_no_bs
    Output
      fcmconfr(
        adj_matrices = salinization_ses_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 1000,
        # Simulation
        initial_state_vector = 1111111111111111111111111111111111111111111111,
        clamping_vector = 0000100000000000000000000000000000000000000000,
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        point_of_inference = '',
        max_iter = 100,
        min_error = 1e-05,
        # Runtime Options
        show_progress = TRUE,
        parallel = TRUE,
        n_cores = 1,
        # Additional Options
        perform_aggregate_analysis = TRUE,
        perform_monte_carlo_analysis = TRUE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )

---

    Code
      agg_and_mc_and_bs
    Output
      fcmconfr(
        adj_matrices = salinization_ses_fcms,
        # Aggregation and Monte Carlo Sampling
        aggregation_function = 'mean',
        monte_carlo_sampling_draws = 1000,
        # Simulation
        initial_state_vector = 1111111111111111111111111111111111111111111111,
        clamping_vector = 0000100000000000000000000000000000000000000000,
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        point_of_inference = '',
        max_iter = 100,
        min_error = 1e-05,
        # Inference Estimation (bootstrap)
        inference_estimation_function = 'mean',
        inference_estimation_CI = 0.95,
        inference_estimation_bootstrap_reps = 1000,
        # Runtime Options
        show_progress = TRUE,
        parallel = TRUE,
        n_cores = 1,
        # Additional Options
        perform_aggregate_analysis = TRUE,
        perform_monte_carlo_analysis = TRUE,
        perform_monte_carlo_inference_bootstrap_analysis = TRUE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )

---

    Code
      no_agg_and_mc_and_no_bs
    Output
      fcmconfr(
        adj_matrices = salinization_ses_fcms,
        # Monte Carlo Sampling
        monte_carlo_sampling_draws = 1000,
        # Simulation
        initial_state_vector = 1111111111111111111111111111111111111111111111,
        clamping_vector = 0000100000000000000000000000000000000000000000,
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        point_of_inference = '',
        max_iter = 100,
        min_error = 1e-05,
        # Runtime Options
        show_progress = TRUE,
        parallel = TRUE,
        n_cores = 1,
        # Additional Options
        perform_aggregate_analysis = FALSE,
        perform_monte_carlo_analysis = TRUE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )

---

    Code
      no_agg_and_mc_and_bs
    Output
      fcmconfr(
        adj_matrices = salinization_ses_fcms,
        # Monte Carlo Sampling
        monte_carlo_sampling_draws = 1000,
        # Simulation
        initial_state_vector = 1111111111111111111111111111111111111111111111,
        clamping_vector = 0000100000000000000000000000000000000000000000,
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        point_of_inference = '',
        max_iter = 100,
        min_error = 1e-05,
        # Inference Estimation (bootstrap)
        inference_estimation_function = 'mean',
        inference_estimation_CI = 0.95,
        inference_estimation_bootstrap_reps = 1000,
        # Runtime Options
        show_progress = TRUE,
        parallel = TRUE,
        n_cores = 1,
        # Additional Options
        perform_aggregate_analysis = FALSE,
        perform_monte_carlo_analysis = TRUE,
        perform_monte_carlo_inference_bootstrap_analysis = TRUE,
        include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
        include_monte_carlo_FCM_simulations_in_output = TRUE
      )

---

    Code
      no_agg_and_no_mc_and_no_bs
    Output
      fcmconfr(
        adj_matrices = salinization_ses_fcms,
        # Simulation
        initial_state_vector = 1111111111111111111111111111111111111111111111,
        clamping_vector = 0000000000000000000000000000000000000000000000,
        activation = 'kosko',
        squashing = 'sigmoid',
        lambda = 1,
        point_of_inference = '',
        max_iter = 100,
        min_error = 1e-05,
        # Runtime Options
        show_progress = TRUE,
        # Additional Options
        perform_aggregate_analysis = FALSE,
        perform_monte_carlo_analysis = FALSE,
        perform_monte_carlo_inference_bootstrap_analysis = FALSE
      )


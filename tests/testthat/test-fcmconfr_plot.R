
test_that("fcmconfr_plot works with Conventional FCMs", {
  set.seed(100)

  # test_initial_state_vector <- rep(1, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_initial_state_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_initial_state_vector[5] <- 1 # Activate Guidance Docs
  #
  # test_clamping_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_clamping_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_clamping_vector[5] <- 1 # Activate Guidance Docs

  test_initial_state_vector = c(1, 1, 1, 1, 1, 1, 1)
  test_pulse_initial_state_vector = c(1, 0, 0, 0, 0, 0, 0)
  test_clamping_vector = c(1, 0, 0, 0, 0, 0, 0)
  test_pulse_clamping_vector = c(0, 0, 0, 0, 0, 0, 0)

  # Clamping: Inputs Only
  invisible(capture.output(
    conventional_clamping_inputs_only <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
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
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("Conventional Clamping Inputs Only", plot(conventional_clamping_inputs_only))
  )

  # Pulse: Inputs Only
  invisible(capture.output(
    conventional_pulse_inputs_only <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'kosko',
      squashing = 'sigmoid',
      lambda = 1,
      point_of_inference = "peak",
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
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("Conventional Pulse Inputs Only", plot(conventional_pulse_inputs_only))
  )

  # Clamping: Inputs and Agg
  invisible(capture.output(
    conventional_clamping_inputs_and_agg <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'rescale',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = FALSE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
      include_monte_carlo_FCM_simulations_in_output = FALSE
    )
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("Conventional Clamping Inputs and Agg", plot(conventional_clamping_inputs_and_agg))
  )

  # Pulse: Inputs and Agg
  invisible(capture.output(
    conventional_pulse_inputs_and_agg <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = FALSE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
      include_monte_carlo_FCM_simulations_in_output = FALSE
    )
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("Conventional Pulse Inputs and Agg", plot(conventional_pulse_inputs_and_agg))
  )

  # Clamping: Inputs, Agg, and MC (NO Bootstrap)
  invisible(capture.output(
    conventional_clamping_inputs_agg_and_mc_no_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
      # adj_matrices = group_conventional_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 1,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = TRUE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
      include_monte_carlo_FCM_simulations_in_output = TRUE
    )
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("Conventional Clamping Inputs Agg and MC NO BS", plot(conventional_clamping_inputs_agg_and_mc_no_bs))
  # )

  # Pulse: Inputs, Agg, and MC (NO Bootstrap)
  invisible(capture.output(
    conventional_pulse_inputs_agg_and_mc_no_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
      # adj_matrices = group_conventional_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 1,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
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
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("Conventional Pulse Inputs Agg and Mc NO BS", plot(conventional_pulse_inputs_agg_and_mc_no_bs))
  # )


  # Clamping: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    conventional_clamping_inputs_agg_and_mc_w_bs <- fcmconfr(
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
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("Conventional Clamping Inputs Agg and Mc w/ BS", plot(conventional_clamping_inputs_agg_and_mc_w_bs))
  # )

  # Pulse: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    conventional_pulse_inputs_agg_and_mc_w_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$conventional_fcms,
      # adj_matrices = group_conventional_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'kosko',
      squashing = 'sigmoid',
      lambda = 1,
      point_of_inference = "peak",
      max_iter = 1000,
      min_error = 1e-05,
      # Inference Estimation (bootstrap)
      inference_estimation_function = mean,
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
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("Conventional Pulse Inputs Agg and Mc w/ BS", plot(conventional_pulse_inputs_agg_and_mc_w_bs))
  # )
})


# Not finished yet
test_that("fcmconfr_plot works with IVFN FCMs", {
  set.seed(100)

  # test_initial_state_vector <- rep(1, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_initial_state_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_initial_state_vector[5] <- 1 # Activate Guidance Docs
  #
  # test_clamping_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_clamping_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_clamping_vector[5] <- 1 # Activate Guidance Docs

  test_initial_state_vector = c(1, 1, 1, 1, 1, 1, 1)
  test_pulse_initial_state_vector = c(1, 0, 0, 0, 0, 0, 0)
  test_clamping_vector = c(1, 0, 0, 0, 0, 0, 0)
  test_pulse_clamping_vector = c(0, 0, 0, 0, 0, 0, 0)

  # Clamping: Inputs Only
  invisible(capture.output(
    ivfn_clamping_inputs_only <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
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
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("IVFN Clamping Inputs Only", plot(ivfn_clamping_inputs_only))
  )

  # Pulse: Inputs Only
  invisible(capture.output(
    ivfn_pulse_inputs_only <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
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
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("IVFN Pulse Inputs Only", plot(ivfn_pulse_inputs_only))
  )


  # Clamping: Inputs and Agg
  invisible(capture.output(
    ivfn_clamping_inputs_and_agg <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = FALSE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
      include_monte_carlo_FCM_simulations_in_output = FALSE
    )
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("IVFN Clamping Inputs and Agg", plot(ivfn_clamping_inputs_and_agg))
  )

  # Pulse: Inputs and Agg
  invisible(capture.output(
    ivfn_pulse_inputs_and_agg <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = FALSE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
      include_monte_carlo_FCM_simulations_in_output = FALSE
    )
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("IVFN Pulse Inputs and Agg", plot(ivfn_pulse_inputs_and_agg))
  )

  # Clamping: Inputs, Agg, and MC (NO Bootstrap)
  invisible(capture.output(
    ivfn_clamping_inputs_agg_and_mc_no_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
      # adj_matrices = group_ivfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = TRUE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
      include_monte_carlo_FCM_simulations_in_output = TRUE
    )
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("IVFN Pulse Inputs and Agg", plot(ivfn_clamping_inputs_agg_and_mc_no_bs))
  # )

  # Pulse: Inputs, Agg, and MC (NO Bootstrap)
  invisible(capture.output(
    ivfn_pulse_inputs_agg_and_mc_no_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
      # adj_matrices = group_ivfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
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
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("IVFN Pulse Inputs and Agg", plot(ivfn_pulse_inputs_agg_and_mc_no_bs))
  # )


  # Clamping: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    ivfn_clamping_inputs_agg_and_mc_w_bs <- fcmconfr(
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
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("IVFN Pulse Inputs and Agg", plot(ivfn_clamping_inputs_agg_and_mc_w_bs))
  # )



  # Pulse: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    ivfn_pulse_inputs_agg_and_mc_w_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$ivfn_fcms,
      # adj_matrices = group_ivfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
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
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
      include_monte_carlo_FCM_simulations_in_output = TRUE
    )
  ))

})



# Not finished yet
test_that("fcmconfr_plot works with TFN FCMs", {
  set.seed(100)

  # test_initial_state_vector <- rep(1, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_initial_state_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_initial_state_vector[5] <- 1 # Activate Guidance Docs
  #
  # test_clamping_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_pulse_clamping_vector <- rep(0, unique(dim(sample_fcms$simple_fcms$conventional_fcms[[1]])))
  # test_clamping_vector[5] <- 1 # Activate Guidance Docs

  test_initial_state_vector = c(1, 1, 1, 1, 1, 1, 1)
  test_pulse_initial_state_vector = c(1, 0, 0, 0, 0, 0, 0)
  test_clamping_vector = c(1, 0, 0, 0, 0, 0, 0)
  test_pulse_clamping_vector = c(0, 0, 0, 0, 0, 0, 0)

  # Clamping: Inputs Only
  invisible(capture.output(
    tfn_clamping_inputs_only <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
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
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("TFN Clamping Inputs Only", plot(tfn_clamping_inputs_only))
  )

  # Pulse: Inputs Only
  invisible(capture.output(
    tfn_pulse_inputs_only <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
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
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("TFN Pulse Inputs Only", plot(tfn_pulse_inputs_only))
  )


  # Clamping: Inputs and Agg
  invisible(capture.output(
    tfn_clamping_inputs_and_agg <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = FALSE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
      include_monte_carlo_FCM_simulations_in_output = FALSE
    )
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("TFN Clamping Inputs and Agg", plot(tfn_clamping_inputs_and_agg))
  )

  # Pulse: Inputs and Agg
  invisible(capture.output(
    tfn_pulse_inputs_and_agg <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = FALSE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
      include_monte_carlo_FCM_simulations_in_output = FALSE
    )
  ))
  suppressWarnings(
    vdiffr::expect_doppelganger("TFN Pulse Inputs and Agg", plot(tfn_pulse_inputs_and_agg))
  )

  # Clamping: Inputs, Agg, and MC (NO Bootstrap)
  invisible(capture.output(
    tfn_clamping_inputs_agg_and_mc_no_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # adj_matrices = group_tfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = test_initial_state_vector,
      clamping_vector = test_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
      # Runtime Options
      show_progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      # Additional Options
      perform_aggregate_analysis = TRUE,
      perform_monte_carlo_analysis = TRUE,
      perform_monte_carlo_inference_bootstrap_analysis = FALSE,
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
      include_monte_carlo_FCM_simulations_in_output = TRUE
    )
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("TFN Pulse Inputs and Agg", plot(tfn_clamping_inputs_agg_and_mc_no_bs))
  # )

  # Pulse: Inputs, Agg, and MC (NO Bootstrap)
  invisible(capture.output(
    tfn_pulse_inputs_agg_and_mc_no_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # adj_matrices = group_tfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
      point_of_inference = "final",
      max_iter = 1000,
      min_error = 1e-05,
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
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("TFN Pulse Inputs and Agg", plot(tfn_pulse_inputs_agg_and_mc_no_bs))
  # )


  # Clamping: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    tfn_clamping_inputs_agg_and_mc_w_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # adj_matrices = group_tfn_fcms,
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
  ))
  # vdiffr is too sensitive for this test, run in personal testing only
  # suppressWarnings(
  #   vdiffr::expect_doppelganger("TFN Pulse Inputs and Agg", plot(tfn_clamping_inputs_agg_and_mc_w_bs))
  # )



  # Pulse: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    tfn_pulse_inputs_agg_and_mc_w_bs <- fcmconfr(
      adj_matrices = sample_fcms$simple_fcms$tfn_fcms,
      # adj_matrices = group_tfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      initial_state_vector = test_pulse_initial_state_vector,
      clamping_vector = test_pulse_clamping_vector,
      activation = 'modified-kosko',
      squashing = 'sigmoid',
      lambda = 0.5,
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
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
      include_monte_carlo_FCM_simulations_in_output = TRUE
    )
  ))

})

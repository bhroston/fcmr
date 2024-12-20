
test_that("fcmconfr_plot works with Conventional FCMs", {
  set.seed(100)

  # Clamping: Inputs Only
  invisible(capture.output(
    conventional_clamping_inputs_only <- fcmconfr(
      adj_matrices = salinization_conventional_fcms,
      # Simulation
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_conventional_fcms,
      # Simulation
      initial_state_vector = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
      clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
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
    vdiffr::expect_doppelganger("Conventional Pulse Inputs Only", (plot(conventional_clamping_inputs_only)))
  )

  # Clamping: Inputs and Agg
  invisible(capture.output(
    conventional_clamping_inputs_and_agg <- fcmconfr(
      adj_matrices = salinization_conventional_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_conventional_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_conventional_fcms,
      # adj_matrices = group_conventional_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_conventional_fcms,
      # adj_matrices = group_conventional_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_conventional_fcms,
      # adj_matrices = group_conventional_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_conventional_fcms,
      # adj_matrices = group_conventional_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      initial_state_vector = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
      clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
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
  # Clamping: Inputs Only
  invisible(capture.output(
    ivfn_clamping_inputs_only <- fcmconfr(
      adj_matrices = salinization_ivfn_fcms,
      # Simulation
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_ivfn_fcms,
      # Simulation
      initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
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
      adj_matrices = salinization_ivfn_fcms,
      # Aggregation
      aggregation_function = 'mean',
      # Simulation
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
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
    vdiffr::expect_doppelganger("IVFN Clamping Inputs and Agg", plot(ivfn_clamping_inputs_and_agg))
  )

  # Pulse: Inputs and Agg
  invisible(capture.output(
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
      adj_matrices = salinization_ivfn_fcms,
      # adj_matrices = group_ivfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
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

  # Pulse: Inputs, Agg, and MC (NO Bootstrap)
  invisible(capture.output(
    ivfn_pulse_inputs_agg_and_mc_no_bs <- fcmconfr(
      adj_matrices = salinization_ivfn_fcms,
      # adj_matrices = group_ivfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
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


  # Clamping: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    ivfn_clamping_inputs_agg_and_mc_w_bs <- fcmconfr(
      adj_matrices = salinization_ivfn_fcms,
      # adj_matrices = group_ivfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
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
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = TRUE,
      include_monte_carlo_FCM_simulations_in_output = TRUE
    )
  ))


  # Pulse: Inputs, Agg, and MC (w/ Bootstrap)
  invisible(capture.output(
    ivfn_pulse_inputs_agg_and_mc_w_bs <- fcmconfr(
      adj_matrices = salinization_ivfn_fcms,
      # adj_matrices = group_ivfn_fcms,
      # Aggregation and Monte Carlo Sampling
      aggregation_function = 'mean',
      monte_carlo_sampling_draws = 100,
      # Simulation
      # initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      # clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
      clamping_vector = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
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


#
#
#
# test_that("fcmconfr_plot works", {
#   test_adj_matrix_1 <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(1, 0, 0, 1),
#     "C" = c(0, 1, 0, 0),
#     "D" = c(0, 0, 1, 0)
#   )
#   test_adj_matrix_2 <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.25, 0, 0, 0.25),
#     "C" = c(0, 0.25, 0, 0),
#     "D" = c(0, 0, 0.25, 0)
#   )
#   test_adj_matrix_3 <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.75, 0, 0, 0.75),
#     "C" = c(0, 0.75, 0, 0),
#     "D" = c(0, 0, 0.75, 0)
#   )
#   test_adj_matrix_4 <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.5, 0, 0, 0.5),
#     "C" = c(0, 0.5, 0, 0),
#     "D" = c(0, 0, 0.5, 0)
#   )
#   test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
#
#   expect_no_error(
#     invisible(capture.output(
#       test <- fcmconfr(
#         adj_matrices = test_fcms,
#         # Aggregation and Monte Carlo Sampling
#         aggregation_function = 'mean',
#         monte_carlo_sampling_draws = 100,
#         # Simulation
#         initial_state_vector = c(1, 1, 1, 1),
#         clamping_vector = c(0, 1, 0, 0),
#         activation = 'kosko',
#         squashing = 'sigmoid',
#         lambda = 1,
#         max_iter = 100,
#         min_error = 1e-05,
#         # Inference Estimation (bootstrap)
#         inference_estimation_function = "median",
#         inference_estimation_CI = 0.95,
#         inference_estimation_bootstrap_reps = 1000,
#         # Runtime Options
#         show_progress = TRUE,
#         parallel = TRUE,
#         n_cores = 2,
#         # Additional Options
#         perform_aggregate_analysis = TRUE,
#         perform_monte_carlo_analysis = TRUE,
#         perform_monte_carlo_inference_bootstrap_analysis = TRUE,
#         include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
#         include_monte_carlo_FCM_simulations_in_output = TRUE
#       ),
#       fcmconfr_plot(test)
#     ))
#   )
#
#
#   lower_adj_matrix_1 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.25, 0)
#   )
#   upper_adj_matrix_1 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.75, 0)
#   )
#   adj_matrix_w_ivfns_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)
#   lower_adj_matrix_2 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.15, 0)
#   )
#   upper_adj_matrix_2 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.85, 0)
#   )
#   adj_matrix_w_ivfns_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)
#   test_fcms <- list(adj_matrix_w_ivfns_1, adj_matrix_w_ivfns_2)
#
#   expect_no_error(
#     invisible(capture.output(
#       test <- fcmconfr(
#         adj_matrices = test_fcms,
#         # Aggregation and Monte Carlo Sampling
#         aggregation_function = 'mean',
#         monte_carlo_sampling_draws = 1000,
#         # Simulation
#         initial_state_vector = c(1, 1),
#         clamping_vector = c(0, 1),
#         activation = 'kosko',
#         squashing = 'sigmoid',
#         lambda = 1,
#         max_iter = 100,
#         min_error = 1e-05,
#         # Inference Estimation (bootstrap)
#         inference_estimation_function = "mean",
#         inference_estimation_CI = 0.95,
#         inference_estimation_bootstrap_reps = 1000,
#         # Runtime Options
#         show_progress = TRUE,
#         parallel = TRUE,
#         n_cores = 2,
#         # Additional Options
#         perform_aggregate_analysis = TRUE,
#         perform_monte_carlo_analysis = TRUE,
#         perform_monte_carlo_inference_bootstrap_analysis = TRUE,
#         include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
#         include_monte_carlo_FCM_simulations_in_output = TRUE
#       ),
#       fcmconfr_plot(test)
#     ))
#   )
#
#
#   lower_adj_matrix_1 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.25, 0)
#   )
#   mode_adj_matrix_1 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.5, 0)
#   )
#   upper_adj_matrix_1 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.75, 0)
#   )
#   adj_matrix_w_tfns_1 <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)
#
#   lower_adj_matrix_2 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.15, 0)
#   )
#   mode_adj_matrix_2 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.65, 0)
#   )
#   upper_adj_matrix_2 <- data.frame(
#     "A" = c(0, 0),
#     "B" = c(0.85, 0)
#   )
#   adj_matrix_w_tfns_2 <- make_adj_matrix_w_tfns(lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2)
#
#   test_fcms <- list(adj_matrix_w_tfns_1, adj_matrix_w_tfns_2)
#   expect_no_error(
#     invisible(capture.output(
#       test <- fcmconfr(
#         adj_matrices = test_fcms,
#         # Aggregation and Monte Carlo Sampling
#         aggregation_function = 'mean',
#         monte_carlo_sampling_draws = 100,
#         # Simulation
#         initial_state_vector = c(1, 1),
#         clamping_vector = c(0, 1),
#         activation = 'kosko',
#         squashing = 'sigmoid',
#         lambda = 1,
#         max_iter = 100,
#         min_error = 1e-05,
#         # Inference Estimation (bootstrap)
#         inference_estimation_function = "mean",
#         inference_estimation_CI = 0.95,
#         inference_estimation_bootstrap_reps = 1000,
#         # Runtime Options
#         show_progress = TRUE,
#         parallel = TRUE,
#         n_cores = 2,
#         # Additional Options
#         perform_aggregate_analysis = TRUE,
#         perform_monte_carlo_analysis = TRUE,
#         perform_monte_carlo_inference_bootstrap_analysis = TRUE,
#         include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
#         include_monte_carlo_FCM_simulations_in_output = TRUE
#       ),
#       fcmconfr_plot(test)
#     )
#     )
#   )
#
#
# })

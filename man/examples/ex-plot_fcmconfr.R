
# Example using TFN FCMs fcmconfr
tfn_example_fcmconfr <- fcmconfr(
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


# Plot Defaults
plot(tfn_example_fcmconfr,
     interactive = FALSE, # Set to TRUE to open shiny app
     # Plot Formatting Parameters
     filter_limit = 1e-4,
     coord_flip = FALSE,
     # Plot Aesthetic Parameters
     mc_avg_and_CIs_color = "blue",
     mc_inferences_color = "blue",
     mc_inferences_shape = 3,
     ind_inferences_color = "black",
     ind_inferences_shape = 16,
     agg_inferences_color = "red",
     agg_inferences_shape = 17,
     ind_ivfn_and_tfn_linewidth = 0.1,
     agg_ivfn_and_tfn_linewidth = 0.6
)

# Changed from Plot Defaults
plot(tfn_example_fcmconfr,
     interactive = FALSE, # Set to TRUE to open shiny app
     # Plot Formatting Parameters
     filter_limit = 1e-4,
     coord_flip = FALSE,
     # Plot Aesthetic Parameters
     mc_avg_and_CIs_color = "blue",
     mc_inferences_color = "blue",
     mc_inferences_shape = 3,
     ind_inferences_color = "black",
     ind_inferences_shape = 16,
     agg_inferences_color = "red",
     agg_inferences_shape = 17,
     ind_ivfn_and_tfn_linewidth = 0.1,
     agg_ivfn_and_tfn_linewidth = 0.6
)


# Plot Defaults w/ Shiny App
# plot(tfn_example_fcmconfr,
#      interactive = TRUE, # Set to TRUE to open shiny app
#      # Plot Formatting Parameters
#      filter_limit = 1e-4,
#      coord_flip = FALSE,
#      text_font_size = 12,
#      # Plot Aesthetic Parameters
#      mc_avg_and_CIs_color = "blue",
#      mc_inferences_color = "blue",
#      mc_inferences_shape = 3,
#      ind_inferences_color = "black",
#      ind_inferences_shape = 16,
#      agg_inferences_color = "red",
#      agg_inferences_shape = 17,
#      ind_ivfn_and_tfn_linewidth = 0.1,
#      agg_ivfn_and_tfn_linewidth = 0.6
# )



#' fcmconfr
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fcmconfr") for more information.
#' @param adj_matrices A list of adjacency matrices (n x n) representing FCMs. This
#' can also be an individual adjacency matrix.Adj. Matrices can be conventional FCMs,
#' FCMs with edge weights as Interval Value Fuzzy Numbers (IVFNs) or FCMs with edge
#' weights as Triangular Fuzzy Numbers (TFNs)
#' @param aggregation_function Aggregate the adj. matrices into a single FCM by taking
#' either the mean or median of the edge weights for edges included in multiple maps
#' @param monte_carlo_sampling_draws The number of FCMs to generate via monte carlo
#' sampling from the input adj. matrices
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param inference_estimation_CI The confidence interval to estimate for the inferences
#' of each concept across all monte carlo FCMs (via bootstrap)
#' @param inference_estimation_bootstrap_reps The number of bootstraps to perform in
#' estimating the confidence interval for the inferences of each concept across all monte
#' carlo FCMs
#' @param inference_estimation_bootstrap_draws_per_rep The number of draws to sample
#' (with replacement) from the estimates for the inferences of each concept across
#' all monte carlo FCMs per bootstrap
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param include_zero_weighted_edges_in_aggregation_and_mc_sampling TRUE/FALSE
#' Whether to incorporate zeroes as intentionally-defined edge weights or ignore
#' them when aggregating adj. matrices and sampling for monte carlo FCMs
#' @param include_monte_carlo_FCM_simulations_in_output TRUE/FALSE
#' Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
#' about the size of the output of fcmconfr (simulations are necessary and will run regardless)
#' @param estimate_inference_CI_w_bootstrap TRUE/FALSE
#' Whether to estimate confidence intervals for the inferences of each concept across
#' all monte carlo FCMs via bootstrap. Switch to FALSE if concerned about runtime
#' and/or want to perform bootstrapping manually.
#'
#' @export
fcmconfr <- function(adj_matrices = list(matrix()),
                     # Aggregation and Monte Carlo Sampling
                     aggregation_function = c("mean", "median"),
                     monte_carlo_sampling_draws = 1000,
                     # Simulation
                     initial_state_vector = c(),
                     clamping_vector = c(),
                     activation = c("kosko", "modified-kosko", "rescale"),
                     squashing = c("sigmoid", "tanh"),
                     lambda = 1,
                     max_iter = 100,
                     min_error = 1e-5,
                     # Inference Estimation (bootstrap)
                     inference_estimation_CI = 0.95,
                     inference_estimation_bootstrap_reps = 5000,
                     inference_estimation_bootstrap_draws_per_rep = 5000,
                     # Runtime Options
                     show_progress = TRUE,
                     parallel = TRUE,
                     n_cores = integer(),
                     # Additional Options
                     include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
                     include_monte_carlo_FCM_simulations_in_output = TRUE,
                     estimate_inference_CI_w_bootstrap = TRUE) {

  # Perform Checks ----
  adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices)
  if (!adj_matrices_input_type$adj_matrices_input_is_list) {
    adj_matrices <- list(adj_matrices)
  }

  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
    initial_state_vector <- rep(1, nrow(adj_matrix))
  }
  if (identical(clamping_vector, c())) {
    warning("No clamping_vector input given. Assuming no values are clamped.")
    clamping_vector <- rep(0, length(initial_state_vector))
  }

  # Perform checks
  checks <- lapply(adj_matrices, check_simulation_inputs, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  # ----

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  # Aggregation and Monte Carlo Simulations ----
  # Build aggregate adj_matrix
  aggregate_adj_matrix <- aggregate_fcms(adj_matrices, aggregation_function, include_zero_weighted_edges_in_aggregation_and_mc_sampling)
  # Infer aggregate adj_matrix
  # Have a generalized simulate function here

  # Build monte carlo models
  sampled_adj_matrices <- build_monte_carlo_fcms(adj_matrices, monte_carlo_sampling_draws, include_zero_weighted_edges_in_aggregation_and_mc_sampling, show_progress)
  sampled_adj_matrices <- lapply(sampled_adj_matrices,
                                 function(sampled_adj_matrix) {
                                   colnames(sampled_adj_matrix) <- IDs
                                   rownames(sampled_adj_matrix) <- IDs
                                   sampled_adj_matrix
                                 })

  # Infer monte carlo models with clamping
  fmcm_results <- infer_fmcm_with_clamping(
    simulated_adj_matrices = sampled_adj_matrices,
    initial_state_vector = initial_state_vector,
    clamping_vector = clamping_vector,
    activation = activation,
    squashing = squashing,
    lambda = lambda,
    max_iter = max_iter,
    min_error = min_error,
    parallel = parallel,
    show_progress = show_progress,
    n_cores = n_cores,
    IDs = nodes
  )

  if (estimate_inference_CI_w_bootstrap) {
    means_of_fmcm_inferences <- get_means_of_fmcm_inference(
      fmcm_inference = fmcm_results$inference,
      get_bootstrapped_means = estimate_inference_CI_w_bootstrap,
      confidence_interval = inference_estimation_CI,
      inference_estimation_bootstrap_reps = inference_estimation_bootstrap_reps,
      bootstrap_samples_per_rep = inference_estimation_bootstrap_draws_per_rep,
      parallel = parallel,
      n_cores = n_cores
    )
  }
  # ----



  # Organize Output
  env_variables <- as.list(environment())
  confr_output <- organize_fcmconfr_output(env_variables)

  confr_output
}



#' organize_fcmconfr_output
#'
#' @description
#' This arranges fcmconfr inputs and outputs into a neatly-arranged list of lists
#'
#' @details
#' A standardized way to organize the fcmconfr into easily navigable data structures.
#'
#' Use vignette("fcmconfr-class") for more information.
#'
#' @param ... additional inputs; typically environmental variables
#'
#' @export
organize_fcmconfr_output <- function(...) {
  variables <- as.list(...)

  params <- list(
    fcm_class = variables$fcm_class,
    fcms = variables$adj_matrices,
    inference_opts = list(initial_state_vector = variables$initial_state_vector,
                          clamping_vector = variables$clamping_vector,
                          activation = variables$activation,
                          squashing = variables$squashing,
                          lambda = variables$lambda,
                          max_iter = variables$max_iter,
                          min_error = variables$min_error,
                          IDs = variables$IDs),
    bootstrap_input_opts = list(sampling = variables$sampling,
                                samples = variables$samples),
    runtime_opts = list(parallel = variables$parallel,
                        n_cores = variables$n_cores,
                        show_progress = variables$show_progress,
                        include_monte_carlo_FCM_simulations_in_output = variables$include_monte_carlo_FCM_simulations_in_output)
  )
  fcmconfr_output <- structure(
    .Data = list(
      inference = variables$fmcm_results$inference,
      params = params
    ),
    class = paste0(variables$fcm_class, "confr")
  )

  if (variables$estimate_inference_CI_w_bootstrap) {
    fcmconfr_output$params$bootstrap_output_opts = list(
      estimate_inference_CI_w_bootstrap = variables$estimate_inference_CI_w_bootstrap,
      inference_estimation_CI = variables$inference_estimation_CI,
      inference_estimation_bootstrap_reps = variables$inference_estimation_bootstrap_reps,
      inference_estimation_bootstrap_draws_per_rep = variables$inference_estimation_bootstrap_draws_per_rep)
    fcmconfr_output$bootstrap = list(
      mean_CI_by_node = variables$means_of_fmcm_inferences$mean_CI_by_node,
      raw_bootstrap_means = variables$means_of_fmcm_inferences$bootstrap_means
    )
  }
  fcmconfr_output
}



#' print.fcmconfr
#'
#' @description
#' This improves the readability of the fcmconfr
#'
#' @details
#' Show the objects listed in the fcmconfr output $inference and $params, as well
#' as $bootstrap if present in output. Additionally, this prints descriptions/summaries
#' of objects within each sub-list like inference_opts, bootstrap_input_opts, etc.
#'
#' Use vignette("fcm-class") for more information.
#'
#' @param x an fgcm_simulation object
#' @param ... additional inputs
#'
#' @export
print.fcmconfr <- function(x, ...) {
  n_sims <- x$params$bootstrap_input_opts$samples
  n_input_fcm <- length(x$params$fcms)

  if ("bootstrap" %in% names(x)) {
    cat("$inference",
        paste0("Inferences of ", n_sims, " fcm constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" -      mean_CI_by_node: ", x$params$bootstrap_output_opts$inference_estimation_CI, "% CI of means of inference\n"),
        paste0(" -  raw_bootstrap_means: ", x$params$bootstrap_output_opts$inference_estimation_bootstrap_reps, " actualizations of the avg inference of ", x$params$bootstrap_output_opts$inference_estimation_bootstrap_draws_per_rep, " draws with replacement"),
        "\n$params\n",
        " -       inference_opts:",
        paste0("act = ", x$params$inference_opts$activation, "; squash = ", x$params$inference_opts$squashing, "; lambda = ", x$params$inference_opts$lambda),
        "\n  - bootstrap_input_opts:",
        paste0("sampling = ", x$params$bootstrap_input_opts$sampling, "; n_samples = ", x$params$bootstrap_input_opts$samples)
    )
  } else {
    cat("$inference",
        paste0("Inferences of ", n_sims, " fcm constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " -   inference_opts:",
        paste0("act = ", x$params$inference_opts$activation, "; squash = ", x$params$inference_opts$squashing, "; lambda = ", x$params$inference_opts$lambda),
        "\n  -  bootstrap_input_opts:",
        paste0("sampling = ", x$params$bootstrap_input_opts$sampling, "; n_samples = ", x$params$bootstrap_input_opts$samples)
    )
  }
}

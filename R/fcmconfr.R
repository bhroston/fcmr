

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
#' @param fuzzy_set_samples The size (n) of the distributions represented by IVFNs or TFNs (only
#' used when IVFNs or TFNs in adj_matrices input)
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
#' @param perform_aggregate_analysis TRUE/FALSE Run the code to generate and simulate an aggregate FCM generated from the input adj_matrices
#' @param perform_monte_carlo_analysis TRUE/FALS Run the code to generate and simulate monte carlo-generated FCM sampled from the input adj_matrices
#' @param include_zero_weighted_edges_in_aggregation_and_mc_sampling TRUE/FALSE
#' Whether to incorporate zeroes as intentionally-defined edge weights or ignore
#' them when aggregating adj. matrices and sampling for monte carlo FCMs
#' @param include_monte_carlo_FCM_simulations_in_output TRUE/FALSE
#' Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
#' about the size of the output of fcmconfr (simulations are necessary and will run regardless)
#' @param estimate_mc_inference_CI_w_bootstrap TRUE/FALSE
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
                     fuzzy_set_samples = 1000,
                     # Inference Estimation (bootstrap)
                     inference_estimation_CI = 0.95,
                     inference_estimation_bootstrap_reps = 5000,
                     inference_estimation_bootstrap_draws_per_rep = 5000,
                     # Runtime Options
                     show_progress = TRUE,
                     parallel = TRUE,
                     n_cores = integer(),
                     # Additional Options
                     perform_aggregate_analysis = TRUE,
                     perform_monte_carlo_analysis = TRUE,
                     include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
                     include_monte_carlo_FCM_simulations_in_output = TRUE,
                     estimate_mc_inference_CI_w_bootstrap = TRUE) {

  # Perform Checks ----
  # browser()

  adj_matrices_dims <- lapply(adj_matrices, dim)
  if (length(unique(unlist(adj_matrices_dims))) > 1) {
    stop("Input Validation Error: Input adj. matrices are either different sizes or contain non-square matrices
       !! Call standardize_adj_matrices() to standardize the size of the list of adj. matrices")
  }
  n_nodes <- unique(unlist(adj_matrices_dims))

  adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices)
  fcm_class <- adj_matrices_input_type$object_types_in_list[1]
  if (!adj_matrices_input_type$adj_matrices_input_is_list) {
    adj_matrices <- list(adj_matrices)
  }

  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
    initial_state_vector <- rep(1, n_nodes)
  }
  if (identical(clamping_vector, c())) {
    warning("No clamping_vector input given. Assuming no values are clamped.")
    clamping_vector <- rep(0, n_nodes)
  }

  # Perform checks
  checks <- lapply(adj_matrices, check_simulation_inputs, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  identified_concepts <- unique(lapply(adj_matrices, colnames))
  if (length(identified_concepts) != 1) {
    stop("All input adjacency matrices must have the same concepts")
  } else {
    concepts <- unlist(identified_concepts)
  }

  # ----

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  # Individual Adj. Matrices Simulations ----
  individual_adj_matrices_inferences <- lapply(adj_matrices, infer_fcm, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error, fuzzy_set_samples)
  if (fcm_class == "conventional") {
    individual_adj_matrices_inferences_df <- do.call(rbind, lapply(individual_adj_matrices_inferences, function(inference) inference$inference))
    individual_adj_matrices_inferences_df <- cbind(input = paste0("adj_matrix_", 1:length(adj_matrices)), individual_adj_matrices_inferences_df)
  } else if (fcm_class %in% c("ivfn", "tfn")) {
    individual_adj_matrices_inferences_df <- lapply(individual_adj_matrices_inferences, function(inference) inference$inference_df)
    names(individual_adj_matrices_inferences_df) <- paste0("adj_matrix_", 1:length(adj_matrices))
  }
  names(individual_adj_matrices_inferences) <- paste0("adj_matrix_", 1:length(adj_matrices))

  # Aggregation and Monte Carlo Simulations ----

  if (length(adj_matrices) == 1 & (perform_monte_carlo_analysis | perform_aggregate_analysis)) {
    perform_monte_carlo_analysis = FALSE
    perform_aggregate_analysis = FALSE
    warning("  Cannot aggregate or generate monte carlo samples from a single adj. matrix.
    Skipping aggregation and monte carlo analyses.")
  }

  if (!perform_monte_carlo_analysis & estimate_mc_inference_CI_w_bootstrap) {
    warning(". Cannot estimate CIs of monte carlo inferences if monte carlo analysis is not being performed.
    Skipping CI bound estimation.")
    estimate_mc_inference_CI_w_bootstrap <- FALSE
  }

  if (perform_aggregate_analysis) {
    # Build aggregate adj_matrix
    aggregate_adj_matrix <- aggregate_fcms(adj_matrices, aggregation_function, include_zero_weighted_edges_in_aggregation_and_mc_sampling)
    # Infer aggregate adj_matrix
    aggregate_fcm_inference <- infer_fcm(aggregate_adj_matrix$adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error, fuzzy_set_samples)
  }

  if (perform_monte_carlo_analysis) {
    # Build monte carlo models
    mc_adj_matrices <- build_monte_carlo_fcms(adj_matrices, monte_carlo_sampling_draws, include_zero_weighted_edges_in_aggregation_and_mc_sampling, show_progress)
    mc_adj_matrices <- lapply(mc_adj_matrices,
                              function(sampled_adj_matrix) {
                                colnames(sampled_adj_matrix) <- concepts
                                rownames(sampled_adj_matrix) <- concepts
                                sampled_adj_matrix
                              })

    # Infer monte carlo models with clamping
    mc_inferences <- infer_monte_carlo_fcm_set(
      mc_adj_matrices = mc_adj_matrices,
      initial_state_vector = initial_state_vector,
      clamping_vector = clamping_vector,
      activation = activation,
      squashing = squashing,
      lambda = lambda,
      max_iter = max_iter,
      min_error = min_error,
      fuzzy_set_samples = fuzzy_set_samples,
      parallel = parallel,
      show_progress = show_progress,
      n_cores = n_cores,
      include_simulations_in_output = include_monte_carlo_FCM_simulations_in_output
    )

    if (estimate_mc_inference_CI_w_bootstrap) {
      CIs_of_means_of_mc_simulation_inferences <- get_mc_simulations_inference_CIs_w_bootstrap(mc_inferences$inference, inference_estimation_CI, inference_estimation_bootstrap_reps, inference_estimation_bootstrap_draws_per_rep, parallel, n_cores, show_progress)
      quantiles_of_mc_simulation_inferences <- data.frame(t(apply(mc_inferences$inference, 2, stats::quantile)))
      mc_inference_distributions_df <- cbind(CIs_of_means_of_mc_simulation_inferences$CI_by_node, quantiles_of_mc_simulation_inferences)
      colnames(mc_inference_distributions_df) <- c("node", "lower_0.025_CI_about_mean", "upper_0.975_CI_about_mean", "min", "lower_quantile_0.25", "median", "upper_quantile_0.75", "max")
    }
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

  # browser()

  fcmconfr_output <- structure(
    .Data = list(
      fcm_class = variables$fcm_class,
      inferences = list(
        input_fcms = list(
          inferences = variables$individual_adj_matrices_inferences_df,
          simulations = variables$individual_adj_matrices_inferences
        )
      ),
      params = list(
        fcm_class = variables$fcm_class,
        adj_matrices = variables$adj_matrices,
        simulation_opts = list(initial_state_vector = variables$initial_state_vector,
                               clamping_vector = variables$clamping_vector,
                               activation = variables$activation,
                               squashing = variables$squashing,
                               lambda = variables$lambda,
                               max_iter = variables$max_iter,
                               min_error = variables$min_error,
                               fuzzy_set_samples = variables$fuzzy_set_samples),
        additional_opts = list(estimate_mc_inference_CI_w_bootstrap = variables$estimate_mc_inference_CI_w_bootstrap,
                               perform_aggregate_analysis = variables$perform_aggregate_analysis,
                               perform_monte_carlo_analysis = variables$perform_monte_carlo_analysis)
      )
    ),
    class = "fcmconfr"
  )


  if (variables$perform_aggregate_analysis) {
    fcmconfr_output$inferences$aggregate_fcm = list(
      inferences = variables$aggregate_fcm_inference$inference,
      simulation = variables$aggregate_fcm_inference
    )
    fcmconfr_output$params$aggregation_function = variables$aggregation_function
    fcmconfr_output$params$additional_opts <- list(
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling,
      estimate_mc_inference_CI_w_bootstrap = variables$estimate_mc_inference_CI_w_bootstrap,
      perform_aggregate_analysis = variables$perform_aggregate_analysis,
      perform_monte_carlo_analysis = variables$perform_monte_carlo_analysis
    )
  }


  if (variables$perform_monte_carlo_analysis) {
    fcmconfr_output$inferences$monte_carlo_fcms = list(
      all_inferences = variables$mc_inferences$inference,
      simulations = variables$mc_inferences$sims
    )
    fcmconfr_output$params$aggregation_function = variables$aggregation_function
    fcmconfr_output$params$monte_carlo_sampling_draws = variables$monte_carlo_sampling_draws
    fcmconfr_output$params$runtime_opts = list(parallel = variables$parallel,
                               n_cores = variables$n_cores,
                               show_progress = variables$show_progress)
    fcmconfr_output$params$additional_opts = list(
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling,
      include_monte_carlo_FCM_simulations_in_output = variables$include_monte_carlo_FCM_simulations_in_output,
      estimate_mc_inference_CI_w_bootstrap = variables$estimate_mc_inference_CI_w_bootstrap,
      perform_aggregate_analysis = variables$perform_aggregate_analysis,
      perform_monte_carlo_analysis = variables$perform_monte_carlo_analysis
    )
  }


  if (variables$perform_monte_carlo_analysis & variables$estimate_mc_inference_CI_w_bootstrap) {
    fcmconfr_output$bootstrap = list(
      CIs_about_means_and_quantiles_of_mc_inferences = variables$mc_inference_distributions_df,
      bootstrapped_means = variables$CIs_of_means_of_mc_simulation_inferences$bootstrap_means
    )
    fcmconfr_output$params$mc_confidence_intervals_opts = list(
      inference_estimation_CI = variables$inference_estimation_CI,
      inference_estimation_bootstrap_reps = variables$inference_estimation_bootstrap_reps,
      inference_estimation_bootstrap_draws_per_rep = variables$inference_estimation_bootstrap_draws_per_rep
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
  performed_aggregate <- x$params$additional_opts$perform_aggregate_analysis
  performed_mc <- x$params$additional_opts$perform_monte_carlo_analysis
  performed_bootstrap <- x$params$additional_opts$estimate_mc_inference_CI_w_bootstrap

  n_input_fcm <- length(x$params$adj_matrices)

  if (performed_aggregate & performed_mc & performed_bootstrap) {
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$aggregation_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" - CIs_about_means_and_quantiles_by_node: ", x$params$mc_confidence_intervals_opts$inference_estimation_CI, "% CI of means of inferences and quantiles by node\n"),
        paste0(" - bootstrapped_means: ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_reps, " actualizations of the avg inference of ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_draws_per_rep, " draws with replacement"),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (performed_aggregate & performed_mc & !performed_bootstrap) {
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$aggregation_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (!performed_aggregate & performed_mc & performed_bootstrap) {
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" - CIs_about_means_and_quantiles_by_node: ", x$params$mc_confidence_intervals_opts$inference_estimation_CI, "% CI of means of inferences and quantiles by node\n"),
        paste0(" - bootstrapped_means: ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_reps, " actualizations of the avg inference of ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_draws_per_rep, " draws with replacement"),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (!performed_aggregate & performed_mc & !performed_bootstrap) {
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (performed_aggregate & !performed_mc) {
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$aggregation_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (!performed_aggregate & !performed_mc) {
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  }
}

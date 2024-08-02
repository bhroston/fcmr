
#' fcmconfr
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param adj_matrices A list of n x n adjacencey matrices representing fcms, fgcms, or ftcms.
#' @param samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes_in_aggregation TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param aggregation_fun The aggregation function, either mean or median
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
#' @param bootstrap_inference_means TRUE/FALSE Whether to estimate a CI about the mean
#' inferences
#' @param bootstrap_CI What are of the distribution should be bounded by the
#' confidence intervals? (e.g. 0.95)
#' @param bootstrap_reps Repetitions for bootstrap process, if chosen
#' @param bootstrap_draws_per_rep Number of samples to draw (with replacement) from
#' the data per bootstrap_rep
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' FCM. Will dramatically increase size of output if TRUE.
#' @param ... Additional input for adjacency matrices of class fcm. sampling = c('nonparametric', 'uniform', 'triangular')
#'
#' @export
fcmconfr <- function(adj_matrices = list(matrix()),
                     samples = 1000,
                     include_zeroes_in_aggregation = TRUE,
                     aggregation_fun = c("mean", "median"),
                     initial_state_vector = c(),
                     clamping_vector = c(),
                     activation = c("kosko", "modified-kosko", "rescale"),
                     squashing = c("sigmoid", "tanh"),
                     lambda = 1,
                     max_iter = 100,
                     min_error = 1e-5,
                     bootstrap_inference_means = TRUE,
                     bootstrap_CI = 0.95,
                     bootstrap_reps = 5000,
                     bootstrap_draws_per_rep = 5000,
                     show_progress = TRUE,
                     parallel = TRUE,
                     n_cores = integer(),
                     IDs = c(),
                     include_simulations_in_output = FALSE,
                     ...) {

  additional_vars <- list(...)

  fcm_class <- get_fcm_class_from_adj_matrices(adj_matrices)
  if (fcm_class == "fcm" & ("sampling" %in% names(as.list(environment()))) | "sampling" %in% names(additional_vars)) {
    sampling <- additional_vars$sampling
  } else if (fcm_class == "fcm" & !("sampling" %in% names(as.list(environment()))) | "sampling" %in% names(additional_vars)) {
    sampling <- "nonparametric"
    warning("Type fcm adjacency matrices requires the sampling parameter as an additional
            input. Assuming sampling = 'nonparametric'.")
  } else {
    sampling <- NULL
  }

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x, IDs))
  nodes <- unlist(unique(concepts_in_adj_matrices))
  confirm_adj_matrices_have_same_concepts(concepts_in_adj_matrices)
  confirm_adj_matrices_have_same_dimensions(adj_matrices)
  confirm_input_vector_is_compatible_with_adj_matrices(adj_matrices[[1]], initial_state_vector, fcm_class)
  confirm_input_vector_is_compatible_with_adj_matrices(adj_matrices[[1]], clamping_vector, fcm_class)

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  # Build monte carlo models
  if (fcm_class == "fcm") {
    sampled_adj_matrices <- build_conventional_fcmconfr_models(adj_matrices, sampling, samples, show_progress)
  } else if (fcm_class == "fgcm" | fcm_class == "ftcm") {
    sampled_adj_matrices <- build_unconventional_fcmconfr_models(adj_matrices, aggregation_fun, samples, include_zeroes_in_aggregation, show_progress)
  } else {
    stop("Incompatible collection of data types found in input adj_matrices")
  }
  sampled_adj_matrices <- lapply(sampled_adj_matrices,
                                 function(sampled_adj_matrix) {
                                   colnames(sampled_adj_matrix) <- nodes
                                   rownames(sampled_adj_matrix) <- nodes
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

  if (bootstrap_inference_means) {
    means_of_fmcm_inferences <- get_means_of_fmcm_inference(
      fmcm_inference = fmcm_results$inference,
      get_bootstrapped_means = bootstrap_inference_means,
      confidence_interval = bootstrap_CI,
      bootstrap_reps = bootstrap_reps,
      bootstrap_samples_per_rep = bootstrap_reps,
      parallel = parallel,
      n_cores = n_cores
    )
  }

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
                        include_simulations_in_output = variables$include_simulations_in_output)
  )
  fcmconfr_output <- structure(
    .Data = list(
      inference = variables$fmcm_results$inference,
      params = params
    ),
    class = paste0(variables$fcm_class, "confr")
  )

  if (variables$bootstrap_inference_means) {
    fcmconfr_output$params$bootstrap_output_opts = list(
      bootstrap_inference_means = variables$bootstrap_inference_means,
      bootstrap_CI = variables$bootstrap_CI,
      bootstrap_reps = variables$bootstrap_reps,
      bootstrap_draws_per_rep = variables$bootstrap_draws_per_rep)
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
        paste0(" -      mean_CI_by_node: ", x$params$bootstrap_output_opts$bootstrap_CI, "% CI of means of inference\n"),
        paste0(" -  raw_bootstrap_means: ", x$params$bootstrap_output_opts$bootstrap_reps, " actualizations of the avg inference of ", x$params$bootstrap_output_opts$bootstrap_draws_per_rep, " draws with replacement"),
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

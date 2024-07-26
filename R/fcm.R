
#' fcmconfr
#'
#' @description
#' [ADD DETAILS HERE!!!!]
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param fcm_adj_matrices A list of n x n adjacencey matrices representing fcms
#' @param sampling The sampling method to be applied. Must be one of the following: "nonparametric", "uniform", or "triangular"
#' @param samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
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
#' @param bootstrap_inference_means TRUE/FALSE Whether to estimate the CIs about the mean inferences
#' of each node via bootstrapping
#' @param bootstrap_CI Bootstrap confidence interval
#' @param bootstrap_reps Number of reps and draws during bootstrapping
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' FCM. Will dramatically increase size of output if TRUE.
#'
#' @export
fcmconfr <- function(fcm_adj_matrices = list(matrix()),
                     sampling = c("nonparametric", "uniform", "triangular"), # Options: 'nonparametric', 'uniform', 'triangular'
                     samples = 1000,
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

  concepts_in_fcms <- lapply(fcm_adj_matrices, function(x) get_node_IDs_from_input(x, IDs))
  all_fcms_have_same_concepts <- length(unique(concepts_in_fcms)) == 1
  if (!all_fcms_have_same_concepts) {
    stop("All adjacency matrices must have the same concepts.")
  }

  dimensions_of_input_adj_matrices <- lapply(fcm_adj_matrices, dim)
  all_fcms_have_same_dimensions <- length(unique(dimensions_of_input_adj_matrices)) == 1
  if (!all_fcms_have_same_dimensions) {
    stop("All adjacency matrices must have the same dimensions (n x n) throughout the entire list")
  }

  # Confirm packages necessary packages are available. If not, change run options
  if (parallel) {
    package_checks <- check_if_local_machine_has_parallel_processing_packages(parallel, show_progress)
    parallel <- package_checks$parallel_check
  }
  if (show_progress) {
    package_checks <- check_if_local_machine_has_parallel_processing_packages(parallel, show_progress)
    show_progress <- package_checks$show_progress_check
  }

  # Check that adj_matrices are correct format
  lapply(fcm_adj_matrices, function(x) fcm(x, IDs))

  nodes <- unlist(unique(concepts_in_fcms))
  sampled_adj_matrices <- build_fcmconfr_models(fcm_adj_matrices, sampling, samples, nodes, show_progress)

  sampled_adj_matrices_are_representative_of_population <- check_simulated_fcmconfr_models(fcm_adj_matrices, sampled_adj_matrices, sampling, samples, nodes, parallel, n_cores, show_progress)
  new_samples <- samples + 1000
  while (!sampled_adj_matrices_are_representative_of_population) {
    cat(" Random draws are likely not representative of the population.\n Adding 1000 to the number of draws (i.e. samples)\n",
        paste0("New samples = ", new_samples, "\n"))
    sampled_adj_matrices <- build_fcmconfr_models(fcm_adj_matrices, sampling, new_samples, nodes, parallel, n_cores, show_progress)
    sampled_adj_matrices_are_representative_of_population <- check_simulated_fcmconfr_models(fcm_adj_matrices, sampled_adj_matrices, sampling, new_samples, nodes, parallel, n_cores, show_progress)
    new_samples <- new_samples + 1000
  }

  fmcm_results <- infer_fmcm(
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
    include_simulations_in_output = include_simulations_in_output
  )

  params <- list(
    fcms = fcm_adj_matrices,
    inference_opts = list(initial_state_vector = initial_state_vector,
                          clamping_vector = clamping_vector,
                          activation = activation,
                          squashing = squashing,
                          lambda = lambda,
                          max_iter = max_iter,
                          min_error = min_error,
                          IDs = IDs),
    bootstrap_input_opts = list(sampling = sampling,
                         samples = samples),
    runtime_opts = list(parallel = parallel,
                       n_cores = n_cores,
                       show_progress = show_progress,
                       include_simulations_in_output = include_simulations_in_output)
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

    params$bootstrap_output_opts = list(bootstrap_inference_means =  bootstrap_inference_means,
                                 bootstrap_CI = bootstrap_CI,
                                 bootstrap_reps = bootstrap_reps,
                                 bootstrap_draws_per_rep = bootstrap_draws_per_rep)

    fcmconfr_output <- structure(
      .Data = list(
        inference = fmcm_results$inference,
        params = params,
        bootstrap = list(
          mean_CI_by_node = means_of_fmcm_inferences$mean_CI_by_node,
          raw_bootstrap_means = means_of_fmcm_inferences$bootstrap_means
        )
      ),
      class = "fcmconfr"
    )
  } else {
    fcmconfr_output <- structure(
      .Data = list(
        inference = fmcm_results$inference,
        params = params
      ),
      class = "fcmconfr"
    )
  }

  fcmconfr_output
}


#' build_fcmconfr_models
#'
#' @description
#' This function generates n fcm models whose edge weights are sampled from either
#' the defined edge values in a set of adjacency matrices or continuous (uniform or triangular)
#' parametric distributions derived from the sets of edge values, and stores them
#' as a list of adjacency matrices.
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fcm-class") for more information.
#'
#' @param adj_matrices A list of n x n adjacencey matrices representing fcms
#' @param sampling The sampling method to be applied. Must be one of the following: "nonparametric", "uniform", or "triangular"
#' @param samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param nodes A vector of node names (IDs) present in every adjacency matrix
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
build_fcmconfr_models <- function(adj_matrices = list(matrix()),
                                  sampling = "nonparametric", # 'nonparametric', 'uniform', or 'triangular'
                                  samples = integer(),
                                  nodes = c(),
                                  parallel = TRUE,
                                  n_cores = integer(),
                                  show_progress = TRUE) {
  n_nodes <- length(nodes)
  n_maps <- length(adj_matrices)
  adj_matrices_as_arrays <- array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps))

  #combined_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), function(x) x, simplify = FALSE)
  #widened_combined_adj_matrix <- array(combined_adj_matrix, c(1, n_nodes^2))
  #widened_combined_adj_matrix <- apply(widened_combined_adj_matrix, 2, unlist)
  #non_zero_value_indexes <- unlist(lapply(apply(widened_combined_adj_matrix, 2, unique), function(x) !identical(x, 0)))
  #nonzero_widened_combined_adj_matrix <- widened_combined_adj_matrix[, non_zero_value_indexes]
  #widened_combined_adj_matrix_as_lists <- lapply(seq_len(ncol(nonzero_widened_combined_adj_matrix)), function(x) nonzero_widened_combined_adj_matrix[, x])

  if (sampling == "nonparametric") {
    adj_matrix_of_samples_per_edge <- apply(adj_matrices_as_arrays, c(1, 2), function(x) sample(x, samples, replace = TRUE), simplify = FALSE)
    wider_adj_matrix_of_samples_per_edge <- apply(array(adj_matrix_of_samples_per_edge, c(1, n_nodes^2)), 2, unlist)
    sampled_adj_matrices <- apply(wider_adj_matrix_of_samples_per_edge, 1, function(x) array(x, c(n_nodes, n_nodes)), simplify = FALSE)
    sampled_adj_matrices <- lapply(sampled_adj_matrices, function(x) {
      x <- as.data.frame(x)
      colnames(x) <- nodes
      rownames(x) <- nodes
      x
    })
  } else if (sampling == "uniform") {
    min_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), min)
    max_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), max)
    empirical_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(min_adj_matrix, max_adj_matrix)
    sampled_adj_matrices <- build_fmcm_models_from_grey_adj_matrix(
      empirical_grey_adj_matrix,
      n_sims = samples,
      distribution = "uniform",
      show_progress = show_progress,
      IDs = IDs
    )
  } else if (sampling == "triangular") {
    min_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), min)
    max_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), max)
    empirical_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(min_adj_matrix, max_adj_matrix)
    mode_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), mean)
    sampled_adj_matrices <- build_fmcm_models_from_grey_adj_matrix(
      empirical_grey_adj_matrix,
      mode_adj_matrix,
      n_sims = samples,
      distribution = "triangular",
      show_progress = show_progress,
      IDs = IDs
    )
  }

  sampled_adj_matrices
}


#' check_simulated_fcmconfr_models
#'
#' @description
#' This functions checks whether a set of sampled adj matrices are representative
#' of the selected distribution.
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fcm-class") for more information.
#'
#' @param input_adj_matrices A list of n x n adjacency matrices used as inputs to fcmconfr
#' @param sampled_Adj_matrices A list of n x n adjacency matrices constructed with build_fcmconfr_models
#' @param sampling The sampling method to be applied. Must be one of the following: "nonparametric", "uniform", or "triangular"
#' @param samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param nodes A vector of node names (IDs) present in every adjacency matrix
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
check_simulated_fcmconfr_models <- function(input_adj_matrices = list(matrix()),
                                            sampled_adj_matrices = list(matrix()),
                                            sampling = c("nonparametric", "uniform", "triangular"),
                                            samples = integer(),
                                            nodes = c(),
                                            parallel = TRUE,
                                            n_cores = integer(),
                                            show_progress = TRUE) {

  input_adj_matrices_values_by_index <- data.frame(do.call(rbind, lapply(input_adj_matrices, unlist)))
  input_adj_matrices_nonzero_values_by_index <- input_adj_matrices_values_by_index[, colSums(input_adj_matrices_values_by_index) != 0]

  sampled_adj_matrices_values_by_index <- data.frame(do.call(rbind, lapply(sampled_adj_matrices, unlist)))
  sampled_adj_matrices_nonzero_values_by_index <- sampled_adj_matrices_values_by_index[, colSums(sampled_adj_matrices_values_by_index) != 0]

  if (sampling == "nonparametric") {
    input_value_occurrences_by_index <- apply(input_adj_matrices_nonzero_values_by_index, 2, table)
    input_value_fractional_occurrences_by_index <- data.frame(apply(input_value_occurrences_by_index, 2, function(col) col/sum(col)))

    sampled_adj_matrices_nonzero_value_occurrence_by_index <- apply(sampled_adj_matrices_nonzero_values_by_index, 2, table)
    sampled_adj_matrices_nonzero_value_fractional_occurrence_by_index <- data.frame(apply(sampled_adj_matrices_nonzero_value_occurrence_by_index, 2, function(col) col/sum(col)))

    diff_in_fractional_occurrences <- abs(sampled_adj_matrices_nonzero_value_fractional_occurrence_by_index - input_value_fractional_occurrences_by_index)
    error_in_fractional_occurrences <- colSums(diff_in_fractional_occurrences)

    if (any(error_in_fractional_occurrences > 0.05)) {
      rebuild_fcmconfr_models <- TRUE
    } else {
      rebuild_fcmconfr_models <- FALSE
    }
  } else if (sampling == "uniform") {
    sampled_means <- apply(sampled_adj_matrices_nonzero_values_by_index, 2, mean)
    expected_means <- apply(input_adj_matrices_nonzero_values_by_index, 2, function(col) (min(col) + max(col))/2)
    percent_diff_in_means <- abs(sampled_means - expected_means)/expected_means

    sampled_variances <- apply(sampled_adj_matrices_nonzero_values_by_index, 2, var)
    expected_variances <- apply(input_adj_matrices_nonzero_values_by_index, 2, function(col) ((max(col) - min(col))^2)/12)
    percent_diff_in_variances <- abs(sampled_variances - expected_variances)/expected_variances

    if (any(percent_diff_in_means > 0.05) | any(percent_diff_in_variances > 0.3)) {
      fcmconfr_models_are_representative_of_population <- FALSE
    } else {
      fcmconfr_models_are_representative_of_population <- TRUE
    }
  } else if (sampling == "triangular") {
    expected_means <- apply(input_adj_matrices_nonzero_values_by_index, 2, function(col) (min(col) + max(col))/2)
    sampled_means <- apply(sampled_adj_matrices_nonzero_values_by_index, 2, mean)
    percent_diff_in_means <- abs((expected_means - sampled_means)/expected_means)

    expected_variances <- apply(input_adj_matrices_nonzero_values_by_index, 2,
                                function(col) {
                                  a <- min(col)
                                  b <- max(col)
                                  c <- mean(col)
                                  (a^2 + b^2 + c^2 - a*b - a*c - b*c)/18
                                })
    sampled_variances <- apply(sampled_adj_matrices_nonzero_values_by_index, 2,
                               function(col) {
                                 a <- min(col)
                                 b <- max(col)
                                 c <- mean(col)
                                 (a^2 + b^2 + c^2 - a*b - a*c - b*c)/18
                               })
    percent_diff_in_variances <- abs((expected_variances - sampled_variances)/expected_variances)

    if (any(percent_diff_in_means > 0.05) | any(percent_diff_in_variances > 0.5)) {
      fcmconfr_models_are_representative_of_population <- FALSE
    } else {
      fcmconfr_models_are_representative_of_population <- TRUE
    }
  }

  fcmconfr_models_are_representative_of_population
}


#' infer_fcm
#'
#' @description
#' This confers with a baseline simulation of an FCM and a scenario (scenario vector)
#' to estimate how outputs change compared to the structural or expected behavior
#' of the system.
#'
#' @details
#' This function performs two fcm simulations and compares the output between the two.
#' The first simulation considers the baseline activity where no nodes are "clamped" and the
#' system behaves without any outside inputs. The second simulation considers a scenario where
#' one or multiple nodes are "clamped" so that the system is reactive to additional inputs.
#' The function returns the difference in simulation results between the scenario and baseline
#' activity to understand how system manipulations compare to structural expectations of the system.
#'
#' This function produces the same output as mental modeler for the following inputs:
#'  - initial_state_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' Use vignette("fcm-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param lambda_optimization A lambda optimization procedure to apply. Must be one
#' of the following: 'none' or 'koutsellis'
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @export
infer_fcm <- function(adj_matrix = matrix(),
                       initial_state_vector = c(),
                       clamping_vector = c(),
                       activation = "kosko", # Problems when activation == "rescale",
                       squashing = "tanh",
                       lambda = 1,
                       max_iter = 100,
                       min_error = 1e-5,
                       lambda_optimization = "none", # Verify this function works
                       IDs = c()) {

  # Add for R CMD Check. Does not impact logic.
  iter <- NULL

  confirm_adj_matrix_is_square(adj_matrix)

  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
    initial_state_vector <- rep(1, nrow(adj_matrix))
  }

  if (identical(clamping_vector, c())) {
    warning("No clamping_vector input given. Assuming no values are clamped.")
    clamping_vector <- rep(0, length(initial_state_vector))
  }

  # Get baseline simulation
  baseline_initial_state_vector <- rep(1, length(initial_state_vector))
  baseline_clamping_vector <- rep(0, length(clamping_vector))
  baseline_simulation <- simulate_fcm(adj_matrix,
                                      baseline_initial_state_vector, baseline_clamping_vector,
                                      activation, squashing, lambda,
                                      max_iter, min_error, lambda_optimization,
                                      IDs)

  # Get scenario simulation
  scenario_initial_state_vector <- initial_state_vector
  scenario_clamping_vector <- clamping_vector
  scenario_simulation <- simulate_fcm(adj_matrix,
                                      scenario_initial_state_vector, scenario_clamping_vector,
                                      activation, squashing, lambda,
                                      max_iter, min_error, lambda_optimization,
                                      IDs)

  n_iters_baseline <- nrow(baseline_simulation$state_vectors)
  n_iters_scenario <- nrow(scenario_simulation$state_vectors)

  if (n_iters_baseline == n_iters_scenario) {
    baseline_state_vectors <- baseline_simulation$state_vectors
    scenario_state_vectors <- scenario_simulation$state_vectors
  } else if (n_iters_baseline < n_iters_scenario) {
    extended_baseline_simulation_state_vectors <- data.frame(apply(
      baseline_simulation$state_vectors, 2, function(sim) {
        c(sim, rep(sim[n_iters_baseline], n_iters_scenario - n_iters_baseline))
      }
    ))
    baseline_state_vectors <- extended_baseline_simulation_state_vectors
    scenario_state_vectors <- scenario_simulation$state_vectors
  } else if (n_iters_scenario < n_iters_baseline) {
    extended_scenario_simulation_state_vectors <- data.frame(apply(
      scenario_simulation$state_vectors, 2, function(sim) {
        c(sim, rep(sim[n_iters_scenario], n_iters_baseline - n_iters_scenario))
      }
    ))
    baseline_state_vectors <- baseline_simulation$state_vectors
    scenario_state_vectors <- extended_scenario_simulation_state_vectors
  }

  inference_state_vectors <- data.frame(scenario_state_vectors - baseline_state_vectors)
  inference_state_vectors$iter <- 0:(nrow(inference_state_vectors) - 1)
  rownames(inference_state_vectors) <- 0:(nrow(inference_state_vectors) - 1)

  inference_values <- inference_state_vectors[nrow(inference_state_vectors),]
  inference_values <- subset(inference_values, select = -c(iter))
  rownames(inference_values) <- 1

  inference_plot_data <- data.frame(
    node = colnames(inference_values),
    value = unlist(inference_values)
  )

  structure(
    .Data = list(
      inference = inference_values,
      inference_for_plotting = inference_plot_data,
      inference_state_vectors = inference_state_vectors,
      scenario_simulation = scenario_simulation,
      baseline_simulation = baseline_simulation
    ),
    class = "fcmconfer"
  )
}


#' simulate_fcm
#'
#' @description
#' This calculates a sequence of iterations of a simulation over an fcm object
#' given an initial state vector along with the activation, squashing, and lambda
#' parameters. Additional variables may be defined to control simulation length,
#' column names, and lambda optimization.
#'
#' @details
#' This simulates how an fcm reacts to an input initial state vector. There is a
#' multi-decadal long body of work that has explored numerous activation and squashing
#' functions as well as algorithms to optimize the lambda value for the
#' sigmoid and tanh squashing functions.
#'
#' Use vignette("fcm-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param lambda_optimization A lambda optimization procedure to apply. Must be one
#' of the following: 'none' or 'koutsellis'
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @export
simulate_fcm <- function(adj_matrix = matrix(),
                          initial_state_vector = c(),
                          clamping_vector = c(),
                          activation = "kosko", # Problems when activation == "rescale",
                          squashing = "tanh",
                          lambda = 1,
                          max_iter = 100,
                          min_error = 1e-5,
                          lambda_optimization = "none", # Verify this function works
                          IDs = c()) {

  confirm_adj_matrix_is_square(adj_matrix)

  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
   initial_state_vector <- rep(1, nrow(adj_matrix))
  }

  if (identical(clamping_vector, c())) {
    warning("No clamping_vector input given. Assuming no values are clamped.")
    clamping_vector <- rep(0, length(initial_state_vector))
  }

  if (lambda_optimization != "none") {
    lambda <- optimize_fcm_lambda(adj_matrix, squashing, lambda_optimization)
  }

  confirm_initial_state_vector_is_compatible_with_adj_matrix(adj_matrix, initial_state_vector)
  IDs <- get_node_IDs_from_input(adj_matrix, IDs)

  state_vectors <- data.frame(matrix(data = numeric(), nrow = max_iter + 1, ncol = length(initial_state_vector)))

  errors <-  data.frame(matrix(data = numeric(), nrow = max_iter, ncol = length(initial_state_vector)))

  state_vectors[1, ] <- initial_state_vector
  errors[1, ] <- 0

  for (i in 2:(max_iter + 1)) {
    state_vector <- state_vectors[i - 1, ]
    next_state_vector <- calculate_next_fcm_state_vector(adj_matrix, state_vector, activation, squashing)
    normalized_state_vector <- squash(next_state_vector, squashing = squashing, lambda = lambda)
    normalized_state_vector[clamping_vector != 0] <- clamping_vector[clamping_vector != 0]
    state_vectors[i, ] <- normalized_state_vector
    errors[i, ] <- abs(as.matrix(state_vectors[i - 1,]) - as.matrix(state_vectors[i, ]))
    total_error <- sum(errors[i, ])
    if (total_error < min_error) {
      state_vectors <- stats::na.omit(state_vectors)
      errors <- stats::na.omit(errors)
      break
    }
  }
  if (i >= max_iter) {
    warning(
      "\tThe simulation reached the maximum number of iterations before
        achieving the minimum allowable error. This may signal that
        the fcm has reached a limit-cycle or is endlessly chaotic.

        It is also possible that the fcm simply requires more iterations
        to converge within the input minimum error.

        Try increasing the max_iter or min_error inputs."
    )
  }

  colnames(state_vectors) <- IDs
  colnames(errors) <- IDs

  state_vectors <- cbind(iter = 0:(nrow(state_vectors) - 1), state_vectors)
  errors <- cbind(iter = 0:(nrow(errors) - 1), errors)

  structure(
    .Data = list(
      state_vectors = state_vectors,
      errors = errors,
      params = list(
        adj_matrix = adj_matrix,
        initial_state_vector = initial_state_vector,
        activation = activation,
        squashing = squashing,
        lambda = lambda,
        max_iter = max_iter,
        min_error = min_error,
        lambda_optimization = lambda_optimization,
        IDs = IDs
      )
    ),
    class = "fcm_simulation"
  )
}


#' calculate_next_fcm_state_vector
#'
#' @description
#' This calculates the next iteration of a state vector in an fcm simulation
#' based on the kosko, modified-kosko, or rescale activation functions
#'
#' @details
#' The state of the art of fcm typically applies one of three activation functions
#' in calculating iterative state vector values: kosko, modified-kosko, and
#' rescale (as identified in Gonzales et al. 2018 - https://doi.org/10.1142/S0218213018600102).
#'
#' kosko: Only considers the current iteration (Kosko, 1986 - https://doi.org/10.1016/S0020-7373(86)80040-2)
#'
#' modified-kosko: The previous value of a node influences its future value (Stylio & Groumpos, 2004 - https://doi.org/10.1109/TSMCA.2003.818878)
#'
#' rescale: Like modified-kosko, but assigns nodes with no value with a
#' value of 0.5 to reduce the influence that a lack of initial state information
#' can have on the simulation output (rescale, 2011 - https://doi.org/10.1016/j.asoc.2009.12.010)=
#'
#' Use vignette("fcm-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#'
#' @export
calculate_next_fcm_state_vector <- function(adj_matrix = matrix(), state_vector = c(), activation = "modified-kosko", squashing = "sigmoid") {
  adj_matrix <- as.matrix(adj_matrix)
  state_vector <- as.matrix(state_vector)

  if (dim(state_vector)[2] != unique(dim(adj_matrix))) {
    state_vector <- t(state_vector)
  }

  if (activation == "kosko") {
    next_state_vector <- state_vector %*% adj_matrix
  } else if (activation == "modified-kosko") {
    next_state_vector <- state_vector %*% adj_matrix + state_vector
  } else if (activation == "rescale") {
    if (squashing != "sigmoid") {
      stop(
        paste0(
          "     !!!Please use the sigmoid squashing function with the rescale activation function!!!

          The rescale activation function is designed to optimize performance
          with the sigmoid squashing function. Results are unreliable if
          using a different squashing function.\n",

          "\n          Input squashing function: ", squashing)
      )
    }
    next_state_vector <- (2*state_vector - 1) %*% adj_matrix + (2*state_vector - 1)
  }
  next_state_vector
}


#' optimize_fcm_lambda
#'
#' @description
#' This calculates optimum lambda value for the sigmoid and tanh squashing
#' function that guarantees convergence of the simulation
#'
#' @details
#' This applies an algorithm to optimize lambda. Currently, the author only
#' identifies one such algorithm, but generalizes the function to leave flexibility
#' for the addition of newly-discovered algorithms in the future.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param squashing A squashing function to apply. Must be one of the following: 'tanh', or 'sigmoid'.
#' @param method An algorithm of which to optimize lambda with. Must be one of the following: "koutsellis" or 'none'
#' if the user does not want to optimize lambda and use the user-defined lambda instead.
#'
#' koutsellis: This algorithm was first explored in Kottas et al. 2010 (https://doi.org/10.1007/978-3-642-03220-2_5),
#' expanded upon in Koutsellis et al. 2022 (https://doi.org/10.1007/s12351-022-00717-x), and
#' further further developed in Koutsellis et al. 2022 (https://doi.org/10.1109/IISA56318.2022.9904369).
#'
#' It estimates lambda such that the 'squashed' values will be contained within
#' the near-linear region of the sigmoid or tanh function, and then re-normalizes
#' those values back to the total possibility spaces of those functions ([0, 1]
#' and [-1, 1] respectively).
#'
#' @export
optimize_fcm_lambda <- function(adj_matrix = matrix(), squashing = "sigmoid", method = "koutsellis") {
  if (squashing != "sigmoid" & squashing != "tanh") {
    stop("Invalid squashing function. Input squashing must be one of the following: 'sigmoid' or 'tanh'")
  }

  if (method == "none") {
    NULL # do nothing
  } else if (method == "koutsellis") {
    # The get_adj_matrix_from_edgelist() function mimics the outputs of mentalmodeler
    # and igraph. However, it is possible for the transpose of an adjacency matrix
    # to represent the same edgelist if it is understood that the source-target
    # axes are switched. In their paper, Koutsellis et al. use the transpose
    # of the matrix that mentalmodeler and igraph implement, so we incorporate that
    # here.
    adj_matrix <- t(adj_matrix)
    input_node_locs <- which(colSums(adj_matrix) == 0)
    adj_matrix_has_input_only_nodes <- length(input_node_locs) > 0
    if (adj_matrix_has_input_only_nodes) {
      extended_adj_matrix <- as.matrix(adj_matrix[, -input_node_locs])
    } else {
      extended_adj_matrix <- as.matrix(adj_matrix)
    }

    frobenius_norm_of_extended_adj_matrix <- norm(extended_adj_matrix, type = "2") # ||W||_F; The authors use "2" over "F" so replicating here
    if (squashing == "sigmoid") {
      s_norm_of_extended_adj_matrix <- max( # As defined in Koutsellis et al. 2022 (https://doi.org/10.1007/s12351-022-00717-x)
        apply(adj_matrix, 1, function(row) {
          max(
            abs(0.211*sum(row[row > 0]) + 0.789*sum(row[row < 0])),
            abs(0.211*sum(row[row < 0]) + 0.789*sum(row[row > 0]))
          )
        })
      )
      lambda_prime <- 4/frobenius_norm_of_extended_adj_matrix
      lambda_star <- 1.317/s_norm_of_extended_adj_matrix
    } else if (squashing == "tanh") {
      infinity_norm_of_extended_adj_matrix <- norm(extended_adj_matrix, type = "I") # ||W||_inf
      lambda_prime <- 1/frobenius_norm_of_extended_adj_matrix
      lambda_star <- 1.14/infinity_norm_of_extended_adj_matrix
    } else {
      stop("Invalid squashing input. Must be either 'sigmoid' or 'tanh'")
    }

    minimum_lambda <- min(lambda_prime, lambda_star)

    # "For the sake of simplicity, we propose as close to infimum 𝜆 value, which
    # is derived after rounding the final bound of Eq. (21) or Eq. (22) at the
    # third decimal digit." (Koutsellis et al. 2021 - https://doi.org/10.1007/s12351-022-00717-x)
    optimized_lambda <- round(minimum_lambda, digits = 3)

    optimized_lambda
  } else {
    stop("Unable to interpret input method. Must be one of the following: 'koutsellis' or 'none'")
  }
}


#' normalize_state_vector_with_optimized_lambda
#'
#' @description
#' This calculates the normalized value of a state back into its originating
#' squashing function's domain rather than the one forced by the optimized lambda
#'
#' @details
#' This
#'
#' @param raw_state The dot product of the state vector by a column vector of an adjacency matrix
#' @param squashed_state The output of a squashing function with the input raw state value
#' @param squashing A squashing function to apply. Must be one of the following: 'tanh', or 'sigmoid'.
#' @param optimized_lambda The optimized lambda calculated by optimize_fcm_lambda
#' @param method An algorithm of which to optimize lambda with. Must be one of the following: "koutsellis" or 'none'
#' if the user does not want to optimize lambda and use the user-defined lambda instead.
#'
#' koutsellis: This algorithm was developed in Koutsellis et al. 2022 (https://doi.org/10.1109/IISA56318.2022.9904369).
#'
#' @export
normalize_state_vector_with_optimized_lambda <- function(raw_state = numeric(),
                                                         squashed_state = numeric(),
                                                         squashing = "sigmoid",
                                                         optimized_lambda = numeric(),
                                                         method = "none") {
  if (method == "koutsellis") {
    if (squashing == "sigmoid") {
      normalized_state <- squashed_state + 0.09*optimized_lambda*raw_state
    } else if (squashing == "tanh") {
      normalized_state <- 1.733*squashed_state
    } else {
      stop("Invalid squashing input. Must be either 'sigmoid' or 'tanh'")
    }
  } else {
    stop("Invalid method. Must be either 'none' or 'koutsellis'")
  }

  normalized_state
}


#' confirm_initial_state_vector_is_compatible_with_adj_matrix
#'
#' @description
#' Confirm that an initial state vector is algorithmically compatible with an adjacency matrix
#'
#' @details
#' Boolean. TRUE if the number of entries in the initial
#' state vector match the number of rows/columns in the adjacency matrix and 2. The
#' datatypes stored within each object are the same (i.e. "numeric" vs "grey_number"),
#' FALSE if not
#'
#' Intended for developer use only to improve package readability.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector An n-length list of the initial states of each node in an fcm simulation
confirm_initial_state_vector_is_compatible_with_adj_matrix <- function(adj_matrix = matrix(), initial_state_vector = c()) {
  if (length(initial_state_vector) != unique(dim(adj_matrix))) {
    stop("Length of input initial_state_vector is does not comply with the dimensions of the input adjacency matrix", .call = FALSE)
  } else {
    TRUE
  }
}



#' fcm (fuzzy cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for ordinary fuzzy cognitive maps (See
#' Kosko, XXXX for example). It stores the nodes of an FCM and its
#' corresponding adjacency matrix and edgelist.
#'
#' @details
#' fcm stores fcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' Use vignette("fcm-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param IDs A list of names for each node (must have n items)
#'
#' @export
#' @examples
#' fcm(adj_matrix = matrix(data = c(0, 1, 1, 0), nrow = 2, ncol = 2))
fcm <- function(adj_matrix = matrix(), IDs = c()) {
  # Validate input
  confirm_adj_matrix_is_square(adj_matrix)
  IDs <- get_node_IDs_from_input(adj_matrix, IDs)

  data_types <- unique(vapply(adj_matrix, class, character(1)))
  only_numeric_data_types <- identical(data_types, "numeric")
  if (!only_numeric_data_types) {
    stop("Input adjacency matrix must only contain numeric objects, and all
         objects must be numeric")
  }

  structure(
    .Data = list(
      concepts = IDs,
      adj_matrix = adj_matrix,
      edgelist = get_edgelist_from_adj_matrix(adj_matrix)
    ),
    class = "fcm"
  )
}



#' Construct an aggregate fcm from a group (list) of fcms
#'
#' @description Construct the aggregate fcm from a group (list)
#' of fcms. Via mean or median.
#'
#' @details Add details here
#'
#' @param adj_matrices A list type object of fcms. Must have a length greater
#' than 1.
#' @param aggregation_fun "mean" or "median"
#' @param include_zeroes TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#' @param IDs A list of names for each node (must have n items)
#'
#' @return A single fcm calculated as the aggregate of the input adjacency matrices
#' @export
aggregate_fcm <- function(adj_matrices = list(), aggregation_fun = "mean", include_zeroes = TRUE, IDs = c()) {
  # error checks ----
  concepts_in_fcms <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x, IDs))
  all_fcms_have_same_concepts <- length(unique(concepts_in_fcms)) == 1
  if (!all_fcms_have_same_concepts) {
    stop("All adjacency matrices must have the same concepts.")
  }

  dimensions_of_input_adj_matrices <- lapply(adj_matrices, dim)
  all_fcms_have_same_dimensions <- length(unique(dimensions_of_input_adj_matrices)) == 1
  if (!all_fcms_have_same_dimensions) {
    stop("All adjacency matrices must have the same dimensions (n x n) throughout the entire list")
  }

  # Check that adj_matrices are correct format
  lapply(adj_matrices, function(x) fcm(x, IDs))

  # function ----
  node_names <- unlist(unique(concepts_in_fcms))
  n_nodes <- length(node_names)
  n_maps <- length(adj_matrices)

  if (!include_zeroes) {
    adj_matrices <- lapply(adj_matrices, function(x) replace(x, x == 0, NA))
  }

  if (aggregation_fun == "mean") {
    aggregate_adj_matrix <- apply(
      array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps)), 1:2,
      function(x) mean(x, na.rm = TRUE)
    )
  } else if (aggregation_fun == "median") {
    aggregate_adj_matrix <- apply(
      array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps)), 1:2,
      function(x) stats::median(x, na.rm = TRUE)
    )
  }

  aggregate_adj_matrix[is.na(aggregate_adj_matrix)] <- 0

  colnames(aggregate_adj_matrix) <- node_names
  rownames(aggregate_adj_matrix) <- node_names

  structure(
    .Data = list(
      adj_matrix = as.data.frame(aggregate_adj_matrix),
      params = list(
        input_adj_matrices = adj_matrices,
        aggregation_fun = aggregation_fun,
        IDs = IDs
      )
    ),
    class = "aggregate"
  )
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
        paste0(" -     mean_CI_by_node: ", x$params$bootstrap_output_opts$bootstrap_CI, "% CI of means of inference\n"),
        paste0(" - raw_bootstrap_means: ", x$params$bootstrap_output_opts$bootstrap_reps, " actualizations of the avg inference of ", x$params$bootstrap_output_opts$bootstrap_draws_per_rep, " draws with replacement"),
        "\n$params\n",
        " -      inference_opts:",
        paste0("act = ", x$params$inference_opts$activation, "; squash = ", x$params$inference_opts$squashing, "; lambda = ", x$params$inference_opts$lambda),
        "\n  -       bootstrap_input_opts:",
        paste0("sampling = ", x$params$bootstrap_input_opts$sampling, "; n_samples = ", x$params$bootstrap_input_opts$samples)
    )
  } else {
    cat("$inference",
        paste0("Inferences of ", n_sims, " fcm constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " -   inference_opts:",
        paste0("act = ", x$params$inference_opts$activation, "; squash = ", x$params$inference_opts$squashing, "; lambda = ", x$params$inference_opts$lambda),
        "\n  -    bootstrap_input_opts:",
        paste0("sampling = ", x$params$bootstrap_input_opts$sampling, "; n_samples = ", x$params$bootstrap_input_opts$samples)
    )
  }

  # average_inference <- data.frame(value = round(apply(x$inference, 2, mean), 3))
  # average_inference$node <- rownames(average_inference)
  # largest_three_average_inferences <- sort(average_inference$value, decreasing = TRUE)[1:3]
  # average_inference$node[vapply(largest_three_average_inferences, function(rank_value) which(average_inference$value == rank_value), numeric(1))]
  #
  # rounded_inferences <- apply(x$inference, c(1, 2), function(value) round(value, 2))

}


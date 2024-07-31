


#' infer_fmcm_with_clamping
#'
#' @description
#' This calculates a sequence of iterations of a simulation over every item in
#' a list of fmcm objects given an initial state vector along with the
#' activation, squashing, and lambda parameters.
#' Additional variables may be defined to control simulation length,
#' column names, and lambda optimization.
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param simulated_adj_matrices A list of adjecency matrices generated from simulation using build_fmcm_models.
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
#' @param lambda_optimization A lambda optimization procedure to apply. Must be one
#' of the following: 'none' or 'koutsellis'
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' FCM. Will dramatically increase size of output if TRUE.
#'
#' @export
infer_fmcm_with_clamping <- function(simulated_adj_matrices = list(matrix()),
                                     initial_state_vector = c(),
                                     clamping_vector = c(),
                                     activation = c("kosko", "modified-kosko", "rescale"),
                                     squashing = c("sigmoid", "tanh"),
                                     lambda = 1,
                                     max_iter = 100,
                                     min_error = 1e-5,
                                     IDs = c(),
                                     parallel = TRUE,
                                     n_cores = integer(),
                                     show_progress = TRUE,
                                     include_simulations_in_output = FALSE) {

  # Adding for R CMD check. Does not impact logic.
  # iter <- NULL
  i <- NULL

  if (identical(activation, c("kosko", "modified-kosko", "rescale"))) {
    activation <- "kosko"
    warning("No activation input declared. Assuming activation = 'kosko'")
  }
  if (!(activation %in% c("kosko", "modified-kosko", "rescale"))) {
    stop("Activation input must be one of the following: 'kosko', 'modified-kosko', 'rescale'")
  }

  if (identical(squashing, c("sigmoid", "tanh"))) {
    activation <- "sigmoid"
    warning("No squashing input declared. Assuming squashing = 'sigmoid'")
  }
  if (!(squashing %in% c("sigmoid", "tanh"))) {
    stop("Squashing input must be one of the following: 'sigmoid', 'tanh'")
  }

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  lapply(simulated_adj_matrices, confirm_adj_matrix_is_square)

  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
    initial_state_vector <- rep(1, nrow(simulated_adj_matrices[[1]]))
  }

  if (identical(clamping_vector, c())) {
    warning("No clamping_vector input given. Assuming no values are clamped.")
    clamping_vector <- rep(0, length(initial_state_vector))
  }

  if (parallel & show_progress) {
    print("Initializing cluster", quote = FALSE)
    max_possible_cores <- parallel::detectCores()
    if (identical(n_cores, integer())) {
      n_cores <- max_possible_cores
    }
    if (n_cores > max_possible_cores) {
      warning(paste0(" Input n_cores is greater than the available cores on this machine.\n Reducing to ", n_cores))
    }
    cl <- parallel::makeCluster(n_cores)

    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("simulated_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
                 "squashing", "lambda", "max_iter", "min_error", "IDs",
                 "infer_fcm_with_clamping", "simulate_fcm_with_pulse",  "confirm_adj_matrix_is_square",
                 "confirm_initial_state_vector_is_compatible_with_adj_matrix",
                 "get_node_IDs_from_input", "optimize_fcm_lambda",
                 "calculate_next_fcm_state_vector", "squash")

    parallel::clusterExport(cl, varlist = vars, envir = environment())

    doSNOW::registerDoSNOW(cl)
    # pb <- utils::txtProgressBar(min = 0, max = length(simulated_adj_matrices)/n_cores, style = 3)
    invisible(utils::capture.output(pb <- utils::txtProgressBar(min = 0, max = length(simulated_adj_matrices)/n_cores, style = 3)))
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    # cat("\n")
    print("Running simulations. There may be an additional wait for larger sets.", quote = FALSE)
    opts <- list(progress = progress)
    fmcm_confer_results <- foreach::foreach(
      i = 1:length(simulated_adj_matrices), .options.snow = opts) %dopar% {
        infer_fcm_with_clamping(
          adj_matrix = simulated_adj_matrices[[i]],
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = 'none',
          IDs = IDs
        )
      }
    close(pb)
    names(fmcm_confer_results) <- paste0("mc_", 1:length(fmcm_confer_results))
    parallel::stopCluster(cl)

  } else if (parallel & !show_progress) {
    print("Initializing cluster", quote = FALSE)
    max_possible_cores <- parallel::detectCores()
    if (identical(n_cores, integer())) {
      n_cores <- max_possible_cores
    }
    if (n_cores > max_possible_cores) {
      warning(paste0(" Input n_cores is greater than the available cores on this machine.\n Reducing to ", n_cores))
    }
    cl <- parallel::makeCluster(n_cores)

    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("simulated_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
                 "squashing", "lambda", "max_iter", "min_error",
                 "lambda_optimization", "IDs",
                 "infer_fcm_with_clamping", "simulate_fcm_with_pulse",  "confirm_adj_matrix_is_square",
                 "confirm_initial_state_vector_is_compatible_with_adj_matrix",
                 "get_node_IDs_from_input", "optimize_fcm_lambda",
                 "calculate_next_fcm_state_vector", "squash")

    parallel::clusterExport(cl, varlist = vars, envir = environment())

    cat("\n")
    print("Running simulations", quote = FALSE)
    fmcm_confer_results <- parallel::parLapply(
      cl,
      simulated_adj_matrices,
      function(simulated_adj_matrix) {
        infer_fcm_with_clamping(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = 'none',
          IDs = IDs
        )
      }
    )
    parallel::stopCluster(cl)

  } else if (!parallel & show_progress) {
    cat("\n")
    print("Running simulations", quote = FALSE)
    fmcm_confer_results <- pbapply::pblapply(
      simulated_adj_matrices,
      function(simulated_adj_matrix) {
        infer_fcm_with_clamping(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = 'none',
          IDs = IDs
        )
      }
    )

  } else if (!parallel & !show_progress) {
    cat("\n")
    print("Running simulations", quote = FALSE)
    fmcm_confer_results <- lapply(
      simulated_adj_matrices,
      function(simulated_adj_matrix) {
        infer_fcm_with_clamping(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = 'none',
          IDs = IDs
        )
      }
    )
  }

  # cat("\n")
  # print("Organizing Output", quote = FALSE)
  # cat("\n")

  inference_values_by_sim <- lapply(fmcm_confer_results, function(sim) sim$inference)
  inference_values_by_sim <- data.frame(do.call(rbind, inference_values_by_sim))

  inference_plot_data <- data.frame(
    node = rep(colnames(inference_values_by_sim), nrow(inference_values_by_sim)),
    value = unlist(lapply(t(inference_values_by_sim), c))
  )

  if (include_simulations_in_output) {
    structure(
      .Data = list(
        inference = inference_values_by_sim,
        inference_for_plotting = inference_plot_data,
        sims = fmcm_confer_results
      ),
      class = "fmcmconfer"
    )
  } else {
    structure(
      .Data = list(
        inference = inference_values_by_sim,
        inference_for_plotting = inference_plot_data
      ),
      class = "fmcmconfer"
    )
  }
}


#' get_quantile_of_fmcm_state_vectors_at_iter
#'
#' @description
#' This gets the user-input quantile of the distribution of simulated values
#' across a given iter, or all iters
#'
#' @details
#' This function is designed to streamline the process of getting the custom quantiles
#' of a distribution of simulated values across an individual iteration.
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param fmcm_inference Output of get_simulated_values_across_iters
#' @param quantile The quantile to return. see ?quantile() for more
#'
#' @export
get_quantile_of_fmcm_inference <- function(fmcm_inference = data.frame(), quantile = 0.5) {
  mc_in_inferences_rownames <- identical("mc", unique(unlist(lapply(strsplit(rownames(fmcm_inference), "_"), function(x) x[[1]]))))
  if (!mc_in_inferences_rownames | !identical(class(fmcm_inference), "data.frame")) {
    stop("Input must be a data frame of values observed for each node across
    numerous simulations. They are produced by the infer_fmcm_with_clamping")
  }
  node_quantile_values <- data.frame(t(apply(fmcm_inference, 2, function(node_sims) stats::quantile(node_sims, quantile))))
  node_quantile_values
  #node_names <- rownames(node_quantile_values)
  #colnames(node_quantile_values) <- node_names
  # node_quantiles = data.frame(
  #  "node" = node_names,
  #  "value" = node_quantile_values
  #)
  #colnames(node_quantiles)[2] <- paste0(quantile, "%_quantile")
  #rownames(node_quantiles) <- NULL
  #node_quantiles
}


#' get_means_of_fmcm_inference
#'
#' @description
#' This gets the mean of the distribution of simulated values
#' across a given iter. Also returns the bootstrapped mean of means of the
#' distribution of simulated values across a given iter if called.
#'
#' @details
#' This function is designed to streamline the process of getting the mean or bootstrapped
#' mean of means of a distribution of simulated values across individual iterations. Use get_bootstrapped_means
#' to estimate the confidence intervals for the mean value across simulations.
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param fmcm_inference The final values of a set of fcm simulations; also the inference of a infer_fmcm object
#' @param get_bootstrapped_means TRUE/FALSE Whether to perform bootstrap sampling to obtain
#' confidence intervals for the estimation of the mean value across simulations
#' @param confidence_interval What are of the distribution should be bounded by the
#' confidence intervals? (e.g. 0.95)
#' @param bootstrap_reps Repetitions for bootstrap process, if chosen
#' @param bootstrap_samples_per_rep Number of samples to draw (with replacement) from
#' the data per bootstrap_rep
#' @param parallel TRUE/FALSE Whether to perform the function using parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
get_means_of_fmcm_inference <- function(fmcm_inference = list(),
                                        get_bootstrapped_means = TRUE,
                                        confidence_interval = 0.95,
                                        bootstrap_reps = 1000,
                                        bootstrap_samples_per_rep = 1000,
                                        parallel = TRUE,
                                        n_cores = integer(),
                                        show_progress = TRUE) {
  # Adding for R CMD Check. Does not impact logic.
  iter <- NULL

  # Write checks to confirm fmcm_inference object is correct... Also write a better name
  # so it is understood that it works for simulate_fmcm objects too
  if (!identical(class(fmcm_inference), "data.frame")) {
    stop("Input fmcm_inference must be a data.frame object from the
         output of simulate_fmcm_models (final_states_across_sims) or infer_fmcm
         (inference)")
  }

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  if (!get_bootstrapped_means) {
    means_of_inference_by_node <- data.frame(apply(fmcm_inference, 2, mean, simplify = FALSE)) # the simplify is purely for data cleaning reasons
    return(means_of_inference_by_node)

  } else if (get_bootstrapped_means & parallel & show_progress) {
    print("Performing bootstrap simulations", quote = FALSE)
    print("Initializing cluster", quote = FALSE)
    if (identical(n_cores, integer())) {
      n_cores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(n_cores)
    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("fmcm_inference",
                 "bootstrap_reps",
                 "bootstrap_samples_per_rep"
    )
    parallel::clusterExport(cl, varlist = vars, envir = environment())
    print("Sampling means", quote = FALSE)
    doSNOW::registerDoSNOW(cl)
    invisible(utils::capture.output(pb <- utils::txtProgressBar(min = 0, max = ceiling(bootstrap_reps/n_cores), width = 50, style = 3)))
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    bootstrapped_means_of_inference_by_node <- foreach::foreach(
      i = 1:bootstrap_reps, .options.snow = opts) %dopar% {
        data.frame(apply(
          fmcm_inference, 2,
          function(inference) {
            random_draws <- sample(inference, bootstrap_samples_per_rep, replace = TRUE)
            mean(random_draws)
          },
          simplify = FALSE
        ))
      }
    close(pb)
    parallel::stopCluster(cl)

  } else if (get_bootstrapped_means & parallel & !show_progress) {
    print("Performing bootstrap simulations", quote = FALSE)
    print("Initializing cluster", quote = FALSE)
    if (identical(n_cores, integer())) {
      n_cores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(n_cores)
    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("fmcm_inference",
                 "bootstrap_reps",
                 "bootstrap_samples_per_rep"
    )
    parallel::clusterExport(cl, varlist = vars, envir = environment())
    print("Sampling means", quote = FALSE)
    rep_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- lapply(rep_inference_by_node, function(duplicate) duplicate <- fmcm_inference)
    bootstrapped_means_of_inference_by_node <- parallel::parLapply(
      cl,
      rep_inference_by_node,
      function(inference_by_node_duplicate) {
        apply(
          inference_by_node_duplicate, 2,
          function(inference) {
            random_draws <- sample(inference, bootstrap_samples_per_rep, replace = TRUE)
            mean(random_draws)
          }
        )
      }
    )
    parallel::stopCluster(cl)

  } else if (get_bootstrapped_means & !parallel & show_progress) {
    bootstrapped_means_of_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- lapply(rep_inference_by_node, function(duplicate) duplicate <- fmcm_inference)
    bootstrapped_means_of_inference_by_node <- pbapply::pblapply(
      rep_inference_by_node,
      function(inference_by_node_duplicate) {
        apply(
          inference_by_node_duplicate, 2,
          function(inference) {
            random_draws <- sample(inference, bootstrap_samples_per_rep, replace = TRUE)
            mean(random_draws)
          }
        )
      }
    )

  } else if (get_bootstrapped_means & !parallel & !show_progress) {
    rep_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- lapply(rep_inference_by_node, function(duplicate) duplicate <- fmcm_inference)
    bootstrapped_means_of_inference_by_node <- lapply(
      rep_inference_by_node,
      function(inference_by_node_duplicate) {
        apply(
          inference_by_node_duplicate, 2,
          function(inference) {
            random_draws <- sample(inference, bootstrap_samples_per_rep, replace = TRUE)
            mean(random_draws)
          }
        )
      }
    )
  }

  bootstrapped_means_of_inference_by_node <- data.frame(do.call(rbind, bootstrapped_means_of_inference_by_node))

  # print("Getting upper and lower quantile estimates of mean", quote = FALSE)
  lower_quantile <- (1 - confidence_interval)/2
  upper_quantile <- (1 + confidence_interval)/2
  lower_quantiles_by_node <- data.frame(apply(bootstrapped_means_of_inference_by_node, 2, function(bootstrapped_means) stats::quantile(bootstrapped_means, lower_quantile), simplify = FALSE))
  upper_quantiles_by_node <- data.frame(apply(bootstrapped_means_of_inference_by_node, 2, function(bootstrapped_means) stats::quantile(bootstrapped_means, upper_quantile), simplify = FALSE))

  nodes <- ifelse(colnames(lower_quantiles_by_node) == colnames(upper_quantiles_by_node), colnames(lower_quantiles_by_node), stop("Error with quantiles calculation"))
  quantiles_by_node <- vector(mode = "list", length = length(nodes))

  quantiles_by_node <- data.frame(
    node = nodes,
    lower_quantile = vector(mode = "numeric", length = length(nodes)),
    upper_quantile = vector(mode = "numeric", length = length(nodes))
  )
  for (i in seq_along(nodes)) {
    quantiles_by_node$lower_quantile[i] <- lower_quantiles_by_node[i][[1]] # not sure why this [[1]] is necessary but it is
    quantiles_by_node$upper_quantile[i] <- upper_quantiles_by_node[i][[1]] # not sure why this [[1]] is necessary but it is
  }
  colnames(quantiles_by_node) <- c("node", paste0("lower_", lower_quantile), paste0("upper_", upper_quantile))
  print("Done", quote = FALSE)

  structure(
    .Data = list(
      mean_CI_by_node = quantiles_by_node,
      bootstrap_means = bootstrapped_means_of_inference_by_node
    )
  )
}



#' build_conventional_fcmconfr_models
#'
#' @description
#' This function generates n fcm models whose edge weights are sampled from either
#' the defined edge values in a set of adjacency matrices derived from the sets
#' of edge values, and stores them as a list of adjacency matrices.
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fcmconfr-class") for more information.
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
build_conventional_fcmconfr_models <- function(adj_matrices = list(matrix()),
                                               sampling = "nonparametric", # 'nonparametric', 'uniform', or 'triangular'
                                               samples = integer(),
                                               show_progress = TRUE) {
  n_nodes <- unique(unlist(lapply(adj_matrices, dim)))
  n_maps <- length(adj_matrices)
  adj_matrices_as_arrays <- array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps))

  if (sampling == "nonparametric") {
    adj_matrix_of_samples_per_edge <- apply(adj_matrices_as_arrays, c(1, 2), function(x) sample(x, samples, replace = TRUE), simplify = FALSE)
    wider_adj_matrix_of_samples_per_edge <- apply(array(adj_matrix_of_samples_per_edge, c(1, n_nodes^2)), 2, unlist)
    sampled_adj_matrices <- apply(wider_adj_matrix_of_samples_per_edge, 1, function(x) array(x, c(n_nodes, n_nodes)), simplify = FALSE)
  } else if (sampling == "uniform") {
    min_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), min)
    max_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), max)
    empirical_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(min_adj_matrix, max_adj_matrix)
    empirical_grey_adj_matrix_with_distributions <- get_unconventional_adj_matrices_with_distributions(empirical_grey_adj_matrix, samples, include_zeroes = TRUE)
    sampled_adj_matrices <- build_models_from_adj_matrices_with_unconventional_values_as_distributions(empirical_grey_adj_matrix_with_distributions, "mean") # The mean of a single value is itself so okay to ignore the "mean" call; just for syntax
  } else if (sampling == "triangular") {
    min_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), min)
    max_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), max)
    mode_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), mean)
    empirical_triangular_adj_matrix <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(min_adj_matrix, mode_adj_matrix, max_adj_matrix)
    empirical_triangular_adj_matrix_with_distributions <- get_unconventional_adj_matrices_with_distributions(empirical_triangular_adj_matrix, samples, include_zeroes = TRUE)
    sampled_adj_matrices <- build_models_from_adj_matrices_with_unconventional_values_as_distributions(empirical_triangular_adj_matrix_with_distributions, "mean") # The mean of a single value is itself so okay to ignore the "mean" call; just for syntax
  }

  sampled_adj_matrices
}




#' build_unconventional_fcmconfr_models
#'
#' @description
#' This function generates n fcm adjacency matrices whose edge weights are sampled
#' from edge values (that may be numeric, grey_numbers, or triangular_numbers) and
#' stores them as a list of adjacency matrices.
#'
#' @details
#' If an edge is represented by multiple grey/triangular_numbers, then those distributions
#' are averaged together to create the aggregate distribution to sample from.
#'
#' Use vignette("fcmconfr-class") for more information.
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
build_unconventional_fcmconfr_models <- function(unconventional_adj_matrix_list,
                                                 aggregation_fun,
                                                 samples,
                                                 include_zeroes = FALSE,
                                                 show_progress = TRUE) {

  if (!is.null(dim(unconventional_adj_matrix_list)) & (length(unique(dim(unconventional_adj_matrix_list))) == 1)) {
    unconventional_adj_matrix_list <- list(unconventional_adj_matrix_list)
  }

  unconventional_adj_matrices_with_special_values_as_distributions <- get_unconventional_adj_matrices_with_distributions(
    unconventional_adj_matrix_list,
    samples = samples,
    include_zeroes = include_zeroes
  )

  unconventional_fcmconfr_models <- build_models_from_adj_matrices_with_unconventional_values_as_distributions(
    unconventional_adj_matrices_with_special_values_as_distributions,
    aggregation_fun = aggregation_fun,
    show_progress = show_progress
  )

  unconventional_fcmconfr_models
}



#' get_unconventional_value_as_distribution
#'
#' @description
#' Convert a grey/triangular number into a list of values sampled from a pdf of the
#' corresponding distribution (uniform for grey_numbers and triangular for triangular_numbers)
#'
#' @details
#' [ADD DETAILS HERE!!!!]
#'
#' Use vignette("fcmconfr-class") for more information.
#'
#' @param value A grey_number or triangular_number object
#' @param n_samples The number of samples drawn from the representative pdf
#' @param value_class The class of the input value. Either grey_number or triangular_number. Can be left blank.
#'
#' @export
get_unconventional_value_as_distribution <- function(value = numeric(), n_samples = 1000, value_class = "") {
  class_of_value <- class(value)
  if (value_class == "") {
    value_class <- class_of_value
  } else if (class_of_value != value_class) {
    stop("Input value_class does not match the class of the input value")
  }

  if (value_class == "grey_number") {
    distribution <- runif(n = n_samples, min = value$lower, max = value$upper)
  } else if (value_class == "triangular_number") {
    distribution <- get_triangular_distribution_of_values(n = n_samples, lower = value$lower, mode = value$mode, upper = value$upper)
  } else if (!(value_class %in% c("grey_number", "triangular_number"))) {
    stop("Input value_class must be one of the following: 'grey_number', 'triangular_number'")
  }

  distribution
}



#' get_unconventional_adj_matrices_with_distributions
#'
#' @description
#' Given a list of adjacency matrices which include either grey_numbers or
#' triangular_numbers, convert those objects to their corresponding
#' distributions representative of those values.
#'
#' @details
#' [ADD DETAILS HERE!!!!]
#'
#' Use vignette("fcmconfr-class") for more information.
#'
#' @param unconventional_adj_matrices A list of adjacency matrices with numeric and
#' either grey_number or triangular_number elements
#' @param samples The number of samples drawn from the representative pdf
#' @param element_class The class of the unconventional elements in the list of
#' adjacency_matrices. Either grey_number or triangular_number.
#' @param include_zeroes TRUE/FALSE Whether or not to incorporate zeroes in
#' aggregation. If FALSE, zeroes are converted to NA values during aggregation and
#' returned to zero afterwards.
#'
#' @export
get_unconventional_adj_matrices_with_distributions <- function(unconventional_adj_matrix_list = list(matrix()),
                                                               samples = 1000,
                                                               include_zeroes = FALSE,
                                                               show_progress = TRUE) {

  if (!is.null(dim(unconventional_adj_matrix_list)) & (length(unique(dim(unconventional_adj_matrix_list))) == 1)) {
    unconventional_adj_matrix_list <- list(unconventional_adj_matrix_list)
  }

  element_class <- get_fcm_class_from_adj_matrices(unconventional_adj_matrix_list)
  if (element_class == "fcm") {
    element_class <- "numeric"
  } else if (element_class == "fgcm") {
    element_class <- "grey_number"
  } else if (element_class == "ftcm") {
    element_class <- "triangular_number"
  }

  possible_element_classes <- c("numeric", "grey_number", "triangular_number")
  if (!(element_class %in% possible_element_classes)) {
    stop(paste0("Input element_class must be one of the following: '", paste0(possible_element_classes, collapse = "', '"), "'"))
  }

  data_types_in_adj_matrices <- unique(unlist(lapply(
    unconventional_adj_matrix_list,
    function(unconventional_adj_matrix) {
      apply(unconventional_adj_matrix, c(1, 2),
            function(element) class(element[[1]])
      )
    }
  )))

  if (!all(data_types_in_adj_matrices %in% possible_element_classes)) {
    stop(paste0("All elements in unconventional_adj_matrix_list input must be either
       of type 'numeric' or '", element_class, "'"))
  }

  if (show_progress) {
    print("Converting Values to Distributions where Appropriate", quote = FALSE)
    if (include_zeroes) {
      adj_matrices_with_unconventional_values_as_distributions <- pbapply::pblapply(
        unconventional_adj_matrix_list,
        function(unconventional_adj_matrix) {
          apply(unconventional_adj_matrix, c(1, 2),
                function(element) {
                  ifelse(
                    class(element[[1]]) == element_class,
                    yes = list(get_unconventional_value_as_distribution(value = element[[1]], value_class = element_class, n_samples = samples)),
                    no = list(rep(0, samples))
                  )
                })
        }
      )
    } else {
      adj_matrices_with_unconventional_values_as_distributions <- pbapply::pblapply(
        unconventional_adj_matrix_list,
        function(unconventional_adj_matrix) {
          apply(unconventional_adj_matrix, c(1, 2),
                function(element) {
                  ifelse(
                    class(element[[1]]) == element_class,
                    yes = list(get_unconventional_value_as_distribution(value = element[[1]], value_class = element_class, n_samples = samples)),
                    no = NA
                  )
                })
        }
      )
    }
  } else {
    if (include_zeroes) {
      adj_matrices_with_unconventional_values_as_distributions <- lapply(
        unconventional_adj_matrix_list,
        function(unconventional_adj_matrix) {
          apply(unconventional_adj_matrix, c(1, 2),
                function(element) {
                  ifelse(
                    class(element[[1]]) == element_class,
                    yes = list(get_unconventional_value_as_distribution(value = element[[1]], value_class = element_class, n_samples = samples)),
                    no = list(rep(0, samples))
                  )
                })
        }
      )
    } else {
      adj_matrices_with_unconventional_values_as_distributions <- lapply(
        unconventional_adj_matrix_list,
        function(unconventional_adj_matrix) {
          apply(unconventional_adj_matrix, c(1, 2),
                function(element) {
                  ifelse(
                    class(element[[1]]) == element_class,
                    yes = list(get_unconventional_value_as_distribution(value = element[[1]], value_class = element_class, n_samples = samples)),
                    no = NA
                  )
                })
        }
      )
    }
  }

  adj_matrices_with_unconventional_values_as_distributions

}



#' build_models_from_adj_matrices_with_unconventional_values_as_distributions
#'
#' @description
#' [ADD DETAILS HERE!!!!]
#'
#' @details
#' [ADD DETAILS HERE!!!!]
#'
#' Use vignette("fcmconfr-class") for more information.
#'
#' @param adj_matrices_with_unconventional_values_as_distributions A list of
#' adjacency matrices with numeric and either grey_number or triangular_number elements
#' represented as their corresponding distributions.
#' @param aggregation_fun Either mean or median. The function to aggregate
#' distributions representing the same edges across multiple adjacency matrices
#'
#' @export
build_models_from_adj_matrices_with_unconventional_values_as_distributions <- function(adj_matrices_with_unconventional_values_as_distributions = list(matrix()),
                                                                                       aggregation_fun = c("mean", "median"),
                                                                                       show_progress = TRUE) {

  n_nodes <- unique(unlist(lapply(adj_matrices_with_unconventional_values_as_distributions, function(x) unique(dim(x)))))
  adj_matrices_with_unconventional_values_as_distributions_by_index <- do.call(cbind, lapply(adj_matrices_with_unconventional_values_as_distributions, function(adj_matrix_with_unconventional_values_as_distributions) do.call(list, adj_matrix_with_unconventional_values_as_distributions)))

  if (show_progress) {
    print("Sampling edges", quote = FALSE)
    aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index <- pbapply::pbapply(
      adj_matrices_with_unconventional_values_as_distributions_by_index, 1,
      function(elements_in_same_locs_across_adj_matrices) {
        sum_of_corresponding_elements <- 0
        n_na_corresponding_elements <- 0
        if (all(lapply(elements_in_same_locs_across_adj_matrices, typeof) == "list")) {
          elements_in_same_locs_across_adj_matrices <- lapply(elements_in_same_locs_across_adj_matrices, unlist)
        }
        elements_in_same_locs_across_adj_matrices <- do.call(cbind, elements_in_same_locs_across_adj_matrices)
        if (aggregation_fun == "mean") {
          apply(elements_in_same_locs_across_adj_matrices, 1, mean, na.rm = TRUE)
        } else if (aggregation_fun == "median") {
          apply(elements_in_same_locs_across_adj_matrices, 1, stats::median, na.rm = TRUE)
        } else {
          stop("Input aggregation_fun must be either 'mean' or 'median'")
        }
      }
    )
  } else {
    aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index <- apply(
      adj_matrices_with_unconventional_values_as_distributions_by_index, 1,
      function(elements_in_same_locs_across_adj_matrices) {
        sum_of_corresponding_elements <- 0
        n_na_corresponding_elements <- 0
        if (all(lapply(elements_in_same_locs_across_adj_matrices, typeof) == "list")) {
          elements_in_same_locs_across_adj_matrices <- lapply(elements_in_same_locs_across_adj_matrices, unlist)
        }
        elements_in_same_locs_across_adj_matrices <- do.call(cbind, elements_in_same_locs_across_adj_matrices)
        if (aggregation_fun == "mean") {
          apply(elements_in_same_locs_across_adj_matrices, 1, mean, na.rm = TRUE)
        } else if (aggregation_fun == "median") {
          apply(elements_in_same_locs_across_adj_matrices, 1, stats::median, na.rm = TRUE)
        } else {
          stop("Input aggregation_fun must be either 'mean' or 'median'")
        }
      }
    )
  }

  # NA_only_values_in_output <- any(unlist(lapply(aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index, is.na)))
  output_returned_as_list <- is.null(dim(aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index)) & length(aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index) == nrow(adj_matrices_with_unconventional_values_as_distributions_by_index)
  if (output_returned_as_list) {
    aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index <- do.call(cbind, aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index)
  }

  if (show_progress) {
    print("Converting NA values generated from mean/median functions to 0's", quote = FALSE)
    aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index <- pbapply::pbapply(aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index, c(1, 2), function(element) ifelse(is.na(element), 0, element))
    print("Building empirical adjacency matrices from samples", quote = FALSE)
    sampled_adj_matrices <- pbapply::pbapply(aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index, 1, function(row) data.frame(array(row, c(n_nodes, n_nodes))), simplify = FALSE)
  } else {
    aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index <- apply(aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index, c(1, 2), function(element) ifelse(is.na(element), 0, element))
    sampled_adj_matrices <- apply(aggregated_sampled_adj_matrices_with_unconventional_values_as_distributions_by_index, 1, function(row) data.frame(array(row, c(n_nodes, n_nodes))), simplify = FALSE)
  }

  sampled_adj_matrices
}


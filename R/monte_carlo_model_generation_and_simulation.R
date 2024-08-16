


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
                 "confirm_input_vector_is_compatible_with_adj_matrix",
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
                 "confirm_input_vector_is_compatible_with_adj_matrix",
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



#' build_monte_carlo_fcms
#'
#' @description
#' This function generates N fcm adjacency matrices whose edge weights are sampled
#' from edge values (that may be numeric, grey_numbers, or triangular_numbers) and
#' stores them as a list of adjacency matrices.
#'
#' @details
#' If an edge is represented by multiple grey/triangular_numbers, then those distributions
#' are averaged together to create the aggregate distribution to sample from.
#'
#' Use vignette("fcmconfr-class") for more information.
#'
#' @param adj_matrix_list A list of n x n adjacencey matrices representing fcms
#' @param N_samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
build_monte_carlo_fcms <- function(adj_matrix_list = list(matrix()),
                                   N_samples = integer(),
                                   include_zeroes = TRUE,
                                   show_progress = TRUE) {
  adj_matrix_list_class <- unique(vapply(adj_matrix_list, get_class_of_adj_matrix, character(1)))
  if (length(adj_matrix_list_class) != 1) {
    stop("All adj. matrices in input adj_matrix_list must be of the same class (i.e. fcm, fgcm, or ftcm")
  }

  if (adj_matrix_list_class == "fcm") {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_conventional_adj_matrices(adj_matrix_list, N_samples, include_zeroes, show_progress)
  } else {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_fuzzy_adj_matrices(adj_matrix_list, adj_matrix_list_class, N_samples, include_zeroes, show_progress)
  }

  sampled_adj_matrices
}



#' build_monte_carlo_fcms_from_conventional_fcms
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
#' @param adj_matrix_list A list of n x n adjacencey matrices representing fcms
#' @param N_samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
build_monte_carlo_fcms_from_conventional_adj_matrices <- function(adj_matrix_list = list(Matrix::sparseMatrix()),
                                                                  N_samples = integer(),
                                                                  include_zeroes = TRUE,
                                                                  show_progress = TRUE) {
  n_nodes <- unique(unlist(lapply(adj_matrix_list, dim)))
  flatten_conventional_adj_matrix <- function(adj_matrix) do.call(c, as.vector(adj_matrix))
  flattened_adj_matrices <- do.call(rbind, lapply(adj_matrix_list, flatten_conventional_adj_matrix))
  if (!include_zeroes) {
    flattened_adj_matrices[flattened_adj_matrices == 0] <- NA
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(flattened_adj_matrices, 2, function(column_vec) {
      na_omit_column_vec <- stats::na.omit(column_vec)
      if (length(na_omit_column_vec) != 0) {
        sample(na_omit_column_vec, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    cat(print("Constructing monte carlo fcms from samples", quote = FALSE))
    sampled_adj_matrices <- pbapply::pbapply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  } else {
    column_samples <- apply(flattened_adj_matrices, 2, function(column_vec) {
      na_omit_column_vec <- stats::na.omit(column_vec)
      if (length(na_omit_column_vec) != 0) {
        sample(na_omit_column_vec, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    sampled_adj_matrices <- apply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  }

  sampled_adj_matrices
}




#' build_monte_carlo_fcms_from_fuzzy_adj_matrices
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
#' @param fuzzy_adj_matrix_list A list of n x n fuzzy adjacencey matrices representing fcms
#' @param fuzzy_adj_matrix_list_class "fgcm" or "ftcm" - the class of elements in the fuzzy_adj_matrix_list
#' @param N_samples The number of samples to draw from the corresponding distribution
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
build_monte_carlo_fcms_from_fuzzy_adj_matrices <- function(fuzzy_adj_matrix_list = list(data.frame()),
                                                           fuzzy_adj_matrix_list_class = c("fcm", "fgcm", "ftcm"),
                                                           N_samples = integer(),
                                                           include_zeroes = FALSE,
                                                           show_progress = TRUE) {

  if (!(fuzzy_adj_matrix_list_class %in% c("fcm", "fgcm", "ftcm"))) {
    stop("Input fuzzy_adj_matrix_list_class must be one of the following: 'fcm', 'fgcm', or 'ftcm'")
  }

  n_nodes <- unique(unlist(lapply(fuzzy_adj_matrix_list, dim)))

  flatten_fuzzy_adj_matrix <- function(fuzzy_adj_matrix) do.call(cbind, lapply(as.vector(fuzzy_adj_matrix), rbind))
  flattened_fuzzy_adj_matrix_list <- do.call(rbind, lapply(fuzzy_adj_matrix_list, flatten_fuzzy_adj_matrix))
  flattened_fuzzy_adj_matrix_list_w_distributions <- convert_fuzzy_elements_in_matrix_to_distributions(flattened_fuzzy_adj_matrix_list, fuzzy_adj_matrix_list_class, N_samples)

  if (!include_zeroes) {
    flattened_fuzzy_adj_matrix_list_w_distributions <- apply(
      flattened_fuzzy_adj_matrix_list_w_distributions, c(1, 2),
      function(element) {
        ifelse(identical(element[[1]], 0), NA, element)
      })
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(flattened_fuzzy_adj_matrix_list_w_distributions, 2, function(column_vec) {
      # sample_list_of_vectors_ignoring_NAs
      na_omit_column_vec <- stats::na.omit(do.call(c, column_vec))
      if (length(na_omit_column_vec) != 0) {
        column_vec_with_numerics_replicated <- lapply(
          column_vec,
          function(value) {
            if (is.numeric(value) & length(value) == 1) {
              rep(value, N_samples)
            } else {
              value
            }
          })
        na_omit_column_vec <- stats::na.omit(do.call(c, column_vec_with_numerics_replicated))
        sample(na_omit_column_vec, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    cat(print("Constructing monte carlo fcms from samples", quote = FALSE))
    sampled_adj_matrices <- pbapply::pbapply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  } else {
    column_samples <- apply(flattened_fuzzy_adj_matrix_list_w_distributions, 2, function(column_vec) {
      # sample_list_of_vectors_ignoring_NAs
      na_omit_column_vec <- stats::na.omit(do.call(c, column_vec))
      if (length(na_omit_column_vec) != 0) {
        column_vec_with_numerics_replicated <- lapply(
          column_vec,
          function(value) {
            if (is.numeric(value) & length(value) == 1) {
              rep(value, N_samples)
            } else {
              value
            }
          })
        na_omit_column_vec <- stats::na.omit(do.call(c, column_vec_with_numerics_replicated))
        sample(na_omit_column_vec, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    sampled_adj_matrices <- apply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  }

  sampled_adj_matrices
}




#' convert_fuzzy_elements_in_matrix_to_distributions
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
#' @param fuzzy_matrix A matrix that can contain fuzzy sets as elements
#' @param fuzzy_element_class "fgcm" or "ftcm" - the class of elements in the fuzzy_matrix
#' @param N_samples The number of samples to draw from the corresponding distribution
#'
#' @export
convert_fuzzy_elements_in_matrix_to_distributions <- function(fuzzy_matrix = data.table::data.table(),
                                                              fuzzy_element_class = c("fgcm", "ftcm"),
                                                              N_samples = integer()) {
  if (!(fuzzy_element_class %in% c("fgcm", "ftcm"))) {
    stop("Input fuzzy_element_class must be either fgcm or ftcm")
  } else if (identical(fuzzy_element_class, c("fgcm", "ftcm"))) {
    fuzzy_element_class <- get_class_of_adj_matrix(fuzzy_matrix)
  }

  if (fuzzy_element_class == "fgcm") {
    fuzzy_matrix_w_distributions <- apply(
      fuzzy_matrix, c(1, 2),
      function(element) {
        if (identical(methods::is(element[[1]]), "grey_number")) {
          element <- stats::runif(N_samples, element[[1]]$lower, element[[1]]$upper)
        } else {
          element[[1]]
        }
      }
    )
  } else if (fuzzy_element_class == "ftcm") {
    fuzzy_matrix_w_distributions <- apply(
      fuzzy_matrix, c(1, 2),
      function(element) {
        if (identical(methods::is(element[[1]]), "triangular_number")) {
          element <- rtri(N_samples, lower = element[[1]]$lower, mode = element[[1]]$mode, upper = element[[1]]$upper)
        } else {
          element[[1]]
        }
      }
    )
  }

  fuzzy_matrix_w_distributions
}






#' infer_monte_carlo_fcm_set
#'
#' @description
#' This calculates a sequence of iterations of a simulation over every item in
#' a list of fmcm objects given an initial state vector along with the
#' activation, squashing, and lambda parameters.
#' Additional variables may be defined to control simulation length,
#' column names, and lambda optimization.
#'
#' @details
#' [ADD DETAILS HERE!!!].
#'
#' @param mc_adj_matrices A list of adjecency matrices generated from simulation using build_fmcm_models.
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
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' FCM. Will dramatically increase size of output if TRUE.
#'
#' @export
infer_monte_carlo_fcm_set <- function(mc_adj_matrices = list(matrix()),
                                      initial_state_vector = c(),
                                      clamping_vector = c(),
                                      activation = c("kosko", "modified-kosko", "rescale"),
                                      squashing = c("sigmoid", "tanh"),
                                      lambda = 1,
                                      max_iter = 100,
                                      min_error = 1e-5,
                                      fuzzy_set_samples = 1000,
                                      parallel = TRUE,
                                      n_cores = integer(),
                                      show_progress = TRUE,
                                      include_simulations_in_output = FALSE) {

  # Adding for R CMD check. Does not impact logic.
  # iter <- NULL
  i <- NULL

  checks <- lapply(mc_adj_matrices, check_simulation_inputs, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

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
    vars <- list(
      "infer_fcm",  "infer_conventional_fcm", "infer_ivfn_or_tfn_fcm", "equalize_baseline_and_scenario_outputs",
      "simulate_fcm", "simulate_conventional_fcm",  "simulate_ivfn_or_tfn_fcm",
      "calculate_next_conventional_fcm_state_vector", "calculate_next_fuzzy_set_fcm_state_vector",
      "check_simulation_inputs", "get_adj_matrices_input_type", "squash", "defuzz",
      "convert_element_to_ivfn_or_tfn_if_numeric", "clean_simulation_output",
      "check_simulation_inputs",
      "mc_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
      "squashing", "lambda", "max_iter", "min_error", "fuzzy_set_samples"
    )

    parallel::clusterExport(cl, varlist = vars, envir = environment())

    doSNOW::registerDoSNOW(cl)
    # pb <- utils::txtProgressBar(min = 0, max = length(simulated_adj_matrices)/n_cores, style = 3)
    invisible(utils::capture.output(pb <- utils::txtProgressBar(min = 0, max = length(mc_adj_matrices)/n_cores, style = 3)))
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    # cat("\n")
    print("Running simulations. There may be an additional wait for larger sets.", quote = FALSE)
    opts <- list(progress = progress)
    inferences_for_mc_adj_matrices <- foreach::foreach(
      i = 1:length(mc_adj_matrices), .options.snow = opts) %dopar% {
        infer_fcm(
          adj_matrix = mc_adj_matrices[[i]],
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          fuzzy_set_samples = fuzzy_set_samples
        )
      }
    close(pb)
    names(inferences_for_mc_adj_matrices) <- paste0("mc_", 1:length(inferences_for_mc_adj_matrices))
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
    vars <- list(
      "infer_fcm",  "infer_conventional_fcm", "infer_ivfn_or_tfn_fcm", "equalize_baseline_and_scenario_outputs",
      "simulate_fcm", "simulate_conventional_fcm",  "simulate_ivfn_or_tfn_fcm",
      "calculate_next_conventional_fcm_state_vector", "calculate_next_fuzzy_set_fcm_state_vector",
      "check_simulation_inputs", "get_adj_matrices_input_type", "squash", "defuzz",
      "convert_element_to_ivfn_or_tfn_if_numeric", "clean_simulation_output",
      "check_simulation_inputs",
      "mc_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
      "squashing", "lambda", "max_iter", "min_error", "fuzzy_set_samples"
    )

    parallel::clusterExport(cl, varlist = vars, envir = environment())

    cat("\n")
    print("Running simulations", quote = FALSE)
    inferences_for_mc_adj_matrices <- parallel::parLapply(
      cl,
      mc_adj_matrices,
      function(mc_adj_matrix) {
        infer_fcm(
          adj_matrix = mc_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          fuzzy_set_samples = fuzzy_set_samples
        )
      }
    )
    parallel::stopCluster(cl)

  } else if (!parallel & show_progress) {
    cat("\n")
    print("Running simulations", quote = FALSE)
    inferences_for_mc_adj_matrices <- pbapply::pblapply(
      mc_adj_matrices,
      function(mc_adj_matrix) {
        infer_fcm(
          adj_matrix = mc_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          fuzzy_set_samples = fuzzy_set_samples
        )
      }
    )

  } else if (!parallel & !show_progress) {
    cat("\n")
    print("Running simulations", quote = FALSE)
    inferences_for_mc_adj_matrices <- lapply(
      mc_adj_matrices,
      function(mc_adj_matrix) {
        infer_fcm(
          adj_matrix = mc_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          fuzzy_set_samples = fuzzy_set_samples
        )
      }
    )
  }

  # cat("\n")
  # print("Organizing Output", quote = FALSE)
  # cat("\n")

  inference_values_by_sim <- lapply(inferences_for_mc_adj_matrices, function(sim) sim$inference)
  inference_values_by_sim <- data.frame(do.call(rbind, inference_values_by_sim))
  rownames(inference_values_by_sim) <- 1:nrow(inference_values_by_sim)

  inference_plot_data <- data.frame(
    node = rep(colnames(inference_values_by_sim), nrow(inference_values_by_sim)),
    value = unlist(lapply(t(inference_values_by_sim), c))
  )

  if (include_simulations_in_output) {
    structure(
      .Data = list(
        inference = inference_values_by_sim,
        inference_for_plotting = inference_plot_data,
        sims = inferences_for_mc_adj_matrices
      ),
      class = "inference_of_monte_carlo_fcm_set"
    )
  } else {
    structure(
      .Data = list(
        inference = inference_values_by_sim,
        inference_for_plotting = inference_plot_data
      ),
      class = "inference_of_monte_carlo_fcm_set"
    )
  }
}


#' get_mc_simulations_inference_CIs_w_bootstrap
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
#' @param mc_simulations_inference_df The final values of a set of fcm simulations; also the inference of a infer_fmcm object
#' @param inference_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param confidence_interval What are of the distribution should be bounded by the
#' confidence intervals? (e.g. 0.95)
#' @param bootstrap_reps Repetitions for bootstrap process, if chosen
#' @param bootstrap_draws_per_rep Number of samples to draw (with replacement) from
#' the data per bootstrap_rep
#' @param parallel TRUE/FALSE Whether to perform the function using parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
get_mc_simulations_inference_CIs_w_bootstrap <- function(mc_simulations_inference_df = data.frame(),
                                                         inference_function = "mean",
                                                         confidence_interval = 0.95,
                                                         bootstrap_reps = 1000,
                                                         bootstrap_draws_per_rep = 1000,
                                                         parallel = TRUE,
                                                         n_cores = integer(),
                                                         show_progress = TRUE) {
  # Adding for R CMD Check. Does not impact logic.
  iter <- NULL

  # Write checks to confirm mc_simulations_inference_df object is correct... Also write a better name
  # so it is understood that it works for simulate_fmcm objects too
  if (!identical(class(mc_simulations_inference_df), "data.frame")) {
    stop("Input mc_simulations_inference_df must be a data.frame object from the
         output of simulate_fmcm_models (final_states_across_sims) or infer_fmcm
         (inference)")
  }

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  # browser()

  if (parallel & show_progress) {
    print("Performing bootstrap simulations", quote = FALSE)
    print("Initializing cluster", quote = FALSE)
    if (identical(n_cores, integer())) {
      n_cores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(n_cores)
    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("mc_simulations_inference_df",
                 "bootstrap_reps",
                 "bootstrap_draws_per_rep"
    )
    parallel::clusterExport(cl, varlist = vars, envir = environment())
    print("Sampling means", quote = FALSE)
    doSNOW::registerDoSNOW(cl)
    invisible(utils::capture.output(pb <- utils::txtProgressBar(min = 0, max = ceiling(bootstrap_reps/n_cores), width = 50, style = 3)))
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    if (inference_function == "mean") {
      bootstrapped_means_of_inference_by_node <- foreach::foreach(
        i = 1:bootstrap_reps, .options.snow = opts) %dopar% {
          data.frame(apply(
            mc_simulations_inference_df, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              mean(random_draws)
            },
            simplify = FALSE
          ))
        }
    } else if (inference_function == "median") {
      bootstrapped_medians_of_inference_by_node <- foreach::foreach(
        i = 1:bootstrap_reps, .options.snow = opts) %dopar% {
          data.frame(apply(
            mc_simulations_inference_df, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              stats::median(random_draws)
            },
            simplify = FALSE
          ))
        }
    }
    close(pb)
    parallel::stopCluster(cl)

  } else if (parallel & !show_progress) {
    print("Performing bootstrap simulations", quote = FALSE)
    print("Initializing cluster", quote = FALSE)
    if (identical(n_cores, integer())) {
      n_cores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(n_cores)
    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("mc_simulations_inference_df",
                 "bootstrap_reps",
                 "bootstrap_draws_per_rep"
    )
    parallel::clusterExport(cl, varlist = vars, envir = environment())
    print("Sampling means", quote = FALSE)
    rep_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- lapply(rep_inference_by_node, function(duplicate) duplicate <- mc_simulations_inference_df)
    if (inference_function == "mean") {
      bootstrapped_means_of_inference_by_node <- parallel::parLapply(
        cl,
        rep_inference_by_node,
        function(inference_by_node_duplicate) {
          apply(
            inference_by_node_duplicate, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              mean(random_draws)
            }
          )
        }
      )
    } else if (inference_function == "median") {
      bootstrapped_medians_of_inference_by_node <- parallel::parLapply(
        cl,
        rep_inference_by_node,
        function(inference_by_node_duplicate) {
          apply(
            inference_by_node_duplicate, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              stats::median(random_draws)
            }
          )
        }
      )
    }
    parallel::stopCluster(cl)

  } else if (!parallel & show_progress) {
    bootstrapped_means_of_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- lapply(rep_inference_by_node, function(duplicate) duplicate <- mc_simulations_inference_df)

    if (inference_function == "mean") {
      bootstrapped_means_of_inference_by_node <- pbapply::pblapply(
        rep_inference_by_node,
        function(inference_by_node_duplicate) {
          apply(
            inference_by_node_duplicate, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              mean(random_draws)
            }
          )
        }
      )
    } else if (inference_function == "median") {
      bootstrapped_medians_of_inference_by_node <- pbapply::pblapply(
        rep_inference_by_node,
        function(inference_by_node_duplicate) {
          apply(
            inference_by_node_duplicate, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              stats::median(random_draws)
            }
          )
        }
      )
    }
  } else if (!parallel & !show_progress) {
    rep_inference_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inference_by_node <- lapply(rep_inference_by_node, function(duplicate) duplicate <- mc_simulations_inference_df)
    if (inference_function == "mean") {
      bootstrapped_means_of_inference_by_node <- lapply(
        rep_inference_by_node,
        function(inference_by_node_duplicate) {
          apply(
            inference_by_node_duplicate, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              mean(random_draws)
            }
          )
        }
      )
    } else if (inference_function == "median") {
      bootstrapped_medians_of_inference_by_node <- lapply(
        rep_inference_by_node,
        function(inference_by_node_duplicate) {
          apply(
            inference_by_node_duplicate, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              stats::median(random_draws)
            }
          )
        }
      )
    }
  }

  if (inference_function == "mean") {
    bootstrapped_expectations_of_inference_by_node <- data.frame(do.call(rbind, bootstrapped_means_of_inference_by_node))
  } else if (inference_function == "median") {
    bootstrapped_expectations_of_inference_by_node <- data.frame(do.call(rbind, bootstrapped_medians_of_inference_by_node))

  }


  # print("Getting upper and lower quantile estimates of mean", quote = FALSE)
  lower_quantile <- (1 - confidence_interval)/2
  upper_quantile <- (1 + confidence_interval)/2
  lower_quantiles_by_node <- data.frame(apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, lower_quantile), simplify = FALSE))
  upper_quantiles_by_node <- data.frame(apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, upper_quantile), simplify = FALSE))

  nodes <- ifelse(colnames(lower_quantiles_by_node) == colnames(upper_quantiles_by_node), colnames(lower_quantiles_by_node), stop("Error with quantiles calculation"))

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
      CI_by_node = quantiles_by_node,
      bootstrap_expected_values = bootstrapped_expectations_of_inference_by_node
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

  # browser()
  adj_matrix_list_class <- get_adj_matrices_input_type(adj_matrix_list)$object_types_in_list[1]

  if (adj_matrix_list_class == "conventional") {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_conventional_adj_matrices(adj_matrix_list, N_samples, include_zeroes, show_progress)
  } else {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrix_list, adj_matrix_list_class, N_samples, include_zeroes, show_progress)

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




#' build_monte_carlo_fcms_from_fuzzy_set_adj_matrices
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
#' @param fuzzy_set_adj_matrix_list A list of n x n fuzzy adjacencey matrices representing fcms
#' @param fuzzy_set_adj_matrix_list_class "fgcm" or "fcm_w_tfn" - the class of elements in the fuzzy_set_adj_matrix_list
#' @param N_samples The number of samples to draw from the corresponding distribution
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
build_monte_carlo_fcms_from_fuzzy_set_adj_matrices <- function(fuzzy_set_adj_matrix_list = list(data.frame()),
                                                               fuzzy_set_adj_matrix_list_class = c("conventional", "ivfn", "tfn"),
                                                               N_samples = integer(),
                                                               include_zeroes = FALSE,
                                                               show_progress = TRUE) {

  #browser()
  if (!(fuzzy_set_adj_matrix_list_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input fuzzy_set_adj_matrix_list_class must be one of the following: 'conventional', 'ivfn', or 'tfn'")
  }

  n_nodes <- unique(unlist(lapply(fuzzy_set_adj_matrix_list, dim)))

  flatten_fuzzy_adj_matrix <- function(fuzzy_adj_matrix) do.call(cbind, lapply(as.vector(fuzzy_adj_matrix), rbind))
  flattened_fuzzy_set_adj_matrix_list <- do.call(rbind, lapply(fuzzy_set_adj_matrix_list, flatten_fuzzy_adj_matrix))
  flattened_fuzzy_set_adj_matrix_list_w_distributions <- convert_fuzzy_set_elements_in_matrix_to_distributions(flattened_fuzzy_set_adj_matrix_list, fuzzy_set_adj_matrix_list_class, N_samples)

  if (!include_zeroes) {
    flattened_fuzzy_set_adj_matrix_list_w_distributions <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, c(1, 2), function(element) ifelse(element[[1]][[1]] == 0, NA, element[[1]][[1]]), simplify = FALSE)
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(
      flattened_fuzzy_set_adj_matrix_list_w_distributions, 2,
      function(column_vec) {
        #browser()
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
    column_samples <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, 2, function(column_vec) {
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


#' Check inputs for monte carlo bootstrap analysis
#'
#' @param inference_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param confidence_interval What are of the distribution should be bounded by the
#' confidence intervals? (e.g. 0.95)
#' @param bootstrap_reps Repetitions for bootstrap process, if chosen
#' @param bootstrap_draws_per_rep Number of samples to draw (with replacement) from
#' the data per bootstrap_rep
#'
#' @export
monte_carlo_bootstrap_checks <- function(inference_function,
                                         confidence_interval,
                                         bootstrap_reps,
                                         bootstrap_draws_per_rep) {
  if (inference_function %in% c("mean", "median")) {
    stop("Input Validation Error: inference_function must be either 'mean' or 'median'")
  }

  if (!is.numeric(confidence_interval)) {
    stop("Input Validation Error: confidence_interval must be a number between 0 and 1")
  }

  if (confidence_interval < 0 | !(confidence_interval < 1)) {
    stop("Input Validation Error: confidence_interval must be a number between 0 and 1")
  }

  if (!is.numeric(bootstrap_reps)) {
    stop("Input Validation Error: bootstrap_reps must be a positive integer")
  }

  if (bootstrap_reps %% 2 != 0) {
    stop("Input Validation Error: bootstrap_reps must be a positive integer")
  }

  if (!is.numeric(bootstrap_draws_per_rep)) {
    stop("Input Validation Error: ootstrap_draws_per_rep must be a positive integer")
  }

  if (bootstrap_draws_per_rep %% 2 != 0) {
    stop("Input Validation Error: ootstrap_draws_per_rep must be a positive integer")
  }
}



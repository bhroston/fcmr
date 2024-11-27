
################################################################################
# monte_carlo_model_generation_and_simulation.R
#
# These functions assist in generating empirical FCMs via monte carlo methods
# and simulating the generated FCMs in bulk.
#
#   - infer_monte_carlo_fcm_set
#   - get_mc_simulations_inference_CIs_w_bootstrap
#   - build_monte_carlo_fcms
#   - build_monte_carlo_fcms_from_conventional_adj_matrices
#   - build_monte_carlo_fcms_from_fuzzy_set_adj_matrices
#   - monte_carlo_bootstrap_checks
#
################################################################################


#' Infer FCMs Generated from Monte Carlo Methods
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @description
#' This function mass simulates a set of FCMs (Conventional, IVFN, and/or TFN)
#' (whose edge weights were sampled using monte carlo methods) by repetitively
#' calling the infer_fcm function for each empirical (monte carlo) adj. matrix.
#'
#' @details
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
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
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' FCM. Will dramatically increase size of output if TRUE.
#'
#' @returns A list of two dataframes: the first contains all inference estimates
#' across the empirical (monte carlo) FCM inferences, and the second is an
#' elongated version of the first dataframe that organizes the data for
#' plotting (particularly with ggplot2)
#'
#' @export
#' @example man/examples/ex-infer_mc_fcm_set.R
infer_monte_carlo_fcm_set <- function(mc_adj_matrices = list(matrix()),
                                      initial_state_vector = c(),
                                      clamping_vector = c(),
                                      activation = c("kosko", "modified-kosko", "rescale"),
                                      squashing = c("sigmoid", "tanh"),
                                      lambda = 1,
                                      max_iter = 100,
                                      min_error = 1e-5,
                                      parallel = TRUE,
                                      n_cores = integer(),
                                      show_progress = TRUE,
                                      include_simulations_in_output = FALSE) {

  # Adding for R CMD check. Does not impact logic.
  i <- NULL

  # browser()
  checks <- lapply(mc_adj_matrices, check_simulation_inputs, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  fcm_class <- get_adj_matrices_input_type(mc_adj_matrices)$object_types_in_list[1]

  # # browser()

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  # # browser()

  if (parallel) {
    max_possible_cores <- parallel::detectCores()
    if (identical(n_cores, integer())) {
      warning(paste0("Input n_cores not defined by user. Assuming n_cores is the maximum available on machine: n_cores = ", max_possible_cores))
      n_cores <- max_possible_cores
    }
    if (!is.numeric(n_cores) | (n_cores %% 2 != 0)) {
      stop("Input Validation Error: n_cores must be an integer.")
    }
    if (n_cores > max_possible_cores) {
      warning(paste0(" Input n_cores is greater than the available cores on this machine.\n Reducing to ", max_possible_cores))
    }
  }
  if (!parallel & !identical(n_cores, integer())) {
    warning(paste0(" Input n_cores is ignored since parallel = FALSE" ))
  }

  # # browser()

  if (parallel & show_progress) {
    print("Initializing cluster", quote = FALSE)
    cl <- parallel::makeCluster(n_cores)

    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list(
      "infer_fcm",  "infer_conventional_fcm", "infer_ivfn_or_tfn_fcm", "equalize_baseline_and_scenario_outputs",
      "simulate_fcm", "simulate_conventional_fcm",  "simulate_ivfn_or_tfn_fcm",
      "calculate_next_conventional_fcm_state_vector", "calculate_next_fuzzy_set_fcm_state_vector",
      "check_simulation_inputs", "get_adj_matrices_input_type", "squash", "defuzz_ivfn_or_tfn",
      "convert_element_to_ivfn_or_tfn_if_numeric", "clean_simulation_output",
      "check_simulation_inputs",
      "mc_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
      "squashing", "lambda", "max_iter", "min_error"
    )

    # browser()
    parallel::clusterExport(cl, varlist = vars, envir = environment())

    doSNOW::registerDoSNOW(cl)
    invisible(utils::capture.output(pb <- utils::txtProgressBar(min = 0, max = length(mc_adj_matrices)/n_cores, style = 3, width = 50)))
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    # cat("\n")
    print("Running simulations. There may be an additional wait for larger sets.", quote = FALSE)
    opts <- list(progress = progress)
    # browser()
    if (fcm_class == "conventional") {
      inferences_for_mc_adj_matrices <- foreach::foreach(
        i = 1:length(mc_adj_matrices), .options.snow = opts
      ) %dopar% {
        infer_conventional_fcm(
          adj_matrix = mc_adj_matrices[[i]],
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error
        )
      }
    } else if (fcm_class %in% c("ivfn", "tfn")) {
      inferences_for_mc_adj_matrices <- foreach::foreach(
        i = 1:length(mc_adj_matrices), .options.snow = opts
      ) %dopar% {
        infer_ivfn_or_tfn_fcm(
          adj_matrix = mc_adj_matrices[[i]],
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error
        )
      }
    }
    close(pb)
    names(inferences_for_mc_adj_matrices) <- paste0("mc_", 1:length(inferences_for_mc_adj_matrices))
    parallel::stopCluster(cl)
  } else if (parallel & !show_progress) {
    print("Initializing cluster", quote = FALSE)
    cl <- parallel::makeCluster(n_cores)

    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list(
      "infer_fcm",  "infer_conventional_fcm", "infer_ivfn_or_tfn_fcm", "equalize_baseline_and_scenario_outputs",
      "simulate_fcm", "simulate_conventional_fcm",  "simulate_ivfn_or_tfn_fcm",
      "calculate_next_conventional_fcm_state_vector", "calculate_next_fuzzy_set_fcm_state_vector",
      "check_simulation_inputs", "get_adj_matrices_input_type", "squash", "defuzz_ivfn_or_tfn",
      "convert_element_to_ivfn_or_tfn_if_numeric", "clean_simulation_output",
      "check_simulation_inputs",
      "mc_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
      "squashing", "lambda", "max_iter", "min_error"
    )

    parallel::clusterExport(cl, varlist = vars, envir = environment())

    cat("\n")
    print("Running simulations", quote = FALSE)
    if (fcm_class == "conventional") {
      inferences_for_mc_adj_matrices <- parallel::parLapply(
        cl,
        mc_adj_matrices,
        function(mc_adj_matrix) {
          infer_conventional_fcm(
            adj_matrix = mc_adj_matrix,
            initial_state_vector = initial_state_vector,
            clamping_vector = clamping_vector,
            activation = activation,
            squashing = squashing,
            lambda = lambda,
            max_iter = max_iter,
            min_error = min_error
          )
        }
      )
    } else if (fcm_class %in% c("ivfn", "tfn")) {
      inferences_for_mc_adj_matrices <- parallel::parLapply(
        cl,
        mc_adj_matrices,
        function(mc_adj_matrix) {
          infer_ivfn_or_tfn_fcm(
            adj_matrix = mc_adj_matrix,
            initial_state_vector = initial_state_vector,
            clamping_vector = clamping_vector,
            activation = activation,
            squashing = squashing,
            lambda = lambda,
            max_iter = max_iter,
            min_error = min_error
          )
        }
      )
    }
    # inferences_for_mc_adj_matrices <- parallel::parLapply(
    #   cl,
    #   mc_adj_matrices,
    #   function(mc_adj_matrix) {
    #     infer_fcm(
    #       adj_matrix = mc_adj_matrix,
    #       initial_state_vector = initial_state_vector,
    #       clamping_vector = clamping_vector,
    #       activation = activation,
    #       squashing = squashing,
    #       lambda = lambda,
    #       max_iter = max_iter,
    #       min_error = min_error
    #     )
    #   }
    # )
    parallel::stopCluster(cl)

  } else if (!parallel & show_progress) {
    cat("\n")
    print("Running simulations", quote = FALSE)
    # browser()
    inferences_for_mc_adj_matrices <- pbapply::pblapply(
      mc_adj_matrices,
      function(mc_adj_matrix) {
        # # browser()
        infer_fcm(
          adj_matrix = mc_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error
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
          min_error = min_error
        )
      }
    )
  }


  # # browser()

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



#' Calculate Inferences (w/ Confidence Intervals via Bootstrap) of MC FCM Simulations
#'
#' @family monte-carlo-model-generation-and-simulation
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
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
#'
#' @param mc_simulations_inference_df The final values of a set of fcm simulations; also the inference of a infer_fmcm object
#' @param inference_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param confidence_interval What are of the distribution should be bounded by the
#' confidence intervals? (e.g. 0.95)
#' @param bootstrap_reps Repetitions for bootstrap process, if chosen
#' @param parallel TRUE/FALSE Whether to perform the function using parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns A list of raw bootstrap draws and a dataframe of confidence intervals
#'
#' @export
#' @example man/examples/ex-get_mc_sims_inference_CIs_w_bootstrap.R
get_mc_simulations_inference_CIs_w_bootstrap <- function(mc_simulations_inference_df = data.frame(),
                                                         inference_function = "mean",
                                                         confidence_interval = 0.95,
                                                         bootstrap_reps = 1000,
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

  # # browser()
  # Check function inputs
  # # browser()
  bootstrap_draws_per_rep <- nrow(mc_simulations_inference_df)
  monte_carlo_bootstrap_checks(inference_function, confidence_interval, bootstrap_reps)

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  # # browser()

  if (parallel) {
    max_possible_cores <- parallel::detectCores()
    if (identical(n_cores, integer())) {
      warning(paste0("Input n_cores not defined by user. Assuming n_cores is the maximum available on machine: n_cores = ", max_possible_cores))
      n_cores <- max_possible_cores
    }
    if (!is.numeric(n_cores) | (n_cores %% 2 != 0)) {
      stop("Input Validation Error: n_cores must be an integer.")
    }
    if (n_cores > max_possible_cores) {
      warning(paste0(" Input n_cores is greater than the available cores on this machine.\n Reducing to ", max_possible_cores))
    }
  }
  if (!parallel & !identical(n_cores, integer())) {
    warning(paste0(" Input n_cores is ignored since parallel = FALSE" ))
  }

  if (parallel & show_progress) {
    print("Performing bootstrap simulations", quote = FALSE)
    print("Initializing cluster", quote = FALSE)
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
  expected_value_of_inference_by_node <- apply(bootstrapped_expectations_of_inference_by_node, 2, mean)

  ## browser()

  # print("Getting upper and lower quantile estimates of mean", quote = FALSE)
  lower_CI <- (1 - confidence_interval)/2
  upper_CI <- (1 + confidence_interval)/2
  lower_CIs_by_node <- data.frame(apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, lower_CI), simplify = FALSE))
  upper_CIs_by_node <- data.frame(apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, upper_CI), simplify = FALSE))

  nodes <- ifelse(colnames(lower_CIs_by_node) == colnames(upper_CIs_by_node), colnames(lower_CIs_by_node), stop("Error with quantiles calculation"))

  # browser()

  CIs_by_node <- data.frame(
    node = nodes,
    expected_value = expected_value_of_inference_by_node,
    lower_CI = vector(mode = "numeric", length = length(nodes)),
    upper_CI = vector(mode = "numeric", length = length(nodes))
  )
  for (i in seq_along(nodes)) {
    CIs_by_node$lower_CI[i] <- lower_CIs_by_node[i][[1]] # not sure why this [[1]] is necessary but it is
    CIs_by_node$upper_CI[i] <- upper_CIs_by_node[i][[1]] # not sure why this [[1]] is necessary but it is
  }

  # browser()

  quantiles_of_mc_simulation_inferences <- data.frame(t(apply(mc_simulations_inference_df, 2, stats::quantile)))
  mc_inference_distributions_df <- data.frame(cbind(
    CIs_by_node$node, CIs_by_node$expected_value, quantiles_of_mc_simulation_inferences$X0.,
    CIs_by_node$lower_CI, quantiles_of_mc_simulation_inferences$X25., quantiles_of_mc_simulation_inferences$X50.,
    quantiles_of_mc_simulation_inferences$X75., CIs_by_node$upper_CI, quantiles_of_mc_simulation_inferences$X100.
  ))

  colnames(mc_inference_distributions_df) <- c(
    "node", "expected_value", "min",
    paste0(lower_CI, "_CI"), "0.25_quantile", "median",
    "0.75_quantile", paste0(upper_CI, "_CI"), "max"
  )

  for (col in 2:ncol(mc_inference_distributions_df)) {
    mc_inference_distributions_df[, col] <- as.numeric(mc_inference_distributions_df[, col])
  }

  # # browser()
  print("Done", quote = FALSE)

  structure(
    .Data = list(
      CIs_and_quantiles_by_node = mc_inference_distributions_df,
      bootstrap_expected_values = bootstrapped_expectations_of_inference_by_node
    )
  )
}



#' Build Monte Carlo FCMs
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @description
#' This function generates N fcm adjacency matrices whose edge weights are sampled
#' from edge values (that may be numeric, IVFNs, or TFNs) and
#' stores them as a list of adjacency matrices.
#'
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
#'
#' @details
#' For Conventional FCMs, edge weights are sampled the edge weight explicitly
#' defined in the input FCMs.
#'
#' For IVFN and TFN FCMs, edge weights are sampled from the combined
#' distributions representative of the IVFN/TFN edge weights. For example,
#' if an edge is given the following weights across two maps: IVFN(0.4, 0.8) and
#' IVFN[0.5, 0.7], the samples will be drawn from the combined distribution:
#' sample(N, c(runif(N, 0.4, 0.8), runif(N, 0.5, 0.7)), replace = TRUE).
#'
#' @param adj_matrix_list A list of n x n adjacencey matrices representing fcms
#' @param N_samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns A list of empirical (Conventional) FCM adj. matrices generated via monte carlo methods
#'
#' @export
#' @example man/examples/ex-build_monte_carlo_fcms.R
build_monte_carlo_fcms <- function(adj_matrix_list = list(matrix()),
                                   N_samples = integer(),
                                   include_zeroes = TRUE,
                                   show_progress = TRUE) {

  # # browser()
  adj_matrix_list_class <- get_adj_matrices_input_type(adj_matrix_list)$object_types_in_list[1]

  if (adj_matrix_list_class == "conventional") {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_conventional_adj_matrices(adj_matrix_list, N_samples, include_zeroes, show_progress)
  } else {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrix_list, adj_matrix_list_class, N_samples, include_zeroes, show_progress)
  }

  sampled_adj_matrices
}



#' Build Monte Carlo (Conventional) FCMs
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @description
#' This function generates n fcm models whose edge weights are sampled from either
#' the defined edge values in a set of adjacency matrices derived from the sets
#' of edge values, and stores them as a list of adjacency matrices.
#'
#' @details
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
#'
#' @param adj_matrix_list A list of n x n adjacencey matrices representing fcms
#' @param N_samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns A list of empirical (Conventional) FCM adj. matrices generated via monte carlo methods
#'
#' @export
#' @example man/examples/ex-build_mc_models_from_conventional_adj_matrices.R
build_monte_carlo_fcms_from_conventional_adj_matrices <- function(adj_matrix_list = list(Matrix::sparseMatrix()),
                                                                  N_samples = integer(),
                                                                  include_zeroes = TRUE,
                                                                  show_progress = TRUE) {
  # browser()
  n_nodes <- unique(unlist(lapply(adj_matrix_list, dim)))
  flatten_conventional_adj_matrix <- function(adj_matrix) {
    # browser()
    flattened_adj_matrix <- do.call(c, as.vector(adj_matrix))
    names(flattened_adj_matrix) <- seq_along(flattened_adj_matrix)
    flattened_adj_matrix
  }
  flattened_adj_matrices <- do.call(rbind, lapply(adj_matrix_list, flatten_conventional_adj_matrix))
  if (!include_zeroes) {
    flattened_adj_matrices[flattened_adj_matrices == 0] <- NA
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(flattened_adj_matrices, 2, function(column_vec) {
      # browser()
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




#' Build Monte Carlo (IVFN or TFN) FCMs
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @description
#' This function generates n fcm adjacency matrices whose edge weights are sampled
#' from edge values (that may be Conventional, IVFNs, or TFNs) and
#' stores them as a list of adjacency matrices.
#'
#' @details
#' If an edge is represented by IVFNs/TFNs, then those distributions
#' are averaged together to create the aggregate distribution to sample from.
#'
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
#'
#' @param fuzzy_set_adj_matrix_list A list of n x n fuzzy adjacencey matrices representing fcms
#' @param fuzzy_set_adj_matrix_list_class "fgcm" or "fcm_w_tfn" - the class of elements in the fuzzy_set_adj_matrix_list
#' @param N_samples The number of samples to draw from the corresponding distribution
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns A list of empirical (Conventional) FCM adj. matrices generated via monte carlo methods
#'
#' @export
#' @example man/examples/ex-build_mc_models_from_fuzzy_set_adj_matrices.R
build_monte_carlo_fcms_from_fuzzy_set_adj_matrices <- function(fuzzy_set_adj_matrix_list = list(data.frame()),
                                                               fuzzy_set_adj_matrix_list_class = c("conventional", "ivfn", "tfn"),
                                                               N_samples = integer(),
                                                               include_zeroes = FALSE,
                                                               show_progress = TRUE) {

  # # browser()
  if (!(fuzzy_set_adj_matrix_list_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input fuzzy_set_adj_matrix_list_class must be one of the following: 'conventional', 'ivfn', or 'tfn'")
  }

  n_nodes <- unique(unlist(lapply(fuzzy_set_adj_matrix_list, dim)))

  flatten_fuzzy_adj_matrix <- function(fuzzy_adj_matrix) do.call(cbind, lapply(as.vector(fuzzy_adj_matrix), rbind))
  flattened_fuzzy_set_adj_matrix_list <- do.call(rbind, lapply(fuzzy_set_adj_matrix_list, flatten_fuzzy_adj_matrix))
  flattened_fuzzy_set_adj_matrix_list_w_distributions <- convert_fuzzy_set_elements_in_matrix_to_distributions(fuzzy_set_matrix = flattened_fuzzy_set_adj_matrix_list, object_class = fuzzy_set_adj_matrix_list_class, N_samples = N_samples)

  if (!include_zeroes) {
    flattened_fuzzy_set_adj_matrix_list_w_distributions <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, c(1, 2), function(element) ifelse(element[[1]][[1]] == 0, NA, element[[1]][[1]]), simplify = FALSE)
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(
      flattened_fuzzy_set_adj_matrix_list_w_distributions, 2,
      function(column_vec) {
        # # browser()
        # sample_list_of_vectors_ignoring_NAs
        na_omit_column_vec <- stats::na.omit(do.call(c, column_vec))
        if (length(na_omit_column_vec) != 0) {
          column_vecs_w_NAs <- lapply(
            column_vec, function(value) value
          )
          column_vecs_w_NAs <- stats::na.omit(do.call(c, column_vecs_w_NAs))
        sample(column_vecs_w_NAs, N_samples, replace = TRUE)
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
        na_omit_column_vec <- stats::na.omit(do.call(c, column_vec))
        if (length(na_omit_column_vec) != 0) {
          column_vecs_w_NAs <- lapply(
            column_vec, function(value) value
          )
          column_vecs_w_NAs <- stats::na.omit(do.call(c, column_vecs_w_NAs))
          sample(column_vecs_w_NAs, N_samples, replace = TRUE)
        }
        # column_vec_with_numerics_replicated <- lapply(
        #   column_vec,
        #   function(value) {
        #     if (is.numeric(value) & length(value) == 1) {
        #       rep(value, N_samples)
        #     } else {
        #       value
        #     }
        #   })
        # na_omit_column_vec <- stats::na.omit(do.call(c, column_vec_with_numerics_replicated))
        # sample(na_omit_column_vec, N_samples, replace = TRUE)
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
#' @family monte-carlo-model-generation-and-simulation
#'
#' @param inference_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param confidence_interval What are of the distribution should be bounded by the
#' confidence intervals? (e.g. 0.95)
#' @param bootstrap_reps Repetitions for bootstrap process, if chosen
#'
#' @returns NULL; Errors if checks fail
#'
#' @export
#' @examples
#' NULL
monte_carlo_bootstrap_checks <- function(inference_function,
                                         confidence_interval,
                                         bootstrap_reps) {
  # # browser()

  if (!(inference_function %in% c("mean", "median"))) {
    stop("Input Validation Error: inference_function must be either 'mean' or 'median'")
  }

  if (!is.numeric(confidence_interval)) {
    stop("Input Validation Error: confidence_interval must be a number between 0 and 1")
  }

  if (confidence_interval < 0 | confidence_interval >= 1) {
    stop("Input Validation Error: confidence_interval must be a number between 0 and 1")
  }

  if (!is.numeric(bootstrap_reps)) {
    stop("Input Validation Error: bootstrap_reps must be a positive integer")
  }

  if (bootstrap_reps %% 2 != 0) {
    stop("Input Validation Error: bootstrap_reps must be a positive integer")
  }
}



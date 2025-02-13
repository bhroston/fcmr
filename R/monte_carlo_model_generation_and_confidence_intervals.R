
################################################################################
# monte_carlo_model_generation_and_confidence_intervals.R
#
# These functions assist in generating empirical FCMs via monte carlo methods
# and simulating the generated FCMs in bulk.
#
#   - build_monte_carlo_fcms
#   - build_monte_carlo_fcms_from_conventional_adj_matrices
#   - build_monte_carlo_fcms_from_fuzzy_set_adj_matrices
#   - get_mc_simulations_inference_CIs_w_bootstrap
#
################################################################################

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
#' @param adj_matrix_list A list of n x n adjacency matrices representing fcms
#' @param N_samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param skip_checks FOR DEVELOPER USE ONLY. TRUE if infer_fcm is called within
#' fcmconfr() and checks have already been performed
#'
#' @returns A list of empirical (Conventional) FCM adj. matrices generated via monte carlo methods
#'
#' @export
#' @example man/examples/ex-build_monte_carlo_fcms.R
build_monte_carlo_fcms <- function(adj_matrix_list = list(matrix()),
                                   N_samples = 1000,
                                   include_zeroes = TRUE,
                                   show_progress = TRUE,
                                   skip_checks = FALSE) {

  if (!is.logical(skip_checks)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var skip_checks} must be a logical value (TRUE/FALSE)",
      "+++++> Input {.var skip_checks} was: {skip_checks}"
    )))
  }
  if (!skip_checks) {
    checks <- check_build_monte_carlo_fcms_inputs(adj_matrix_list, N_samples, include_zeroes, show_progress)
    adj_matrix_list_class <- checks$adj_matrix_list_class
    show_progress <- checks$show_progress
  } else {
    adj_matrix_list_class <- get_adj_matrices_input_type(adj_matrix_list)$object_types_in_list[1]
  }

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
#' @param adj_matrix_list A list of n x n adjacency matrices representing fcms
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

  n_nodes <- unique(unlist(lapply(adj_matrix_list, dim)))
  flatten_conventional_adj_matrix <- function(adj_matrix) {
    if (identical(methods::is(as.vector(adj_matrix)), methods::is(list()))) {
      flattened_adj_matrix <- do.call(c, as.vector(adj_matrix))
    } else {
      flattened_adj_matrix <- as.vector(adj_matrix)
    }

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
#' @param fuzzy_set_adj_matrix_list A list of n x n fuzzy adjacency matrices representing fcms
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

  if (!(fuzzy_set_adj_matrix_list_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input fuzzy_set_adj_matrix_list_class must be one of the following: 'conventional', 'ivfn', or 'tfn'")
  }

  n_nodes <- unique(unlist(lapply(fuzzy_set_adj_matrix_list, dim)))

  flatten_fuzzy_adj_matrix <- function(fuzzy_adj_matrix) do.call(cbind, lapply(as.vector(fuzzy_adj_matrix), rbind))
  flattened_fuzzy_set_adj_matrix_list <- do.call(rbind, lapply(fuzzy_set_adj_matrix_list, flatten_fuzzy_adj_matrix))
  flattened_fuzzy_set_adj_matrix_list_w_distributions <- convert_fuzzy_set_elements_in_matrix_to_distributions(fuzzy_set_matrix = flattened_fuzzy_set_adj_matrix_list, object_class = fuzzy_set_adj_matrix_list_class, N_samples = N_samples)

  if (!include_zeroes) {
    flattened_fuzzy_set_adj_matrix_list_w_distributions <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, c(1, 2), function(element) ifelse(element[[1]][[1]] == 0, NA, element[[1]][[1]]), simplify = FALSE)
  } else {
    flattened_fuzzy_set_adj_matrix_list_w_distributions <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, c(1, 2), function(element) element[[1]][[1]], simplify = FALSE)
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(
      flattened_fuzzy_set_adj_matrix_list_w_distributions, 2,
      function(column_vec) {
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
    # browser()
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
#' @param skip_checks FOR DEVELOPER USE ONLY. TRUE if infer_fcm is called within
#' fcmconfr() and checks have already been performed
#'
#' @returns A list of raw bootstrap draws and a dataframe of confidence intervals
#'
#' @keywords internal
#'
#' @export
#' @example man/examples/ex-get_mc_sims_inference_CIs_w_bootstrap.R
get_mc_simulations_inference_CIs_w_bootstrap <- function(mc_simulations_inference_df = data.frame(),
                                                         inference_function = "mean",
                                                         confidence_interval = 0.95,
                                                         bootstrap_reps = 1000,
                                                         parallel = TRUE,
                                                         n_cores = integer(),
                                                         show_progress = TRUE,
                                                         skip_checks = FALSE) {
  # Adding for R CMD Check. Does not impact logic.
  iter <- NULL

  # Write checks to confirm mc_simulations_inference_df object is correct... Also write a better name
  # so it is understood that it works for simulate_fmcm objects too
  if (!identical(class(mc_simulations_inference_df), "data.frame")) {
    stop("Input mc_simulations_inference_df must be a data.frame object from the
         output of simulate_fmcm_models (final_states_across_sims) or infer_fmcm
         (inference)")
  }

  # Check function inputs
  if (!is.logical(skip_checks)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var skip_checks} must be a logical value (TRUE/FALSE)",
      "+++++> Input {.var skip_checks} was: {skip_checks}"
    )))
  }
  if (!skip_checks) {
    mcbc_checks <- check_monte_carlo_bootstrap_inputs(inference_function, confidence_interval, bootstrap_reps, parallel, n_cores, show_progress)
    ci_centering_function <- mcbc_checks$ci_centering_function
    parallel <- mcbc_checks$parallel
    n_cores <- mcbc_checks$n_cores
    show_progress <- mcbc_checks$show_progress
  }

  bootstrap_draws_per_rep <- nrow(mc_simulations_inference_df)

  node_names <- colnames(mc_simulations_inference_df)

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
          #data.frame(apply(
          apply(
            mc_simulations_inference_df, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              mean(random_draws)
            },
            simplify = TRUE
          )
        }
    } else if (inference_function == "median") {
      bootstrapped_medians_of_inference_by_node <- foreach::foreach(
        i = 1:bootstrap_reps, .options.snow = opts) %dopar% {
          #data.frame(apply(
          apply(
            mc_simulations_inference_df, 2,
            function(inference) {
              random_draws <- sample(inference, bootstrap_draws_per_rep, replace = TRUE)
              stats::median(random_draws)
            },
            simplify = TRUE
          )
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
    bootstrapped_expectations_of_inference_by_node <- do.call(rbind, bootstrapped_means_of_inference_by_node)
  } else if (inference_function == "median") {
    bootstrapped_expectations_of_inference_by_node <- do.call(rbind, bootstrapped_medians_of_inference_by_node)
  }
  colnames(bootstrapped_expectations_of_inference_by_node) <- node_names
  expected_value_of_inference_by_node <- apply(bootstrapped_expectations_of_inference_by_node, 2, mean)

  # print("Getting upper and lower quantile estimates of mean", quote = FALSE)
  lower_CI <- (1 - confidence_interval)/2
  upper_CI <- (1 + confidence_interval)/2
  lower_CIs_by_node <- apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, lower_CI), simplify = TRUE)
  upper_CIs_by_node <- apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, upper_CI), simplify = TRUE)

  nodes <- ifelse(names(lower_CIs_by_node) == names(upper_CIs_by_node), names(lower_CIs_by_node), stop("Error with quantiles calculation"))

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

  quantiles_of_mc_simulation_inferences <- data.frame(t(apply(mc_simulations_inference_df, 2, stats::quantile)))
  mc_inference_distributions_df <- data.frame(cbind(
    CIs_by_node$node, CIs_by_node$expected_value, CIs_by_node$lower_CI, CIs_by_node$upper_CI,
    quantiles_of_mc_simulation_inferences$X0., quantiles_of_mc_simulation_inferences$X25., quantiles_of_mc_simulation_inferences$X50.,
    quantiles_of_mc_simulation_inferences$X75., quantiles_of_mc_simulation_inferences$X100.
  ))

  colnames(mc_inference_distributions_df) <- c(
    "node", "expected_value", paste0(lower_CI, "_CI"), paste0(upper_CI, "_CI"),
    "min",  "0.25_quantile", "median", "0.75_quantile", "max"
  )

  for (col in 2:ncol(mc_inference_distributions_df)) {
    mc_inference_distributions_df[, col] <- as.numeric(mc_inference_distributions_df[, col])
  }

  print("Done", quote = FALSE)

  structure(
    .Data = list(
      CIs_and_quantiles_by_node = mc_inference_distributions_df,
      bootstrap_expected_values = bootstrapped_expectations_of_inference_by_node
    )
  )
}


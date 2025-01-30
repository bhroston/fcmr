

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

  # if (parallel) {
  #   max_possible_cores <- parallel::detectCores()
  #   if (identical(n_cores, integer())) {
  #     warning(cli::format_warning(c(
  #       "!" = "Warning: {.var n_cores} not defined",
  #       "~~~~~ Assuming {.var n_cores} is the maximum available on the machine: n_cores = {max_possible_cores}"
  #     )))
  #     n_cores <- max_possible_cores
  #   }
  #   if (n_cores > max_possible_cores) {
  #     warning(cli::format_warning(c(
  #       "!" = "Warning: {.var n_cores} is {n_cores} which is greater than the max. cores available on the machine (n = {max_possible_cores})",
  #       "~~~~~ Reducing {.var n_cores} to {max_possible_cores}"
  #     )))
  #   }
  # }
  # if (!parallel & !identical(n_cores, integer())) {
  #   warning(cli::format_warning(c(
  #     "!" = "Warning: {.var n_cores} is ignored since {.var parallel} = FALSE"
  #   )))
  # }

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
    bootstrapped_expectations_of_inference_by_node <- do.call(rbind, bootstrapped_means_of_inference_by_node)
  } else if (inference_function == "median") {
    bootstrapped_expectations_of_inference_by_node <- do.call(rbind, bootstrapped_medians_of_inference_by_node)
  }
  colnames(bootstrapped_expectations_of_inference_by_node) <- node_names
  expected_value_of_inference_by_node <- apply(bootstrapped_expectations_of_inference_by_node, 2, mean)

  # print("Getting upper and lower quantile estimates of mean", quote = FALSE)
  lower_CI <- (1 - confidence_interval)/2
  upper_CI <- (1 + confidence_interval)/2
  lower_CIs_by_node <- data.frame(apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, lower_CI), simplify = FALSE))
  upper_CIs_by_node <- data.frame(apply(bootstrapped_expectations_of_inference_by_node, 2, function(bootstrapped_expectations) stats::quantile(bootstrapped_expectations, upper_CI), simplify = FALSE))

  nodes <- ifelse(colnames(lower_CIs_by_node) == colnames(upper_CIs_by_node), colnames(lower_CIs_by_node), stop("Error with quantiles calculation"))

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



#' Check inputs for building monte carlo fcm inputs
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @param adj_matrix_list A list of n x n adjacency matrices representing fcms
#' @param N_samples The number of samples to draw from the corresponding distribution
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns NULL; Errors if checks fail
#'
#' @keywords internal
#'
#' @export
#' @examples
#' NULL
check_build_monte_carlo_fcms_inputs <- function(adj_matrix_list,
                                                N_samples,
                                                include_zeroes,
                                                show_progress) {

  adj_matrix_list_class <- get_adj_matrices_input_type(adj_matrix_list)$object_types_in_list[1]

  # Check N_samples
  if (!is.numeric(N_samples)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var N_samples} must be a positive integer",
      "+++++> Input {.var N_samples} was {N_samples}"
    )))
  }

  if (!(N_samples == round(N_samples))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var N_samples} must be a positive integer",
      "+++++> Input {.var N_samples} was {N_samples}"
    )))
  }
  # ----

  # Check include_zeroes
  if (!is.logical(include_zeroes)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var include_zeroes} must be logical (TRUE/FALSE)",
      "+++++> Input {.var include_zeroes} was {include_zeroes}"
    )))
  }

  # Check show_progress
  show_progress = check_if_local_machine_has_access_to_show_progress_functionalities(use_parallel = FALSE, show_progress)

  list(
    adj_matrix_list_class = adj_matrix_list_class,
    show_progress = show_progress
  )
}



#' Check inputs for monte carlo bootstrap analysis
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @param ci_centering_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param confidence_interval What are of the distribution should be bounded by the
#' confidence intervals? (e.g. 0.95)
#' @param num_ci_bootstraps Repetitions for bootstrap process, if chosen
#' @param parallel TRUE/FALSE Whether to perform the function using parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns NULL; Errors if checks fail
#'
#' @keywords internal
#'
#' @export
#' @examples
#' NULL
check_monte_carlo_bootstrap_inputs <- function(ci_centering_function = c("mean", "median"),
                                               confidence_interval = 0.95,
                                               num_ci_bootstraps = 1000,
                                               parallel = TRUE,
                                               n_cores = integer(),
                                               show_progress = TRUE) {

  # Check ci_centering_function ----
  if (identical(ci_centering_function, c("mean", "median"))) {
    warning(cli::format_warning(c(
      "!" = "Warning: No {.var ci_centering_function} given",
      "~~~~~ Assuming {.var ci_centering_function} is 'mean'"
    )))
    ci_centering_function <- "mean"
  }
  if (!(ci_centering_function %in% c("mean", "median"))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var ci_centering_function} must be one of the following: 'mean' or 'median'",
      "+++++> Input {.var ci_centering_function} was '{ci_centering_function}'"
    )))
  }
  # ----

  # Check confidence_interval ----

  if (!is.numeric(confidence_interval)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var confidence_interval} must be a positive value between 0 and 1",
      "+++++> Input {.var confidence_interval} was '{confidence_interval}'"
    )))
  }

  if (confidence_interval < 0 | confidence_interval >= 1) {
    stop(cli::format_error(c(
      "x" = "Error: {.var confidence_interval} must be a positive value between 0 and 1",
      "+++++> Input {.var confidence_interval} was {confidence_interval}"
    )))
  }
  # ----

  # Check num_ci_bootstraps
  if (!is.numeric(num_ci_bootstraps)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var num_ci_bootstraps} must be a positive integer, typically greater than 1000",
      "+++++> Input {.var num_ci_bootstraps} was '{num_ci_bootstraps}'"
    )))
  }

  if (!(num_ci_bootstraps == round(num_ci_bootstraps))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var num_ci_bootstraps} must be a positive integer, typically greater than 1000",
      "+++++> Input {.var num_ci_bootstraps} was {num_ci_bootstraps}"
    )))
  }
  if (num_ci_bootstraps <= 0) {
    stop(cli::format_error(c(
      "x" = "Error: {.var num_ci_bootstraps} must be a positive value (typically > 1000)",
      "+++++> Input {.var num_ci_bootstraps} was {num_ci_bootstraps}"
    )))
  }
  # ----

  # Check Runtime Options ----
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)
  if (parallel) {
    if (identical(n_cores, integer())) {
      warning(cli::format_warning(c(
        "!" = "Warning: No {.var n_cores} given.",
        "~~~~~ Assuming {.var n_cores} is {parallel::detectCores() - 1} (i.e. the max available cores minus 1)"
      )))
      n_cores <- parallel::detectCores() - 1
    }
    if (!is.numeric(n_cores)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var n_cores} must be a positive integer",
        "+++++ Input {.var n_cores} was '{n_cores}'"
      )))
    }
    if (!(n_cores == round(n_cores))) {
      stop(cli::format_error(c(
        "x" = "Error: {.var n_cores} must be a positive integer",
        "+++++ Input {.var n_cores} was {n_cores}"
      )))
    }
    if (n_cores <= 0) {
      stop(cli::format_error(c(
        "x" = "Error: {.var n_cores} must be a positive integer",
        "+++++ Input {.var n_cores} was {n_cores}"
      )))
    }
    if (n_cores > parallel::detectCores()) {
      stop(cli::format_error(c(
        "x" = "Error: {.var n_cores} must be a positive integer less than or equal to {parallel::detectCores()} (i.e. the max available cores on your machine)",
        "+++++ Input {.var n_cores} was {n_cores}"
      )))
    }
  }
  if (!parallel & !identical(n_cores, integer())) {
    warning(cli::format_warning(c(
      "!" = "Warning: {.var n_cores} given but {.var parallel} = FALSE.",
      "~~~~~ Ignoring {.var n_cores} input."
    )))
  }
  # ----

  list(
    ci_centering_function = ci_centering_function,
    parallel = parallel,
    n_cores = n_cores,
    show_progress = show_progress
  )
}



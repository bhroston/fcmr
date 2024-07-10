
#' confer_fmcm
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
confer_fmcm <- function(simulated_adj_matrices = list(matrix()),
                        initial_state_vector = c(),
                        clamping_vector = c(),
                        activation = "kosko", # Something wrong with papageorgiou activation; returning negative numbers... works for sigmoid, but not tanh
                        squashing = "tanh",
                        lambda = 1,
                        max_iter = 100,
                        min_error = 1e-5,
                        lambda_optimization = "none", # Getting error with lambda optimization
                        IDs = c(),
                        parallel = TRUE,
                        n_cores = integer(),
                        show_progress = TRUE,
                        include_simulations_in_output = FALSE) {

  # Adding for R CMD check. Does not impact logic.
  # iter <- NULL
  i <- NULL

  # Confirm packages necessary packages are available. If not, change run options
  if (parallel) {
    if (!requireNamespace("parallel")) {
      parallel <- FALSE
      warning("Parallel processing requires the 'parallel' package which is
              currently not installed. Running without parallel processing.")
    }
    if (show_progress) {
      if (!requireNamespace("doSNOW")) {
        show_progress <- FALSE
        warning("Showing progress with parallel processing requires the 'doSNOW' package which is
              currently not installed. Running in parallel but without showing progress.")
      }
      if (!requireNamespace("foreach")) {
        show_progress <- FALSE
        warning("Showing progress with parallel processing requires the 'foreach' package which is
              currently not installed. Running in parallel but without showing progress.")
      }
    }
  }
  if (!parallel) {
    if (show_progress) {
      if (!requireNamespace("pbapply")) {
        show_progress <- FALSE
        warning("Showing progress requires the 'pbapply' package which is
              currently not installed. Running without showing progress.")
      }
    }
  }

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
    if (identical(n_cores, integer())) {
      n_cores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(n_cores)

    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("simulated_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
                 "squashing", "lambda", "max_iter", "min_error",
                 "lambda_optimization", "IDs",
                 "confer_fcm", "simulate_fcm",  "confirm_adj_matrix_is_square",
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
        confer_fcm(
          adj_matrix = simulated_adj_matrices[[i]],
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = lambda_optimization,
          IDs = IDs
        )
      }
    close(pb)
    names(fmcm_confer_results) <- paste0("sim_", 1:length(fmcm_confer_results))
    parallel::stopCluster(cl)

  } else if (parallel & !show_progress) {
    print("Initializing cluster", quote = FALSE)
    if (identical(n_cores, integer())) {
      n_cores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(n_cores)

    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("simulated_adj_matrices", "initial_state_vector", "clamping_vector", "activation",
                 "squashing", "lambda", "max_iter", "min_error",
                 "lambda_optimization", "IDs",
                 "confer_fcm", "simulate_fcm",  "confirm_adj_matrix_is_square",
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
        confer_fcm(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = lambda_optimization,
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
        confer_fcm(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = lambda_optimization,
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
        confer_fcm(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
          clamping_vector = clamping_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = lambda_optimization,
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
        inferences_for_plotting = inference_plot_data,
        sims = fmcm_confer_results
      ),
      class = "fmcmconfer"
    )
  } else {
    structure(
      .Data = list(
        inference = inference_values_by_sim,
        inferences_for_plotting = inference_plot_data
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
#' @param fmcm_inferences Output of get_simulated_values_across_iters
#' @param quantile The quantile to return. see ?quantile() for more
#'
#' @export
get_quantile_of_fmcm_inferences <- function(fmcm_inferences = data.frame(), quantile = 0.5) {
  sims_in_inferences_rownames <- identical("sim", unique(unlist(lapply(strsplit(rownames(fmcm_inferences), "_"), function(x) x[[1]]))))
  if (!sims_in_inferences_rownames | !identical(class(fmcm_inferences), "data.frame")) {
    stop("Input must be a data frame of values observed for each node across
    numerous simulations. They are produced by the confer_fmcm and simulate fmcm functions.")
  }
  node_quantiles <- data.frame(apply(fmcm_inferences, 2, function(node_sims) stats::quantile(node_sims, quantile)))
  node_quantiles
}


#' get_means_of_fmcm_inferences
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
#' @param fmcm_inferences The final values of a set of fcm simulations; also the inferences of a confer_fmcm object
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
get_means_of_fmcm_inferences <- function(fmcm_inferences = list(),
                                         get_bootstrapped_means = TRUE,
                                         confidence_interval = 0.95,
                                         bootstrap_reps = 1000,
                                         bootstrap_samples_per_rep = 1000,
                                         parallel = FALSE,
                                         n_cores = integer(),
                                         show_progress = TRUE) {
  # Adding for R CMD Check. Does not impact logic.
  iter <- NULL

  # Write checks to confirm fmcm_inferences object is correct... Also write a better name
  # so it is understood that it works for simulate_fmcm objects too
  if (!identical(class(fmcm_inferences), "data.frame")) {
    stop("Input fmcm_inferences must be a data.frame object from the
         output of simulate_fmcm_models (final_states_across_sims) or confer_fmcm
         (inferences)")
  }

  # Confirm packages necessary packages are available. If not, change run options
  if (parallel) {
    if (!requireNamespace("parallel")) {
      parallel <- FALSE
      warning("Parallel processing requires the 'parallel' package which is
              currently not installed. Running without parallel processing.")
    }
    if (show_progress) {
      if (!requireNamespace("doSNOW")) {
        show_progress <- FALSE
        warning("Showing progress with parallel processing requires the 'doSNOW' package which is
              currently not installed. Running in parallel but without showing progress.")
      }
      if (!requireNamespace("foreach")) {
        show_progress <- FALSE
        warning("Showing progress with parallel processing requires the 'foreach' package which is
              currently not installed. Running in parallel but without showing progress.")
      }
    }
  }
  if (!parallel) {
    if (show_progress) {
      if (!requireNamespace("pbapply")) {
        show_progress <- FALSE
        warning("Showing progress requires the 'pbapply' package which is
              currently not installed. Running without showing progress.")
      }
    }
  }

  if (!get_bootstrapped_means) {
    means_of_inferences_by_node <- data.frame(apply(fmcm_inferences, 2, mean, simplify = FALSE)) # the simplify is purely for data cleaning reasons
    return(means_of_inferences_by_node)

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
    vars <- list("fmcm_inferences",
                 "bootstrap_reps",
                 "bootstrap_samples_per_rep"
    )
    parallel::clusterExport(cl, varlist = vars, envir = environment())
    print("Sampling means", quote = FALSE)
    doSNOW::registerDoSNOW(cl)
    invisible(utils::capture.output(pb <- utils::txtProgressBar(min = 0, max = ceiling(bootstrap_reps/n_cores), width = 50, style = 3)))
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    bootstrapped_means_of_inferences_by_node <- foreach::foreach(
      i = 1:bootstrap_reps, .options.snow = opts) %dopar% {
        data.frame(apply(
         fmcm_inferences, 2,
          function(inferences) {
            random_draws <- sample(inferences, bootstrap_samples_per_rep, replace = TRUE)
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
    vars <- list("fmcm_inferences",
                 "bootstrap_reps",
                 "bootstrap_samples_per_rep"
    )
    parallel::clusterExport(cl, varlist = vars, envir = environment())
    print("Sampling means", quote = FALSE)
    rep_inferences_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inferences_by_node <- lapply(rep_inferences_by_node, function(duplicate) duplicate <- fmcm_inferences)
    bootstrapped_means_of_inferences_by_node <- parallel::parLapply(
      cl,
      rep_inferences_by_node,
      function(inferences_by_node_duplicate) {
        apply(
          inferences_by_node_duplicate, 2,
          function(inferences) {
            random_draws <- sample(inferences, bootstrap_samples_per_rep, replace = TRUE)
            mean(random_draws)
          }
        )
      }
    )
    parallel::stopCluster(cl)

  } else if (get_bootstrapped_means & !parallel & show_progress) {
    bootstrapped_means_of_inferences_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inferences_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inferences_by_node <- lapply(rep_inferences_by_node, function(duplicate) duplicate <- fmcm_inferences)
    bootstrapped_means_of_inferences_by_node <- pbapply::pblapply(
      rep_inferences_by_node,
      function(inferences_by_node_duplicate) {
        apply(
          inferences_by_node_duplicate, 2,
          function(inferences) {
            random_draws <- sample(inferences, bootstrap_samples_per_rep, replace = TRUE)
            mean(random_draws)
          }
        )
      }
    )

  } else if (get_bootstrapped_means & !parallel & !show_progress) {
    rep_inferences_by_node <- vector(mode = "list", length = bootstrap_reps)
    rep_inferences_by_node <- lapply(rep_inferences_by_node, function(duplicate) duplicate <- fmcm_inferences)
    bootstrapped_means_of_inferences_by_node <- lapply(
      rep_inferences_by_node,
      function(inferences_by_node_duplicate) {
        apply(
          inferences_by_node_duplicate, 2,
          function(inferences) {
            random_draws <- sample(inferences, bootstrap_samples_per_rep, replace = TRUE)
            mean(random_draws)
          }
        )
      }
    )
  }

  bootstrapped_means_of_inferences_by_node <- data.frame(do.call(rbind, bootstrapped_means_of_inferences_by_node))

  # print("Getting upper and lower quantile estimates of mean", quote = FALSE)
  lower_quantile <- (1 - confidence_interval)/2
  upper_quantile <- (1 + confidence_interval)/2
  lower_quantiles_by_node <- data.frame(apply(bootstrapped_means_of_inferences_by_node, 2, function(bootstrapped_means) stats::quantile(bootstrapped_means, lower_quantile), simplify = FALSE))
  upper_quantiles_by_node <- data.frame(apply(bootstrapped_means_of_inferences_by_node, 2, function(bootstrapped_means) stats::quantile(bootstrapped_means, upper_quantile), simplify = FALSE))

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

  return(quantiles_by_node)
}




#' build_fmcm_models
#'
#' @description
#' This function generates n fcm models whose edge weights are sampled from the
#' defined distribution ('uniform', 'gaussian', 'beta', or 'triangular') and stores
#' them as a list of fmcm objects
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents the edge weights
#' of an FCM
#' @param IDs A list of names for each node (must have n items)
#' @param n_sims The number of simulated fcm's to generate
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param distribution A statistical distribution to draw random samples from.
#' Must be one of the following: 'uniform', 'gaussian', 'beta', or 'triangular'
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param ... Additional adj_matrix objects whose weights describe shape parameters
#' of the chosen distribution.
#' IF distribution = "uniform", must include: lower_adj_matrix and upper_adj_matrix objects
#' IF distribution = "gaussian", must include: sd_adj_matrix
#' IF distribution = "beta", must include: sd_adj_matrix
#' IF distribution = "triangular", must include: lower_adj_matrix, upper_adj_matrix, and mode_adj_matrix objects
#'  Note: if no mode_adj_matrix given, will assume its values are the average of the lower and upper adj_matrix objects.
#'
#' @export
build_fmcm_models <- function(adj_matrix = matrix(),
                              IDs = c(),
                              n_sims = 100,
                              parallel = TRUE,
                              distribution = "uniform", # Also accepts "gaussian", "beta", and triangular
                              show_progress = TRUE,
                              ...) {
  # Validate parameter adj_matrix inputs
  additional_inputs <- list(...)
  if (identical(names(additional_inputs), "...")) additional_inputs <- additional_inputs[[1]] # if ... = list() as input
  names_of_additional_inputs <- names(additional_inputs)

  additional_inputs_have_correct_dims <- unlist(lapply(additional_inputs, function(input) unique(dim(input)) == unique(dim(adj_matrix))))
  if (!all(additional_inputs_have_correct_dims)) {
    stop("all additional input adjacency matrices must have the same dimensions as the input adj_matrix object")
  }

  additional_inputs_have_correct_edges <- unlist(lapply(additional_inputs, function(input) identical(which(input == 0), which(adj_matrix == 0))))
  if (!all(additional_inputs_have_correct_edges)) {
    stop("all additional input adjacency matrices must have the same edges as the input adj_matrix (i.e. values
         in the same locations in the adjacency matrix")
  }

  # Confirm packages necessary packages are available. If not, change run options
  if (parallel) {
    if (!requireNamespace("parallel")) {
      parallel <- FALSE
      warning("Parallel processing requires the 'parallel' package which is
              currently not installed. Running without parallel processing.")
    }
    if (show_progress) {
      if (!requireNamespace("doSNOW")) {
        show_progress <- FALSE
        warning("Showing progress with parallel processing requires the 'doSNOW' package which is
              currently not installed. Running in parallel but without showing progress.")
      }
      if (!requireNamespace("foreach")) {
        show_progress <- FALSE
        warning("Showing progress with parallel processing requires the 'foreach' package which is
              currently not installed. Running in parallel but without showing progress.")
      }
    }
  }
  if (!parallel) {
    if (show_progress) {
      if (!requireNamespace("pbapply")) {
        show_progress <- FALSE
        warning("Showing progress requires the 'pbapply' package which is
              currently not installed. Running without showing progress.")
      }
    }
  }

  node_order <- colnames(adj_matrix)

  if (distribution == "uniform") {
    # UNIFORM
    uniform_inputs_given <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs))
    if (!uniform_inputs_given) {
      stop("Additional inputs missing for 'uniform' distribution.
        Must include: lower_adj_matrix and upper_adj_matrix objects")
    }
    if (length(additional_inputs) > 2) {
      stop("Too many additional inputs given. Only lower_adj_matrix and upper_adj_matrix needed for UNIFORM distribution fmcm")
    }
    lower <- additional_inputs$lower_adj_matrix
    upper <- additional_inputs$upper_adj_matrix
    if (any(lower[lower != 0] > upper[upper != 0])) {
      stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a UNIFORM distribution")
    }
    fmcm_data <- fmcm(adj_matrix, IDs, distribution,
                      "lower_adj_matrix" = lower,
                      "upper_adj_matrix" = upper)
    edgelist <- fmcm_data$edgelist
    if (show_progress) {
      print("Building models", quote = FALSE)
      edgelist$dist <- pbapply::pbmapply(function(lower, upper) stats::runif(n = n_sims, min = lower, max = upper),
                                         lower = edgelist$lower, upper = edgelist$upper,
                                         SIMPLIFY = FALSE)
    } else {
      edgelist$dist <- mapply(function(lower, upper) stats::runif(n = n_sims, min = lower, max = upper),
                              lower = edgelist$lower, upper = edgelist$upper,
                              SIMPLIFY = FALSE)
    }
  } else if (distribution == "beta") {
    # BETA
    beta_input_given <- "sd_adj_matrix" %in% names_of_additional_inputs
    if (!beta_input_given) {
      stop("Additional inputs missing for 'beta' distribution.
        Must include: sd_adj_matrix object")
    }
    if (length(additional_inputs) > 1) {
      stop("Too many additional inputs given. Only sd_adj_matrix needed for BETA distribution fmcm")
    }
    sd_values <- additional_inputs$sd_adj_matrix
    if (any(sd_values[sd_values != 0] < 0 | sd_values[sd_values != 0] > 0.5)) {
      stop("all values in sd_adj_matrix must be between 0 and +0.5 for a BETA distribution")
    }
    fmcm_data <- fmcm(adj_matrix, IDs, distribution,
                      "sd_adj_matrix" = additional_inputs$sd_adj_matrix)
    edgelist <- fmcm_data$edgelist
    if (show_progress) {
      print("Building models", quote = FALSE)
      edgelist$dist <- pbapply::pbmapply(function(mu, sd) get_beta_distribution_of_values(mu = mu, sd = sd, n = n_sims),
                                         mu = edgelist$weight, sd = edgelist$sd,
                                         SIMPLIFY = FALSE)
    } else {
      edgelist$dist <- mapply(function(mu, sd) get_beta_distribution_of_values(mu = mu, sd = sd, n = n_sims),
                              mu = edgelist$weight, sd = edgelist$sd,
                              SIMPLIFY = FALSE)
    }
  } else if (distribution == "triangular") {
    # TRIANGULAR
    triangular_input_given_w_mode <- all((c("lower_adj_matrix", "upper_adj_matrix", "mode_adj_matrix") %in% names_of_additional_inputs))
    triangular_input_given_wo_mode <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs)) & !("mode_adj_matrix" %in% names_of_additional_inputs)
    if (!(triangular_input_given_w_mode | triangular_input_given_wo_mode)) {
      stop("Additional inputs missing for 'uniform' distribution.
        Must include: lower_adj_matrix and upper_adj_matrix objects.
        Note: if no mode_adj_matrix given, will assume its values are the average of the lower and upper adj_matrix objects.")
    }
    if (triangular_input_given_wo_mode) {
      additional_inputs$mode_adj_matrix <- adj_matrix
      warning("No input mode_adj_matrix given. Using adj_matrix as mode_adj_matrix.")
    }
    if (length(additional_inputs) > 3) {
      stop("Too many additional inputs given. Only lower_adj_matrix, upper_adj_matrix, and mode_adj_matrix needed for TRIANGULAR distribution fmcm")
    }
    lower <- additional_inputs$lower_adj_matrix
    upper <- additional_inputs$upper_adj_matrix
    mode_values <- additional_inputs$mode_adj_matrix
    if (any(lower > upper)) {
      stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a TRIANGULAR distribution")
    }
    if (any((any(lower > mode_values) | any(upper < mode_values)))) {
      stop("all values in mode_adj_matrix must be between their counterparts in lower_adj_matrix and upper_adj_matrix for a TRIANGULAR distribution")
    }
    fmcm_data <- fmcm(adj_matrix, IDs, distribution,
                      "lower_adj_matrix" = additional_inputs$lower_adj_matrix,
                      "upper_adj_matrix" = additional_inputs$upper_adj_matrix,
                      "mode_adj_matrix" = additional_inputs$mode_adj_matrix)
    edgelist <- fmcm_data$edgelist
    if (show_progress) {
      print("Building models", quote = FALSE)
      edgelist$dist <- pbapply::pbmapply(function(lower, upper, mode) get_triangular_distribution_of_values(lower = lower, upper = upper, mode = mode, n_sims),
                                         lower = edgelist$lower, upper = edgelist$upper, mode = edgelist$mode,
                                         SIMPLIFY = FALSE)
    } else {
      edgelist$dist <- mapply(function(lower, upper, mode) get_triangular_distribution_of_values(lower = lower, upper = upper, mode = mode, n_sims),
                              lower = edgelist$lower, upper = edgelist$upper, mode = edgelist$mode,
                              SIMPLIFY = FALSE)
    }
  } else {
    # FINAL CHECK STOP
    stop("Invalid distribution input. Must be one of the following:
         'uniform', 'beta', or 'triangular'")
  }

  # Generate adjacency matrices from sampled distributions
  blank_weight_edgelist <- edgelist[c("source", "target")]
  simulated_edgelists <- rep(list(blank_weight_edgelist), n_sims)
  if (show_progress) {
    print("Adding edge weights to models", quote = FALSE)
    pb <- utils::txtProgressBar(min = 0, max = length(simulated_edgelists), initial = 0, width = 50, char = "+", style = 3)
    for (i in seq_along(simulated_edgelists)) {
      simulated_edgelists[[i]]$weight <- unlist(lapply(edgelist$dist, function(dist) dist[i]))
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
    print("Generating adjacency matrices from models", quote = FALSE)
    simulated_adj_matrices <- pbapply::pblapply(simulated_edgelists, function(edgelist) get_adj_matrix_from_edgelist(edgelist, node_order = node_order))
  } else {
    for (i in seq_along(simulated_edgelists)) {
      simulated_edgelists[[i]]$weight <- unlist(lapply(edgelist$dist, function(dist) dist[i]))
    }
    # Given an edgelist, get_adj_matrix_from_edgelist returns an adjacency matrix
    # whose column and rows are organized alphabetically rather than in accordance
    # with the original input...
    simulated_adj_matrices <- lapply(simulated_edgelists, function(edgelist) get_adj_matrix_from_edgelist(edgelist, node_order = node_order))
  }

  names(simulated_adj_matrices) <- paste0("sim_", seq_along(simulated_adj_matrices))

  simulated_adj_matrices
}



#' fmcm (fuzzy monte carlo markov chain cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for fuzzy cognitive maps that model
#' uncertainty using monte carlo markov chain simulations (See
#' Koasidis et al. 2022 - https://dx.doi.org/10.2139/ssrn.4233987 or
#' Koutsellis et al. 2023 - https://doi.org/10.1016/j.softx.2023.101513 for
#' an application in python.
#'
#' ###It stores the nodes of an FCM and its corresponding adjacency matrix
#' and edgelist.
#'
#' @details
#' fmcm stores fmcmcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents the edge weights
#' of an FCM
#' @param IDs A list of names for each node (must have n items)
#' @param distribution A statistical distribution to draw random samples from.
#' Must be one of the following: 'uniform', 'gaussian', 'beta', or 'triangular'
#' @param ... Additional adj_matrix objects whose weights describe shape parameters
#' of the chosen distribution.
#' IF distribution = "uniform", must include: lower_adj_matrix and upper_adj_matrix objects
#' IF distribution = "gaussian", must include: sd_adj_matrix
#' IF distribution = "beta", must include: sd_adj_matrix
#' IF distribution = "triangular", must include: lower_adj_matrix, upper_adj_matrix, and mode_adj_matrix objects
#'  Note: if no mode_adj_matrix given, will assume its values are the adj_matrix values.
#'
#' @export
fmcm <- function(adj_matrix = matrix(),
                 IDs = c(),
                 distribution = "uniform", # Also accepts "gaussian", "beta", and triangular
                 ...) {

  # Validate adj_matrix input
  confirm_adj_matrix_is_square(adj_matrix)
  confirm_only_numeric_data_in_adj_matrix(adj_matrix)

  # Validate parameter adj_matrix inputs
  additional_inputs <- list(...)
  if (identical(names(additional_inputs), "...")) additional_inputs <- additional_inputs[[1]] # if ... = list() as input
  names_of_additional_inputs <- names(additional_inputs)

  additional_inputs_have_correct_dims <- unlist(lapply(additional_inputs, function(input) unique(dim(input)) == unique(dim(adj_matrix))))
  if (!all(additional_inputs_have_correct_dims)) {
    stop("all additional input adjacency matrices must have the same dimensions as the input adj_matrix object")
  }

  additional_inputs_have_correct_edges <- unlist(lapply(additional_inputs, function(input) identical(which(input == 0), which(adj_matrix == 0))))
  if (!all(additional_inputs_have_correct_edges)) {
    stop("all additional input adjacency matrices must have the same edges as the input adj_matrix (i.e. values
         in the same locations in the adjacency matrix")
  }

  if (distribution == "uniform") {
    # UNIFORM
    uniform_inputs_given <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs))
    if (!uniform_inputs_given) {
      stop("Additional inputs missing for 'uniform' distribution.
        Must include: lower_adj_matrix and upper_adj_matrix objects")
    }
    if (length(additional_inputs) > 2) {
      stop("Too many additional inputs given. Only lower_adj_matrix and upper_adj_matrix needed for UNIFORM distribution fmcm")
    }
    lower <- additional_inputs$lower_adj_matrix
    upper <- additional_inputs$upper_adj_matrix
    if (any(lower[lower != 0] > upper[upper != 0])) {
      stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a UNIFORM distribution")
    }
    distribution_param_adj_matrix_list <- list("lower" = additional_inputs$lower_adj_matrix, "upper" = additional_inputs$upper_adj_matrix)
  } else if (distribution == "beta") {
    # BETA
    beta_input_given <- "sd_adj_matrix" %in% names_of_additional_inputs
    if (!beta_input_given) {
      stop("Additional inputs missing for 'beta' distribution.
        Must include: sd_adj_matrix object")
    }
    if (length(additional_inputs) > 1) {
      stop("Too many additional inputs given. Only sd_adj_matrix needed for BETA distribution fmcm")
    }
    sd_values <- additional_inputs$sd_adj_matrix
    if (any(sd_values[sd_values != 0] < 0 | sd_values[sd_values != 0] > 0.5)) {
      stop("all values in sd_adj_matrix must be between 0 and +0.5 for a BETA distribution")
    }
    distribution_param_adj_matrix_list <- list("sd" = additional_inputs$sd_adj_matrix)
  } else if (distribution == "triangular") {
    # TRIANGULAR
    triangular_input_given_w_mode <- all((c("lower_adj_matrix", "upper_adj_matrix", "mode_adj_matrix") %in% names_of_additional_inputs))
    triangular_input_given_wo_mode <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs)) & !("mode_adj_matrix" %in% names_of_additional_inputs)
    if (!(triangular_input_given_w_mode | triangular_input_given_wo_mode)) {
      stop("Additional inputs missing for 'uniform' distribution.
        Must include: lower_adj_matrix and upper_adj_matrix objects.
        Note: if no mode_adj_matrix given, will assume its values are the average of the lower and upper adj_matrix objects.")
    }
    if (triangular_input_given_wo_mode) {
      additional_inputs$mode_adj_matrix <- adj_matrix
      warning("No input mode_adj_matrix given. Using adj_matrix as mode_adj_matrix.")
    }
    if (length(additional_inputs) > 3) {
      stop("Too many additional inputs given. Only lower_adj_matrix, upper_adj_matrix, and mode_adj_matrix needed for TRIANGULAR distribution fmcm")
    }
    lower <- additional_inputs$lower_adj_matrix
    upper <- additional_inputs$upper_adj_matrix
    mode_values <- additional_inputs$mode_adj_matrix

    if (any(lower > upper)) {
      stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a TRIANGULAR distribution")
    }
    if (any((any(lower > mode_values) | any(upper < mode_values)))) {
      stop("all values in mode_adj_matrix must be between their counterparts in lower_adj_matrix and upper_adj_matrix for a TRIANGULAR distribution")
    }
    distribution_param_adj_matrix_list <- list("lower" = additional_inputs$lower_adj_matrix, "upper" = additional_inputs$upper_adj_matrix, "mode" = additional_inputs$mode_adj_matrix)
  } else {
    # FINAL CHECK STOP
    stop("Invalid distribution input. Must be one of the following:
         'uniform', 'beta', or 'triangular'")
  }

  # Verify additional params
  lapply(distribution_param_adj_matrix_list, confirm_adj_matrix_is_square)
  lapply(distribution_param_adj_matrix_list, confirm_only_numeric_data_in_adj_matrix)

  distribution_params_edgelists <- lapply(distribution_param_adj_matrix_list, function(adj_mat) get_edgelist_from_adj_matrix(adj_mat, IDs))
  distribution_params <- names(distribution_params_edgelists)
  for (i in seq_along(distribution_params)) {
    colnames(distribution_params_edgelists[[i]]) <- c("source", "target", distribution_params[i])
  }

  if (length(distribution_params_edgelists) > 1) {
    for (i in seq_along(distribution_params_edgelists)) {
      if (i == 1) {
        additional_params_edgelist <- merge(distribution_params_edgelists[[1]], distribution_params_edgelists[[2]], all = TRUE)
      } else if (i == length(distribution_params_edgelists)) {
        NULL
      } else {
        additional_params_edgelist <- merge(additional_params_edgelist, distribution_params_edgelists[[i + 1]], all = TRUE)
      }
    }
  } else {
    additional_params_edgelist <- distribution_params_edgelists[[1]]
  }

  # Create fmcm object
  IDs <- get_node_IDs_from_input(adj_matrix, IDs)
  adj_matrix_edgelist <- get_edgelist_from_adj_matrix(adj_matrix, IDs)

  # Combine parameter adj_matrix objects into a single edgelist
  edgelist <- merge(adj_matrix_edgelist, additional_params_edgelist)

  structure(
    .Data = list(
      concepts = IDs,
      distribution = distribution,
      adj_matrix = adj_matrix,
      edgelist = edgelist,
      distribution_params = distribution_param_adj_matrix_list
    ),
    class = "fmcm"
  )
}


#' get_beta_distribution_of_values
#'
#' @description
#' This pulls n samples from a beta distribution described by shape parameters
#' defined by a mean and standard deviation
#'
#' @details
#'
#' Use vignette("fcmcmrr-class") for more information.
#'
#' @param mu mean of the population distribution
#' @param sd standard deviation of the population distribution
#' @param n number of samples to draw from the defined beta distribution
#'
#' @export
get_beta_distribution_of_values <- function(mu = double(), sd = double(), n = 1000) {
  if (!(mu >= 0 & mu <= 1)) {
    stop("mu must be greater than 0 and less than or equal to 1 (i.e. 0 <= mu <= 1).")
  }
  if (!(sd >= 0 & sd <= 0.5)) {
    stop("mu must be greater than 0 and less than or equal to 0.5 (i.e. 0 <= mu <= 1).")
  }

  a <- ((mu*(1 - mu))/(sd^2)) - mu
  b <- a/mu - a
  values_distribution <- stats::rbeta(n, shape1 = a, shape2 = b)

  values_distribution
}


#' get_triangular_distribution_of_values
#'
#' @description
#' This pulls n samples from a triangular distribution described by shape parameters
#' defined by a lower limit, upper limit, and mode
#'
#' @details
#'
#' Use vignette("fcmcmrr-class") for more information.
#'
#' @param lower lower limit or minimum of the sample space
#' @param upper upper limit or maximum of the sample space
#' @param mode peak of the sample space
#' @param n number of samples to draw from the triangular distribution
#'
#' @export
get_triangular_distribution_of_values <- function(lower = double(), upper = double(), mode = double(), n = 1000) {
  if (lower > upper) {
    stop("lower input must be less than upper input")
  }

  if (identical(mode, double())) {
    mode <- (lower + upper)/2
  }
  inv_cdf <- vector(mode = "numeric", length = n)
  for (i in 1:n) {
    x <- i/n
    if (x <= mode) {
      inv_cdf[i] <- sqrt(x*(upper - lower)*(mode - lower)) + lower
    } else if (x > mode) {
      inv_cdf[i] <- upper - sqrt((-x + 1)*(upper - lower)*(upper - mode))
    } else {
      stop("Unknown input")
    }
  }
  values_distribution <- inv_cdf

  values_distribution
}


#' #' get_state_vectors_by_iter_from_fmcm_sims
#' #'
#' #' @description
#' #' This outputs simulation results at a specific or all iters
#' #'
#' #' @details
#' #' This function is designed to streamline the process of getting a distribution
#' #' of values at a specific iter to create histograms or perform bootstrap sampling
#' #' operations to estimate confidence boundes.
#' #'
#' #' Use vignette("fmcm-class") for more information.
#' #'
#' #' @param state_vectors_by_sim Output from simulate_fmcm_models, represents results across
#' #' each simulated fcm
#' #' @param iter The iteration at which to return a distribution of values i.e. get
#' #' values at iteration 'iter'. If no value is given, returns distributions of values across
#' #' each iter in the data set
#' #'
#' #' @export
#' get_state_vectors_by_iter_from_fmcm_sims <- function(state_vectors_by_sim, iter = integer()) {
#'   #state_vectors_by_sim <- lapply(fmcm_simulation, function(model) model$state_vectors)
#'   for (i in seq_along(state_vectors_by_sim)) {
#'     state_vectors_by_sim[[i]] <- cbind(sim = i, state_vectors_by_sim[[i]])
#'   }
#'
#'   state_vectors_by_sim_df <- data.frame(do.call(rbind, state_vectors_by_sim))
#'
#'   if (identical(iter, integer())) {
#'     iter_index <- unique(state_vectors_by_sim_df$iter)
#'     state_vectors_by_iter <- vector(mode = "list", length = length(iter_index))
#'     names(state_vectors_by_iter) <- paste0("iter_", iter_index)
#'     for (i in seq_along(iter_index)) {
#'       state_vectors_by_iter[[i]] <- state_vectors_by_sim_df[state_vectors_by_sim_df$iter == i - 1, ]
#'     }
#'   } else {
#'     state_vectors_by_iter <- state_vectors_by_sim_df[state_vectors_by_sim_df$iter == iter, ]
#'   }
#'
#'   return(state_vectors_by_iter)
#' }
#'
#'
#' #' #' get_quantile_of_fmcm_state_vectors_at_iter
#' #'
#' #' @description
#' #' This gets the user-input quantile of the distribution of simulated values
#' #' across a given iter, or all iters
#' #'
#' #' @details
#' #' This function is designed to streamline the process of getting the custom quantiles
#' #' of a distribution of simulated values across an individual iteration.
#' #'
#' #' Use vignette("fmcm-class") for more information.
#' #'
#' #' @param fmcm_state_vectors_by_iter Output of get_simulated_values_across_iters
#' #' @param quantile The quantile to return. see ?quantile() for more
#' #'
#' #' @export
#' get_quantile_of_fmcm_inferences <- function(fmcm_inferences = list(), quantile = 0.5) {
#'
#'
#'   state_vector_list_is_not_by_iter <- unlist(unique((lapply(names(fmcm_state_vectors_by_iter), function(x) strsplit(x, "_")[[1]][1])))) != "iter"
#'   state_vector_df_variables <- unlist(unique(lapply(fmcm_state_vectors_by_iter, colnames)))
#'   sim_not_in_state_vectors_domain <- !("sim" %in% state_vector_df_variables)
#'
#'   if (state_vector_list_is_not_by_iter | sim_not_in_state_vectors_domain) {
#'     stop("Input must come directly get_state_vectors_by_iter_from_fmcm_sims or
#'         confer_fmcm. If input from confer_fmcm make sure using state_vectors_by_iter.")
#'   }
#'
#'   quantile_by_iter_by_node <- lapply(
#'     fmcm_state_vectors_by_iter,
#'     function(state_vector) {
#'       apply(state_vector, 2, function(state_vector_node) stats::quantile(state_vector_node, quantile))
#'     }
#'   )
#'   quantile_by_iter_by_node <- data.frame(do.call(rbind, quantile_by_iter_by_node))
#'   quantile_by_iter_by_node <- subset(quantile_by_iter_by_node, select = -c(sim))
#'   rownames(quantile_by_iter_by_node) <- NULL
#'
#'   return(quantile_by_iter_by_node)
#' }

# # Get baseline simulations
# print("Performing BASELINE simulations", quote = FALSE)
# baseline_clamping_vector <- rep(0, length(clamping_vector))
# baseline_simulations <- simulate_fmcm_models(simulated_adj_matrices,
#                                               initial_state_vector, baseline_clamping_vector,
#                                               activation, squashing, lambda,
#                                               max_iter, min_error, lambda_optimization,
#                                               IDs, parallel, n_cores, show_progress)
#
# # Get scenario simulations
# cat("\n")
# print("Performing SCENARIO simulations", quote = FALSE)
# scenario_simulations <- simulate_fmcm_models(simulated_adj_matrices,
#                                               initial_state_vector, clamping_vector,
#                                               activation, squashing, lambda,
#                                               max_iter, min_error, lambda_optimization,
#                                               IDs, parallel, n_cores, show_progress)
#
# baseline_state_vectors <- baseline_simulations$state_vectors_by_sim_across_iters
# scenario_state_vectors <- scenario_simulations$state_vectors_by_sim_across_iters
#
# plot(baseline_state_vectors$sim_8$Salinization.of.the.Occoquan.Reservoir)
# points(scenario_state_vectors$sim_8$Salinization.of.the.Occoquan.Reservoir)


# test <- mapply(
#   function(baseline_states, scenario_states) {
#     final_baseline_state <- baseline_states[nrow(baseline_states), ]
#     final_scenario_state <- scenario_states[nrow(scenario_states), ]
#     final_scenario_state - final_baseline_state
#   },
#   baseline_states = baseline_state_vectors,
#   scenario_states = scenario_state_vectors
# )
# df <- data.frame(t(test))

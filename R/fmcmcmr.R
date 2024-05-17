
#' simulate_fmcmcmr_models
#'
#' @description
#' This calculates a sequence of iterations of a simulation over every item in
#' a list of fmcmcmr objects given an initial state vector along with the
#' activation, squashing, and lambda parameters.
#' Additional variables may be defined to control simulation length,
#' column names, and lambda optimization.
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fmcmcmr-class") for more information.
#'
#' @param simulated_adj_matrices A list of adjecency matrices generated from simulation using build_fmcmcmr_models.
#' @param initial_state_vector A list state values at the start of an fcm simulation
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
#'
#' @export
simulate_fmcmcmr_models <- function(simulated_adj_matrices = list(matrix()),
                                    initial_state_vector = c(),
                                    activation = "modified-kosko", # Something wrong with papageorgiou activation; returning negative numbers... works for sigmoid, but not tanh
                                    squashing = "sigmoid",
                                    lambda = 1,
                                    max_iter = 10,
                                    min_error = 1e-5,
                                    lambda_optimization = "none", # Getting error with lambda optimization
                                    IDs = c(),
                                    parallel = TRUE) {
  if (parallel == TRUE) {
    n_cores <- parallel::detectCores()
    # n_cores <- 2
    cl <- parallel::makeCluster(n_cores)

    # Have to store variables in new env that can be accessed by parLapply. There
    # is surely a better way to do this, but this way works
    # start <- Sys.time()
    vars <- list("simulated_adj_matrices", "initial_state_vector", "activation",
                 "squashing", "lambda", "max_iter", "min_error",
                 "lambda_optimization", "IDs",
                 "simulate_fcmr", "confirm_adj_matrix_is_square",
                 "confirm_initial_state_vector_is_compatible_with_adj_matrix",
                 "get_node_IDs_from_input", "optimize_fcmr_lambda",
                 "calculate_next_fcm_state_vector", "squash")

    parallel::clusterExport(cl, varlist = vars, envir = environment())

    fmcmcmr_simulation_results <- parallel::parLapply(
      cl,
      simulated_adj_matrices,
      function(simulated_adj_matrix) {
        simulate_fcmr(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
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
    #print(Sys.time() - start)
    parallel::stopCluster(cl)
  } else {
    fmcmcmr_simulation_results <- lapply(
      simulated_adj_matrices,
      function(simulated_adj_matrix) {
        simulate_fcmr(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
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

  names(fmcmcmr_simulation_results) <- paste0("sim_", seq_along(fmcmcmr_simulation_results))
  fmcmcmr_simulation_results
}


#' get_simulated_values_across_iters
#'
#' @description
#' This outputs simulation results at a specific or all iters
#'
#' @details
#' This function is designed to streamline the process of getting a distribution
#' of values at a specific iter to create histograms or perform bootstrap sampling
#' operations to estimate confidence boundes.
#'
#' Use vignette("fmcmcmr-class") for more information.
#'
#' @param fmcmcmr_simulation Output from simulate_fmcmcmr_models, represents results across
#' each simulated fcm
#' @param iter The iteration at which to return a distribution of values i.e. get
#' values at iteration 'iter'. If no value is given, returns distributions of values across
#' each iter in the data set
#'
#' @export
get_simulated_values_across_iters <- function(fmcmcmr_simulation, iter = integer()) {
  sim_state_vectors <- lapply(fmcmcmr_simulation, function(model) model$state_vectors)
  for (i in seq_along(sim_state_vectors)) {
    sim_state_vectors[[i]] <- cbind(
      iter = 1:nrow(sim_state_vectors[[i]]),
      sim = i,
      sim_state_vectors[[i]]
    )
  }

  sim_state_vectors_df <- data.frame(do.call(rbind, sim_state_vectors))

  if (identical(iter, integer())) {
    iter_index <- unique(sim_state_vectors_df$iter)
    iter_values_across_sims <- vector(mode = "list", length = length(iter_index))
    names(iter_values_across_sims) <- paste0("iter_", iter_index)
    for (i in seq_along(iter_index)) {
      iter_values_across_sims[[i]] <- sim_state_vectors_df[sim_state_vectors_df$iter == i, ]
    }
  } else {
    iter_values_across_sims <- sim_state_vectors_df[sim_state_vectors_df$iter == iter, ]
  }

  return(iter_values_across_sims)
}


#' get_quantiles_of_simulated_values_across_iters
#'
#' @description
#' This gets the user-input quantiles of the distribution of simulated values
#' across a given iter
#'
#' @details
#' This function is designed to streamline the process of getting the custom quantiles
#' of a distribution of simulated values across an individual iteration. Use get_bootstrapped_means
#' to estimate the confidence intervals for the mean value across simulations.
#'
#' Use vignette("fmcmcmr-class") for more information.
#'
#' @param simulated_values_across_iters Output of get_simulated_values_across_iters
#' @param lower_quantile The lower quantile. see ?quantile() for more
#' @param upper_quantile The upper quantile, see ?quantile() for more
#' @param get_bootstrapped_means TRUE/FALSE Whether to perform bootstrap sampling to obtain
#' confidence intervals for the estimation of the mean value across simulations
#' @param bootstrap_reps Repetitions for bootstrap process, if chosen
#' @param bootstrap_samples_per_rep Number of samples to draw (with replacement) from
#' the data per bootstrap_rep
#'
#' @export
get_quantiles_of_simulated_values_across_iters <- function(simulated_values_across_iters = list(),
                                                          lower_quantile = 0.025,
                                                          upper_quantile = 0.975,
                                                          get_bootstrapped_means = FALSE,
                                                          bootstrap_reps = 1000,
                                                          bootstrap_samples_per_rep = 1000) {
  get_quantiles_at_iter <- function(simulated_values_across_iter,
                                    lower_quantile = 0.025,
                                    upper_quantile = 0.975,
                                    get_bootstrapped_means = FALSE,
                                    bootstrap_reps = 100,
                                    bootstrap_samples_per_rep = 100) {
    iter <- NULL
    sim <- NULL # Added to pass R CMD Check. Does NOT change logic flow or function output.
    if (all(c("iter", "sim") %in% colnames(simulated_values_across_iter))) {
      simulated_values_across_iter <- subset(simulated_values_across_iter, select = -c(iter, sim))
    } else {
      stop("simulated_values_across_iter must come directly from an output of
         get_simulated_values_across_iters()")
    }

    if (!get_bootstrapped_means) {
      quantile_values_by_node <- vector(mode = "list", length = ncol(simulated_values_across_iter))
      names(quantile_values_by_node) <- colnames(simulated_values_across_iter)
      for (i in seq_along(quantile_values_by_node)) {
        quantile_values_by_node[[i]] <- data.frame(
          lower = stats::quantile(simulated_values_across_iter[, i], lower_quantile),
          upper = stats::quantile(simulated_values_across_iter[, i], upper_quantile)
        )
        colnames(quantile_values_by_node[[i]]) <- c(paste0("lower_", lower_quantile), paste0("upper_", upper_quantile))
        rownames(quantile_values_by_node[[i]]) <- NULL
      }
    } else {
      if (!is.numeric(bootstrap_reps)) stop("bootstrap_reps must be a positive integer")
      if (bootstrap_reps < 1) stop("bootstrap_reps must be a positive integer")

      if (!is.numeric(bootstrap_samples_per_rep)) stop("bootstrap_samples_per_rep must be a positive integer")
      if (bootstrap_samples_per_rep < 1) stop("bootstrap_samples_per_rep must be a positive integer")

      bootstrapped_means <- data.frame(matrix(data = NA, nrow = bootstrap_reps, ncol = ncol(simulated_values_across_iter)))
      colnames(bootstrapped_means) <- colnames(simulated_values_across_iter)
      for (i in 1:bootstrap_reps) {
        bootstrapped_means[i, ] <- apply(simulated_values_across_iter, 2, function(sim_values) mean(sample(sim_values, bootstrap_samples_per_rep, replace = TRUE)))
      }

      quantile_values_by_node <- vector(mode = "list", length = ncol(simulated_values_across_iter))
      names(quantile_values_by_node) <- colnames(simulated_values_across_iter)
      for (i in seq_along(quantile_values_by_node)) {
        quantile_values_by_node[[i]] <- data.frame(
          lower = stats::quantile(bootstrapped_means[, i], lower_quantile),
          upper = stats::quantile(bootstrapped_means[, i], upper_quantile)
        )
        colnames(quantile_values_by_node[[i]]) <- c(paste0("mean_lower_", lower_quantile), paste0("mean_upper_", upper_quantile))
        rownames(quantile_values_by_node[[i]]) <- NULL
      }
    }
    return(quantile_values_by_node)
  }

  quantiles_across_iters <- lapply(simulated_values_across_iters,
                                   function(values_at_iter) {
                                     get_quantiles_at_iter(values_at_iter,
                                                           lower_quantile,
                                                           upper_quantile,
                                                           get_bootstrapped_means,
                                                           bootstrap_reps,
                                                           bootstrap_samples_per_rep)
                                   })
  node_names <- unlist(unique(lapply(quantiles_across_iters, names)))
  quantiles_across_iters_by_node <- vector(mode = "list", length = length(node_names))
  names(quantiles_across_iters_by_node) <- node_names
  for (i in seq_along(node_names)) {
    upper_quantiles_across_iters <- lapply(quantiles_across_iters, function(means_at_iter) means_at_iter[[i]][1])
    lower_quantiles_across_iters <- lapply(quantiles_across_iters, function(means_at_iter) means_at_iter[[i]][2])
    quantiles_across_iters_by_node[[i]] <- data.frame(
      iter = 1:length(upper_quantiles_across_iters),
      do.call(rbind, lower_quantiles_across_iters),
      do.call(rbind, upper_quantiles_across_iters)
    )
  }

  return(quantiles_across_iters_by_node)
}


#' build_fmcmcmr_models
#'
#' @description
#' This function generates n fcm models whose edge weights are sampled from the
#' defined distribution ('uniform', 'gaussian', 'beta', or 'triangular') and stores
#' them as a list of fmcmcmr objects
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fmcmcmr-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents the edge weights
#' of an FCM
#' @param IDs A list of names for each node (must have n items)
#' @param n_sims The number of simulated fcm's to generate
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param distribution A statistical distribution to draw random samples from.
#' Must be one of the following: 'uniform', 'gaussian', 'beta', or 'triangular'
#' @param ... Additional adj_matrix objects whose weights describe shape parameters
#' of the chosen distribution.
#' IF distribution = "uniform", must include: lower_adj_matrix and upper_adj_matrix objects
#' IF distribution = "gaussian", must include: sd_adj_matrix
#' IF distribution = "beta", must include: sd_adj_matrix
#' IF distribution = "triangular", must include: lower_adj_matrix, upper_adj_matrix, and mode_adj_matrix objects
#'  Note: if no mode_adj_matrix given, will assume its values are the average of the lower and upper adj_matrix objects.
#'
#' @export
build_fmcmcmr_models <- function(adj_matrix = matrix(),
                                 IDs = c(),
                                 n_sims = 100,
                                 parallel = TRUE,
                                 distribution = "uniform", # Also accepts "gaussian", "beta", and triangular
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

  if (distribution == "uniform") {
    # UNIFORM
    uniform_inputs_given <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs))
    if (!uniform_inputs_given) {
      stop("Additional inputs missing for 'uniform' distribution.
        Must include: lower_adj_matrix and upper_adj_matrix objects")
    }
    if (length(additional_inputs) > 2) {
      stop("Too many additional inputs given. Only lower_adj_matrix and upper_adj_matrix needed for UNIFORM distribution fmcmcmr")
    }
    lower <- additional_inputs$lower_adj_matrix
    upper <- additional_inputs$upper_adj_matrix
    if (any(lower[lower != 0] > upper[upper != 0])) {
      stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a UNIFORM distribution")
    }
    fmcmcmr_data <- fmcmcmr(adj_matrix, IDs, distribution,
                            "lower_adj_matrix" = additional_inputs$lower_adj_matrix,
                            "upper_adj_matrix" = additional_inputs$upper_adj_matrix)
    edgelist <- fmcmcmr_data$edgelist
    edgelist$dist <- mapply(function(lower, upper) stats::runif(n = n_sims, min = lower, max = upper),
                            lower = edgelist$lower, upper = edgelist$upper,
                            SIMPLIFY = FALSE)
  } else if (distribution == "beta") {
    # BETA
    beta_input_given <- "sd_adj_matrix" %in% names_of_additional_inputs
    if (!beta_input_given) {
      stop("Additional inputs missing for 'beta' distribution.
        Must include: sd_adj_matrix object")
    }
    if (length(additional_inputs) > 1) {
      stop("Too many additional inputs given. Only sd_adj_matrix needed for BETA distribution fmcmcmr")
    }
    sd_values <- additional_inputs$sd_adj_matrix
    if (any(sd_values[sd_values != 0] < 0 | sd_values[sd_values != 0] > 0.5)) {
      stop("all values in sd_adj_matrix must be between 0 and +0.5 for a BETA distribution")
    }
    fmcmcmr_data <- fmcmcmr(adj_matrix, IDs, distribution,
                            "sd_adj_matrix" = additional_inputs$sd_adj_matrix)
    edgelist <- fmcmcmr_data$edgelist
    edgelist$dist <- mapply(function(mu, sd) get_beta_distribution_of_values(mu = mu, sd = sd, n = n_sims),
                            mu = edgelist$weight, sd = edgelist$sd,
                            SIMPLIFY = FALSE)
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
      stop("Too many additional inputs given. Only lower_adj_matrix, upper_adj_matrix, and mode_adj_matrix needed for TRIANGULAR distribution fmcmcmr")
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
    fmcmcmr_data <- fmcmcmr(adj_matrix, IDs, distribution,
                            "lower_adj_matrix" = additional_inputs$lower_adj_matrix,
                            "upper_adj_matrix" = additional_inputs$upper_adj_matrix,
                            "mode_adj_matrix" = additional_inputs$mode_adj_matrix)
    edgelist <- fmcmcmr_data$edgelist
    edgelist$dist <- mapply(function(lower, upper, mode) get_triangular_distribution_of_values(lower = lower, upper = upper, mode = mode, n_sims),
                            lower = edgelist$lower, upper = edgelist$upper, mode = edgelist$mode,
                            SIMPLIFY = FALSE)
  } else {
    # FINAL CHECK STOP
    stop("Invalid distribution input. Must be one of the following:
         'uniform', 'beta', or 'triangular'")
  }

  # Generate adjacency matrices from sampled distributions
  blank_weight_edgelist <- edgelist[c("source", "target")]
  simulated_edgelists <- rep(list(blank_weight_edgelist), n_sims)
  for (i in seq_along(simulated_edgelists)) {
    simulated_edgelists[[i]]$weight <- unlist(lapply(edgelist$dist, function(dist) dist[i]))
  }
  simulated_adj_matrices <- lapply(simulated_edgelists, get_adj_matrix_from_edgelist)
  names(simulated_adj_matrices) <- paste0("sim_", seq_along(simulated_adj_matrices))

  simulated_adj_matrices
}



#' fmcmcmr (fuzzy monte carlo markov chain cognitive map) S3 class
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
#' fmcmcmr stores fmcmcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' Use vignette("fmcmcmr-class") for more information.
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
fmcmcmr <- function(adj_matrix = matrix(),
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
      stop("Too many additional inputs given. Only lower_adj_matrix and upper_adj_matrix needed for UNIFORM distribution fmcmcmr")
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
      stop("Too many additional inputs given. Only sd_adj_matrix needed for BETA distribution fmcmcmr")
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
      stop("Too many additional inputs given. Only lower_adj_matrix, upper_adj_matrix, and mode_adj_matrix needed for TRIANGULAR distribution fmcmcmr")
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

  # Create fmcmcmr object
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
    class = "fmcmcmr"
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


# start <- Sys.time()
# test <- parallel::mclapply(
#   simulated_adj_matrices,
#   function(simulated_adj_matrix) {
#     simulate_fcmr(
#       adj_matrix = simulated_adj_matrix,
#       initial_state_vector = initial_state_vector,
#       activation = activation,
#       squashing = squashing,
#       lambda = lambda,
#       max_iter = max_iter,
#       min_error = min_error,
#       lambda_optimization = lambda_optimization,
#       IDs = IDs
#     )
#   },
#   mc.cores = n_cores
# )
# print(Sys.time() - start)

# env <- rlang::child_env("base")
# env$simulated_adj_matrices = simulated_adj_matrices
# env$initial_state_vector = initial_state_vector
# env$activation = activation
# env$squashing = squashing
# env$lambda = lambda
# env$max_iter = max_iter
# env$min_error = min_error
# env$lambda_optimization = lambda_optimization
# env$IDs = IDs
#
# env$simulate_fcmr <- rlang::set_env(simulate_fcmr, env)
# env$confirm_adj_matrix_is_square <- rlang::set_env(confirm_adj_matrix_is_square, env)
# env$confirm_initial_state_vector_is_compatible_with_adj_matrix <- rlang::set_env(confirm_initial_state_vector_is_compatible_with_adj_matrix, env)
# env$get_node_IDs_from_input <- rlang::set_env(get_node_IDs_from_input, env)
# env$optimize_fcmr_lambda <- rlang::set_env(optimize_fcmr_lambda, env)
# env$calculate_next_fcm_state_vector <- rlang::set_env(calculate_next_fcm_state_vector, env)
# env$squash <- rlang::set_env(squash, env)

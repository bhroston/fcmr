


test <- data.frame(
  source = c("A", "B"),
  target = c("B", "C"),
  weight = c(0.4, 0.7),
  sd = c(0.1, 0.3)
)

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
#'
#' @export
simulate_fmcmcmr_models <- function(simulated_adj_matrices = list(matrix()),
                                    initial_state_vector = c(),
                                    activation = "modified-kosko",
                                    squashing = "sigmoid",
                                    lambda = 1,
                                    max_iter = 10,
                                    min_error = 1e-5,
                                    lambda_optimization = "none",
                                    IDs = c(),
                                    parallel = TRUE) {
  if (parallel == TRUE) {
    # Only returns one item for parallel processing?
    `%dofuture%` <- doFuture::`%dofuture%`
    future::plan(future::multisession())

    env <- rlang::child_env("base")
    env$simulate_fcmr <- rlang::set_env(simulate_fcmr, env)
    env$confirm_adj_matrix_is_square <- rlang::set_env(confirm_adj_matrix_is_square, env)
    env$confirm_initial_state_vector_is_compatible_with_adj_matrix <- rlang::set_env(confirm_initial_state_vector_is_compatible_with_adj_matrix, env)
    env$get_node_IDs_from_input <- rlang::set_env(get_node_IDs_from_input, env)
    env$optimize_fcmr_lambda <- rlang::set_env(optimize_fcmr_lambda, env)
    env$calculate_next_fcm_state_vector <- rlang::set_env(calculate_next_fcm_state_vector, env)
    env$squash <- rlang::set_env(squash, env)

    fmcmcmr_simulation_results <- foreach::foreach(
      simulated_adj_matrix = simulated_adj_matrices,
      options.future = list(env = env)
    ) %dofuture% {
      env$simulate_fcmr(
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
  names_of_additional_inputs <- names(additional_inputs)

  uniform_inputs_given <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs))
  gaussian_input_given <- "sd_adj_matrix" %in% names_of_additional_inputs
  beta_input_given <- "sd_adj_matrix" %in% names_of_additional_inputs
  triangular_input_given_w_mode <- all((c("lower_adj_matrix", "upper_adj_matrix", "mode_adj_matrix") %in% names_of_additional_inputs))
  triangular_input_given_wo_mode <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs)) & !("mode_adj_matrix" %in% names_of_additional_inputs)

  if (distribution == "uniform" & !uniform_inputs_given) {
    stop("Additional inputs missing for 'uniform' distribution.
        Must include: lower_adj_matrix and upper_adj_matrix objects")
  } else if (distribution == "gaussian" & !gaussian_input_given) {
    stop("Additional inputs missing for 'gaussian' distribution.
        Must include: sd_adj_matrix object")
  } else if (distribution == "beta" & !beta_input_given) {
    stop("Additional inputs missing for 'beta' distribution.
        Must include: sd_adj_matrix object")
  } else if (distribution == "triangular" & !(triangular_input_given_w_mode | triangular_input_given_wo_mode)) {
    stop("Additional inputs missing for 'uniform' distribution.
        Must include: lower_adj_matrix and upper_adj_matrix objects.
        Note: if no mode_adj_matrix given, will assume its values are the average of the lower and upper adj_matrix objects.")
  }

  # Create fmcmcmr objects from inputs
  if (distribution == "uniform") {
    fmcmcmr_data <- fmcmcmr(adj_matrix, IDs, distribution,
                            "lower_adj_matrix" = additional_inputs$lower_adj_matrix,
                            "upper_adj_matrix" = additional_inputs$upper_adj_matrix)
  } else if (distribution == "gaussian") {
    fmcmcmr_data <- fmcmcmr(adj_matrix, IDs, distribution,
                            "sd_adj_matrix" = additional_inputs$sd_adj_matrix)
  } else if (distribution == "beta") {
    fmcmcmr_data <- fmcmcmr(adj_matrix, IDs, distribution,
                            "sd_adj_matrix" = additional_inputs$sd_adj_matrix)
  } else if (distribution == "triangular") {
    if (triangular_input_given_wo_mode) {
      additional_inputs$mode_adj_matrix <- (additional_inputs$lower_adj_matrix + additional_inputs$upper_adj_matrix)/2
    }
    fmcmcmr_data <- fmcmcmr(adj_matrix, IDs, distribution,
                            "lower_adj_matrix" = additional_inputs$lower_adj_matrix,
                            "upper_adj_matrix" = additional_inputs$upper_adj_matrix,
                            "mode_adj_matrix" = additional_inputs$mode_adj_matrix)
  } else {
    stop("Invalid distribution input. Must be one of the following:
         'uniform', 'gaussian', 'beta', or 'triangular'")
  }

  edgelist <- fmcmcmr_data$edgelist

  if (distribution == "uniform") {
    edgelist$dist <- mapply(function(lower, upper) runif(n = n_sims, min = lower, max = upper),
                            lower = edgelist$lower, upper = edgelist$upper,
                            SIMPLIFY = FALSE)
  } else if (distribution == "gaussian") {
    edgelist$dist <- mapply(function(mu, sd) rnorm(n = n_sims, mean = mu, sd = sd),
                            mu = edgelist$weight, sd = edgelist$sd,
                            SIMPLIFY = FALSE)
  } else if (distribution == "beta") {
    edgelist$dist <- mapply(function(mu, sd) get_beta_distribution_of_values(mu = mu, sd = sd, n = n_sims),
                            mu = edgelist$weight, sd = edgelist$sd,
                            SIMPLIFY = FALSE)
  } else if (distribution == "triangular") {
    edgelist$dist <- mapply(function(lower, upper, mode) get_triangular_distribution_of_values(lower = lower, upper = upper, mode = mode, n_sims),
                            lower = edgelist$lower, upper = edgelist$upper, mode = edgelist$mode,
                            SIMPLIFY = FALSE)
  }


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
#'  Note: if no mode_adj_matrix given, will assume its values are the average of the lower and upper adj_matrix objects.
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
#'
#' @export
get_beta_distribution_of_values <- function(mu = double(), sd = double(), n = 1000) {
  a <- ((mu*(1 - mu))/(sd^2)) - mu
  b <- a/mu - a
  values_distribution <- rbeta(n, shape1 = a, shape2 = b)

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



# uniform_inputs_given <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs))
# gaussian_input_given <- "sd_adj_matrix" %in% names_of_additional_inputs
# beta_input_given <- "sd_adj_matrix" %in% names_of_additional_inputs
# triangular_input_given_w_mode <- all((c("lower_adj_matrix", "upper_adj_matrix", "mode_adj_matrix") %in% names_of_additional_inputs))
# triangular_input_given_wo_mode <- all((c("lower_adj_matrix", "upper_adj_matrix") %in% names_of_additional_inputs)) & !("mode_adj_matrix" %in% names_of_additional_inputs)
#
# if (distribution == "uniform" & !uniform_inputs_given) {
#   stop("Additional inputs missing for 'uniform' distribution.
#         Must include: lower_adj_matrix and upper_adj_matrix objects")
# } else if (distribution == "gaussian" & !gaussian_input_given) {
#   stop("Additional inputs missing for 'gaussian' distribution.
#         Must include: sd_adj_matrix object")
# } else if (distribution == "beta" & !beta_input_given) {
#   stop("Additional inputs missing for 'beta' distribution.
#         Must include: sd_adj_matrix object")
# } else if (distribution == "triangular" & !(triangular_input_given_w_mode | triangular_input_given_wo_mode)) {
#   stop("Additional inputs missing for 'uniform' distribution.
#         Must include: lower_adj_matrix and upper_adj_matrix objects.
#         Note: if no mode_adj_matrix given, will assume its values are the average of the lower and upper adj_matrix objects.")
# }
#
# if (distribution == "uniform") {
#   distribution_param_adj_matrix_list <- list("lower" = additional_inputs$lower_adj_matrix, "upper" = additional_inputs$upper_adj_matrix)
# } else if (distribution == "gaussian") {
#   distribution_param_adj_matrix_list <- list("sd" = additional_inputs$sd_adj_matrix)
# } else if (distribution == "beta") {
#   distribution_param_adj_matrix_list <- list("sd" = additional_inputs$sd_adj_matrix)
# } else if (distribution == "triangular") {
#   if (triangular_input_given_wo_mode) {
#     additional_inputs$mode_adj_matrix <- (additional_inputs$lower_adj_matrix + additional_inputs$upper_adj_matrix)/2
#   }
#   distribution_param_adj_matrix_list <- list("lower" = additional_inputs$lower_adj_matrix, "upper" = additional_inputs$upper_adj_matrix, "mode" = additional_inputs$mode_adj_matrix)
# } else {
#   stop("Invalid distribution input. Must be one of the following:
#          'uniform', 'gaussian', 'beta', or 'triangular'")
# }

# # Create fmcmcmr object
# IDs <- get_node_IDs_from_input(adj_matrix, IDs)
# adj_matrix_edgelist <- get_edgelist_from_adj_matrix(adj_matrix, IDs)
#
# # Combine parameter adj_matrix objects into a single edgelist
# lapply(distribution_param_adj_matrix_list, confirm_adj_matrix_is_square)
# lapply(distribution_param_adj_matrix_list, confirm_only_numeric_data_in_adj_matrix)
# distribution_params_edgelists <- lapply(distribution_param_adj_matrix_list, function(adj_mat) get_edgelist_from_adj_matrix(adj_mat, IDs))
# distribution_params <- names(distribution_params_edgelists)
# for (i in seq_along(distribution_params)) {
#   colnames(distribution_params_edgelists[[i]]) <- c("source", "target", distribution_params[i])
# }
#
# if (length(distribution_params_edgelists) > 1) {
#   for (i in seq_along(distribution_params_edgelists)) {
#     if (i == 1) {
#       additional_params_edgelist <- merge(distribution_params_edgelists[[1]], distribution_params_edgelists[[2]], all = TRUE)
#     } else if (i == length(distribution_params_edgelists)) {
#       NULL
#     } else {
#       additional_params_edgelist <- merge(additional_params_edgelist, distribution_params_edgelists[[i + 1]], all = TRUE)
#     }
#   }
# } else {
#   additional_params_edgelist <- distribution_params_edgelists[[1]]
# }
#
# edgelist <- merge(adj_matrix_edgelist, additional_params_edgelist)

# if (any(edgelist$lower > edgelist$upper)) {
#   stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a UNIFORM distribution")
# }
#
#
#
# # Validate edgelist inputs
# if (distribution == "uniform") {
#   if (any(edgelist$lower > edgelist$upper)) {
#     stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a UNIFORM distribution")
#   }
# } else if (distribution == "gaussian") {
#   if (any(edgelist$sd < 0 | edgelist$sd > 1)) {
#     stop("all values in sd_adj_matrix must be between 0 and +1 for a GAUSSIAN distribution")
#   }
# } else if (distribution == "beta") {
#   if (any(edgelist$sd < 0 | edgelist$sd > 0.5)) {
#     stop("all values in sd_adj_matrix must be between 0 and +0.5 for a BETA distribution")
#   }
# } else if (distribution == "triangular") {
#   if (any(edgelist$lower > edgelist$upper)) {
#     stop("all values in lower_adj_matrix must be less than their counterparts in upper_adj_matrix for a TRIANGULAR distribution")
#   }
#   if (any(edgelist$mode < edgelist$lower | edgelist$mode > edgelist$upper)) {
#     stop("all values in mode_adj_matrix must be between their counterparts in lower_adj_matrix and upper_adj_matrix for a TRIANGULAR distribution")
#   }
# }

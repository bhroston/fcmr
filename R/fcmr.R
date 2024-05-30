
#' confer_fcm
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
#'  - activation_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' Use vignette("fcm-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param activation_vector A list state values at the start of an fcm simulation
#' @param scenario_vector A list of values representing specific actions taken to
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
#'
#' @export
confer_fcm <- function(adj_matrix = matrix(),
                       activation_vector = c(),
                       scenario_vector = c(),
                       activation = "kosko", # Problems when activation == "papageorgiou",
                       squashing = "tanh",
                       lambda = 1,
                       max_iter = 10,
                       min_error = 1e-5,
                       lambda_optimization = "none", # Verify this function works
                       IDs = c()) {

  confirm_adj_matrix_is_square(adj_matrix)

  if (identical(activation_vector, c())) {
    warning("No activation_vector input given. Assuming all nodes have an initial state of 1.")
    activation_vector <- rep(1, nrow(adj_matrix))
  }

  if (identical(scenario_vector, c())) {
    warning("No scenario_vector input given. Assuming no values are clamped.")
    scenario_vector <- rep(0, length(activation_vector))
  }

  # Get baseline simulation
  baseline_activation_vector <- rep(1, length(activation_vector))
  baseline_scenario_vector <- rep(0, length(scenario_vector))
  baseline_simulation <- simulate_fcm(adj_matrix,
                                      baseline_activation_vector, baseline_scenario_vector,
                                      activation, squashing, lambda,
                                      max_iter, min_error, lambda_optimization,
                                      IDs)

  # Get scenario simulation
  scenario_activation_vector <- activation_vector
  scenario_scenario_vector <- scenario_vector
  scenario_simulation <- simulate_fcm(adj_matrix,
                                      scenario_activation_vector, scenario_scenario_vector,
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

  inference_state_vectors <- scenario_state_vectors - baseline_state_vectors
  rownames(inference_state_vectors) <- 1:nrow(inference_state_vectors)

  inference_values <- inference_state_vectors[nrow(inference_state_vectors),]
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
#' @param activation_vector A list state values at the start of an fcm simulation
#' @param scenario_vector A list of values representing specific actions taken to
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
#'
#' @export
simulate_fcm <- function(adj_matrix = matrix(),
                          activation_vector = c(),
                          scenario_vector = c(),
                          activation = "kosko", # Problems when activation == "papageorgiou",
                          squashing = "tanh",
                          lambda = 1,
                          max_iter = 10,
                          min_error = 1e-5,
                          lambda_optimization = "none", # Verify this function works
                          IDs = c()) {

  confirm_adj_matrix_is_square(adj_matrix)

  if (identical(activation_vector, c())) {
    warning("No activation_vector input given. Assuming all nodes have an initial state of 1.")
   activation_vector <- rep(1, nrow(adj_matrix))
  }

  if (identical(scenario_vector, c())) {
    warning("No scenario_vector input given. Assuming no values are clamped.")
    scenario_vector <- rep(0, length(activation_vector))
  }

  if (lambda_optimization != "none") {
    lambda <- optimize_fcm_lambda(adj_matrix, squashing, lambda_optimization)
  }

  confirm_activation_vector_is_compatible_with_adj_matrix(adj_matrix, activation_vector)
  IDs <- get_node_IDs_from_input(adj_matrix, IDs)

  state_vectors <- data.frame(matrix(data = numeric(), nrow = max_iter + 1, ncol = length(activation_vector)))

  errors <-  data.frame(matrix(data = numeric(), nrow = max_iter, ncol = length(activation_vector)))

  state_vectors[1, ] <- activation_vector
  errors[1, ] <- 0

  for (i in 2:(max_iter + 1)) {
    state_vector <- state_vectors[i - 1, ]
    next_state_vector <- calculate_next_fcm_state_vector(adj_matrix, state_vector, activation)
    normalized_state_vector <- squash(next_state_vector, squashing = squashing, lambda = lambda)
    normalized_state_vector[scenario_vector != 0] <- scenario_vector[scenario_vector != 0]
    state_vectors[i, ] <- normalized_state_vector
    errors[i, ] <- abs(as.matrix(state_vectors[i - 1,]) - as.matrix(state_vectors[i, ]))
    total_error <- sum(errors[i, ])
    if (total_error < min_error) {
      state_vectors <- stats::na.omit(state_vectors)
      errors <- stats::na.omit(errors)
      break
    }
  }

  colnames(state_vectors) <- IDs
  colnames(errors) <- IDs

  state_vetors <- cbind(iter = 0:(nrow(state_vectors) - 1), state_vectors)
  errors <- cbind(iter = 0:(nrow(errors) - 1), errors)

  structure(
    .Data = list(
      state_vectors = state_vectors,
      errors = errors,
      params = list(
        adj_matrix = adj_matrix,
        activation_vector = activation_vector,
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
#' based on the kosko, modified-kosko, or papageorgiou activation functions
#'
#' @details
#' The state of the art of fcm typically applies one of three activation functions
#' in calculating iterative state vector values: kosko, modified-kosko, and
#' papageorgiou (as identified in Gonzales et al. 2018 - https://doi.org/10.1142/S0218213018600102).
#'
#' kosko: Only considers the current iteration (Kosko, 1986 - https://doi.org/10.1016/S0020-7373(86)80040-2)
#'
#' modified-kosko: The previous value of a node influences its future value (Stylio & Groumpos, 2004 - https://doi.org/10.1109/TSMCA.2003.818878)
#'
#' papageorgiou: Like modified-kosko, but assigns nodes with no value with a
#' value of 0.5 to reduce the influence that a lack of initial state information
#' can have on the simulation output (Papageorgiou, 2011 - https://doi.org/10.1016/j.asoc.2009.12.010)=
#'
#' Use vignette("fcm-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#'
#' @export
calculate_next_fcm_state_vector <- function(adj_matrix = matrix(), state_vector = c(), activation = "modified-kosko") {
  adj_matrix <- as.matrix(adj_matrix)
  state_vector <- as.matrix(state_vector)

  if (dim(state_vector)[2] != unique(dim(adj_matrix))) {
    state_vector <- t(state_vector)
  }

  if (activation == "kosko") {
    next_state_vector <- state_vector %*% adj_matrix
  } else if (activation == "modified-kosko") {
    next_state_vector <- state_vector %*% adj_matrix + state_vector
  } else if (activation == "papageorgiou") {
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


#' confirm_activation_vector_is_compatible_with_adj_matrix
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
#' @param activation_vector An n-length list of the initial states of each node in an fcm simulation
confirm_activation_vector_is_compatible_with_adj_matrix <- function(adj_matrix = matrix(), activation_vector = c()) {
  if (length(activation_vector) != unique(dim(adj_matrix))) {
    stop("Length of input activation_vector is does not comply with the dimensions of the input adjacency matrix", .call = FALSE)
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


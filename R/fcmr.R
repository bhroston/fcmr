
#' simulate_fcmr
#'
#' @description
#' This calculates a sequence of iterations of a simulation over an fcmr object
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
#' Use vignette("fcmr-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param state_vector A list state values at the start of an fcm simulation
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
#' of the following: 'NA' or 'koutsellis'
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @export
simulate_fcmr <- function(adj_matrix = matrix(),
                          initial_state_vector = c(),
                          activation = "modified-kosko",
                          squashing = "sigmoid",
                          lambda = 1,
                          max_iter = 10,
                          min_error = 1e-5,
                          lambda_optimization = NA,
                          IDs = c()) {

  confirm_adj_matrix_is_square(adj_matrix)
  confirm_initial_state_vector_is_compatible_with_adj_matrix(adj_matrix, initial_state_vector)
  IDs <- get_node_IDs_from_input(adj_matrix, IDs)

  state_vectors <- data.frame(matrix(data = numeric(), nrow = max_iter + 1, ncol = length(initial_state_vector)))
  colnames(state_vectors) <- IDs

  state_vectors[1, ] <- initial_state_vector

  for (i in 2:(max_iter + 1)) {
    state_vector <- state_vectors[i - 1, ]
    next_state_vector <- calculate_next_fcm_state_vector(adj_matrix, state_vector, activation)
    state_vectors[i, ] <- squash(next_state_vector, squashing = squashing, lambda = lambda)
    total_error <- sum(abs(state_vectors[i - 1,] - state_vectors[i, ]))
    if (total_error < min_error) {
      state_vectors <- na.omit(state_vectors)
      break
    }
  }

  state_vetors <- cbind(iter = 0:(nrow(state_vectors) - 1), state_vectors)

  state_vectors
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
#' Use vignette("fcmr-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#'
#' @export
calculate_next_fcm_state_vector <- function(adj_matrix = matrix(), state_vector = c(), activation = "standard") {
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


#' fcmr (fuzzy cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for ordinary fuzzy cognitive maps (See
#' Kosko, XXXX for example). It stores the nodes of an FCM and its
#' corresponding adjacency matrix and edgelist.
#'
#' @details
#' fcmr stores fcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' fcmr are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fcmr-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param IDs A list of names for each node (must have n items)
#'
#' @export
#' @examples
#' fcmr(adj_matrix = matrix(data = c(0, 1, 1, 0), nrow = 2, ncol = 2))
fcmr <- function(adj_matrix = matrix(), IDs = c()) {
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
    class = "fcmr"
  )
}



#' get_edgelist_from_adj_matrix
#'
#' @description
#' This "gets" an edgelist representing graph described by the input adjacency
#' matrix.
#'
#' @details
#' The input adjacency matrix must be a square n x n matrix. It can be either
#' a matrix, data.frame, tibble, or data.table type object.
#'
#' If the input matrix has named columns, those names will be used as node IDs
#' in the edgelist. Otherwise, generic node IDs will be used (1, 2, ... n)
#'
#' The edgelist returns the following columns: source, target, weight
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param IDs A list of names for each node (must have n items)
#'
#' @export
#' @examples
#' get_edgelist_from_adj_matrix(matrix(data = c(0, 1, 1, 0), nrow = 2, ncol = 2))
get_edgelist_from_adj_matrix <- function(adj_matrix = matrix(), IDs = c()) {
  confirm_adj_matrix_is_square(adj_matrix)
  IDs <- get_node_IDs_from_input(adj_matrix, IDs)

  data_types <- unique(vapply(adj_matrix, class, character(1)))
  only_numeric_data_types <- identical(data_types, "numeric")
  if (only_numeric_data_types) {
    edge_locs <- data.table::data.table(which(adj_matrix != 0, arr.ind = TRUE))
    edge_weights <- mapply(function(row, col) adj_matrix[row, col], row = edge_locs$row, col = edge_locs$col)
  } else {
    stop("Unable to interpret input adjacency matrix. Is this for a 'numeric' matrix?")
  }

  source_IDs <- IDs[edge_locs$row]
  target_IDs <- IDs[edge_locs$col]

  edgelist <- data.frame(
    source = source_IDs,
    target = target_IDs,
    weight = edge_weights
  )

  edgelist
}




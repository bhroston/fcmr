
#' simulate_fcmr
#'
#' @description
#' This calculates a sequence of iterations of a simulation over an fgcmr object
#' given an initial state vector along with the activation, squashing, and lambda
#' parameters. Additional variables may be defined to control simulation length,
#' column names, and lambda optimization.
#'
#' @details
#' This simulates how an fgcm reacts to an input initial state vector. There is a
#' multi-decadal long body of work that has explored numerous activation and squashing
#' functions as well as algorithms to optimize the lambda value for the
#' sigmoid and tanh squashing functions.
#'
#' Use vignette("fgcmr-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param grey_adj_matrix An n x n grey_adjacency matrix that represents an FCM
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
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#' @param algorithm The algorithm to run calculate the activation vector. Must
#' be one of the following: 'salmeron' or 'concepcion'. The 'salmeron'
#' algorithm is the traditional one applied in most papers (https://doi.org/10.1016%2Fj.eswa.2010.04.085),
#' while the 'concepcion' algorithm is a recent advancement that reduces the
#' calculation steps with a minimal reduction in accuracy (https://doi.org/10.1007/978-3-030-52705-1_34)
#'
#' @export
simulate_fgcmr <- function(grey_adj_matrix = matrix(),
                          initial_state_vector = c(),
                          activation = "modified-kosko",
                          squashing = "sigmoid",
                          lambda = 1,
                          max_iter = 50,
                          min_error = 1e-5,
                          IDs = c(),
                          algorithm = "salmeron") {

  confirm_adj_matrix_is_square(grey_adj_matrix)
  confirm_initial_state_vector_is_compatible_with_grey_adj_matrix(grey_adj_matrix, initial_state_vector)
  IDs <- get_node_IDs_from_input(grey_adj_matrix, IDs)


  for (i in seq_along(initial_state_vector)) {
    if (is.numeric(initial_state_vector[[i]])) {
      initial_state_vector[[i]] <- grey_number(initial_state_vector[[i]], initial_state_vector[[i]])
    }
  }

  state_vectors <- matrix(data = list(NA), nrow = max_iter + 1, ncol = length(initial_state_vector))
  ranges <- data.frame(matrix(data = list(NA), nrow = max_iter + 1, ncol = length(initial_state_vector)))
  errors <-  data.frame(matrix(data = NA, nrow = max_iter, ncol = length(initial_state_vector)))

  state_vectors[1, ] <- initial_state_vector
  ranges[1, ] <- vapply(initial_state_vector, function(value) value$upper - value$lower, numeric(1))
  errors[1, ] <- 0

  for (i in 2:(max_iter + 1)) {
    state_vector <- state_vectors[i - 1, ]
    next_state_vector <- calculate_next_fgcm_state_vector(grey_adj_matrix, state_vector, activation, algorithm)
    normalized_next_state_vector <- state_vectors[i, ]
    for (j in seq_along(next_state_vector)) {
      normalized_next_state_vector[[j]] <- grey_number(
        lower = squash(next_state_vector[[j]]$lower, squashing = squashing, lambda = lambda),
        upper = squash(next_state_vector[[j]]$upper, squashing = squashing, lambda = lambda)
      )
      lower_value_error <- abs(state_vector[[j]]$lower - normalized_next_state_vector[[j]]$lower)
      upper_value_error <- abs(state_vector[[j]]$upper - normalized_next_state_vector[[j]]$upper)
      errors[i, j] <- lower_value_error + upper_value_error
    }

    state_vectors[i, ] <- normalized_next_state_vector
    ranges[i, ] <- vapply(normalized_next_state_vector, function(x) x$upper - x$lower, numeric(1))

    total_error <- sum(errors[i, ])
    if (total_error < min_error) {
      state_vectors <- stats::na.omit(state_vectors)
      break
    }
  }

  colnames(state_vectors) <- IDs
  colnames(ranges) <- IDs
  colnames(errors) <- IDs

  state_vectors <- data.frame(state_vectors[rowSums(is.na(state_vectors)) == 0,])
  ranges <- ranges[rowSums(is.na(ranges)) == 0,]
  errors <- errors[rowSums(is.na(errors)) == 0,]

  rownames(state_vectors) <- 0:(nrow(state_vectors) - 1)
  rownames(errors) <- 0:(nrow(errors) - 1)
  rownames(ranges) <- 0:(nrow(ranges) - 1)

  structure(
    .Data = list(
      state_vectors = state_vectors,
      errors = errors,
      ranges = ranges,
      params = list(
        grey_adj_matrix = grey_adj_matrix,
        initial_state_vector = initial_state_vector,
        activation = activation,
        squashing = squashing,
        lambda = lambda,
        max_iter = max_iter,
        min_error = min_error,
        IDs = IDs,
        algorithm = algorithm
      )
    ),
    class = "fgcmr_simulation"
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
#' Use vignette("fgcmr-class") for more information.
#'
#' @param grey_adj_matrix An n x n adjacency matrix that represents an FCM
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#' @param algorithm The algorithm to run calculate the activation vector. Must
#' be one of the following: 'salmeron' or 'concepcion'. The 'salmeron'
#' algorithm is the traditional one applied in most papers (https://doi.org/10.1016%2Fj.eswa.2010.04.085),
#' while the 'concepcion' algorithm is a recent advancement that reduces the
#' calculation steps (https://doi.org/10.1007/978-3-030-52705-1_34) with a minimal reduction in accuracy
#'
#' @export
calculate_next_fgcm_state_vector <- function(grey_adj_matrix = matrix(), state_vector = c(), activation = "modified-kosko", algorithm = "salmeron") {
  next_state_vector <- vector(length = length(state_vector), mode = "list")
  for (k in 1:nrow(grey_adj_matrix)) {
    grey_adj_matrix_col_vector <- grey_adj_matrix[, k]
    for (j in seq_along(grey_adj_matrix_col_vector)) {
      if (is.numeric(grey_adj_matrix_col_vector[[j]])) {
        grey_adj_matrix_col_vector[[j]] <- grey_number(grey_adj_matrix_col_vector[[j]], grey_adj_matrix_col_vector[[j]])
      }
    }

    if (algorithm == "salmeron") {
      state_vector_grey_adj_matrix_dot_product <- calculate_next_fgcm_state_vector_with_salmeron_algorithm(state_vector, grey_adj_matrix_col_vector, activation)
    } else if (algorithm == "concepcion") {
      state_vector_grey_adj_matrix_dot_product <- calculate_next_fgcm_state_vector_with_concepcion_algorithm(state_vector, grey_adj_matrix_col_vector, activation)
    } else {
      stop("Invalid algorithm. Must be one of the following: 'salmeron' or 'concepcion'.
           Both return the same values, but concepcion is faster than salmeron.")
    }

    if (activation == "kosko") {
      next_state_vector[[k]] <- grey_number(
        lower = state_vector_grey_adj_matrix_dot_product$lower,
        upper = state_vector_grey_adj_matrix_dot_product$upper
      )
    } else if (activation == "modified-kosko") {
      next_state_vector[[k]] <- grey_number(
        lower = state_vector[[k]]$lower + state_vector_grey_adj_matrix_dot_product$lower,
        upper = state_vector[[k]]$upper + state_vector_grey_adj_matrix_dot_product$upper
      )
    } else if (activation == "papageorgiou") {
      next_state_vector[[k]] <- grey_number(
        lower = (2*state_vector[[k]]$lower - 1) + state_vector_grey_adj_matrix_dot_product$lower,
        upper = (2*state_vector[[k]]$upper - 1) + state_vector_grey_adj_matrix_dot_product$upper
      )
    }
  }
  next_state_vector
}

#' calculate_next_fgcm_state_vector_with_salmeron_algorithm
#'
#' @description
#' This calculates the next iteration of an individual node of a state vector in
#' an fgcm simulation using the algorithm defined by Salmeron 2010 -
#' https://doi.org/10.1016%2Fj.eswa.2010.04.085 and using the kosko,
#' modified-kosko, or papageorgiou activation functions
#'
#' @details
#' This algorithm from Salmeron 2010 - https://doi.org/10.1016%2Fj.eswa.2010.04.085
# reduces the steps needed to calculate the next state vector
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param grey_adj_matrix_col_vector An column of an adjacency matrix, typically as a list
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#'
#' @export
calculate_next_fgcm_state_vector_with_salmeron_algorithm <- function(state_vector = c(),
                                                                     grey_adj_matrix_col_vector = c(),
                                                                     activation = "modified-kosko") {
  if (activation == "kosko" | activation == "modified-kosko") {
    state_vector_grey_adj_matrix_product <- mapply(
      function(state, adj_matrix_col) {
        product_lower <- min(
          state$lower*adj_matrix_col$lower,
          state$lower*adj_matrix_col$upper,
          state$upper*adj_matrix_col$lower,
          state$upper*adj_matrix_col$upper
        )
        product_upper <- max(
          state$lower*adj_matrix_col$lower,
          state$lower*adj_matrix_col$upper,
          state$upper*adj_matrix_col$lower,
          state$upper*adj_matrix_col$upper
        )
        grey_number(product_lower, product_upper)
      },
      state = state_vector,
      adj_matrix_col = grey_adj_matrix_col_vector
    )
  } else if (activation == "papageorgiou") {
    # (2*state_vector - 1) %*% adj_matrix + (2*state_vector - 1)
    state_vector_grey_adj_matrix_product <- mapply(
      function(state, adj_matrix_col) {
        product_lower <- min(
          (2*state$lower - 1)*adj_matrix_col$lower,
          (2*state$lower - 1)*adj_matrix_col$upper,
          (2*state$upper - 1)*adj_matrix_col$lower,
          (2*state$upper - 1)*adj_matrix_col$upper
        )
        product_upper <- max(
          (2*state$lower - 1)*adj_matrix_col$lower,
          (2*state$lower - 1)*adj_matrix_col$upper,
          (2*state$upper - 1)*adj_matrix_col$lower,
          (2*state$upper - 1)*adj_matrix_col$upper
        )
        grey_number(product_lower, product_upper)
      },
      state = state_vector,
      adj_matrix_col = grey_adj_matrix_col_vector
    )
  }
  state_vector_grey_adj_matrix_dot_product <- grey_number(
    lower = sum(unlist(state_vector_grey_adj_matrix_product[1,])),
    upper = sum(unlist(state_vector_grey_adj_matrix_product[2,]))
  )
  state_vector_grey_adj_matrix_dot_product
}


#' calculate_next_fgcm_state_vector_with_concepcion_algorithm
#'
#' @description
#' This calculates the next iteration of an individual node of a state vector in
#' an fgcm simulation using the algorithm defined by Cocepcicon et al. 2020 -
#' https://doi.org/10.1007/978-3-030-52705-1_34 and using the kosko,
#' modified-kosko, or papageorgiou activation functions
#'
#' @details
#' This algorithm from Concepcícon et al. 2020 - https://doi.org/10.1007/978-3-030-52705-1_34
#  reduces the steps needed to calculate the next state vector
#'
#' There is a minor reduction in accuracy with this algorithm, so practitioners
#' should be aware of the cost-benefit of the faster run-time allowed
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param grey_adj_matrix_col_vector An column of an adjacency matrix, typically as a list
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#'
#' @export
calculate_next_fgcm_state_vector_with_concepcion_algorithm <- function(state_vector = c(),
                                                                       grey_adj_matrix_col_vector = c(),
                                                                       activation = "modified-kosko") {
  if (activation == "kosko" | activation == "modified-kosko") {
    state_vector_grey_adj_matrix_product <- mapply(
      function(state, edge) {
        grey_number(
          lower = (edge$lower*(state$upper*(1 - sin(edge$lower)) + state$lower*(1 + sin(edge$lower))))/2,
          upper = (edge$upper*(state$lower*(1 - sin(edge$upper)) + state$upper*(1 + sin(edge$upper))))/2
        )
      },
      state = state_vector,
      edge = grey_adj_matrix_col_vector
    )
  } else if (activation == "papageorgiou") {
    # (2*state_vector - 1) %*% adj_matrix + (2*state_vector - 1)
    state_vector_grey_adj_matrix_product <- mapply(
      function(state, edge) {
        grey_number(
          lower = (edge$lower*((2*state$upper - 1)*(1 - sin(edge$lower)) + (2*state$lower - 1)*(1 + sin(edge$lower))))/2,
          upper = (edge$upper*((2*state$lower - 1)*(1 - sin(edge$upper)) + (2*state$upper - 1)*(1 + sin(edge$upper))))/2
        )
      },
      state = state_vector,
      edge = grey_adj_matrix_col_vector
    )
  }
  state_vector_grey_adj_matrix_dot_product <- grey_number(
    lower = sum(unlist(state_vector_grey_adj_matrix_product[1,])),
    upper = sum(unlist(state_vector_grey_adj_matrix_product[2,]))
  )
  state_vector_grey_adj_matrix_dot_product
}


#' confirm_initial_state_vector_is_compatible_with_grey_adj_matrix
#'
#' @description
#' Confirm that an initial state vector is algorithmically compatible with a grey adjacency matrix
#'
#' @details
#' Boolean. TRUE if the number of entries in the initial
#' state vector match the number of rows/columns in the adjacency matrix and 2. The
#' datatypes stored within each object are the same (i.e. "numeric" vs "grey_number"),
#' FALSE if not
#'
#' Intended for developer use only to improve package readability.
#'
#' @param grey_adj_matrix An n x n grey adjacency matrix that represents an FCM
#' @param initial_state_vector An n-length list of the initial states of each node in an fcm simulation
confirm_initial_state_vector_is_compatible_with_grey_adj_matrix <- function(grey_adj_matrix = matrix(), initial_state_vector = c()) {
  if (length(initial_state_vector) != unique(dim(grey_adj_matrix))) {
    stop("Length of input initial_state_vector is does not comply with the dimensions of the input adjacency matrix", .call = FALSE)
  } else {
    TRUE
  }

  data_types <- unique(vapply(initial_state_vector, class, character(1)))
  both_numeric_or_grey_number_data_types <- identical(data_types, c("numeric", "grey_number")) | identical(data_types, c("grey_number", "numeric"))
  only_numeric_data_types <- identical(data_types, "numeric")
  only_grey_number_data_types <- identical(data_types, "grey_number")

  if (both_numeric_or_grey_number_data_types | only_numeric_data_types | only_grey_number_data_types) {
    TRUE
  } else {
    stop("Input initial state vector must contain only numeric or grey_number values")
  }
}


#' fgcmr (fuzzy grey cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for fuzzy grey cognitive maps (See
#' ____, XXXX for example). It stores the nodes of an FGCM and its
#' corresponding adjacency matrix and edgelist.
#'
#' @details
#' fgcmr stores fgcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' fgcmr allow for the depiction of uncertainty in fcm as edge weights are
#' represented as Grey Numbers (___, XXXX). Grey Numbers represent a range of
#' possible values that an edge may be within.
#'
#' fgcmr are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param grey_adj_matrix An n x n adjacency matrix that represents an FCM
#' @param IDs A list of names for each node (must have n items)
#'
#' @export
#' @examples
#' NULL
fgcmr <- function(grey_adj_matrix = matrix(), IDs = c()) {
  # Validate input
  confirm_adj_matrix_is_square(grey_adj_matrix)
  IDs <- get_node_IDs_from_input(grey_adj_matrix, IDs)

  data_types <- unique(unlist(lapply(grey_adj_matrix, function(x) vapply(x, class, character(1)))))
  only_numeric_and_grey_number_data_types <- identical(data_types, c("numeric", "grey_number")) | identical(data_types, c("grey_number", "numeric"))
  if (!only_numeric_and_grey_number_data_types) {
    stop("Input adjacency matrix must only contain numeric objects, and all
         objects must be numeric or grey_numbers")
  }

  structure(
    .Data = list(
      concepts = IDs,
      grey_adj_matrix = grey_adj_matrix,
      edgelist = get_edgelist_from_grey_adj_matrix(grey_adj_matrix)
    ),
    class = "fgcmr"
  )
}


#' get_grey_adj_matrix_from_lower_and_upper_adj_matrices
#'
#' @description
#' This "gets" a Grey adjacency matrix from an adjacency matrix of the lower
#' limits of edges in an FCM and an adjacency matrix of the upper limits of edges
#' in an FCM.
#'
#' @details
#' The input adjacency matrices must square n x n matrices with the same dimensions.
#' The input can be either matrix, data.table, tibble, or data.table type objects,
#' but the output will always be a data.frame. This is for output readability.
#' data.table and tibble objects work logically, but their outputs require
#' additional steps to parse from the user's perspective.
#'
#' If the input matrices have named columns, those names will be carried over
#' in the grey adjacency matrix. Otherwise, generic node IDs will be used
#' (C1, C2, ... Cn).
#'
#' #' Use vignette("fgcmr-class") for more information.
#'
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @export
#' @examples
#' get_grey_adj_matrix_from_lower_and_upper_adj_matrices(
#'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' )
get_grey_adj_matrix_from_lower_and_upper_adj_matrices <- function(lower = matrix(),
                                                                  upper = matrix()) {
  if (!identical(dim(lower), dim(upper))) {
    stop("Failed Validation: Input adjacency matrices must be the same size")
  }

  if (nrow(lower) != ncol(lower) | nrow(upper) != ncol(upper)) {
    stop("Failed Validation: Input adjacency matrices must be square matrices (n x n)")
  } else {
    size <- nrow(lower)
  }

  if (identical(names(lower), names(upper)) & !identical(names(lower), NULL)) {
    IDs <- names(lower)
  } else {
    IDs <- paste0("C", 1:nrow(lower))
  }

  edge_locs_in_lower <- data.table::data.table(which(lower != 0, arr.ind = TRUE))
  edge_locs_in_upper <- data.table::data.table(which(upper != 0, arr.ind = TRUE))
  if (!identical(edge_locs_in_lower, edge_locs_in_upper)) {
    warning("Input adjacency matrices must be structurally equivallent. i.e. If
    there is a non-zero value in one matrix, there must be another non-zero
    value at the same location in the other matrix. If one matrix has a non-zero
    value where the other has a zero value, it is assumed that the zero value is
    a part of the grey number for that edge.")
  }
  all_edge_locs <- unique(rbind(edge_locs_in_lower, edge_locs_in_upper))

  #grey_adj_matrix <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  grey_adj_matrix <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  colnames(grey_adj_matrix) <- IDs
  rownames(grey_adj_matrix) <- IDs

  for (index in 1:nrow(all_edge_locs)) {
    i <- all_edge_locs$row[index]
    j <- all_edge_locs$col[index]
    grey_adj_matrix[[j]][[i]] <- grey_number( # [[j]][[i]] instead of [[i]][[j]]
      # because this notation is
      # [[col]][[row]] for data.frames
      lower = lower[i, j],
      upper = upper[i, j]
    )
  }

  colnames(grey_adj_matrix) <- paste0("C", 1:ncol(grey_adj_matrix))

  grey_adj_matrix
}


#' get_grey_adj_matrix_from_list_of_grey_numbers
#'
#' @description
#' This "gets" a Grey adjacency matrix from a list of input values and a list
#' of matrix indexes that identify where those values are located in the matrix
#' (row-col).
#'
#' @details
#' The input values and locations (locs) lists must have equivalent lengths, and
#' must be of type list.
#'
#' If the input lists have named columns, those names will be carried over
#' in the grey adjacency matrix. Otherwise, generic node IDs will be used
#' (C1, C2, ... Cn).
#'
#' #' Use vignette("fgcmr-class") for more information.
#'
#' @param values A list of grey_number or numeric type objects
#' @param locs A list of matrix locations/indexes (row-col) to place entries
#'             in the values list
#' @param size The square (n x n) dimensions of the output matrix. Must be
#'             greater than or equal to the number of values and locs given. Size
#'             is assumed to be the square root of the length of the number of
#'             values given, rounded up to the nearest whole number.
#'
#' @export
#' @examples
#' get_grey_adj_matrix_from_list_of_grey_numbers(
#'  values = c(grey_number(-1, 1), grey_number(-0.5, 0.5)),
#'  locs = c("1-2", "2-1"),
#'  size = 2
#' )
get_grey_adj_matrix_from_list_of_grey_numbers <- function(values = c(),
                                                          locs = c("row-col"),
                                                          size = numeric()) {
  if (identical(size, numeric()) & !identical(locs, c("row-col"))) {
    rowDims <- lapply(locs, function(x) as.numeric(gsub("-.*", "", x)))
    colDims <- lapply(locs, function(x) as.numeric(gsub(".*-", "", x)))
    size = max(unlist(c(rowDims, colDims)))
  } else if (identical(size, numeric()) & identical(locs, c("row-col"))) {
    size = ceiling(sqrt(length(locs)))
  } else if (size^2 < length(locs)) {
    stop("The square of matrix dims of values must be greater than or equal to
         the number of input values")
  } else {
    size = size
  }

  matrix_locs <- paste(
    expand.grid(1:size, 1:size)[,1], expand.grid(1:size, 1:size)[,2],
    sep = "-"
  )

  if (!identical(locs, c("row-col")) & any(!(locs %in% matrix_locs))) {
    stop("Input locs must be in the form 'row-col' i.e. '1-1' and must be within
         the size of the matrix (each dimension must be less than or equal to
         the matrix size/dimensions.")
  }
  if (!identical(locs, c("row-col")) & length(values) != length(locs)) {
    stop("Length of input values must be equivalent to the length of input locs")
  }

  grey_adj_matrix <- data.frame(matrix(data = list(), nrow = size, ncol = size))
  if (is.null(names(values))) {
    colnames(grey_adj_matrix) <- paste0("C", 1:size)
    rownames(grey_adj_matrix) <- paste0("C", 1:size)
  } else {
    colnames(grey_adj_matrix) <- names(values)
    # rownames(matrix_values) <- names(values)
  }

  for (i in seq_along(matrix_locs)) {
    rowLoc <- as.numeric(gsub("-.*", "", matrix_locs[i]))
    colLoc <- as.numeric(gsub(".*-", "", matrix_locs[i]))
    if (matrix_locs[i] %in% locs) {
      grey_adj_matrix[[colLoc]][[rowLoc]] <- values[[which(locs == matrix_locs[i])]]
    } else {
      grey_adj_matrix[rowLoc, colLoc] <- 0
    }
  }

  grey_adj_matrix
}


#' get_edgelist_from_grey_adj_matrix
#'
#' @description
#' This "gets" an edgelist representing a graph described by the input grey adjacency
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
#' @param grey_adj_matrix An n x n adjacency matrix that represents an FCM
#' @param IDs A list of names for each node (must have n items)
#'
#' @export
#' @examples
#' NULL
get_edgelist_from_grey_adj_matrix <- function(grey_adj_matrix = matrix(), IDs = c()) {
  confirm_adj_matrix_is_square(grey_adj_matrix)
  IDs <- get_node_IDs_from_input(grey_adj_matrix, IDs)

  data_types <- unique(unlist(lapply(grey_adj_matrix, function(x) vapply(x, class, character(1)))))
  only_numeric_and_grey_number_data_types <- identical(data_types, c("numeric", "grey_number")) | identical(data_types, c("grey_number", "numeric"))
  if (only_numeric_and_grey_number_data_types) {
    size <- unique(dim(grey_adj_matrix))
    non_zero_locs_binary <- lapply(grey_adj_matrix, function(col) lapply(col, function(x) !identical(x, 0)))
    non_zero_locs_binary <- matrix(data = unlist(non_zero_locs_binary), nrow = size, ncol = size)
    edge_locs <- data.table::data.table(which(non_zero_locs_binary != 0, arr.ind = TRUE))
    edge_weights <- mapply(function(row, col) grey_adj_matrix[[col]][[row]], row = edge_locs$row, col = edge_locs$col, SIMPLIFY = FALSE)
    for (i in seq_along(edge_weights)) {
      if (is.numeric(edge_weights[[i]])) {
        edge_weights[[i]] <- grey_number(edge_weights[[i]], edge_weights[[i]])
      } else if (methods::is(edge_weights[[i]]) == "grey_number") {
        NULL
      }
    }
  } else {
    stop("Input adjacency matrix must only contain numeric objects, and all
         objects must be numeric or grey_numbers")
  }

  source_IDs <- IDs[edge_locs$row]
  target_IDs <- IDs[edge_locs$col]

  edgelist <- data.table::data.table(
    source = source_IDs,
    target = target_IDs,
    weight = edge_weights,
    weight_lower = unlist(lapply(edge_weights, function(weight) weight$lower)),
    weight_upper = unlist(lapply(edge_weights, function(weight) weight$upper))
  )

  edgelist
}


#' grey_number S3 class
#'
#' @description
#' This class is an organization scheme for Grey Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Grey Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The grey_number class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of grey numbers
#'
#' grey_numbers are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param lower lower limit of a Grey Number set (the lower value must be less than or equal to the upper value)
#' @param upper upper limit of a Grey Number set (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' grey_number(lower = 0, upper = 1)
grey_number <- function(lower = double(), upper = double()) {
  if (identical(lower, double())) {
    lower <- -Inf
  }

  if (identical(upper, double())) {
    upper <- Inf
  }

  if ((!is.numeric(lower)) | (!is.numeric(upper))) {
    stop("lower and upper must be single, numeric values", call. = FALSE)
  }

  if (lower > upper) {
    stop("The lower input must be less than or equal to the upper input", call. = FALSE)
  }

  structure(
    .Data = data.frame(lower = lower, upper = upper),
    class = "grey_number"
  )
}


#' print.grey_number
#'
#' @description
#' This improves the readability of the output
#'
#' @details
#' Grey Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The grey_number class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of grey numbers
#'
#' grey_numbers are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param x a grey_number object
#' @param ... additional inputs
#'
#' @export
#' @examples
#' grey_number(lower = 0, upper = 1)
print.grey_number <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$upper, "]", sep = "")
}


#' c.grey_number
#'
#' @description
#' This forces the output of c() to the equivalent of list() only for inputs of
#' type grey_number
#'
#' @details
#' For grey_number objects, c() combines all of the lower and upper data into
#' a single grey_number object, but list() returns the expected output of a
#' list of distinct grey_number objects.
#'
#' @param ... a set of grey_number objects
#'
#' @export
#' @examples
#' c(grey_number(0, 1), grey_number(0.2, 0.5))
c.grey_number <- function(...) {
  list(...)
}


#' print.fgcmr_simulation
#'
#' @description
#' This improves the readability of the simulate_fgcmr output
#'
#' @details
#' Show the first two iterations of the simulation, followed by a gap, and then
#' the final state vector in an organized data frame. Additionally, show the
#' activation, squashing, lambda, and algorithm inputs as well as the total
#' number of iterations and goal minimum error.
#'
#' Use vignette("fgcmr-class") for more information.
#'
#' @param x an fgcmr_simulation object
#' @param ... additional inputs
#'
#' @export
print.fgcmr_simulation <- function(x, ...) {
  first_iter <- lapply(x$state_vectors[1, ], function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
  second_iter <- lapply(x$state_vectors[2, ], function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
  skipped_iters_text <- rep("...", ncol(x$state_vectors))
  final_iter <- lapply(x$state_vectors[nrow(x$state_vectors), ], function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))

  pretty_states <- data.frame(rbind(first_iter, second_iter, skipped_iters_text, final_iter))
  rownames(pretty_states) <- c("1", "2", "...", as.character(nrow(x$state_vectors)))

  cat("State Vectors:\n")
  print(pretty_states)
  cat("\n    Also: $errors $ranges\n")
  cat("  Params: ", "activation = ", x$params$activation, ", squashing = ",
      x$params$squashing, ", lambda = ", x$params$lambda,
      ", algorithm = ", x$params$algorithm,
      "\nRun Info: iters = ", nrow(x$state_vectors), ", min error = ", x$params$min_error,
      sep = "")
}

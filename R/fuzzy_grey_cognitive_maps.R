

#' infer_fgcm_with_pulse
#'
#' @description
#' This calculates a sequence of iterations of a simulation over an fgcm object
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
#' Use vignette("fgcm-class") for more information about each of these
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
infer_fgcm_with_pulse <- function(grey_adj_matrix = matrix(),
                        initial_state_vector = c(),
                        activation = "kosko",
                        squashing = "sigmoid",
                        lambda = 1,
                        max_iter = 50,
                        min_error = 1e-5,
                        IDs = c(),
                        algorithm = "salmeron") {

  if (activation == "rescale" & squashing != "sigmoid") {
    stop("Input activation ccan only use rescale with the sigmoid squashing function.
         It will not produce coherent results otherwise.")
  }

  confirm_adj_matrix_is_square(grey_adj_matrix)
  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
    initial_state_vector <- rep(1, nrow(grey_adj_matrix))
  }
  if (!typeof(initial_state_vector) == "list") {
    initial_state_vector <- as.list(initial_state_vector)
  }
  confirm_input_vector_is_compatible_with_grey_adj_matrix(grey_adj_matrix, initial_state_vector)

  IDs <- get_node_IDs_from_input(grey_adj_matrix, IDs)
  grey_adj_matrix_domain <- get_domain_of_grey_adj_matrix(grey_adj_matrix)
  initial_state_vector <- lapply(initial_state_vector, function(x) {
    if (is.numeric(x)) grey_number(x, x)
    else x
  })

  scenario_state_vectors <-  matrix(data = list(NA), nrow = max_iter + 1, ncol = length(initial_state_vector))
  colnames(scenario_state_vectors) <- IDs
  scenario_state_vectors[1, ] <- initial_state_vector
  errors <- data.frame(matrix(data = numeric(1), nrow = max_iter + 1, ncol = length(initial_state_vector)))
  colnames(errors) <- IDs
  errors[1, ] <- 0

  for (i in 2:(max_iter + 1)) {
    current_scenario_state_vector <- scenario_state_vectors[i - 1, ]
    next_scenario_state_vector <- calculate_next_fgcm_state_vector(grey_adj_matrix, current_scenario_state_vector, activation, algorithm)
    squashed_next_scenario_state_vector <- vapply(
      next_scenario_state_vector,
      function(state) list(grey_number(lower = squash(state$lower, squashing, lambda), upper = squash(state$upper, squashing, lambda))),
      list(list(1))
    )
    scenario_state_vectors[i, ] <- squashed_next_scenario_state_vector

    if (i == 2) {
      errors[1, ] <- 1
    } else {
      errors[i - 1, ] <- mapply(
        function(current, previous) {
          abs(current$lower - previous$lower) + abs(current$upper - previous$upper)
        },
        current <- scenario_state_vectors[i - 1, ],
        previous <- scenario_state_vectors[i - 2, ]
      )
    }
    if (sum(unlist(errors[i - 1, ])) < min_error) {
      break
    }
  }
  if (i >= max_iter) {
    warning(
      "\tThe simulation reached the maximum number of iterations before
        achieving the minimum allowable error. This may signal that
        the fcm has reached a limit-cycle or is endlessly chaotic.

        It is also possible that the fcm simply requires more iterations
        to converge within the input minimum error.

        Try increasing the max_iter or min_error inputs."
    )
  }

  scenario_state_vectors <- data.frame(scenario_state_vectors[1:(i), ])
  errors <- errors[1:i, ]

  state_vector_bounds <- list(
    lower = data.frame(apply(scenario_state_vectors, c(1, 2), function(x) x[[1]]$lower)),
    upper = data.frame(apply(scenario_state_vectors, c(1, 2), function(x) x[[1]]$upper))
  )

  greyness <- data.frame(apply(scenario_state_vectors, c(1, 2), function(x) calculate_greyness(x[[1]], grey_adj_matrix_domain)))
  ranges <- data.frame(apply(scenario_state_vectors, c(1, 2), function(x) x[[1]]$upper - x[[1]]$lower))

  structure(
    .Data = list(
      state_vectors = scenario_state_vectors,
      state_vectors_bounds = state_vector_bounds,
      greyness = greyness,
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
    class = "fgcm_simulation"
  )
}


#' calculate_next_fgcm_state_vector
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
#' Use vignette("fgcm-class") for more information.
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
calculate_next_fgcm_state_vector <- function(grey_adj_matrix = matrix(), state_vector = c(), activation = "kosko", algorithm = "salmeron") {
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
    } else if (activation == "rescale") {
      next_state_vector[[k]] <- grey_number(
        lower = (2*state_vector[[k]]$lower - 1) + state_vector_grey_adj_matrix_dot_product$lower,
        upper = (2*state_vector[[k]]$upper - 1) + state_vector_grey_adj_matrix_dot_product$upper
      )
    } else {
      stop("Input activation must be one of the following:
           'kosko', 'modified-kosko' or 'rescale'.")
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
#' Use vignette("fgcm-class") for more information.
#'
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param grey_adj_matrix_col_vector An column of an adjacency matrix, typically as a list
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#'
#' @export
calculate_next_fgcm_state_vector_with_salmeron_algorithm <- function(state_vector = c(),
                                                                     grey_adj_matrix_col_vector = c(),
                                                                     activation = "kosko") {
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
  } else if (activation == "rescale") {
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
  } else {
    stop("Input activation must be one of the following:
         'kosko' 'modified-kosko' or 'rescale'")
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
#' Use vignette("fgcm-class") for more information.
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


#' confirm_input_vector_is_compatible_with_grey_adj_matrix
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
confirm_input_vector_is_compatible_with_grey_adj_matrix <- function(grey_adj_matrix = matrix(), initial_state_vector = c()) {
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


#' fgcm (fuzzy grey cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for fuzzy grey cognitive maps (See
#' ____, XXXX for example). It stores the nodes of an FGCM and its
#' corresponding adjacency matrix and edgelist.
#'
#' @details
#' fgcm stores fgcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' fgcm allow for the depiction of uncertainty in fcm as edge weights are
#' represented as Grey Numbers (___, XXXX). Grey Numbers represent a range of
#' possible values that an edge may be within.
#'
#' fgcm are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcm-class") for more information.
#'
#' @param grey_adj_matrix An n x n adjacency matrix that represents an FCM
#' @param IDs A list of names for each node (must have n items)
#'
#' @export
#' @examples
#' NULL
fgcm <- function(grey_adj_matrix = matrix(), IDs = c()) {
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
    class = "fgcm"
  )
}


#' make_adj_matrix_w_ivfns
#'
#' @description
#' This constructs an adjacency matrix with edges represented by interval-value
#' fuzzy numbers (IVFNs) from an adjacency matrix of lower bounds and an
#' adjacency matrix of upper bounds
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
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @export
#' @examples
#' make_adj_matrix_w_IVFNs(
#'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' )
make_adj_matrix_w_ivfns <- function(lower = matrix(), upper = matrix()) {
  if (!identical(dim(lower), dim(upper))) {
    stop("Failed Validation: Input adjacency matrices must be the same size")
  }

  if (nrow(lower) != ncol(lower) | nrow(upper) != ncol(upper)) {
    stop("Failed Validation: Input adjacency matrices must be square matrices (n x n)")
  } else {
    size <- nrow(lower)
  }

  if (identical(colnames(lower), colnames(upper)) & !identical(colnames(lower), NULL)) {
    IDs <- colnames(lower)
  } else {
    IDs <- paste0("C", 1:nrow(lower))
  }

  adj_matrix_w_ivfns <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  colnames(adj_matrix_w_ivfns) <- IDs
  rownames(adj_matrix_w_ivfns) <- IDs

  for (i in 1:length(IDs)) {
    for (j in 1:length(IDs)) {
      adj_matrix_w_ivfns[[j]][[i]] <- ivfn(
        # [[j]][[i]] instead of [[i]][[j]]
        # because this notation is
        # [[col]][[row]] for data.frames
        lower = lower[i, j],
        upper = upper[i, j]
      )
    }
  }

  class(adj_matrix_w_ivfns) <- c("adj_matrix_w_ivfns", methods::is(adj_matrix_w_ivfns))

  adj_matrix_w_ivfns
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
#' #' Use vignette("fgcm-class") for more information.
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


#' ivfn S3 class
#'
#' @description
#' This class is an organization scheme for Grey Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Interval Value Fuzzy Numbers (IVFNs) represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The IVFN class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of IVFNs
#'
#' @param lower lower limit of an IVFN (the lower value must be less than or equal to the upper value)
#' @param upper upper limit of an IVFN (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' ivfn(lower = 0, upper = 1)
ivfn <- function(lower = double(), upper = double()) {
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
    class = "ivfn"
  )
}

#' calculate_greyness
#'
#' @description
#' This calculates the "greyness" of a grey number. Greyness describes uncertainty
#' and is defined as the grey number's range (absolute value of the upper value - lower value)
#' divided by the total possible range which is either 1 if edges can only contain
#' positive values or 2 if edges can be either positive or negative.
#'
#' @details
#' Greyness assigns a numeric value to describe the percent of the possibility space
#' that the grey number occupies. It is like a ratio of observed uncertainty to
#' maximum possible uncertainty. See https://doi.org/10.1016/j.eswa.2010.04.085
#'
#' Use vignette("fgcm-class") for more information.
#'
#' @param grey_num a grey_number object
#' @param domain a vector c() of the minimum and maximum possible values a grey number
#' (or edge) may take. Either 1 if domain is bounded by c(0, 1) (i.e. only positive values)
#' or 2 if domain is bounded by c(-1, 1) (i.e. positive and negative values)
#'
#' @export
calculate_greyness <- function(grey_num = grey_number(), domain = 2) {
  lower <- grey_num$lower
  upper <- grey_num$upper

  if ((!identical(domain, 1)) & (!identical(domain, 2))) {
    stop("Input domain must be either c(0, 1) or c(-1, 1).")
  }
  if ((upper < 0 | lower < 0) & (!identical(domain, 2))) {
    stop("Input domain is from 0 to 1, but identified negative values in the grey
         number. Please change domain to c(-1, 1).")
  }

  greyness <- (upper - lower)/domain

  greyness
}


#' get_domain_of_grey_adj_matrix
#'
#' @description
#' This calculates the domain of a grey adjacency matrix. The domain is the
#' absolute value of the difference between the maximum and minimum possible values
#' that a grey number may encompass within a grey adjacency matrix.
#'
#' @details
#' The domain of a grey adjacency matrix may be either 1 if it contains only positive
#' values (i.e. c(0, 1)) or 2 if it contains positive and negative values (i.e. c(-1, 1))
#' See https://doi.org/10.1016/j.eswa.2010.04.085
#'
#' Use vignette("fgcm-class") for more information.
#'
#' @param grey_adj_matrix an adjacency matrix of grey_number class objects
#'
#' @export
get_domain_of_grey_adj_matrix <- function(grey_adj_matrix = matrix()) {
  domains <- apply(
    grey_adj_matrix, c(1, 2),
    function(x) {
      ifelse(
        class(x[[1]]) == "grey_number",
        ifelse(x[[1]]$lower < 0 | x[[1]]$upper < 0, 2, 1),
        x[[1]])
    }
  )

  if (2 %in% domains) {
    domain <- 2
  } else if (1 %in% domains) {
    domain <- 1
  } else {
    stop("Unable to determin domain of input grey adjacency matrix.")
  }

  domain
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
#' Use vignette("fgcm-class") for more information.
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


#' print.fgcm_simulation
#'
#' @description
#' This improves the readability of the infer_fgcm_with_pulse output
#'
#' @details
#' Show the first two iterations of the simulation, followed by a gap, and then
#' the final state vector in an organized data frame. Additionally, show the
#' activation, squashing, lambda, and algorithm inputs as well as the total
#' number of iterations and goal minimum error.
#'
#' Use vignette("fgcm-class") for more information.
#'
#' @param x an fgcm_simulation object
#' @param ... additional inputs
#'
#' @export
print.fgcm_simulation <- function(x, ...) {
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



#' #' fgcmconfr
#' #'
#' #' @description
#' #' [ADD DETAILS HERE!!!!]
#' #'
#' #' @details
#' #' [ADD DETAILS HERE!!!]
#' #'
#' #' Use vignette("fmcm-class") for more information.
#' #'
#' #' @param fgcm_adj_matrices A list of n x n adjacencey matrices representing fcms
#' #' @param samples The number of samples to draw with the selected sampling method. Also,
#' #' the number of sampled models to generate
#' #' @param initial_state_vector A list state values at the start of an fcm simulation
#' #' @param clamping_vector A list of values representing specific actions taken to
#' #' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' #' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' #' @param activation The activation function to be applied. Must be one of the following:
#' #' 'kosko', 'modified-kosko', or 'papageorgiou'.
#' #' @param squashing A squashing function to apply. Must be one of the following:
#' #' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' #' @param lambda A numeric value that defines the steepness of the slope of the
#' #' squashing function when tanh or sigmoid are applied
#' #' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' #' @param min_error The lowest error (sum of the absolute value of the current state
#' #' vector minus the previous state vector) at which no more iterations are necessary
#' #' and the simulation will stop
#' #' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' #' from the pbapply package as the underlying function.
#' #' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' #' @param n_cores Number of cores to use in parallel processing. If no input given,
#' #' will use all available cores in the machine.
#' #' @param IDs A list of names for each node (must have n items). If empty, will use
#' #' column names of adjacancy matrix (if given).
#' #' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' #' FCM. Will dramatically increase size of output if TRUE.
#' #'
#' #' @export
#' fgcmconfr <- function(fgcm_adj_matrices = list(matrix()),
#'                       samples = 1000,
#'                       initial_state_vector = c(),
#'                       clamping_vector = c(),
#'                       activation = c("kosko", "modified-kosko", "rescale"),
#'                       squashing = c("sigmoid", "tanh"),
#'                       lambda = 1,
#'                       max_iter = 100,
#'                       min_error = 1e-5,
#'                       show_progress = TRUE,
#'                       parallel = TRUE,
#'                       n_cores = integer(),
#'                       IDs = c(),
#'                       include_simulations_in_output = FALSE) {
#'
#'   concepts_in_fgcms <- lapply(fgcm_adj_matrices, function(x) get_node_IDs_from_input(x, IDs))
#'   all_fgcms_have_same_concepts <- length(unique(concepts_in_fgcms)) == 1
#'   if (!all_fgcms_have_same_concepts) {
#'     stop("All grey adjacency matrices must have the same concepts.")
#'   }
#'
#'   dimensions_of_input_grey_adj_matrices <- lapply(fgcm_adj_matrices, dim)
#'   all_fgcms_have_same_dimensions <- length(unique(dimensions_of_input_grey_adj_matrices)) == 1
#'   if (!all_fgcms_have_same_dimensions) {
#'     stop("All grey adjacency matrices must have the same dimensions (n x n) throughout the entire list")
#'   }
#'
#'   # Confirm packages necessary packages are available. If not, change run options
#'   if (parallel) {
#'     package_checks <- check_if_local_machine_has_parallel_processing_packages(parallel, show_progress)
#'     parallel <- package_checks$parallel_check
#'   }
#'   if (show_progress) {
#'     package_checks <- check_if_local_machine_has_parallel_processing_packages(parallel, show_progress)
#'     show_progress <- package_checks$show_progress_check
#'   }
#'
#'   # Check that adj_matrices are correct format
#'   lapply(fgcm_adj_matrices, function(x) fgcm(x, IDs))
#'
#'   nodes <- unlist(unique(concepts_in_fgcms))
#'   sampled_grey_adj_matrices <- build_fgcmconfr_models(fgcm_adj_matrices, samples, aggregation_fun, include_zeroes, nodes, show_progress)
#'
#'   fmcm_results <- infer_fmcm(
#'     simulated_adj_matrices = sampled_grey_adj_matrices,
#'     initial_state_vector = initial_state_vector,
#'     clamping_vector = clamping_vector,
#'     activation = activation,
#'     squashing = squashing,
#'     lambda = lambda,
#'     max_iter = max_iter,
#'     min_error = min_error
#'   )
#'
#'   params <- list(
#'     fgcms = fgcm_adj_matrices,
#'     inference_opts = list(initial_state_vector = initial_state_vector,
#'                           clamping_vector = clamping_vector,
#'                           activation = activation,
#'                           squashing = squashing,
#'                           lambda = lambda,
#'                           max_iter = max_iter,
#'                           min_error = min_error,
#'                           IDs = IDs),
#'     bootstrap_input_opts = list(aggregation_fun = aggregation_fun,
#'                                 samples = samples,
#'                                 include_zeroes = include_zeroes),
#'     runtime_opts = list(parallel = parallel,
#'                         n_cores = n_cores,
#'                         show_progress = show_progress,
#'                         include_simulations_in_output = include_simulations_in_output)
#'   )
#'
#'   if (bootstrap_inference_means) {
#'     means_of_fmcm_inferences <- get_means_of_fmcm_inference(
#'       fmcm_inference = fmcm_results$inference,
#'       get_bootstrapped_means = bootstrap_inference_means,
#'       confidence_interval = bootstrap_CI,
#'       bootstrap_reps = bootstrap_reps,
#'       bootstrap_samples_per_rep = bootstrap_reps,
#'       parallel = parallel,
#'       n_cores = n_cores
#'     )
#'
#'     params$bootstrap_output_opts = list(bootstrap_inference_means =  bootstrap_inference_means,
#'                                         bootstrap_CI = bootstrap_CI,
#'                                         bootstrap_reps = bootstrap_reps,
#'                                         bootstrap_draws_per_rep = bootstrap_draws_per_rep)
#'
#'     fgcmconfr_output <- structure(
#'       .Data = list(
#'         inference = fmcm_results$inference,
#'         params = params,
#'         bootstrap = list(
#'           mean_CI_by_node = means_of_fmcm_inferences$mean_CI_by_node,
#'           raw_bootstrap_means = means_of_fmcm_inferences$bootstrap_means
#'         )
#'       ),
#'       class = "fgcmconfr"
#'     )
#'   } else {
#'     fgcmconfr_output <- structure(
#'       .Data = list(
#'         inference = fmcm_results$inference,
#'         params = params
#'       ),
#'       class = "fgcmconfr"
#'     )
#'   }
#'
#'   fgcmconfr_output
#' }
#'
#'
#' #' build_fgcmconfr_models
#' #'
#' #' @description
#' #' This function generates n fgcm models whose edge weights are sampled from either
#' #' the defined edge values in a set of adjacency matrices or continuous (uniform or triangular)
#' #' parametric distributions derived from the sets of edge values, and stores them
#' #' as a list of adjacency matrices.
#' #'
#' #' @details
#' #' [ADD DETAILS HERE!!!]
#' #'
#' #' Use vignette("fgcm-class") for more information.
#' #'
#' #' @param grey_adj_matrices A list of n x n adjacencey matrices representing fcms
#' #' @param sampling The sampling method to be applied. Must be one of the following: "nonparametric", "uniform", or "triangular"
#' #' @param samples The number of samples to draw with the selected sampling method. Also,
#' #' the number of sampled models to generate
#' #' @param nodes A vector of node names (IDs) present in every adjacency matrix
#' #' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' #' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' #' from the pbapply package as the underlying function.
#' #'
#' #' @export
#' build_fgcmconfr_models <- function(grey_adj_matrices, samples, aggregation_fun = c("mean", "median"), include_zeroes = FALSE,  nodes, show_progress) {
#'   n_nodes <- length(nodes)
#'   n_maps <- length(grey_adj_matrices)
#'
#'   lower_adj_matrices <- lapply(grey_adj_matrices, function(grey_adj_matrix) apply(grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$lower, x[[1]])))
#'   upper_adj_matrices <- lapply(grey_adj_matrices, function(grey_adj_matrix) apply(grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$upper, x[[1]])))
#'
#'   lower_adj_matrices_as_arrays <- array(unlist(lower_adj_matrices), c(n_nodes, n_nodes, n_maps))
#'   upper_adj_matrices_as_arrays <- array(unlist(upper_adj_matrices), c(n_nodes, n_nodes, n_maps))
#'
#'   if (!include_zeroes) {
#'     grey_adj_matrices_with_distributions <- lapply(
#'       grey_adj_matrices,
#'       function(grey_adj_matrix) {
#'         apply(grey_adj_matrix, c(1, 2),
#'               function(x) {
#'                 ifelse(class(x[[1]]) == "grey_number",
#'                        yes = list(runif(samples, x[[1]]$lower, x[[1]]$upper)),
#'                        no = NA)
#'               })})
#'   } else {
#'     grey_adj_matrices_with_distributions <- lapply(
#'       grey_adj_matrices,
#'       function(grey_adj_matrix) {
#'         apply(grey_adj_matrix, c(1, 2),
#'               function(x) {
#'                 ifelse(class(x[[1]]) == "grey_number",
#'                        yes = list(runif(samples, x[[1]]$lower, x[[1]]$upper)),
#'                        no = list(rep(0, samples)))
#'               })})
#'   }
#'   grey_adj_matrices_distributions_by_index <- do.call(cbind, lapply(grey_adj_matrices_with_distributions, function(grey_adj_matrix_with_distributions) do.call(list, grey_adj_matrix_with_distributions)))
#'
#'   if (aggregation_fun == "mean") {
#'     combined_grey_adj_matrices_distributions_by_index <- apply(
#'       grey_adj_matrices_distributions_by_index, 1,
#'       function(distributions) {
#'         sum_of_distributions <- rep(0, samples)
#'         n_nonzero_distributions <- 0
#'         if (all(lapply(distributions, typeof) == "list")) {
#'           distributions <- lapply(distributions, unlist)
#'         }
#'         for (i in 1:n_maps) {
#'           if (!identical(unique(distributions[[i]]), NA)) {
#'             n_nonzero_distributions <- n_nonzero_distributions + 1
#'             sum_of_distributions <- sum_of_distributions + distributions[[i]]
#'           }
#'         }
#'         sum_of_distributions/n_nonzero_distributions
#'       }
#'     )
#'     combined_grey_adj_matrices_distributions_by_index <- apply(combined_grey_adj_matrices_distributions_by_index, c(1, 2), function(x) ifelse(is.na(x), 0, x))
#'   } else if (aggregation_fun == "median") {
#'     combined_grey_adj_matrices_distributions_by_index <- do.call(cbind, apply(
#'       grey_adj_matrices_distributions_by_index, 1,
#'       function(distributions) {
#'         if (all(lapply(distributions, typeof) == "list")) {
#'           distributions <- lapply(distributions, unlist)
#'         }
#'         combined_distributions <- do.call(cbind, lapply(distributions, unlist))
#'         apply(combined_distributions, 1, stats::median, na.rm = TRUE)
#'       }, simplify = FALSE
#'     ))
#'     combined_grey_adj_matrices_distributions_by_index <- apply(combined_grey_adj_matrices_distributions_by_index, c(1, 2), function(x) ifelse(is.na(x), 0, x))
#'   }
#'
#'   sampled_adj_matrices <- apply(combined_grey_adj_matrices_distributions_by_index, 1, function(row) data.frame(array(row, c(n_nodes, n_nodes))), simplify = FALSE)
#'   sampled_adj_matrices <- lapply(sampled_adj_matrices,
#'                                  function(sampled_adj_matrix) {
#'                                    colnames(sampled_adj_matrix) <- nodes
#'                                    rownames(sampled_adj_matrix) <- nodes
#'                                    sampled_adj_matrix
#'                                  })
#'
#'   sampled_adj_matrices
#'
#'   # test <- data.frame(do.call(rbind, lapply(sampled_adj_matrices, unlist)))
#'   # test <- test[, colSums(test) != 0]
#' }

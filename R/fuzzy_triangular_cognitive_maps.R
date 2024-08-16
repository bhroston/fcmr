
#' infer_ftcm_with_pulse
#'
#' @description
#' This calculates a sequence of iterations of a simulation over an ftcm object
#' given an initial state vector along with the activation, squashing, and lambda
#' parameters. Additional variables may be defined to control simulation length,
#' column names, and lambda optimization.
#'
#' @details
#' This simulates how an ftcm reacts to an input initial state vector. There is a
#' multi-decadal long body of work that has explored numerous activation and squashing
#' functions as well as algorithms to optimize the lambda value for the
#' sigmoid and tanh squashing functions.
#'
#' Use vignette("ftcm-class") for more information about each of these
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
infer_ftcm_with_pulse <- function(triangular_adj_matrix = matrix(),
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
    next_scenario_state_vector <- calculate_next_ftcm_state_vector(grey_adj_matrix, current_scenario_state_vector, activation, algorithm)
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
    class = "ftcm_simulation"
  )
}


#' calculate_next_ftcm_state_vector
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
#' Use vignette("ftcm-class") for more information.
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
calculate_next_ftcm_state_vector <- function(triangular_adj_matrix = matrix(), state_vector = c(), activation = "kosko", algorithm = "salmeron") {
  next_state_vector <- vector(length = length(state_vector), mode = "list")
  for (k in 1:nrow(grey_adj_matrix)) {
    grey_adj_matrix_col_vector <- grey_adj_matrix[, k]
    for (j in seq_along(grey_adj_matrix_col_vector)) {
      if (is.numeric(grey_adj_matrix_col_vector[[j]])) {
        grey_adj_matrix_col_vector[[j]] <- grey_number(grey_adj_matrix_col_vector[[j]], grey_adj_matrix_col_vector[[j]])
      }
    }

    if (algorithm == "salmeron") {
      state_vector_grey_adj_matrix_dot_product <- calculate_next_ftcm_state_vector_with_salmeron_algorithm(state_vector, grey_adj_matrix_col_vector, activation)
    } else if (algorithm == "concepcion") {
      state_vector_grey_adj_matrix_dot_product <- calculate_next_ftcm_state_vector_with_concepcion_algorithm(state_vector, grey_adj_matrix_col_vector, activation)
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


#' confirm_input_vector_is_compatible_with_triangular_adj_matrix
#'
#' @description
#' Confirm that an initial state vector is algorithmically compatible with a triangular adjacency matrix
#'
#' @details
#' Boolean. TRUE if the number of entries in the initial
#' state vector match the number of rows/columns in the adjacency matrix and 2. The
#' datatypes stored within each object are the same (i.e. "numeric" vs "triangular_number"),
#' FALSE if not
#'
#' Intended for developer use only to improve package readability.
#'
#' @param triangular_adj_matrix An n x n triangular adjacency matrix that represents an FCM
#' @param initial_state_vector An n-length list of the initial states of each node in an fcm simulation
confirm_input_vector_is_compatible_with_triangular_adj_matrix <- function(triangular_adj_matrix = matrix(), initial_state_vector = c()) {
  if (length(initial_state_vector) != unique(dim(triangular_adj_matrix))) {
    stop("Length of input initial_state_vector is does not comply with the dimensions of the input adjacency matrix", .call = FALSE)
  } else {
    TRUE
  }

  data_types <- unique(vapply(initial_state_vector, class, character(1)))
  both_numeric_or_triangular_number_data_types <- identical(data_types, c("numeric", "triangular_number")) | identical(data_types, c("triangular_number", "numeric"))
  only_numeric_data_types <- identical(data_types, "numeric")
  only_triangular_number_data_types <- identical(data_types, "triangular_number")

  if (both_numeric_or_triangular_number_data_types | only_numeric_data_types | only_triangular_number_data_types) {
    TRUE
  } else {
    stop("Input initial state vector must contain only numeric or triangular_number values")
  }
}


#' get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices
#'
#' @description
#' This "gets" a triangular adjacency matrix from an adjacency matrix of the lower
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
#' in the triangular adjacency matrix. Otherwise, generic node IDs will be used
#' (C1, C2, ... Cn).
#'
#' #' Use vignette("ftcm-class") for more information.
#'
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param mode An n x n adjacency matrix that represents the modes of edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @export
#' @examples
#' get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#'  mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
#'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' )
get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices <- function(lower = matrix(),
                                                                             mode = matrix(),
                                                                             upper = matrix()) {
  if (!identical(dim(lower), dim(mode), dim(upper))) {
    stop("Failed Validation: Input adjacency matrices must be the same size")
  }

  if (nrow(lower) != ncol(lower) | nrow(mode) != ncol(mode) | nrow(upper) != ncol(upper)) {
    stop("Failed Validation: Input adjacency matrices must be square matrices (n x n)")
  } else {
    size <- nrow(lower)
  }

  all_input_matrices_have_same_colnames <- length(unique(list(colnames(lower), colnames(mode), colnames(upper)))) == 1
  if (all_input_matrices_have_same_colnames & !identical(colnames(lower), NULL)) {
    IDs <- colnames(lower)
  } else {
    IDs <- paste0("C", 1:nrow(lower))
  }

  if ((!all(lower_adj_matrix <= mode_adj_matrix) | !all(mode_adj_matrix <= upper_adj_matrix))) {
    offense_locs <- unique(rbind(which(!lower_adj_matrix <= mode_adj_matrix, arr.ind = TRUE), which(!mode_adj_matrix <= upper_adj_matrix, arr.ind = TRUE)))
    offenses_df <- data.frame(
      row = offense_locs[, 1],
      col = offense_locs[, 2],
      lower = apply(offense_locs, 1,function(locs) lower_adj_matrix[locs[1], locs[2]]),
      mode = apply(offense_locs, 1,function(locs) mode_adj_matrix[locs[1], locs[2]]),
      upper = apply(offense_locs, 1,function(locs) upper_adj_matrix[locs[1], locs[2]])
    )
    rownames(offenses_df) <- NULL
    writeLines("\n\nERROR: Failed to create triangular adj. matrix from input.\nCheck:")
    print(offenses_df)
    stop(
      "All lower values must be less than or equal to mode values which in turn, \n  must be less than or equal to upper values."
    )
  }

  edge_locs <- unique(rbind(which(lower != 0, arr.ind = TRUE), which(mode != 0, arr.ind = TRUE), which(upper != 0, arr.ind = TRUE)))

  triangular_adj_matrix <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  colnames(triangular_adj_matrix) <- IDs
  rownames(triangular_adj_matrix) <- IDs

  for (i in seq_along(triangular_adj_matrix)) {
    row <- edge_locs[, 1][i]
    col <- edge_locs[, 2][i]
    triangular_adj_matrix[[col]][[row]] <- triangular_number(lower[row, col], mode[row, col], upper[row, col])
  }

  triangular_adj_matrix
}



#' triangular_number S3 class
#'
#' @description
#' This class is an organization scheme for Triangular Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The triangular_number class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' Use vignette("ftcm-class") for more information.
#'
#' @param lower lower limit of a Triangular Number set (the lower value must be less than or equal to the upper value)
#' @param mode the most likely value of a Triangular Number set
#' @param upper upper limit of a Triangular Number set (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' triangular_number(lower = 0, mode = 0.5, upper = 1)
triangular_number <- function(lower = double(), mode = double(), upper = double()) {
  if (identical(lower, double())) {
    lower <- -Inf
  }

  if (identical(mode, double())) {
    mode <- 0
  }

  if (identical(upper, double())) {
    upper <- Inf
  }

  if ((!is.numeric(lower)) | (!is.numeric(mode)) | (!is.numeric(upper))) {
    stop("lower, mode, and upper must be single, numeric values", call. = FALSE)
  }

  if (lower > upper | lower > mode) {
    stop("The lower input must be less than or equal to both the mode and upper inputs", call. = FALSE)
  }

  if (mode > upper) {
    stop("The mode input must be less than or equal to the upper input", call. = FALSE)
  }

  structure(
    .Data = data.frame(lower = lower, mode = mode, upper = upper),
    class = "triangular_number"
  )
}


#' print.triangular_number
#'
#' @description
#' This improves the readability of the output
#'
#' @details
#' triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The triangular_number class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' triangular_numbers are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("ftcm-class") for more information.
#'
#' @param x a triangular_number object
#' @param ... additional inputs
#'
#' @export
#' @examples
#' triangular_number(lower = 0, upper = 1)
print.triangular_number <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$mode, ", ", x$upper, "]", sep = "")
}


#' c.triangular_number
#'
#' @description
#' This forces the output of c() to the equivalent of list() only for inputs of
#' type triangular_number
#'
#' @details
#' For triangular_number objects, c() combines all of the lower and upper data into
#' a single triangular_number object, but list() returns the expected output of a
#' list of distinct triangular_number objects.
#'
#' @param ... a set of triangular_number objects
#'
#' @export
#' @examples
#' c(triangular_number(0, 1), triangular_number(0.2, 0.5))
c.triangular_number <- function(...) {
  list(...)
}



#' rtri
#'
#' @description
#' This pulls n samples from a triangular distribution described by shape parameters
#' defined by a lower limit, upper limit, and mode
#'
#' @details
#'
#' Use vignette("fcmcmr-class") for more information.
#'
#' @param lower lower limit or minimum of the sample space
#' @param upper upper limit or maximum of the sample space
#' @param mode peak of the sample space
#' @param n number of samples to draw from the triangular distribution
#'
#' @export
rtri <- function(n = integer(), lower = double(), mode = double(), upper = double()) {
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




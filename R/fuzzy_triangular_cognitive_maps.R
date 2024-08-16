

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

  if ((!all(lower <= mode) | !all(mode <= upper))) {
    offense_locs <- unique(rbind(which(!lower <= mode, arr.ind = TRUE), which(!mode <= upper, arr.ind = TRUE)))
    offenses_df <- data.frame(
      row = offense_locs[, 1],
      col = offense_locs[, 2],
      lower = apply(offense_locs, 1,function(locs) lower[locs[1], locs[2]]),
      mode = apply(offense_locs, 1,function(locs) mode[locs[1], locs[2]]),
      upper = apply(offense_locs, 1,function(locs) upper[locs[1], locs[2]])
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




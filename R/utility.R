
#' squash
#'
#' @description
#' Calculate squashing function output of an input value and lambda values
#'
#' @details
#' This function calculates the 'squashed' value of a state based upon five
#' available squashing functions typical in the literature (as identified in
#' Gonzales et al. 2018 - https://doi.org/10.1142/S0218213018600102)
#'
#' @param value A numeric value to 'squash'
#' @param squashing A squashing function to apply. Must be one of the following: 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'
#' @param lambda A numeric value that defines the steepness of the slope of the squashing function when tanh or sigmoid are applied
squash <- function(value = numeric(), squashing = "sigmoid", lambda = 1) {
  if (lambda <= 0) {
    stop("Input lambda must be greater than zero")
  }

  # Use full names here instead of abbreviations to improve readability even
  # though developers will need to type more characters.
  if (squashing == "bivalent") {
    if (value > 0) {
      squashed_value <- 1
    } else if (value <= 0) {
      squashed_value <- 0
    }
  } else if (squashing == "saturation") {
    if (value <= 0) {
      squashed_value <- 0
    } else if (value > 0 & value < 1) {
      squashed_value <- value
    } else if (value >= 1) {
      squashed_value <- 1
    }
  } else if (squashing == "trivalent") {
    if (value < 0) {
      squashed_value <- -1
    } else if (value == 0) {
      squashed_value <- 0
    } else if (value > 0) {
      squashed_value <- 1
    }
  } else if (squashing == "tanh") {
    squashed_value <- (exp(2*lambda*value) - 1)/(exp(2*lambda*value) + 1)
  } else if (squashing == "sigmoid") {
    squashed_value <- 1/(1 + exp(-lambda*value))
  } else {
    stop("squashing value must be one of the following:
      'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'")
  }

  squashed_value
}



#' confirm_adj_matrix_is_square
#'
#' @description
#' Confirm that an adjacency matrix is square (n x n)
#'
#' @details
#' Boolean. TRUE if the dimensions of an adjacency matrix are equivalent (n x n).
#' FALSE if not.
#'
#' Intended for developer use only to improve package readability.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
confirm_adj_matrix_is_square <- function(adj_matrix = matrix()) {
  rows <- nrow(adj_matrix)
  cols <- ncol(adj_matrix)
  if (rows != cols) {
    stop("Failed Validation: Input adjacency matrix must be a square (n x n) matrix")
  } else {
    TRUE
  }
}


#' confirm_initial_state_vector_is_compatible_with_adj_matrix
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
#' @param initial_state_vector An n-length list of the initial states of each node in an fcm simulation
confirm_initial_state_vector_is_compatible_with_adj_matrix <- function(adj_matrix = matrix(), initial_state_vector = c()) {
  if (length(initial_state_vector) != unique(dim(adj_matrix))) {
    stop("Length of input initial_state_vector is does not comply with the dimensions of the input adjacency matrix", .call = FALSE)
  } else {
    TRUE
  }
}


#' confirm_unique_datatype_in_object
#'
#' @description
#' Confirm that an object contains data of a single class
#'
#' @details
#' Boolean. TRUE if an object contains only data of a single class (e.g. "numeric"),
#' FALSE if multiple classes detected
#'
#' Intended for developer use only to improve package readability.
#'
#' @param object A list-like object (matrix, data.frame, list, etc.)
#' @param datatype The datatype of which the class of every value within the object should match
confirm_unique_datatype_in_object <- function(object, datatype = "numeric") {
  data_types <- unique(vapply(object, class, character(1)))
  only_numeric_data_types <- identical(data_types, datatype)
  if (!only_numeric_data_types) {
    stop(paste(
      "Input object must only containt", datatype, "objects, and all objects must be", datatype
    ))
  } else {
    TRUE
  }
}

#' get_node_IDs_from_input
#'
#' @description
#' Determine node/concept IDs vector depending on the input adjacency matrix and
#' IDs
#'
#' @details
#' Node/concept IDs can come from two sources: the column names of the input
#' adjacency matrix or an explicitly-defined IDs vector. If an IDs vector is
#' given, confirm that it is applicable to the input adjacency matrix. If no
#' IDs are available, use C1, C2, ..., Cn for IDs.
#'
#' Intended for developer use only to improve package readability.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param IDs A list of names for each node (must have n items)
get_node_IDs_from_input <- function(adj_matrix = matrix(), IDs = c()) {
  empty_colnames <- identical(colnames(adj_matrix), NULL)
  no_IDs_given <- identical(IDs, c())
  colnames_same_as_IDs <- identical(colnames(adj_matrix), IDs)
  if (empty_colnames & no_IDs_given) {
    IDs <- paste0("C", 1:nrow(adj_matrix))
  } else if (empty_colnames & length(IDs) != nrow(adj_matrix)) {
    stop("Input IDs must be the same length as matrix dimensions. i.e. if matrix
         is n x n, length of IDs must be n.")
  } else if (!empty_colnames & no_IDs_given) {
    IDs <- colnames(adj_matrix)
  } else if (!colnames_same_as_IDs) {
    warning("Input adjacency matrix has different column names that input IDs.
            Using input IDs for node/concept names.")
  } else {
    stop("Unable to interpret input adjacency matrix and IDs objects")
  }

  IDs
}

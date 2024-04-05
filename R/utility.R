
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
#' @param IDs A list of names for each node (must have n items)
confirm_adj_matrix_is_square <- function(adj_matrix = matrix()) {
  rows <- nrow(adj_matrix)
  cols <- ncol(adj_matrix)
  if (rows != cols) {
    stop("Failed Validation: Input adjacency matrix must be a square (n x n) matrix")
  }

  TRUE
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
  if (empty_colnames | no_IDs_given) {
    IDs <- paste0("C", 1:nrow(adj_matrix))
  } else if (empty_colnames & length(IDs) != nrow(adj_matrix)) {
    stop("Input IDs must be the same length as matrix dimensions. i.e. if matrix
         is n x n, length of IDs must be n.")
  } else if (!empty_colnames & no_IDs_given) {
    IDs <- colnames(adj_matrix)
  } else if (!colnames_same_as_IDs) {
    warning("Input adjacency matrix has different column names that input IDs.
            Using input IDs for node/concept names.")
  }

  IDs
}

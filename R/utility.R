
#' get_edgelist_from_adj_matrix
#'
#' @description
#' This "gets" an edgelist representing a graph described by the input adjacency
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
  confirm_only_numeric_data_in_adj_matrix(adj_matrix)

  IDs <- get_node_IDs_from_input(adj_matrix, IDs)

  edge_locs <- data.table::data.table(which(adj_matrix != 0, arr.ind = TRUE))
  edge_weights <- mapply(function(row, col) adj_matrix[row, col], row = edge_locs$row, col = edge_locs$col)

  source_IDs <- IDs[edge_locs$row]
  target_IDs <- IDs[edge_locs$col]

  edgelist <- data.frame(
    source = source_IDs,
    target = target_IDs,
    weight = edge_weights
  )

  edgelist
}


#' get_adj_matrix_from_edgelist
#'
#' @description
#' This "gets" an adjacency matrix representing a graph described by the input
#' edgelist.
#'
#' @details
#' The input edgelist must have the following column names: 'source' or 'from',
#' 'target' or 'to'. The user must manually note if different names are used
#' for the edgelist. An additional column may be selected to describe a value
#' attributed to a given edge.
#'
#' The input edgelist can be either a matrix, data.frame, tibble, or
#' data.table type object.
#'
#' @param edgelist An edgelist representing an fcm. Default column names are
#' "source", "target", and "weight", but these may be defined explicitly.
#' @param source_colname Column name in the input eddgelist that represents
#' edge source nodes
#' @param target_colname Column name in the input edgelist that represents
#' edge target nodes
#' @param value_colname Colname in th einput edgelist that represents represents
#' the values displayed in the adjacency matrix (i.e. weight, standard_deviation)
#'
#' @export
get_adj_matrix_from_edgelist <- function(edgelist = matrix(),
                                         source_colname = "source",
                                         target_colname = "target",
                                         value_colname = "weight") {
  source_nodes <- edgelist[[source_colname]]
  target_nodes <- edgelist[[target_colname]]
  edge_values <- edgelist[[value_colname]]

  nodes <- unique(c(source_nodes, target_nodes))

  adj_matrix <- data.frame(matrix(data = 0, nrow = length(nodes), ncol = length(nodes)))
  colnames(adj_matrix) <- nodes
  rownames(adj_matrix) <- nodes

  for (i in seq_along(nodes)) {
    edge <- edgelist[i, ]
    edge_row_loc <- which(nodes == edge$source)
    edge_col_loc <- which(nodes == edge$target)
    adj_matrix[edge_row_loc, edge_col_loc] <- edge_values[i]
  }

  adj_matrix
}



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
  } else if (colnames_same_as_IDs) {
    NULL
  } else {
    stop("Unable to interpret input adjacency matrix and IDs objects")
  }

  IDs
}


#' confirm_only_numeric_data_in_adj_matrix
#'
#' @description
#' Confirm all values in an adj_matrix object are of type numeric
#'
#' @details
#' Check that all values in an adjacency matrix are of type numeric (i.e. int,
#' double, etc.)
#'
#' Intended for developer use only to improve package readability.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
confirm_only_numeric_data_in_adj_matrix <- function(adj_matrix = matrix()) {
  if (sum(dim(adj_matrix)) == 2) {
    warning("Input adj_matrix object is an empty 1 x 1 matrix")
  } else {
    adj_matrix_data_types <- unique(vapply(adj_matrix, class, character(1)))
    only_numeric_data_types_in_adj_matrix <- identical(adj_matrix_data_types, "numeric")
    if (!only_numeric_data_types_in_adj_matrix) {
      stop("Input adj_matrix must only contain numeric objects, and all
         objects must be numeric")
    }
  }
  only_numeric_data_types_in_adj_matrix
}


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

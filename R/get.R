
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
# get_edgelist_from_adj_matrix <- function(adj_matrix = data.table::data.table(), IDs = c()) {
#   # Assuming adj_matrix is square
#   if (identical(names(adj_matrix), NULL) | identical(IDs, c())) {
#     IDs <- paste0("C", 1:nrow(adj_matrix))
#   } else if (identical(names(adj_matrix), NULL) & length(IDs) == nrow(adj_matrix)) {
#     IDs <- names(adj_matrix)
#   } else if (!identical(names(adj_matrix), IDs) & identical(IDs, c())) {
#     IDs <- names(adj_matrix)
#   } else if (!identical(names(adj_matrix), IDs) & !identical(IDs, c())) {
#     warning("Input adjacency matrix has different column names than IDs. Using
#             IDs input for concept names.")
#   } else if (identical(names(adj_matrix), NULL) & length(IDs) != nrow(adj_matrix)) {
#     stop("Input IDs must be the same length as matrix dimensions. i.e. if matrix
#          is n x n, length of IDs must be n.")
#   } else {
#     stop("Unable to manage input adjacency matrix and IDs objects")
#   }
#
#   if (identical(class(adj_matrix), "numeric")) {
#     edge_locs <- data.table::data.table(which(adj_matrix != 0, arr.ind = TRUE))
#     edge_weights <- mapply(function(row, col) adj_matrix[row, col], row = edge_locs$row, col = edge_locs$col)
#   } else if (identical(methods::is(adj_matrix), "grey_number")) {
#     size <- nrow(adj_matrix)
#     potential_edge_locs <- as.data.frame(expand.grid(1:size, 1:size))
#     for (i in 1:nrow(potential_edge_locs)) {
#       potential_edge <- adj_matrix[[potential_edge_locs[i, 2]]][[potential_edge_locs[i, 1]]]
#       # Determine which edges are zero and remove them from the list of edges.
#       # Have to do this roundabout strategy because there are multiple data types
#       # stored in an fgcm adjacency matrix and which(adj_matrix != 0) throws an error
#       if (identical(potential_edge, 0)) {
#         potential_edge_locs[i, ] <- c(NA, NA)
#       }
#     }
#     edge_locs <- stats::na.omit(potential_edge_locs)
#     colnames(edge_locs) <- c("row", "col")
#     edge_weights <- mapply(function(row, col) adj_matrix[[col]][[row]], row = edge_locs$row, col = edge_locs$col, SIMPLIFY = FALSE)
#     edge_weights <- data.frame(data.table::data.table(edge_weights)) # This is
#     # like this purely for output readability; may change in the future
#   } else {
#     stop("Unable to interpret input adjacency matrix")
#   }
#
#   source_IDs <- IDs[edge_locs$row]
#   target_IDs <- IDs[edge_locs$col]
#
#   edgelist <- data.frame(
#     source = source_IDs,
#     target = target_IDs,
#     weight = edge_weights
#   )
#
#   edgelist
# }

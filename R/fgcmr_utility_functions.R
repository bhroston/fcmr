#' #' get_grey_adj_matrix_from_lower_and_upper_adj_matrices
#' #'
#' #' @description
#' #' This "gets" a Grey adjacency matrix from an adjacency matrix of the lower
#' #' limits of edges in an FCM and an adjacency matrix of the upper limits of edges
#' #' in an FCM.
#' #'
#' #' @details
#' #' The input adjacency matrices must square n x n matrices with the same dimensions.
#' #' The input can be either matrix, data.table, tibble, or data.table type objects,
#' #' but the output will always be a data.frame. This is for output readability.
#' #' data.table and tibble objects work logically, but their outputs require
#' #' additional steps to parse from the user's perspective.
#' #'
#' #' If the input matrices have named columns, those names will be carried over
#' #' in the grey adjacency matrix. Otherwise, generic node IDs will be used
#' #' (C1, C2, ... Cn).
#' #'
#' #' #' Use vignette("fgcmr-class") for more information.
#' #'
#' #' @param lower An n x n adjacency matrix that represents the lower limits of
#' #'              edges in an FCM
#' #' @param upper An n x n adjacency matrix that represents the upper limits of
#' #'              edges in an FCM
#' #'
#' #' @export
#' #' @examples
#' #' get_grey_adj_matrix_from_lower_and_upper_adj_matrices(
#' #'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#' #'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' #' )
#' get_grey_adj_matrix_from_lower_and_upper_adj_matrices <- function(lower = matrix(),
#'                                                                   upper = matrix()) {
#'   if (!identical(dim(lower), dim(upper))) {
#'     stop("Failed Validation: Input adjacency matrices must be the same size")
#'   }
#'
#'   if (nrow(lower) != ncol(lower) | nrow(upper) != ncol(upper)) {
#'     stop("Failed Validation: Input adjacency matrices must be square matrices (n x n)")
#'   } else {
#'     size <- nrow(lower)
#'   }
#'
#'   if (identical(names(lower), names(upper)) & !identical(names(lower), NULL)) {
#'     IDs <- names(lower)
#'   } else {
#'     IDs <- paste0("C", 1:nrow(lower))
#'   }
#'
#'   edge_locs_in_lower <- data.table::data.table(which(lower != 0, arr.ind = TRUE))
#'   edge_locs_in_upper <- data.table::data.table(which(upper != 0, arr.ind = TRUE))
#'   if (!identical(edge_locs_in_lower, edge_locs_in_upper)) {
#'     warning("Input adjacency matrices must be structurally equivallent. i.e. If
#'     there is a non-zero value in one matrix, there must be another non-zero
#'     value at the same location in the other matrix. If one matrix has a non-zero
#'     value where the other has a zero value, it is assumed that the zero value is
#'     a part of the grey number for that edge.")
#'   }
#'   all_edge_locs <- unique(rbind(edge_locs_in_lower, edge_locs_in_upper))
#'
#'   #grey_adj_matrix <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
#'   grey_adj_matrix <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
#'   colnames(grey_adj_matrix) <- IDs
#'   rownames(grey_adj_matrix) <- IDs
#'
#'   for (index in 1:nrow(all_edge_locs)) {
#'     i <- all_edge_locs$row[index]
#'     j <- all_edge_locs$col[index]
#'     grey_adj_matrix[[j]][[i]] <- grey_number( # [[j]][[i]] instead of [[i]][[j]]
#'                                               # because this notation is
#'                                               # [[col]][[row]] for data.frames
#'       lower = lower[i, j],
#'       upper = upper[i, j]
#'     )
#'   }
#'
#'   colnames(grey_adj_matrix) <- paste0("C", 1:ncol(grey_adj_matrix))
#'
#'   vctrs::new_data_frame(
#'     x = grey_adj_matrix,
#'     class = "grey_number"
#'   )
#' }
#'
#'
#' #' get_grey_adj_matrix_from_list_of_grey_numbers
#' #'
#' #' @description
#' #' This "gets" a Grey adjacency matrix from a list of input values and a list
#' #' of matrix indexes that identify where those values are located in the matrix
#' #' (row-col).
#' #'
#' #' @details
#' #' The input values and locations (locs) lists must have equivalent lengths, and
#' #' must be of type list.
#' #'
#' #' If the input lists have named columns, those names will be carried over
#' #' in the grey adjacency matrix. Otherwise, generic node IDs will be used
#' #' (C1, C2, ... Cn).
#' #'
#' #' #' Use vignette("fgcmr-class") for more information.
#' #'
#' #' @param values A list of grey_number or numeric type objects
#' #' @param locs A list of matrix locations/indexes (row-col) to place entries
#' #'             in the values list
#' #' @param size The square (n x n) dimensions of the output matrix. Must be
#' #'             greater than or equal to the number of values and locs given. Size
#' #'             is assumed to be the square root of the length of the number of
#' #'             values given, rounded up to the nearest whole number.
#' #'
#' #' @export
#' #' @examples
#' #' get_grey_adj_matrix_from_list_of_grey_numbers(
#' #'  values = c(grey_number(-1, 1), grey_number(-0.5, 0.5)),
#' #'  locs = c("1-2", "2-1"),
#' #'  size = 2
#' #' )
#' get_grey_adj_matrix_from_list_of_grey_numbers <- function(values = c(),
#'                                                           locs = c("row-col"),
#'                                                           size = numeric()) {
#'   if (identical(size, numeric()) & !identical(locs, c("row-col"))) {
#'     rowDims <- lapply(locs, function(x) as.numeric(gsub("-.*", "", x)))
#'     colDims <- lapply(locs, function(x) as.numeric(gsub(".*-", "", x)))
#'     size = max(unlist(c(rowDims, colDims)))
#'   } else if (identical(size, numeric()) & identical(locs, c("row-col"))) {
#'     size = ceiling(sqrt(length(locs)))
#'   } else if (size^2 < length(locs)) {
#'     stop("The square of matrix dims of values must be greater than or equal to
#'          the number of input values")
#'   } else {
#'     size = size
#'   }
#'
#'   matrix_locs <- paste(
#'     expand.grid(1:size, 1:size)[,1], expand.grid(1:size, 1:size)[,2],
#'     sep = "-"
#'   )
#'
#'   if (!identical(locs, c("row-col")) & any(!(locs %in% matrix_locs))) {
#'     stop("Input locs must be in the form 'row-col' i.e. '1-1' and must be within
#'          the size of the matrix (each dimension must be less than or equal to
#'          the matrix size/dimensions.")
#'   }
#'   if (!identical(locs, c("row-col")) & length(values) != length(locs)) {
#'     stop("Length of input values must be equivalent to the length of input locs")
#'   }
#'
#'   grey_adj_matrix <- data.frame(matrix(data = list(), nrow = size, ncol = size))
#'   if (is.null(names(values))) {
#'     colnames(grey_adj_matrix) <- paste0("C", 1:size)
#'     rownames(grey_adj_matrix) <- paste0("C", 1:size)
#'   } else {
#'     colnames(grey_adj_matrix) <- names(values)
#'     # rownames(matrix_values) <- names(values)
#'   }
#'
#'   for (i in seq_along(matrix_locs)) {
#'     rowLoc <- as.numeric(gsub("-.*", "", matrix_locs[i]))
#'     colLoc <- as.numeric(gsub(".*-", "", matrix_locs[i]))
#'     if (matrix_locs[i] %in% locs) {
#'       grey_adj_matrix[[colLoc]][[rowLoc]] <- values[[which(locs == matrix_locs[i])]]
#'     } else {
#'       grey_adj_matrix[rowLoc, colLoc] <- 0
#'     }
#'   }
#'
#'   vctrs::new_data_frame(
#'     x = grey_adj_matrix,
#'     class = "grey_number"
#'   )
#' }
#'
#'
#' #' c.grey_number
#' #'
#' #' @description
#' #' This forces the output of c() to the equivalent of list() only for inputs of
#' #' type grey_number
#' #'
#' #' @details
#' #' For grey_number objects, c() combines all of the lower and upper data into
#' #' a single grey_number object, but list() returns the expected output of a
#' #' list of distinct grey_number objects.
#' #'
#' #' @param ... a set of grey_number objects
#' #'
#' #' @export
#' #' @examples
#' #' c(grey_number(0, 1), grey_number(0.2, 0.5))
#' c.grey_number <- function(...) {
#'   list(...)
#' }

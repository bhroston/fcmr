

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


#' get_grey_adj_matrix_from_list_of_ivfns
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
#' @param values A list of ivfn or numeric type objects
#' @param locs A list of matrix locations/indexes (row-col) to place entries
#'             in the values list
#' @param size The square (n x n) dimensions of the output matrix. Must be
#'             greater than or equal to the number of values and locs given. Size
#'             is assumed to be the square root of the length of the number of
#'             values given, rounded up to the nearest whole number.
#'
#' @export
#' @examples
#' get_grey_adj_matrix_from_list_of_ivfns(
#'  values = c(ivfn(-1, 1), ivfn(-0.5, 0.5)),
#'  locs = c("1-2", "2-1"),
#'  size = 2
#' )
get_grey_adj_matrix_from_list_of_ivfns <- function(values = c(),
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
  only_numeric_and_ivfn_data_types <- identical(data_types, c("numeric", "ivfn")) | identical(data_types, c("ivfn", "numeric"))
  if (only_numeric_and_ivfn_data_types) {
    size <- unique(dim(grey_adj_matrix))
    non_zero_locs_binary <- lapply(grey_adj_matrix, function(col) lapply(col, function(x) !identical(x, 0)))
    non_zero_locs_binary <- matrix(data = unlist(non_zero_locs_binary), nrow = size, ncol = size)
    edge_locs <- data.table::data.table(which(non_zero_locs_binary != 0, arr.ind = TRUE))
    edge_weights <- mapply(function(row, col) grey_adj_matrix[[col]][[row]], row = edge_locs$row, col = edge_locs$col, SIMPLIFY = FALSE)
    for (i in seq_along(edge_weights)) {
      if (is.numeric(edge_weights[[i]])) {
        edge_weights[[i]] <- ivfn(edge_weights[[i]], edge_weights[[i]])
      } else if (methods::is(edge_weights[[i]]) == "ivfn") {
        NULL
      }
    }
  } else {
    stop("Input adjacency matrix must only contain numeric objects, and all
         objects must be numeric or ivfns")
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
#' @param grey_num a ivfn object
#' @param domain a vector c() of the minimum and maximum possible values a grey number
#' (or edge) may take. Either 1 if domain is bounded by c(0, 1) (i.e. only positive values)
#' or 2 if domain is bounded by c(-1, 1) (i.e. positive and negative values)
#'
#' @export
calculate_greyness <- function(grey_num = ivfn(), domain = 2) {
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
#' @param grey_adj_matrix an adjacency matrix of ivfn class objects
#'
#' @export
get_domain_of_grey_adj_matrix <- function(grey_adj_matrix = matrix()) {
  domains <- apply(
    grey_adj_matrix, c(1, 2),
    function(x) {
      ifelse(
        class(x[[1]]) == "ivfn",
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


#' print.ivfn
#'
#' @description
#' This improves the readability of the output
#'
#' @details
#' Grey Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The ivfn class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of grey numbers
#'
#' ivfns are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fgcm-class") for more information.
#'
#' @param x a ivfn object
#' @param ... additional inputs
#'
#' @export
#' @examples
#' ivfn(lower = 0, upper = 1)
print.ivfn <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$upper, "]", sep = "")
}


#' c.ivfn
#'
#' @description
#' This forces the output of c() to the equivalent of list() only for inputs of
#' type ivfn
#'
#' @details
#' For ivfn objects, c() combines all of the lower and upper data into
#' a single ivfn object, but list() returns the expected output of a
#' list of distinct ivfn objects.
#'
#' @param ... a set of ivfn objects
#'
#' @export
#' @examples
#' c(ivfn(0, 1), ivfn(0.2, 0.5))
c.ivfn <- function(...) {
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
print.ivfn_simulation <- function(x, ...) {
  first_iter <- lapply(x$state_vectors[1, ], function(x) ivfn(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
  second_iter <- lapply(x$state_vectors[2, ], function(x) ivfn(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
  skipped_iters_text <- rep("...", ncol(x$state_vectors))
  final_iter <- lapply(x$state_vectors[nrow(x$state_vectors), ], function(x) ivfn(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))

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

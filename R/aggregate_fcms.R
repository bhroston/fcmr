
################################################################################
# aggregate_fcms.R
#
# These functions are involved with FCM aggregation (conventional, ivfn, and tfn)
#
#   - aggregate_fcms
#   - aggregate_conventional_fcms
#   - aggregate_fcms_w_ivfns
#   - aggregate_fcms_w_tfns
#   - print.aggregate
#
################################################################################


#' Aggregate FCMs
#'
#' @family aggregate_fcms
#'
#' @description
#' Generate an aggregate adj. matrix from a list of adj. matrices. FCM
#' aggregation works by calculating the mean/median edge weight for all edges
#' across the input adj. matrices (i.e. the mean/median of the edge weight
#' connecting A -> B across all maps, the mean/median of the edge weight
#' connecting B -> C across all maps, and so on). The user may dictate whether
#' to incorporate 0-valued edge weights in the mean/median calculations.
#'
#' @details
#' All input adj. matrices must have the same dimensions and concept names to
#' generate an aggregate.
#'
#' @param adj_matrices A list of adj. matrix objects; these can represent
#' conventional FCM, IVFN FCM, and TFN FCM, but all adj. matrices must be of
#' the same type between the three options.
#' @param agg_function Calculate aggregate edge weights as either the
#' "mean" or "median" of the input edge weights across inputs
#' @param include_zeroes_in_sampling TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#'
#' @references \insertRef{aminpourWisdomStakeholderCrowds2020}{fcmconfr}
#'
#' @returns An aggregate adj. matrix (of class 'aggregate') with edges represented
#' as the same data types as the inputs (i.e. Numerics for conventional, IVFNs, or TFNs)
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#' @example  man/examples/ex-aggregate_fcms.R
aggregate_fcms <- function(adj_matrices = list(matrix()),
                           agg_function = c("mean", "median"),
                           include_zeroes_in_sampling = FALSE) {

  adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices)
  if (!adj_matrices_input_type$adj_matrices_input_is_list) {
    adj_matrices <- list(adj_matrices)
  }

  dimensions_of_input_adj_matrices <- lapply(adj_matrices, dim)
  all_adj_matrices_have_same_dimensions <- length(unique(dimensions_of_input_adj_matrices)) == 1
  if (!all_adj_matrices_have_same_dimensions) {
    stop("All input adjacency matrices must have the same dimensions (n x n) throughout the entire list")
  }

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))

  all_adj_matrices_have_same_concepts <- length(unique(concepts_in_adj_matrices)) == 1
  if (!all_adj_matrices_have_same_concepts) {
    stop("All input adjacency matrices must have the same concepts.")
  }

  fcm_class <- adj_matrices_input_type$object_types_in_list[1]
  if (fcm_class == "conventional") {
    aggregate_adj_matrix <- aggregate_conventional_fcms(adj_matrices, agg_function, include_zeroes_in_sampling)
  } else if (fcm_class == "ivfn") {
    aggregate_adj_matrix <- aggregate_fcms_w_ivfns(adj_matrices, agg_function, include_zeroes_in_sampling)
  } else if (fcm_class == "tfn") {
    aggregate_adj_matrix <- aggregate_fcms_w_tfns(adj_matrices, agg_function, include_zeroes_in_sampling)
  }

  class(aggregate_adj_matrix) <- "aggregate"

  aggregate_adj_matrix
}




#' Aggregate (Conventional) FCMs
#'
#' @family aggregate_fcms
#'
#' @description
#' Generate an aggregate adj. matrix from a list of (Conventional) adj. matrices.
#' FCM aggregation works by calculating the mean/median edge weight for all edges
#' across the input adj. matrices (i.e. the mean/median of the edge weight
#' connecting A -> B across all maps, the mean/median of the edge weight
#' connecting B -> C across all maps, and so on). The user may dictate whether
#' to incorporate 0-valued edge weights in the mean/median calculations.
#'
#' @details
#' All input adj. matrices must represent Conventional FCMs
#' All input adj. matrices must have the same dimensions and concept names to
#' generate an aggregate.
#'
#' @param adj_matrices A list of Conventional FCM adj. matrix objects
#' @param agg_function Calculate aggregate edge weights as either the
#' "mean" or "median" of the input edge weights across inputs
#' @param include_zeroes_in_sampling TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#' @param false_zero_locs_by_adj_matrix !FOR DEVELOPER USE ONLY! A list of array
#' indexes for IVFN and TFN matrices that contain false-zero edges (False-zero
#' edges are those that contain zero as a lower bound for IVFNs and/or mode for
#' TFNs but a non-zero value for the upper bound)
#'
#' @returns An aggregate adj. matrix (of class 'aggregate') with edges represented
#' as numeric data types
#'
#' @keywords internal
#'
#' @export
#' @example man/examples/ex-aggregate_conventional_fcms.R
aggregate_conventional_fcms <- function(adj_matrices = list(matrix()),
                                        agg_function = c("mean", "median"),
                                        include_zeroes_in_sampling = TRUE,
                                        false_zero_locs_by_adj_matrix = list()) {

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))
  n_nodes <- length(node_names)
  n_maps <- length(adj_matrices)

  if (!include_zeroes_in_sampling & identical(false_zero_locs_by_adj_matrix, list())) {
    adj_matrices <- lapply(adj_matrices, function(x) replace(x, x == 0, NA))
  } else if (!include_zeroes_in_sampling & !identical(false_zero_locs_by_adj_matrix, list())) {
    adj_matrices <- lapply(adj_matrices, function(x) replace(x, x == 0, NA))
    adj_matrices <- mapply(
      function(adj_matrix, false_zero_locs) {
        adj_matrix[false_zero_locs] <- 0
        adj_matrix
      },
      adj_matrix = adj_matrices,
      false_zero_locs = false_zero_locs_by_adj_matrix,
      SIMPLIFY = FALSE
    )
  }

  if (agg_function == "mean") {
    aggregate_adj_matrix <- apply(
      array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps)), 1:2,
      function(x) {
        mean(x, na.rm = TRUE)
      }
    )
  } else if (agg_function == "median") {
    aggregate_adj_matrix <- apply(
      array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps)), 1:2,
      function(x) stats::median(x, na.rm = TRUE)
    )
  }

  aggregate_adj_matrix[is.na(aggregate_adj_matrix)] <- 0
  adj_matrices <- lapply(adj_matrices, function(x) replace(x, is.na(x), 0))

  colnames(aggregate_adj_matrix) <- node_names
  rownames(aggregate_adj_matrix) <- node_names

  structure(
    .Data = list(
      adj_matrix = as.data.frame(aggregate_adj_matrix),
      params = list(
        input_adj_matrices = adj_matrices,
        aggregation_fun = agg_function,
        IDs = node_names
      )
    ),
    class = "aggregate_of_conventional_fcms"
  )
}


#' Aggregate (IVFN) FCMs
#'
#' @family aggregate_fcms
#'
#' @description
#' Generate an aggregate adj. matrix from a list of (IVFN) adj. matrices.
#' FCM aggregation works by calculating the mean/median edge weight for all edges
#' across the input adj. matrices (i.e. the mean/median of the edge weight
#' connecting A -> B across all maps, the mean/median of the edge weight
#' connecting B -> C across all maps, and so on). The user may dictate whether
#' to incorporate 0-valued edge weights in the mean/median calculations.
#'
#' @details
#' All input adj. matrices must represent IVFN FCMs
#' All input adj. matrices must have the same dimensions and concept names to
#' generate an aggregate.
#'
#' @param adj_matrices A list of IVFN FCM adj. matrix objects
#' @param agg_function Calculate aggregate edge weights as either the
#' "mean" or "median" of the input edge weights across inputs
#' @param include_zeroes_in_sampling TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#'
#' @returns An aggregate adj. matrix (of class 'aggregate') with edges represented
#' as IVFN data types
#'
#' @keywords internal
#'
#' @export
#' @example  man/examples/ex-aggregate_fcms_w_ivfns.R
aggregate_fcms_w_ivfns <- function(adj_matrices = list(matrix()),
                                   agg_function = c("mean", "median"),
                                   include_zeroes_in_sampling = TRUE) {

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))
  n_nodes <- length(node_names)
  n_maps <- length(adj_matrices)

  lower_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$lower))
  upper_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$upper))

  if (include_zeroes_in_sampling) {
    lower_aggregate_adj_matrix <- aggregate_conventional_fcms(lower_adj_matrices, agg_function, include_zeroes_in_sampling)
  } else {
    # Do NOT count IVFNs with a 0-lower bound as 0-weighted edges!!!!!!
    false_zero_ivfn_locs_across_adj_matrices <- lapply(adj_matrices, function(adj_matrix) {
      which(apply(adj_matrix, c(1, 2), function(element) (element[[1]]$lower == 0 & element[[1]]$upper != 0)), arr.ind = TRUE)
    })
    lower_aggregate_adj_matrix <- aggregate_conventional_fcms(lower_adj_matrices, agg_function, include_zeroes_in_sampling, false_zero_ivfn_locs_across_adj_matrices)
  }
  upper_aggregate_adj_matrix <- aggregate_conventional_fcms(upper_adj_matrices, agg_function, include_zeroes_in_sampling)

  aggregate_adj_matrix_w_ivfns <- make_adj_matrix_w_ivfns(lower_aggregate_adj_matrix$adj_matrix, upper_aggregate_adj_matrix$adj_matrix)
  colnames(aggregate_adj_matrix_w_ivfns) <- node_names
  rownames(aggregate_adj_matrix_w_ivfns) <- node_names

  structure(
    .Data = list(
      adj_matrix = aggregate_adj_matrix_w_ivfns,
      params = list(
        input_adj_matrices = adj_matrices,
        aggregation_fun = agg_function,
        IDs = node_names
      )
    ),
    class = "aggregate_of_fcms_w_ivfns"
  )
}


#' Aggregate (TFN) FCMs
#'
#' @family aggregate_fcms
#'
#' @description
#' Generate an aggregate adj. matrix from a list of (TFN) adj. matrices.
#' FCM aggregation works by calculating the mean/median edge weight for all edges
#' across the input adj. matrices (i.e. the mean/median of the edge weight
#' connecting A -> B across all maps, the mean/median of the edge weight
#' connecting B -> C across all maps, and so on). The user may dictate whether
#' to incorporate 0-valued edge weights in the mean/median calculations.
#'
#' @details
#' All input adj. matrices must represent TFN FCMs
#' All input adj. matrices must have the same dimensions and concept names to
#' generate an aggregate.
#'
#' @param adj_matrices A list of TFN FCM adj. matrix objects
#' @param agg_function Calculate aggregate edge weights as either the
#' "mean" or "median" of the input edge weights across inputs
#' @param include_zeroes_in_sampling TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#'
#' @returns An aggregate adj. matrix (of class 'aggregate') with edges represented
#' as TFN data types
#'
#' @keywords internal
#'
#' @export
#' @example  man/examples/ex-aggregate_fcms_w_tfns.R
aggregate_fcms_w_tfns <- function(adj_matrices = list(matrix()),
                                  agg_function = c("mean", "median"),
                                  include_zeroes_in_sampling = TRUE) {

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))
  n_nodes <- length(node_names)
  n_maps <- length(adj_matrices)

  lower_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$lower))
  mode_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$mode))
  upper_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$upper))

  if (include_zeroes_in_sampling) {
    lower_aggregate_adj_matrix <- aggregate_conventional_fcms(lower_adj_matrices, agg_function, include_zeroes_in_sampling)
    mode_aggregate_adj_matrix <- aggregate_conventional_fcms(mode_adj_matrices, agg_function, include_zeroes_in_sampling)
  } else {
    # Do NOT count TFNs with a 0-lower and 0-mode bounds as 0-weighted edges!!!!!!
    false_zero_lower_tfn_locs_across_adj_matrices <- lapply(adj_matrices, function(adj_matrix) {
      which(apply(adj_matrix, c(1, 2), function(element) (element[[1]]$lower == 0 & element[[1]]$upper != 0)), arr.ind = TRUE)
    })
    false_zero_mode_tfn_locs_across_adj_matrices <- lapply(adj_matrices, function(adj_matrix) {
      which(apply(adj_matrix, c(1, 2), function(element) (element[[1]]$lower == 0 & element[[1]]$mode == 0 & element[[1]]$upper != 0)), arr.ind = TRUE)
    })
    lower_aggregate_adj_matrix <- aggregate_conventional_fcms(lower_adj_matrices, agg_function, include_zeroes_in_sampling, false_zero_lower_tfn_locs_across_adj_matrices)
    mode_aggregate_adj_matrix <- aggregate_conventional_fcms(mode_adj_matrices, agg_function, include_zeroes_in_sampling, false_zero_mode_tfn_locs_across_adj_matrices)
  }

  upper_aggregate_adj_matrix <- aggregate_conventional_fcms(upper_adj_matrices, agg_function, include_zeroes_in_sampling)

  aggregate_adj_matrix_w_tfns <- make_adj_matrix_w_tfns(lower_aggregate_adj_matrix$adj_matrix, mode_aggregate_adj_matrix$adj_matrix, upper_aggregate_adj_matrix$adj_matrix)

  colnames(aggregate_adj_matrix_w_tfns) <- node_names
  rownames(aggregate_adj_matrix_w_tfns) <- node_names

  structure(
    .Data = list(
      adj_matrix = aggregate_adj_matrix_w_tfns,
      params = list(
        input_adj_matrices = adj_matrices,
        aggregation_fun = agg_function,
        IDs = node_names
      )
    ),
    class = "aggregate_of_fcms_w_tfns"
  )
}


#' Print method for aggregate
#'
#' @family aggregate_fcms
#'
#' @param x an aggregate object
#' @param ... additional inputs
#'
#' @returns A console printout of aggregate_fcm output
#'
#' @keywords internal
#'
#' @export
#' @examples
#' NULL
print.aggregate <- function(x, ...) {
  print(x$adj_matrix)
  cat(paste0("\nAggregate (", x$params$aggregation_fun, ") of ", length(x$params$input_adj_matrices), " adj. matrices"))
}


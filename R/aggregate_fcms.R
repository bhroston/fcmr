
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

  # browser()

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


#' aggregate_fcms
#'
#' @export
aggregate_fcms <- function(adj_matrices = list(matrix()),
                           aggregation_function = c("mean", "median"),
                           include_zeroes = FALSE) {

  adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices)
  if (!adj_matrices_input_type$adj_matrices_input_is_list) {
    adj_matrices <- list(adj_matrices)
  }

  confirm_adj_matrices_have_same_dimensions(adj_matrices)
  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))
  confirm_adj_matrices_have_same_concepts(concepts_in_adj_matrices)

  fcm_class <- adj_matrices_input_type$object_types_in_list[1]
  if (fcm_class == "conventional") {
    aggregate_adj_matrix <- aggregate_conventional_fcms(adj_matrices, aggregation_function, include_zeroes)
  } else if (fcm_class == "ivfn") {
    aggregate_adj_matrix <- aggregate_fcms_w_ivfns(adj_matrices, aggregation_function, include_zeroes)
  } else if (fcm_class == "tfn") {
    aggregate_adj_matrix <- aggregate_fcms_w_tfns(adj_matrices, aggregation_function, include_zeroes)
  }

  aggregate_adj_matrix
}

#' aggregate_conventional_fcms
#'
#' Construct an aggregate fcm from a group (list) of fcms
#'
#' @description Construct the aggregate fcm from a group (list)
#' of fcms. Via mean or median.
#'
#' @details Add details here
#'
#' @param adj_matrices A list type object of fcms. Must have a length greater
#' than 1.
#' @param aggregation_fun "mean" or "median"
#' @param include_zeroes TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#'
#' @return A single fcm calculated as the aggregate of the input adjacency matrices
#' @export
aggregate_conventional_fcms <- function(adj_matrices = list(matrix()),
                                        aggregation_function = c("mean", "median"),
                                        include_zeroes = TRUE) {

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))
  n_nodes <- length(node_names)
  n_maps <- length(adj_matrices)

  if (!include_zeroes) {
    adj_matrices <- lapply(adj_matrices, function(x) replace(x, x == 0, NA))
  }

  if (aggregation_function == "mean") {
    aggregate_adj_matrix <- apply(
      array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps)), 1:2,
      function(x) mean(x, na.rm = TRUE)
    )
  } else if (aggregation_function == "median") {
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
        aggregation_fun = aggregation_function,
        IDs = node_names
      )
    ),
    class = "aggregate_of_conventional_fcms"
  )
}


#' aggregate_fcms_w_ivfns
#'
#' Construct an aggregate fcm w/ edges represented as IVFNs from a group (list)
#' of fcms w/ edges represented as IVFNs
#'
#' @param adj_matrices A list type object of fcms. Must have a length greater
#' than 1.
#' @param aggregation_fun "mean" or "median"
#' @param include_zeroes TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#'
#' @return A single fcm w/ edges represented as IVFNs calculated as the aggregate
#' of the input adjacency matrices
#' @export
aggregate_fcms_w_ivfns <- function(adj_matrices = list(matrix()),
                                   aggregation_function = c("mean", "median"),
                                   include_zeroes = TRUE) {

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))
  n_nodes <- length(node_names)
  n_maps <- length(adj_matrices)

  lower_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$lower))
  upper_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$upper))

  lower_aggregate_adj_matrix <- aggregate_conventional_fcms(lower_adj_matrices, aggregation_function, include_zeroes)
  upper_aggregate_adj_matrix <- aggregate_conventional_fcms(upper_adj_matrices, aggregation_function, include_zeroes)

  #lower_aggregate_adj_matrix$adj_matrix[is.na(lower_aggregate_adj_matrix$adj_matrix)] <- 0
  #upper_aggregate_adj_matrix$adj_matrix[is.na(upper_aggregate_adj_matrix$adj_matrix)] <- 0

  aggregate_adj_matrix_w_ivfns <- make_adj_matrix_w_ivfns(lower_aggregate_adj_matrix$adj_matrix, upper_aggregate_adj_matrix$adj_matrix)

  colnames(aggregate_adj_matrix_w_ivfns) <- node_names
  rownames(aggregate_adj_matrix_w_ivfns) <- node_names

  structure(
    .Data = list(
      adj_matrix = as.data.frame(aggregate_adj_matrix_w_ivfns),
      params = list(
        input_adj_matrices = adj_matrices,
        aggregation_fun = aggregation_function,
        IDs = node_names
      )
    ),
    class = "aggregate_of_fcms_w_ivfns"
  )
}


#' aggregate_fcms_w_tfns
#'
#' Construct an aggregate fcm w/ edges represented as TFNs from a group (list)
#' of fcms w/ edges represented as TFNs
#'
#' @param adj_matrices A list type object of fcms. Must have a length greater
#' than 1.
#' @param aggregation_fun "mean" or "median"
#' @param include_zeroes TRUE/FALSE Whether to include zeroes in the mean/median
#' calculations. (i.e. if edges not included in a map should count as a zero-weighted
#' edge or not at all)
#'
#' @return A single fcm w/ edges represenetd as TFNs calculated as the aggregate
#' of the input adjacency matrices
#' @export
aggregate_fcms_w_tfns <- function(adj_matrices = list(matrix()),
                                  aggregation_function = c("mean", "median"),
                                  include_zeroes = TRUE) {

  concepts_in_adj_matrices <- lapply(adj_matrices, function(x) get_node_IDs_from_input(x))
  node_names <- unlist(unique(concepts_in_adj_matrices))
  n_nodes <- length(node_names)
  n_maps <- length(adj_matrices)

  lower_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$lower))
  mode_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$mode))
  upper_adj_matrices <- lapply(adj_matrices, function(adj_matrix) apply(adj_matrix, c(1, 2), function(x) x[[1]]$upper))

  lower_aggregate_adj_matrix <- aggregate_conventional_fcms(lower_adj_matrices, aggregation_function, include_zeroes)
  mode_aggregate_adj_matrix <- aggregate_conventional_fcms(mode_adj_matrices, aggregation_function, include_zeroes)
  upper_aggregate_adj_matrix <- aggregate_conventional_fcms(upper_adj_matrices, aggregation_function, include_zeroes)

  #lower_aggregate_adj_matrix$adj_matrix[is.na(lower_aggregate_adj_matrix$adj_matrix)] <- 0
  #mode_aggregate_adj_matrix$adj_matrix[is.na(mode_aggregate_adj_matrix$adj_matrix)] <- 0
  #upper_aggregate_adj_matrix$adj_matrix[is.na(upper_aggregate_adj_matrix$adj_matrix)] <- 0

  aggregate_adj_matrix_w_tfns <- make_adj_matrix_w_tfns(lower_aggregate_adj_matrix$adj_matrix, mode_aggregate_adj_matrix$adj_matrix, upper_aggregate_adj_matrix$adj_matrix)

  colnames(aggregate_adj_matrix_w_tfns) <- node_names
  rownames(aggregate_adj_matrix_w_tfns) <- node_names

  structure(
    .Data = list(
      adj_matrix = as.data.frame(aggregate_adj_matrix_w_tfns),
      params = list(
        input_adj_matrices = adj_matrices,
        aggregation_fun = aggregation_function,
        IDs = node_names
      )
    ),
    class = "aggregate_of_fcms_w_tfns"
  )
}


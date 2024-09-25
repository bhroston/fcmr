
#' dopar operator
#'
#' See https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf for details.
#'
#' @name %dopar%
#' @keywords internal
#' @export
#' @importFrom foreach %dopar%
NULL

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
#' @param value_colname Column name in the input edgelist that represents represents
#' the values displayed in the adjacency matrix (i.e. weight, standard_deviation)
#' @param node_order The order in which concepts should be arranged in the output
#' adjacency matrix. If no input given, concepts will be arranged alphabetically.
#'
#' @export
get_adj_matrix_from_edgelist <- function(edgelist = matrix(),
                                         source_colname = "source",
                                         target_colname = "target",
                                         value_colname = "weight",
                                         node_order = c()) {

  edgelist_column_inputs <- c(source_colname, target_colname, value_colname)
  edgelist_columns_match_inputs <- identical(colnames(edgelist), edgelist_column_inputs)

  if (!edgelist_columns_match_inputs) {
    stop("Edgelist column names do not match inputs source_colname, target_colname
         or value_colname. The default values for these are 'sourrce', 'target',
         and 'weight'. Check to make sure that these match the actual column
         names of the input edgelist.")
  }

  source_nodes <- edgelist[[source_colname]]
  target_nodes <- edgelist[[target_colname]]
  edge_values <- edgelist[[value_colname]]

  nodes <- unique(c(source_nodes, target_nodes))

  adj_matrix <- data.frame(matrix(data = 0, nrow = length(nodes), ncol = length(nodes)))
  colnames(adj_matrix) <- nodes
  rownames(adj_matrix) <- nodes

  for (i in seq_along(edge_values)) {
    edge <- edgelist[i, ]
    edge_row_loc <- which(nodes == edge[[source_colname]])
    edge_col_loc <- which(nodes == edge[[target_colname]])
    adj_matrix[edge_row_loc, edge_col_loc] <- edge_values[i]
  }

  node_order_given <- !identical(node_order, c())
  node_order_is_not_correct_length <- length(node_order) != length(nodes)
  node_order_input_is_not_type_character <- !identical(unique(typeof(node_order)), "character")
  if (!node_order_given) {
    adj_matrix <- adj_matrix
  } else if (node_order_given & (node_order_is_not_correct_length | node_order_input_is_not_type_character)) {
    stop("Input node_order must only contain character strings and must contain
         as many values as their are unique nodes depicted in the input edgelist.")
  } else {
    adj_matrix <- adj_matrix[node_order, node_order]
  }

  adj_matrix
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
get_node_IDs_from_input <- function(adj_matrix = matrix()) {
  empty_colnames <- identical(colnames(adj_matrix), NULL)
  if (empty_colnames) {
    IDs <- paste0("C", 1:nrow(adj_matrix))
  } else if (!empty_colnames) {
    IDs <- colnames(adj_matrix)
  }

  IDs
}



#' check_if_local_machine_has_access_to_show_progress_functionalities
#'
#' @description
#' Check whether the local machine has access to the necessary packages to
#' run code in parallel and/or using a progress bar. Specifically, checks for
#' the doSNOW, foreach, and pbapply packages.
#'
#' @details
#' Confirms that a local machine can access the required packages for
#' displaying progress bars at runtime. Will revise inputs
#' if particular packages are unavailable and warn the user of such changes, but will
#' not halt a run.
#'
#' @param use_parallel TRUE/FALSE The user intends to use parallel processing
#' @param use_show_progress TRUE/FALSE The user intends to display progress bars
check_if_local_machine_has_access_to_show_progress_functionalities <- function(use_parallel, use_show_progress) {
  # Confirm packages necessary packages are available. If not, change run options
  parallel_check <- use_parallel
  show_progress_check <- use_show_progress

  if (use_show_progress) {
    if (use_parallel) {
      local_machine_has_access_to_doSNOW <- requireNamespace("doSNOW")
      local_machine_has_access_to_foreach <- requireNamespace("foreach")
      if (!local_machine_has_access_to_doSNOW | !local_machine_has_access_to_foreach) {
        show_progress_check <- FALSE
        warning("\tShowing progress with parallel processing requires the 'doSNOW' and 'foreach' packages which are
        currently not installed. Running in parallel but without showing progress.")
      }
    } else {
      local_machine_has_access_to_pbapply <- requireNamespace("pbapply")
      if (!local_machine_has_access_to_pbapply) {
        show_progress_check <- FALSE
        warning("\tShowing progress requires the 'pbapply' package which is
        currently not installed. Running without showing progress.")
      }
    }
  }

  show_progress_check
}


#' check_if_local_machine_has_access_to_parallel_processing_functionalities
#'
#' @description
#' Check whether the local machine has access to the necessary packages to
#' run code in parallel and/or using a progress bar. Specifically, checks for
#' the parallel, doSNOW, foreach, and pbapply packages.
#'
#' @details
#' Confirms that a local machine can access the required packages for parallel
#' processing and/or displaying progress bars at runtime. Will revise inputs
#' if particular packages are unavailable and warn the user of such changes, but will
#' not halt a run.
#'
#' @param use_parallel TRUE/FALSE The user intends to use parallel processing
#' @param use_show_progress TRUE/FALSE The user intends to display progress bars
check_if_local_machine_has_access_to_parallel_processing_functionalities <- function(use_parallel, use_show_progress) {
  # Confirm packages necessary packages are available. If not, change run options
  parallel_check <- use_parallel
  show_progress_check <- use_show_progress

  if (use_parallel) {
    if (use_show_progress) {
      local_machine_has_access_to_doSNOW <- requireNamespace("doSNOW")
      local_machine_has_access_to_foreach <- requireNamespace("foreach")
      if (!local_machine_has_access_to_doSNOW | !local_machine_has_access_to_foreach) {
        parallel_check <- FALSE
        warning("\tShowing progress with parallel processing requires the 'doSNOW' and 'foreach' packages which are
        currently not installed. Running in parallel but without showing progress.")
      }
    } else {
      local_machine_has_access_to_parallel <- requireNamespace("parallel")
      if (!local_machine_has_access_to_parallel) {
        parallel_check <- FALSE
        warning("\tParallel processing requires the 'parallel' package which is
        currently not installed. Running without parallel processing.")
      }
    }
  }
  parallel_check
}


#' get_adj_matrices_input_type
#'
#' @description
#' This function identifies whether input is a list of adjacency matrices or
#' an individual adj matrix (input_type). If input is a list of adj matrices,
#' checks what data types the adj matrices are (list_objects) (e.g. tibble, matrix, etc.)
#'
#' @param adj_matrix_list_input A list of adj matrices or an individual adj matrix
get_adj_matrices_input_type <- function(adj_matrix_list_input) {
  classes_in_list_objects <- methods::is(list())
  classes_in_dataframe_objects <- methods::is(data.frame())
  classes_in_matrix_objects <- methods::is(matrix())
  classes_in_datatable_objects <- methods::is(data.table::data.table())
  classes_in_tibble_objects <- methods::is(tibble::tibble())

  classes_in_adj_matrix_list_input <- methods::is(adj_matrix_list_input)
  if (identical(classes_in_adj_matrix_list_input, classes_in_list_objects)) {
    adj_matrices_input_is_list <- TRUE
  } else {
    adj_matrices_input_is_list <- FALSE
  }

  if (adj_matrices_input_is_list) {
    num_object_types_in_input_list <- length(unique(lapply(adj_matrix_list_input, methods::is)))
    if (shiny::isRunning() & num_object_types_in_input_list != 1) {
      object_types_in_list = "unavailable"
    } else if (!shiny::isRunning() & num_object_types_in_input_list != 1) {
      stop("All objects in adj matrix list must be of the same type.")
    }
    object_types_in_input_list <- unique(lapply(adj_matrix_list_input, is))[[1]]
  } else {
    object_types_in_input_list <- methods::is(adj_matrix_list_input)
  }

  if (identical(object_types_in_input_list, classes_in_dataframe_objects)) {
    object_types_in_list <- c("conventional", "data.frame")
  } else if (identical(object_types_in_input_list, classes_in_matrix_objects)) {
    object_types_in_list <- c("conventional", "matrix")
  } else if (identical(object_types_in_input_list, classes_in_datatable_objects)) {
    object_types_in_list <- c("conventional", "data.table")
  } else if (identical(object_types_in_input_list, classes_in_tibble_objects)) {
    object_types_in_list <- c("conventional", "tibble")
  } else if (identical(object_types_in_input_list, "adj_matrix_w_ivfns")) {
    object_types_in_list <- "ivfn"
  } else if (identical(object_types_in_input_list, "adj_matrix_w_tfns")) {
    object_types_in_list <- "tfn"
  } else {
    if (shiny::isRunning()) {
      object_types_in_list <- "unavailable"
    } else {
      stop("The list of objects in adj. matrix list must be one of the following: 'data.frame' 'matrix' 'sparseMatrix' 'data,table' 'tibble'")
    }
  }

  list(
    adj_matrices_input_is_list = adj_matrices_input_is_list,
    object_types_in_list = object_types_in_list
  )
}


#' get_class_of_adj_matrix
#'
#' @description
#' Get the class of map from the input adj matrix
#'
#' @details
#' This returns the class of fcm represented by the input adjacency matrices i.e.
#' fcm, fgcm, fcm_w_tfn, etc.
#'
#' Intended for developer use only to improve package readability.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM, FGCM, or fcm_w_tfn
get_class_of_adj_matrix <- function(adj_matrix = matrix()) {
  object_classes_in_adj_matrix <- unique(as.vector(apply(adj_matrix, c(1, 2), function(element) class(element[[1]]))))

  if (all(object_classes_in_adj_matrix %in% "numeric")) {
    adj_matrix_class <- "fcm"
  } else if (all(object_classes_in_adj_matrix %in% c("numeric", "grey_number"))) {
    adj_matrix_class <- "fgcm"
  } else if (all(object_classes_in_adj_matrix %in% c("numeric", "tfn"))) {
    adj_matrix_class <- "fcm_w_tfn"
  } else {
    stop(paste0("Incompatible collection of data types found in input adj_matrix: '", paste0(object_classes_in_adj_matrix, collapse = '', "'")))
  }

  adj_matrix_class
}

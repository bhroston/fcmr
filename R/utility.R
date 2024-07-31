
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



#' confirm_adj_matrices_have_same_concepts
#'
#' @description
#' This checks a list of lists of column names of adjacency matrices and
#' confirms that each list is identical.
#'
#' @details
#' Note: This function does NOT take the raw adjacency matrices as an input. Rather,
#' it takes a list of lists of column names of adjacency matrices.
#'
#'
#' @param list_of_concepts_by_adj_matrix A list of lists of column names of adjacency matrices
#'
#' @export
confirm_adj_matrices_have_same_concepts <- function(list_of_concepts_by_adj_matrix = list()) {
  all_adj_matrices_have_same_concepts <- length(unique(list_of_concepts_by_adj_matrix)) == 1
  if (!all_adj_matrices_have_same_concepts) {
    stop("All input adjacency matrices must have the same concepts.")
  }
}



#' confirm_adj_matrices_have_same_dimensions
#'
#' @description
#' This checks that all adjacency matrices in a list have the same dimensions
#' (i.e. all are n x n)
#'
#' @details
#' Note: This function DOES take the raw adjacency matrices as an input.
#'
#' @param adj_matrices A list of n x n adjacency matrices
#'
#' @export
confirm_adj_matrices_have_same_dimensions <- function(adj_matrices = list(matrix())) {
  dimensions_of_input_adj_matrices <- lapply(adj_matrices, dim)
  all_adj_matrices_have_same_dimensions <- length(unique(dimensions_of_input_adj_matrices)) == 1
  if (!all_adj_matrices_have_same_dimensions) {
    stop("All input adjacency matrices must have the same dimensions (n x n) throughout the entire list")
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


#' get_fcm_class_from_adj_matrices
#'
#' @description
#' Get the class of fcm from the input adj matrices
#'
#' @details
#' This returns the class of fcm represented by the input adjacency matrices i.e.
#' fcm, fgcm, ftcm, etc.
#'
#' Intended for developer use only to improve package readability.
#'
#' @param adj_matrices A list of n x n adjacency matrix that represents an FCM
get_fcm_class_from_adj_matrices <- function(adj_matrices = list(matrix())) {
  data_types_in_adj_matrices_by_adj_matrix <- lapply(
    adj_matrices,
    function(adj_matrix) {
      unique(unlist(apply(adj_matrix, c(1, 2), function(element) class(element[[1]]), simplify = FALSE)))  # ifelse(class(element[[1]]) != "numeric", class(element[[1]]), class(element[[1]])))
    }
  )

  all_adj_matrices_are_of_same_class <- length(unique(data_types_in_adj_matrices_by_adj_matrix)) == 1
  if (!all_adj_matrices_are_of_same_class) {
    stop("All input adj_matrices must have the same data types, i.e. they must all be
         either conventional fcm, fuzzy grey cognitive maps, or fuzzy triangular cognitive maps.")
  }

  data_types_in_adj_matrices <- unlist(unique(data_types_in_adj_matrices_by_adj_matrix))
  if (all(data_types_in_adj_matrices %in% "numeric")) {
    fcm_class <- "fcm"
  } else if (all(data_types_in_adj_matrices %in% c("numeric", "grey_number"))) {
    fcm_class <- "fgcm"
  } else if (all(data_types_in_adj_matrices %in% c("numeric", "triangular_number"))) {
    fcm_class <- "ftcm"
  } else {
    stop(paste0("Incompatible collection of data types found in input adj_matrices: '", paste0(data_types_in_adj_matrices, collapse = '', "'")))
  }

  fcm_class
}



#' confirm_input_vector_is_compatible_with_adj_matrices
#'
#' @description
#' Check whether an input vector (initial_state_vector or clamping_vector) is
#' compatible with the data types present in the representative_adj_matrix.
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Intended for developer use only to improve package readability.
#'
#' @param representative_adj_matrix An adjacency matrix whose format (i.e. dims and data.types)
#' are representative of a larger list of adjacency matrices
#' @param input_vector An input vector, either the initial_state_vector input or
#' the clamping_vector input
#' @param fcm_class The class of fcm represented by the representative_adj_matrix
confirm_input_vector_is_compatible_with_adj_matrices <- function(representative_adj_matrix = matrix(),
                                                                 input_vector = c(),
                                                                 fcm_class = c("fcm", "fgcm", "ftcm")) {

  if (fcm_class == "fcm") {
    confirm_input_vector_is_compatible_with_adj_matrix(representative_adj_matrix, input_vector)
  } else if (fcm_class == "fgcm") {
    confirm_input_vector_is_compatible_with_grey_adj_matrix(representative_adj_matrix, input_vector)
  } else if (fcm_class == "ftcm") {
    confirm_input_vector_is_compatible_with_triangular_adj_matrix(representative_adj_matrix, input_vector)
  }
}



#' match_state_vector_df_shapes
#'
#' @description
#' Given two data frames of state vectors, extend the one with the least number of rows
#' by repeating its final iteration value until the data frames are the same shape (i.e.
#' have the same number of rows)
#'
#' @details
#' Ensure that both input data frames are the same shape
#'
#' Intended for developer use only to improve package readability.
#'
#' @param baseline_state_vectors A state vectors dataframe for the baseline simulation
#' @param scenario_state_vectors A state vectors dataframe for the scenario simulation
match_state_vector_df_shapes <- function(baseline_state_vectors, scenario_state_vectors) {
  n_rows_baseline <- nrow(baseline_state_vectors)
  n_rows_scenario <- nrow(scenario_state_vectors)

  if (n_rows_baseline == n_rows_scenario) {
    new_baseline_state_vectors <- baseline_state_vectors
    new_scenario_state_vectors <- scenario_state_vectors
  } else if (n_rows_baseline < n_rows_scenario) {
    extended_baseline_state_vectors <- data.frame(apply(
      baseline_state_vectors, 2, function(sim) {
        c(sim, rep(sim[n_rows_baseline], n_rows_scenario - n_rows_baseline))
      }
    ))
    new_baseline_state_vectors <- extended_baseline_state_vectors
    new_scenario_state_vectors <- scenario_state_vectors
  } else if (n_rows_scenario < n_rows_baseline) {
    extended_scenario_state_vectors <- data.frame(apply(
      scenario_state_vectors, 2, function(sim) {
        c(sim, rep(sim[n_rows_scenario], n_rows_baseline - n_rows_scenario))
      }
    ))
    new_baseline_state_vectors <- baseline_state_vectors
    new_scenario_state_vectors <- extended_scenario_state_vectors
  }

  list(
    baseline = data.frame(new_baseline_state_vectors),
    scenario = data.frame(new_scenario_state_vectors)
  )
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


# get_number_of_fcm_combinations <- function(adj_matrices = list(matrix())) {
#   non_zero_edge_indexes_by_adj_matrix <- lapply(adj_matrices, function(adj_matrix) which(adj_matrix != 0, arr.ind = TRUE))
#   non_zero_edge_indexes_across_adj_matrices <- unique(do.call(rbind, non_zero_edge_indexes_by_adj_matrix))
#
#
#
#   included_edge_weights_across_adj_matrices <- lapply(adj_matrices, function(adj_matrix) adj_matrix[non_zero_edge_indexes_across_adj_matrices])
#   unique_included_edge_weights_across_adj_matrices <- lapply(included_edge_weights_across_adj_matrices, unique)
#   unique(expand.grid(unique_included_edge_weights_across_adj_matrices))
#
#   if (!include_zeroes) {
#     included_edge_weights_across_adj_matrices <- lapply(included_edge_weights_across_adj_matrices, function(x) x[x != 0])
#   }
#   num_included_edge_weights_across_adj_matrices <- lapply(included_edge_weights_across_adj_matrices, length)
#   estimated_number_of_fcm_combinations <- prod(unlist(num_included_edge_weights_across_adj_matrices))
#
#   estimated_number_of_fcm_combinations
# }
#
#.  adj_matrices_dims <- unlist(unique(lapply(adj_matrices, function(adj_matrix) dim(adj_matrix))))
#   edge_weights_by_adj_matrix <- lapply(adj_matrices, function(adj_matrix) unlist(array(adj_matrix, c(1, n_nodes))))
#   edge_weights_by_adj_matrix <- do.call(rbind, edge_weights_by_adj_matrix)
#   non_zero_edge_indexes <- apply(edge_weights_by_adj_matrix, 2, function(col_values) !identical(unique(col_values), 0))
#   valued_edge_weights_by_adj_matrix <- edge_weights_by_adj_matrix[, non_zero_edge_indexes]
#
#   number_of_fcm_combinations <- apply(nrow(valued_edge_weights_by_adj_matrix), 2, function(x) length(x))
#
#   widened_combined_adj_matrix_as_lists <- lapply(seq_len(ncol(nonzero_widened_combined_adj_matrix)), function(x) nonzero_widened_combined_adj_matrix[, x])
#
#   seq_len()
#
#
#   non_zero_edge_indexes <- apply(edge_weights_by_adj_matrix, 2, function(col_values) !identical(unique(col_values), 0))
#
#
#
#
#   combined_adj_matrix <- apply(adj_matrices, c(1, 2), function(x) x, simplify = FALSE)
#   widened_combined_adj_matrix <- array(combined_adj_matrix, c(1, n_nodes^2))
#   widened_combined_adj_matrix <- apply(widened_combined_adj_matrix, 2, unlist)
#   non_zero_value_indexes <- unlist(lapply(apply(widened_combined_adj_matrix, 2, unique), function(x) !identical(x, 0)))
#   nonzero_widened_combined_adj_matrix <- widened_combined_adj_matrix[, non_zero_value_indexes]
#
#   widened_combined_adj_matrix_as_lists <- lapply(seq_len(ncol(nonzero_widened_combined_adj_matrix)), function(x) nonzero_widened_combined_adj_matrix[, x])
#
# }


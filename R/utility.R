
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
get_node_IDs_from_input <- function(adj_matrix = matrix()) {
  empty_colnames <- identical(colnames(adj_matrix), NULL)
  if (empty_colnames) {
    IDs <- paste0("C", 1:nrow(adj_matrix))
  } else if (!empty_colnames) {
    IDs <- colnames(adj_matrix)
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


#' check_adj_matrix_list_is_list
#'
#' @description
#' Check that the input adj_matrix_list is either a list of adj. matrices or if
#' only one adj. matrix is given, abstract that matrix within a list (this is to
#' comply with lapply functionalities within the codebase)
#'
#' @details
#' This is just in case the function isn't passed an actual list of adj. matrices.
#' In R, both matrix and list are of class "list" so you have to do some odd checks
#' to check if the input is just a singular matrix or a list of matrices
#'
#' Intended for developer use only to improve package readability.
#'
#' @param adj_matrix_list A list of n x n adjacency matrix that represents an FCM
check_adj_matrix_list_is_list <- function(adj_matrix_list = list(matrix())) {
  if (!is.null(dim(adj_matrix_list)) & (length(unique(dim(adj_matrix_list))) == 1)) {
    adj_matrix_list <- list(adj_matrix_list)
  }
  adj_matrix_list
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



#' #' confirm_input_vector_is_compatible_with_adj_matrices
#' #'
#' #' @description
#' #' Check whether an input vector (initial_state_vector or clamping_vector) is
#' #' compatible with the data types present in the representative_adj_matrix.
#' #'
#' #' @details
#' #' [ADD DETAILS HERE!!!]
#' #'
#' #' Intended for developer use only to improve package readability.
#' #'
#' #' @param representative_adj_matrix An adjacency matrix whose format (i.e. dims and data.types)
#' #' are representative of a larger list of adjacency matrices
#' #' @param input_vector An input vector, either the initial_state_vector input or
#' #' the clamping_vector input
#' #' @param fcm_class The class of fcm represented by the representative_adj_matrix
#' confirm_input_vector_is_compatible_with_adj_matrices <- function(representative_adj_matrix = matrix(),
#'                                                                  input_vector = c(),
#'                                                                  fcm_class = c("fcm", "fgcm", "fcm_w_tfn")) {
#'
#'   if (fcm_class == "fcm") {
#'     confirm_input_vector_is_compatible_with_adj_matrix(representative_adj_matrix, input_vector)
#'   } else if (fcm_class == "fgcm") {
#'     confirm_input_vector_is_compatible_with_grey_adj_matrix(representative_adj_matrix, input_vector)
#'   } else if (fcm_class == "fcm_w_tfn") {
#'     confirm_input_vector_is_compatible_with_fcm_w_tfn_adj_matrix(representative_adj_matrix, input_vector)
#'   }
#' }


#' convert_fuzzy_elements_in_matrix_to_distributions
#'
#' @description
#' Given a list of adjacency matrices which include either grey_numbers or
#' tfns, convert those objects to their corresponding
#' distributions representative of those values.
#'
#' @details
#' [ADD DETAILS HERE!!!!]
#'
#' Use vignette("fcmconfr-class") for more information.
#'
#' @param fuzzy_matrix A matrix that can contain fuzzy sets as elements
#' @param fuzzy_element_class "fgcm" or "fcm_w_tfn" - the class of elements in the fuzzy_matrix
#' @param N_samples The number of samples to draw from the corresponding distribution
#'
#' @export
convert_fuzzy_elements_in_matrix_to_distributions <- function(fuzzy_matrix = data.frame(),
                                                              fuzzy_element_class = c("fgcm", "fcm_w_tfn"),
                                                              N_samples = integer()) {
  if (!(fuzzy_element_class %in% c("fgcm", "fcm_w_tfn"))) {
    stop("Input fuzzy_element_class must be either fgcm or fcm_w_tfn")
  } else if (identical(fuzzy_element_class, c("fgcm", "fcm_w_tfn"))) {
    fuzzy_element_class <- get_class_of_adj_matrix(fuzzy_matrix)
  }

  # browser()
  if (fuzzy_element_class == "fgcm") {
    fuzzy_matrix_w_distributions <- apply(
      fuzzy_matrix, c(1, 2),
      function(element) {
        if (identical(methods::is(element[[1]]), "grey_number")) {
          element <- list(stats::runif(N_samples, element[[1]]$lower, element[[1]]$upper))
        } else {
          element[[1]]
        }
      }
    )
  } else if (fuzzy_element_class == "fcm_w_tfn") {
    fuzzy_matrix_w_distributions <- apply(
      fuzzy_matrix, c(1, 2),
      function(element) {
        if (identical(methods::is(element[[1]]), "tfn")) {
          element <- list(rtri(N_samples, lower = element[[1]]$lower, mode = element[[1]]$mode, upper = element[[1]]$upper))
        } else {
          element[[1]]
        }
      }
    )
  }

  fuzzy_matrix_w_distributions
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



#' #' check_simulation_inputs
#' #'
#' #' @description
#' #' Confirm that all inputs will work with the simulation function and return
#' #' appropriate error messages where necessary
#' #'
#' #' @details
#' #' This checks that all inputs for a simulation function are of an appropriate
#' #' format, and also fills in missing inputs for initial_state_vector, clamping_vector,
#' #' and IDs when appropriate.
#' #'
#' #' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' #' @param initial_state_vector A list state values at the start of an fcm simulation
#' #' @param clamping_vector A list of values representing specific actions taken to
#' #' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' #' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' #' @param activation The activation function to be applied. Must be one of the following:
#' #' 'kosko', 'modified-kosko', or 'rescale'.
#' #' @param squashing A squashing function to apply. Must be one of the following:
#' #' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' #' @param lambda A numeric value that defines the steepness of the slope of the
#' #' squashing function when tanh or sigmoid are applied
#' #' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' #' @param min_error The lowest error (sum of the absolute value of the current state
#' #' vector minus the previous state vector) at which no more iterations are necessary
#' #' and the simulation will stop
#' #'
#' #' @export
#' check_simulation_inputs <- function(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error) {
#'   confirm_adj_matrix_is_square(adj_matrix)
#'   if (identical(initial_state_vector, c())) {
#'     warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
#'     initial_state_vector <- rep(1, nrow(adj_matrix))
#'   }
#'   if (identical(clamping_vector, c())) {
#'     warning("No clamping_vector input given. Assuming no values are clamped.")
#'     clamping_vector <- rep(0, length(initial_state_vector))
#'   }
#'   confirm_input_vector_is_compatible_with_adj_matrix(adj_matrix, initial_state_vector)
#'   confirm_input_vector_is_compatible_with_adj_matrix(adj_matrix, clamping_vector)
#'
#'   if (!(activation %in% c("kosko", "modified-kosko", "rescale"))) {
#'     stop("Input activation must be one of the following: 'kosko', 'modified-kosko', or 'rescale'")
#'   }
#'   if (!(squashing %in% c("sigmoid", "tanh"))) {
#'     stop("Input squashing must be one of the following: 'sigmoid', 'tanh'")
#'   }
#'   if (activation == "rescale" & squashing != "sigmoid") {
#'     stop(
#'       paste0(
#'         "   !!!Please use the sigmoid squashing function with the rescale activation function!!!
#'
#'           The rescale activation function is designed to optimize performance
#'           with the sigmoid squashing function. Results are unreliable if
#'           using a different squashing function.\n",
#'
#'         "\n          Input squashing function: ", squashing)
#'     )
#'   }
#'
#'   if (!is.numeric(lambda)) {
#'     stop("Input lambda must be numeric")
#'   }
#'   if (lambda <= 0) {
#'     stop("Input lambda must be greater than 0")
#'   }
#'
#'   if (!(max_iter == round(max_iter))) {
#'     stop("Input max_iter must be a positive integer")
#'   }
#'   if (max_iter <= 0) {
#'     stop("Input max_iter must be a positive integer")
#'   }
#'
#'   if (!is.numeric(min_error)) {
#'     stop("Input min_error must be numeric")
#'   }
#'   if (min_error <= 0) {
#'     stop("Input min_error must be greater than 0")
#'   }
#'
#'   list(
#'     initial_state_vector = initial_state_vector,
#'     clamping_vector = clamping_vector
#'   )
#' }



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

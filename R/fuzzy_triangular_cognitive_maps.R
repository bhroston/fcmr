

#' confirm_input_vector_is_compatible_with_triangular_adj_matrix
#'
#' @description
#' Confirm that an initial state vector is algorithmically compatible with a triangular adjacency matrix
#'
#' @details
#' Boolean. TRUE if the number of entries in the initial
#' state vector match the number of rows/columns in the adjacency matrix and 2. The
#' datatypes stored within each object are the same (i.e. "numeric" vs "triangular_number"),
#' FALSE if not
#'
#' Intended for developer use only to improve package readability.
#'
#' @param triangular_adj_matrix An n x n triangular adjacency matrix that represents an FCM
#' @param initial_state_vector An n-length list of the initial states of each node in an fcm simulation
confirm_input_vector_is_compatible_with_triangular_adj_matrix <- function(triangular_adj_matrix = matrix(), initial_state_vector = c()) {
  if (length(initial_state_vector) != unique(dim(triangular_adj_matrix))) {
    stop("Length of input initial_state_vector is does not comply with the dimensions of the input adjacency matrix", .call = FALSE)
  } else {
    TRUE
  }

  data_types <- unique(vapply(initial_state_vector, class, character(1)))
  both_numeric_or_triangular_number_data_types <- identical(data_types, c("numeric", "triangular_number")) | identical(data_types, c("triangular_number", "numeric"))
  only_numeric_data_types <- identical(data_types, "numeric")
  only_triangular_number_data_types <- identical(data_types, "triangular_number")

  if (both_numeric_or_triangular_number_data_types | only_numeric_data_types | only_triangular_number_data_types) {
    TRUE
  } else {
    stop("Input initial state vector must contain only numeric or triangular_number values")
  }
}


#' get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices
#'
#' @description
#' This "gets" a triangular adjacency matrix from an adjacency matrix of the lower
#' limits of edges in an FCM and an adjacency matrix of the upper limits of edges
#' in an FCM.
#'
#' @details
#' The input adjacency matrices must square n x n matrices with the same dimensions.
#' The input can be either matrix, data.table, tibble, or data.table type objects,
#' but the output will always be a data.frame. This is for output readability.
#' data.table and tibble objects work logically, but their outputs require
#' additional steps to parse from the user's perspective.
#'
#' If the input matrices have named columns, those names will be carried over
#' in the triangular adjacency matrix. Otherwise, generic node IDs will be used
#' (C1, C2, ... Cn).
#'
#' #' Use vignette("ftcm-class") for more information.
#'
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param mode An n x n adjacency matrix that represents the modes of edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @export
#' @examples
#' get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#'  mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2)
#'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' )
get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices <- function(lower = matrix(),
                                                                        mode = matrix(),
                                                                        upper = matrix()) {
  if (!identical(dim(lower), dim(mode), dim(upper))) {
    stop("Failed Validation: Input adjacency matrices must be the same size")
  }

  if (nrow(lower) != ncol(lower) | nrow(mode) != ncol(mode) | nrow(upper) != ncol(upper)) {
    stop("Failed Validation: Input adjacency matrices must be square matrices (n x n)")
  } else {
    size <- nrow(lower)
  }

  all_input_matrices_have_same_colnames <- length(unique(list(colnames(lower), colnames(mode), colnames(upper)))) == 1
  if (all_input_matrices_have_same_colnames & !identical(colnames(lower), NULL)) {
    IDs <- colnames(lower)
  } else {
    IDs <- paste0("C", 1:nrow(lower))
  }

  edge_locs_in_lower <- data.table::data.table(which(lower != 0, arr.ind = TRUE))
  edge_locs_in_mode <- data.table::data.table(which(mode != 0, arr.ind = TRUE))
  edge_locs_in_upper <- data.table::data.table(which(upper != 0, arr.ind = TRUE))
  all_input_matrices_have_same_edge_locs <- length(unique(list(edge_locs_in_lower, edge_locs_in_mode, edge_locs_in_upper))) == 1
  if (!all_input_matrices_have_same_edge_locs) {
    warning("Input adjacency matrices must be structurally equivallent. i.e. If
    there is a non-zero value in one matrix, there must be another non-zero
    value at the same location in the other matrix. If one matrix has a non-zero
    value where the other has a zero value, it is assumed that the zero value is
    a part of the triangular number for that edge.")
  }
  all_edge_locs <- unique(rbind(edge_locs_in_lower, edge_locs_in_mode, edge_locs_in_upper))

  #triangular_adj_matrix <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  triangular_adj_matrix <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  colnames(triangular_adj_matrix) <- IDs
  rownames(triangular_adj_matrix) <- IDs

  for (index in 1:nrow(all_edge_locs)) {
    i <- all_edge_locs$row[index]
    j <- all_edge_locs$col[index]
    triangular_adj_matrix[[j]][[i]] <- triangular_number( # [[j]][[i]] instead of [[i]][[j]]
      # because this notation is
      # [[col]][[row]] for data.frames
      lower = lower[i, j],
      mode = mode[i, j],
      upper = upper[i, j]
    )
  }

  triangular_adj_matrix
}



#' triangular_number S3 class
#'
#' @description
#' This class is an organization scheme for Triangular Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The triangular_number class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' Use vignette("ftcm-class") for more information.
#'
#' @param lower lower limit of a Triangular Number set (the lower value must be less than or equal to the upper value)
#' @param mode the most likely value of a Triangular Number set
#' @param upper upper limit of a Triangular Number set (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' triangular_number(lower = 0, mode = 0.5, upper = 1)
triangular_number <- function(lower = double(), mode = double(), upper = double()) {
  if (identical(lower, double())) {
    lower <- -Inf
  }

  if (identical(mode, double())) {
    mode <- 0
  }

  if (identical(upper, double())) {
    upper <- Inf
  }

  if ((!is.numeric(lower)) | (!is.numeric(mode)) | (!is.numeric(upper))) {
    stop("lower, mode, and upper must be single, numeric values", call. = FALSE)
  }

  if (lower > upper | lower > mode) {
    stop("The lower input must be less than or equal to both the mode and upper inputs", call. = FALSE)
  }

  if (mode > upper) {
    stop("The mode input must be less than or equal to the upper input", call. = FALSE)
  }

  structure(
    .Data = data.frame(lower = lower, mode = mode, upper = upper),
    class = "triangular_number"
  )
}


#' print.triangular_number
#'
#' @description
#' This improves the readability of the output
#'
#' @details
#' triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The triangular_number class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' triangular_numbers are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("ftcm-class") for more information.
#'
#' @param x a triangular_number object
#' @param ... additional inputs
#'
#' @export
#' @examples
#' triangular_number(lower = 0, upper = 1)
print.triangular_number <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$mode, ", ", x$upper, "]", sep = "")
}


#' c.triangular_number
#'
#' @description
#' This forces the output of c() to the equivalent of list() only for inputs of
#' type triangular_number
#'
#' @details
#' For triangular_number objects, c() combines all of the lower and upper data into
#' a single triangular_number object, but list() returns the expected output of a
#' list of distinct triangular_number objects.
#'
#' @param ... a set of triangular_number objects
#'
#' @export
#' @examples
#' c(triangular_number(0, 1), triangular_number(0.2, 0.5))
c.triangular_number <- function(...) {
  list(...)
}



#' get_triangular_distribution_of_values
#'
#' @description
#' This pulls n samples from a triangular distribution described by shape parameters
#' defined by a lower limit, upper limit, and mode
#'
#' @details
#'
#' Use vignette("fcmcmrr-class") for more information.
#'
#' @param lower lower limit or minimum of the sample space
#' @param upper upper limit or maximum of the sample space
#' @param mode peak of the sample space
#' @param n number of samples to draw from the triangular distribution
#'
#' @export
get_triangular_distribution_of_values <- function(lower = double(), upper = double(), mode = double(), n = 1000) {
  if (lower > upper) {
    stop("lower input must be less than upper input")
  }

  if (identical(mode, double())) {
    mode <- (lower + upper)/2
  }
  inv_cdf <- vector(mode = "numeric", length = n)
  for (i in 1:n) {
    x <- i/n
    if (x <= mode) {
      inv_cdf[i] <- sqrt(x*(upper - lower)*(mode - lower)) + lower
    } else if (x > mode) {
      inv_cdf[i] <- upper - sqrt((-x + 1)*(upper - lower)*(upper - mode))
    } else {
      stop("Unknown input")
    }
  }
  values_distribution <- inv_cdf

  values_distribution
}





#' #' ftcmconfr
#' #'
#' #' @description
#' #' [ADD DETAILS HERE!!!!]
#' #'
#' #' @details
#' #' [ADD DETAILS HERE!!!]
#' #'
#' #' Use vignette("fmcm-class") for more information.
#' #'
#' #' @param ftcm_adj_matrices A list of n x n adjacencey matrices representing fcms
#' #' @param samples The number of samples to draw with the selected sampling method. Also,
#' #' the number of sampled models to generate
#' #' @param initial_state_vector A list state values at the start of an fcm simulation
#' #' @param clamping_vector A list of values representing specific actions taken to
#' #' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' #' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' #' @param activation The activation function to be applied. Must be one of the following:
#' #' 'kosko', 'modified-kosko', or 'papageorgiou'.
#' #' @param squashing A squashing function to apply. Must be one of the following:
#' #' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' #' @param lambda A numeric value that defines the steepness of the slope of the
#' #' squashing function when tanh or sigmoid are applied
#' #' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' #' @param min_error The lowest error (sum of the absolute value of the current state
#' #' vector minus the previous state vector) at which no more iterations are necessary
#' #' and the simulation will stop
#' #' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' #' from the pbapply package as the underlying function.
#' #' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' #' @param n_cores Number of cores to use in parallel processing. If no input given,
#' #' will use all available cores in the machine.
#' #' @param IDs A list of names for each node (must have n items). If empty, will use
#' #' column names of adjacancy matrix (if given).
#' #' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' #' FCM. Will dramatically increase size of output if TRUE.
#' #'
#' #' @export
#' ftcmconfr <- function(ftcm_adj_matrices = list(matrix()),
#'                       samples = 1000,
#'                       include_zeroes_in_aggregation = TRUE,
#'                       aggregation_fun = c("mean", "median"),
#'                       initial_state_vector = c(),
#'                       clamping_vector = c(),
#'                       activation = c("kosko", "modified-kosko", "rescale"),
#'                       squashing = c("sigmoid", "tanh"),
#'                       lambda = 1,
#'                       max_iter = 100,
#'                       min_error = 1e-5,
#'                       bootstrap_inference_means = TRUE,
#'                       bootstrap_CI = 0.95,
#'                       bootstrap_reps = 5000,
#'                       bootstrap_draws_per_rep = 5000,
#'                       show_progress = TRUE,
#'                       parallel = TRUE,
#'                       n_cores = integer(),
#'                       IDs = c(),
#'                       include_simulations_in_output = FALSE,
#'                       ...) {
#'
#'   concepts_in_ftcms <- lapply(ftcm_adj_matrices, function(x) get_node_IDs_from_input(x, IDs))
#'   all_ftcms_have_same_concepts <- length(unique(concepts_in_ftcms)) == 1
#'   if (!all_ftcms_have_same_concepts) {
#'     stop("All triangular adjacency matrices must have the same concepts.")
#'   }
#'
#'   dimensions_of_input_triangular_adj_matrices <- lapply(ftcm_adj_matrices, dim)
#'   all_ftcms_have_same_dimensions <- length(unique(dimensions_of_input_triangular_adj_matrices)) == 1
#'   if (!all_ftcms_have_same_dimensions) {
#'     stop("All triangular adjacency matrices must have the same dimensions (n x n) throughout the entire list")
#'   }
#'
#'   # Confirm packages necessary packages are available. If not, change run options
#'   if (parallel) {
#'     package_checks <- check_if_local_machine_has_parallel_processing_packages(parallel, show_progress)
#'     parallel <- package_checks$parallel_check
#'   }
#'   if (show_progress) {
#'     package_checks <- check_if_local_machine_has_parallel_processing_packages(parallel, show_progress)
#'     show_progress <- package_checks$show_progress_check
#'   }
#'
#'   # Check that adj_matrices are correct format
#'   # lapply(ftcm_adj_matrices, function(x) ftcm(x, IDs))
#'
#'   nodes <- unlist(unique(concepts_in_ftcms))
#'   sampled_triangular_adj_matrices <- build_ftcmconfr_models(ftcm_adj_matrices, samples, aggregation_fun, include_zeroes_in_aggregation, nodes, show_progress)
#'
#'   fmcm_results <- infer_fmcm(
#'     simulated_adj_matrices = sampled_triangular_adj_matrices,
#'     initial_state_vector = initial_state_vector,
#'     clamping_vector = clamping_vector,
#'     activation = activation,
#'     squashing = squashing,
#'     lambda = lambda,
#'     max_iter = max_iter,
#'     min_error = min_error
#'   )
#'
#'   params <- list(
#'     ftcms = ftcm_adj_matrices,
#'     inference_opts = list(initial_state_vector = initial_state_vector,
#'                           clamping_vector = clamping_vector,
#'                           activation = activation,
#'                           squashing = squashing,
#'                           lambda = lambda,
#'                           max_iter = max_iter,
#'                           min_error = min_error,
#'                           IDs = IDs),
#'     bootstrap_input_opts = list(aggregation_fun = aggregation_fun,
#'                                 samples = samples,
#'                                 include_zeroes = include_zeroes_in_aggregation),
#'     runtime_opts = list(parallel = parallel,
#'                         n_cores = n_cores,
#'                         show_progress = show_progress,
#'                         include_simulations_in_output = include_simulations_in_output)
#'   )
#'
#'   if (bootstrap_inference_means) {
#'     means_of_fmcm_inferences <- get_means_of_fmcm_inference(
#'       fmcm_inference = fmcm_results$inference,
#'       get_bootstrapped_means = bootstrap_inference_means,
#'       confidence_interval = bootstrap_CI,
#'       bootstrap_reps = bootstrap_reps,
#'       bootstrap_samples_per_rep = bootstrap_reps,
#'       parallel = parallel,
#'       n_cores = n_cores
#'     )
#'
#'     params$bootstrap_output_opts = list(bootstrap_inference_means =  bootstrap_inference_means,
#'                                         bootstrap_CI = bootstrap_CI,
#'                                         bootstrap_reps = bootstrap_reps,
#'                                         bootstrap_draws_per_rep = bootstrap_draws_per_rep)
#'
#'     ftcmconfr_output <- structure(
#'       .Data = list(
#'         inference = fmcm_results$inference,
#'         params = params,
#'         bootstrap = list(
#'           mean_CI_by_node = means_of_fmcm_inferences$mean_CI_by_node,
#'           raw_bootstrap_means = means_of_fmcm_inferences$bootstrap_means
#'         )
#'       ),
#'       class = "ftcmconfr"
#'     )
#'   } else {
#'     ftcmconfr_output <- structure(
#'       .Data = list(
#'         inference = fmcm_results$inference,
#'         params = params
#'       ),
#'       class = "ftcmconfr"
#'     )
#'   }
#'
#'   ftcmconfr_output
#' }
#'

#' #' build_ftcmconfr_models
#' #'
#' #' @description
#' #' This function generates n ftcm models whose edge weights are sampled from either
#' #' the defined edge values in a set of adjacency matrices or continuous (uniform or triangular)
#' #' parametric distributions derived from the sets of edge values, and stores them
#' #' as a list of adjacency matrices.
#' #'
#' #' @details
#' #' [ADD DETAILS HERE!!!]
#' #'
#' #' Use vignette("ftcm-class") for more information.
#' #'
#' #' @param triangular_adj_matrices A list of n x n adjacencey matrices representing fcms
#' #' @param sampling The sampling method to be applied. Must be one of the following: "nonparametric", "uniform", or "triangular"
#' #' @param samples The number of samples to draw with the selected sampling method. Also,
#' #' the number of sampled models to generate
#' #' @param nodes A vector of node names (IDs) present in every adjacency matrix
#' #' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' #' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' #' from the pbapply package as the underlying function.
#' #'
#' #' @export
#' build_ftcmconfr_models <- function(triangular_adj_matrices, samples, aggregation_fun = c("mean", "median"), include_zeroes = FALSE,  nodes, show_progress) {
#'   n_nodes <- length(nodes)
#'   n_maps <- length(triangular_adj_matrices)
#'
#'   lower_adj_matrices <- lapply(triangular_adj_matrices, function(triangular_adj_matrix) apply(triangular_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "triangular_number", x[[1]]$lower, x[[1]])))
#'   upper_adj_matrices <- lapply(triangular_adj_matrices, function(triangular_adj_matrix) apply(triangular_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "triangular_number", x[[1]]$upper, x[[1]])))
#'
#'   lower_adj_matrices_as_arrays <- array(unlist(lower_adj_matrices), c(n_nodes, n_nodes, n_maps))
#'   upper_adj_matrices_as_arrays <- array(unlist(upper_adj_matrices), c(n_nodes, n_nodes, n_maps))
#'
#'   if (!include_zeroes) {
#'     triangular_adj_matrices_with_distributions <- lapply(
#'       triangular_adj_matrices,
#'       function(triangular_adj_matrix) {
#'         apply(triangular_adj_matrix, c(1, 2),
#'               function(x) {
#'                 #print(class(x[[1]]))
#'                 ifelse(class(x[[1]]) == "triangular_number",
#'                        yes = list(get_triangular_distribution_of_values(lower = x[[1]]$lower, mode = x[[1]]$mode, upper = x[[1]]$upper, n = samples)),
#'                        no = NA)
#'               })
#'         })
#'   } else {
#'     triangular_adj_matrices_with_distributions <- lapply(
#'       triangular_adj_matrices,
#'       function(triangular_adj_matrix) {
#'         apply(triangular_adj_matrix, c(1, 2),
#'               function(x) {
#'                 ifelse(class(x[[1]]) == "triangular_number",
#'                        yes = list(runif(samples, x[[1]]$lower, x[[1]]$upper)),
#'                        no = list(rep(0, samples)))
#'               })})
#'   }
#'   triangular_adj_matrices_distributions_by_index <- do.call(cbind, lapply(triangular_adj_matrices_with_distributions, function(triangular_adj_matrix_with_distributions) do.call(list, triangular_adj_matrix_with_distributions)))
#'
#'   if (aggregation_fun == "mean") {
#'     combined_triangular_adj_matrices_distributions_by_index <- apply(
#'       triangular_adj_matrices_distributions_by_index, 1,
#'       function(distributions) {
#'         sum_of_distributions <- rep(0, samples)
#'         n_nonzero_distributions <- 0
#'         if (all(lapply(distributions, typeof) == "list")) {
#'           distributions <- lapply(distributions, unlist)
#'         }
#'         for (i in 1:n_maps) {
#'           if (!identical(unique(distributions[[i]]), NA)) {
#'             n_nonzero_distributions <- n_nonzero_distributions + 1
#'             sum_of_distributions <- sum_of_distributions + distributions[[i]]
#'           }
#'         }
#'         sum_of_distributions/n_nonzero_distributions
#'       }
#'     )
#'     combined_triangular_adj_matrices_distributions_by_index <- apply(combined_triangular_adj_matrices_distributions_by_index, c(1, 2), function(x) ifelse(is.na(x), 0, x))
#'   } else if (aggregation_fun == "median") {
#'     combined_triangular_adj_matrices_distributions_by_index <- do.call(cbind, apply(
#'       triangular_adj_matrices_distributions_by_index, 1,
#'       function(distributions) {
#'         if (all(lapply(distributions, typeof) == "list")) {
#'           distributions <- lapply(distributions, unlist)
#'         }
#'         combined_distributions <- do.call(cbind, lapply(distributions, unlist))
#'         apply(combined_distributions, 1, stats::median, na.rm = TRUE)
#'       }, simplify = FALSE
#'     ))
#'     combined_triangular_adj_matrices_distributions_by_index <- apply(combined_triangular_adj_matrices_distributions_by_index, c(1, 2), function(x) ifelse(is.na(x), 0, x))
#'   }
#'
#'   sampled_adj_matrices <- apply(combined_triangular_adj_matrices_distributions_by_index, 1, function(row) data.frame(array(row, c(n_nodes, n_nodes))), simplify = FALSE)
#'   sampled_adj_matrices <- lapply(sampled_adj_matrices,
#'                                  function(sampled_adj_matrix) {
#'                                    colnames(sampled_adj_matrix) <- nodes
#'                                    rownames(sampled_adj_matrix) <- nodes
#'                                    sampled_adj_matrix
#'                                  })
#'
#'   sampled_adj_matrices
#'
#'   # test <- data.frame(do.call(rbind, lapply(sampled_adj_matrices, unlist)))
#'   # test <- test[, colSums(test) != 0]
#' }


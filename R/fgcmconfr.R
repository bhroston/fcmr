
#' fgcmconfr
#'
#' @description
#' [ADD DETAILS HERE!!!!]
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fmcm-class") for more information.
#'
#' @param fgcm_adj_matrices A list of n x n adjacencey matrices representing fcms
#' @param sampling The sampling method to be applied. Must be one of the following: "nonparametric", "uniform", or "triangular"
#' @param samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#' @param include_simulations_in_output TRUE/FALSE whether to include simulations of monte-carlo-generated
#' FCM. Will dramatically increase size of output if TRUE.
#'
#' @export
fgcmconfr <- function(fgcm_adj_matrices = list(matrix()),
                     sampling = "nonparametric", # Options: 'nonparametric', 'uniform', 'triangular'
                     samples = 1000,
                     initial_state_vector = c(),
                     clamping_vector = c(),
                     activation = "kosko",
                     squashing = "sigmoid",
                     lambda = 1,
                     max_iter = 100,
                     min_error = 1e-5,
                     show_progress = TRUE,
                     parallel = TRUE,
                     n_cores = integer(),
                     IDs = c(),
                     include_simulations_in_output = FALSE) {

  concepts_in_fgcms <- lapply(fgcm_adj_matrices, function(x) get_node_IDs_from_input(x, IDs))
  all_fgcms_have_same_concepts <- length(unique(concepts_in_fgcms)) == 1
  if (!all_fcms_have_same_concepts) {
    stop("All grey adjacency matrices must have the same concepts.")
  }

  dimensions_of_input_grey_adj_matrices <- lapply(fgcm_adj_matrices, dim)
  all_fgcms_have_same_dimensions <- length(unique(dimensions_of_input_grey_adj_matrices)) == 1
  if (!all_fgcms_have_same_dimensions) {
    stop("All grey adjacency matrices must have the same dimensions (n x n) throughout the entire list")
  }

  # Check that adj_matrices are correct format
  lapply(fgcm_adj_matrices, function(x) fgcm(x, IDs))

  nodes <- unlist(unique(concepts_in_fgcms))
  sampled_grey_adj_matrices <- build_fgcmconfr_models(fgcm_adj_matrices, sampling, samples, nodes, parallel, show_progress)

  fmcm_results <- infer_fmcm(
    simulated_adj_matrices = sampled_adj_matrices,
    initial_state_vector = initial_state_vector,
    clamping_vector = clamping_vector,
    activation = activation,
    squashing = squashing,
    lambda = lambda,
    max_iter = max_iter,
    min_error = min_error
  )

  fmcm_results
}


#' build_fgcmconfr_models
#'
#' @description
#' This function generates n fgcm models whose edge weights are sampled from either
#' the defined edge values in a set of adjacency matrices or continuous (uniform or triangular)
#' parametric distributions derived from the sets of edge values, and stores them
#' as a list of adjacency matrices.
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fgcm-class") for more information.
#'
#' @param grey_adj_matrices A list of n x n adjacencey matrices representing fcms
#' @param sampling The sampling method to be applied. Must be one of the following: "nonparametric", "uniform", or "triangular"
#' @param samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param nodes A vector of node names (IDs) present in every adjacency matrix
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @export
build_fgcmconfr_models <- function(grey_adj_matrices, sampling, aggregation_fun = "mean", samples, nodes, parallel, show_progress) {
  if (!(aggregation_fun %in% c("mean", "median"))) {
    stop("Input aggregation_fun must be one of the followin: 'mean' or 'median'.")
  }
  if (sampling == "nonparametric" & aggregation_fun == "median") {
    warning("Nonparametric sampling can only be performed by taking the mean as the
            expected value of the grey number edge weights. Although input
            aggregation_fun = 'median', ignoring and setting to 'mean'.")
  }

  n_nodes <- length(nodes)
  n_maps <- length(grey_adj_matrices)
  grey_adj_matrices_as_arrays <- array(unlist(grey_adj_matrices), c(n_nodes, n_nodes, n_maps))

  lower_adj_matrices <- lapply(grey_adj_matrices, function(grey_adj_matrix) apply(grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$lower, x[[1]])))
  upper_adj_matrices <- lapply(grey_adj_matrices, function(grey_adj_matrix) apply(grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$upper, x[[1]])))

  lower_adj_matrices_as_arrays <- array(unlist(lower_adj_matrices), c(n_nodes, n_nodes, n_maps))
  upper_adj_matrices_as_arrays <- array(unlist(upper_adj_matrices), c(n_nodes, n_nodes, n_maps))

  if (sampling == "nonparametric") {
    lower_adj_matrix_of_samples_per_edge <- apply(lower_adj_matrices_as_arrays, c(1, 2), function(x) sample(x, samples, replace = TRUE), simplify = FALSE)
    upper_adj_matrix_of_samples_per_edge <- apply(lower_adj_matrices_as_arrays, c(1, 2), function(x) sample(x, samples, replace = TRUE), simplify = FALSE)

    wider_lower_adj_matrix_of_samples_per_edge <- apply(array(lower_adj_matrix_of_samples_per_edge, c(1, n_nodes^2)), 2, unlist)
    wider_upper_adj_matrix_of_samples_per_edge <- apply(array(upper_adj_matrix_of_samples_per_edge, c(1, n_nodes^2)), 2, unlist)

    lower_sampled_adj_matrices <- apply(wider_lower_adj_matrix_of_samples_per_edge, 1, function(x) array(x, c(n_nodes, n_nodes)), simplify = FALSE)
    upper_sampled_adj_matrices <- apply(wider_lower_adj_matrix_of_samples_per_edge, 1, function(x) array(x, c(n_nodes, n_nodes)), simplify = FALSE)

    expected_value_sampled_adj_matrices <- mapply(
      function(lower_adj_matrix, upper_adj_matrix) {
        (lower_adj_matrix + upper_adj_matrix)/2
      },
      lower_adj_matrix = lower_sampled_adj_matrices,
      upper_adj_matrix = upper_sampled_adj_matrices,
      SIMPLIFY = FALSE
    )

    sampled_adj_matrices <- lapply(expected_value_sampled_adj_matrices, function(x) {
      x <- as.data.frame(x)
      colnames(x) <- nodes
      rownames(x) <- nodes
      x
    })
  } else if (sampling == "uniform") {
    sampled_grey_adj_matrices <- lapply(
      grey_adj_matrices,
      function(grey_adj_matrix) {
        build_fmcm_models_from_grey_adj_matrix(
          grey_adj_matrix,
          n_sims = samples,
          distribution = "uniform",
          parallel = parallel,
          show_progress = show_progress
        )
      }
    )
  } else if (sampling == "uniform" & aggregation_fun == "mean") {
    median_lower_adj_matrix <- apply(lower_adj_matrices_as_arrays, c(1, 2), stats::mean)
    median_upper_adj_matrix <- apply(upper_adj_matrices_as_arrays, c(1, 2), stats::mean)
    empirical_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(median_lower_adj_matrix, median_upper_adj_matrix)
    sampled_adj_matrices <- build_fmcm_models_from_grey_adj_matrix(
      empirical_grey_adj_matrix,
      n_sims = samples,
      parallel = parallel,
      distribution = "uniform",
      show_progress = show_progress,
      IDs = IDs
    )
  } else if (sampling == "uniform" & aggregation_fun == "median") {
    median_lower_adj_matrix <- apply(lower_adj_matrices_as_arrays, c(1, 2), stats::median)
    median_upper_adj_matrix <- apply(upper_adj_matrices_as_arrays, c(1, 2), stats::median)
    empirical_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(median_lower_adj_matrix, median_upper_adj_matrix)
    sampled_adj_matrices <- build_fmcm_models_from_grey_adj_matrix(
      empirical_grey_adj_matrix,
      n_sims = samples,
      parallel = parallel,
      distribution = "uniform",
      show_progress = show_progress,
      IDs = IDs
    )
  } else if (sampling == "triangular") {
    min_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), min)
    max_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), max)
    empirical_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(min_adj_matrix, max_adj_matrix)
    mode_adj_matrix <- apply(adj_matrices_as_arrays, c(1, 2), mean)
    sampled_adj_matrices <- build_fmcm_models_from_grey_adj_matrix(
      empirical_grey_adj_matrix,
      mode_adj_matrix,
      n_sims = samples,
      parallel = parallel,
      distribution = "triangular",
      show_progress = show_progress,
      IDs = IDs
    )
  }

  sampled_adj_matrices
}



# Given a set of FCM, let w_i be the set of weight values of a given edge
# w = {w_i1, w_i2, ..., w_ij}.
# The E{w_i1, w_i2, ..., w_ij} = product of the expected values of each w_ij in w
# where E[w_ij] = (max(w_ij) + min(w_ij))/2
# The var{w_i1, w_i2, ... w_ij} = product of the (variance(w_ij) + (expected value(w_ij))^2) - the product of (expected value(w_ij))^2
# where var[w_ij] = (1/12)(max(w_ij) - min(w_ij))^2

# else if (sampling == "parametric") {
#   adj_matrices_as_arrays <- array(unlist(adj_matrices), c(n_nodes, n_nodes, n_maps))
#   mean_adj_matrix <- apply(adj_matrices_as_arrays, 1:2, function(x) mean(x))
#   variance_adj_matrix <- apply(adj_matrices_as_arrays, 1:2, function(x) var(x))
#   sampled_adj_matrices <- apply(
#     mapply(
#       function(expected_value, variance) {
#         rnorm(samples, mean = expected_value, sd = sqrt(variance))
#       },
#       expected_value = array(mean_adj_matrix),
#       variance = array(variance_adj_matrix)
#     ),
#     1, function(x) array(x, c(n_nodes, n_nodes)), simplify = FALSE
#   )
# }


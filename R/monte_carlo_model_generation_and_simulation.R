
################################################################################
# monte_carlo_model_generation_and_simulation.R
#
# These functions assist in generating empirical FCMs via monte carlo methods
# and simulating the generated FCMs in bulk.
#
#   - get_mc_simulations_inference_CIs_w_bootstrap
#   - build_monte_carlo_fcms
#   - build_monte_carlo_fcms_from_conventional_adj_matrices
#   - build_monte_carlo_fcms_from_fuzzy_set_adj_matrices
#   - check_monte_carlo_bootstrap_inputs
#
################################################################################

#' Build Monte Carlo FCMs
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @description
#' This function generates N fcm adjacency matrices whose edge weights are sampled
#' from edge values (that may be numeric, IVFNs, or TFNs) and
#' stores them as a list of adjacency matrices.
#'
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
#'
#' @details
#' For Conventional FCMs, edge weights are sampled the edge weight explicitly
#' defined in the input FCMs.
#'
#' For IVFN and TFN FCMs, edge weights are sampled from the combined
#' distributions representative of the IVFN/TFN edge weights. For example,
#' if an edge is given the following weights across two maps: IVFN(0.4, 0.8) and
#' IVFN[0.5, 0.7], the samples will be drawn from the combined distribution:
#' sample(N, c(runif(N, 0.4, 0.8), runif(N, 0.5, 0.7)), replace = TRUE).
#'
#' @param adj_matrix_list A list of n x n adjacency matrices representing fcms
#' @param N_samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param skip_checks FOR DEVELOPER USE ONLY. TRUE if infer_fcm is called within
#' fcmconfr() and checks have already been performed
#'
#' @returns A list of empirical (Conventional) FCM adj. matrices generated via monte carlo methods
#'
#' @export
#' @example man/examples/ex-build_monte_carlo_fcms.R
build_monte_carlo_fcms <- function(adj_matrix_list = list(matrix()),
                                   N_samples = 1000,
                                   include_zeroes = TRUE,
                                   show_progress = TRUE,
                                   skip_checks = FALSE) {

  if (!is.logical(skip_checks)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var skip_checks} must be a logical value (TRUE/FALSE)",
      "+++++> Input {.var skip_checks} was: {skip_checks}"
    )))
  }
  if (!skip_checks) {
    checks <- check_build_monte_carlo_fcms_inputs(adj_matrix_list, N_samples, include_zeroes, show_progress)
    adj_matrix_list_class <- checks$adj_matrix_list_class
    show_progress <- checks$show_progress
  } else {
    adj_matrix_list_class <- get_adj_matrices_input_type(adj_matrix_list)$object_types_in_list[1]
  }

  if (adj_matrix_list_class == "conventional") {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_conventional_adj_matrices(adj_matrix_list, N_samples, include_zeroes, show_progress)
  } else {
    sampled_adj_matrices <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrix_list, adj_matrix_list_class, N_samples, include_zeroes, show_progress)
  }

  sampled_adj_matrices
}



#' Build Monte Carlo (Conventional) FCMs
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @description
#' This function generates n fcm models whose edge weights are sampled from either
#' the defined edge values in a set of adjacency matrices derived from the sets
#' of edge values, and stores them as a list of adjacency matrices.
#'
#' @details
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
#'
#' @param adj_matrix_list A list of n x n adjacency matrices representing fcms
#' @param N_samples The number of samples to draw with the selected sampling method. Also,
#' the number of sampled models to generate
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns A list of empirical (Conventional) FCM adj. matrices generated via monte carlo methods
#'
#' @export
#' @example man/examples/ex-build_mc_models_from_conventional_adj_matrices.R
build_monte_carlo_fcms_from_conventional_adj_matrices <- function(adj_matrix_list = list(Matrix::sparseMatrix()),
                                                                  N_samples = integer(),
                                                                  include_zeroes = TRUE,
                                                                  show_progress = TRUE) {

  n_nodes <- unique(unlist(lapply(adj_matrix_list, dim)))
  flatten_conventional_adj_matrix <- function(adj_matrix) {
    if (identical(methods::is(as.vector(adj_matrix)), methods::is(list()))) {
      flattened_adj_matrix <- do.call(c, as.vector(adj_matrix))
    } else {
      flattened_adj_matrix <- as.vector(adj_matrix)
    }

    names(flattened_adj_matrix) <- seq_along(flattened_adj_matrix)
    flattened_adj_matrix
  }
  flattened_adj_matrices <- do.call(rbind, lapply(adj_matrix_list, flatten_conventional_adj_matrix))
  if (!include_zeroes) {
    flattened_adj_matrices[flattened_adj_matrices == 0] <- NA
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(flattened_adj_matrices, 2, function(column_vec) {

      na_omit_column_vec <- stats::na.omit(column_vec)
      if (length(na_omit_column_vec) != 0) {
        sample(na_omit_column_vec, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    cat(print("Constructing monte carlo fcms from samples", quote = FALSE))
    sampled_adj_matrices <- pbapply::pbapply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  } else {
    column_samples <- apply(flattened_adj_matrices, 2, function(column_vec) {
      na_omit_column_vec <- stats::na.omit(column_vec)
      if (length(na_omit_column_vec) != 0) {
        sample(na_omit_column_vec, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    sampled_adj_matrices <- apply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  }

  sampled_adj_matrices
}




#' Build Monte Carlo (IVFN or TFN) FCMs
#'
#' @family monte-carlo-model-generation-and-simulation
#'
#' @description
#' This function generates n fcm adjacency matrices whose edge weights are sampled
#' from edge values (that may be Conventional, IVFNs, or TFNs) and
#' stores them as a list of adjacency matrices.
#'
#' @details
#' If an edge is represented by IVFNs/TFNs, then those distributions
#' are averaged together to create the aggregate distribution to sample from.
#'
#' The show_progress and parallel inputs change the functions called, but do NOT
#' change the output! These are allowed to be toggled on/off to increase user
#' control at runtime.
#'
#' @param fuzzy_set_adj_matrix_list A list of n x n fuzzy adjacency matrices representing fcms
#' @param fuzzy_set_adj_matrix_list_class "fgcm" or "fcm_w_tfn" - the class of elements in the fuzzy_set_adj_matrix_list
#' @param N_samples The number of samples to draw from the corresponding distribution
#' @param include_zeroes TRUE/FALSE Whether to incorporate zeroes as intentionally-defined
#' edge weights or ignore them in aggregation
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#'
#' @returns A list of empirical (Conventional) FCM adj. matrices generated via monte carlo methods
#'
#' @export
#' @example man/examples/ex-build_mc_models_from_fuzzy_set_adj_matrices.R
build_monte_carlo_fcms_from_fuzzy_set_adj_matrices <- function(fuzzy_set_adj_matrix_list = list(data.frame()),
                                                               fuzzy_set_adj_matrix_list_class = c("conventional", "ivfn", "tfn"),
                                                               N_samples = integer(),
                                                               include_zeroes = FALSE,
                                                               show_progress = TRUE) {

  if (!(fuzzy_set_adj_matrix_list_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input fuzzy_set_adj_matrix_list_class must be one of the following: 'conventional', 'ivfn', or 'tfn'")
  }

  n_nodes <- unique(unlist(lapply(fuzzy_set_adj_matrix_list, dim)))

  flatten_fuzzy_adj_matrix <- function(fuzzy_adj_matrix) do.call(cbind, lapply(as.vector(fuzzy_adj_matrix), rbind))
  flattened_fuzzy_set_adj_matrix_list <- do.call(rbind, lapply(fuzzy_set_adj_matrix_list, flatten_fuzzy_adj_matrix))
  flattened_fuzzy_set_adj_matrix_list_w_distributions <- convert_fuzzy_set_elements_in_matrix_to_distributions(fuzzy_set_matrix = flattened_fuzzy_set_adj_matrix_list, object_class = fuzzy_set_adj_matrix_list_class, N_samples = N_samples)

  if (!include_zeroes) {
    flattened_fuzzy_set_adj_matrix_list_w_distributions <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, c(1, 2), function(element) ifelse(element[[1]][[1]] == 0, NA, element[[1]][[1]]), simplify = FALSE)
  } else {
    flattened_fuzzy_set_adj_matrix_list_w_distributions <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, c(1, 2), function(element) element[[1]][[1]], simplify = FALSE)
  }

  if (show_progress) {
    cat(print("Sampling from column vectors", quote = FALSE))
    column_samples <- pbapply::pbapply(
      flattened_fuzzy_set_adj_matrix_list_w_distributions, 2,
      function(column_vec) {
        # sample_list_of_vectors_ignoring_NAs
        na_omit_column_vec <- stats::na.omit(do.call(c, column_vec))
        if (length(na_omit_column_vec) != 0) {
          column_vecs_w_NAs <- lapply(
            column_vec, function(value) value
          )
          column_vecs_w_NAs <- stats::na.omit(do.call(c, column_vecs_w_NAs))
        sample(column_vecs_w_NAs, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    cat(print("Constructing monte carlo fcms from samples", quote = FALSE))
    # browser()
    sampled_adj_matrices <- pbapply::pbapply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  } else {
    column_samples <- apply(flattened_fuzzy_set_adj_matrix_list_w_distributions, 2, function(column_vec) {
      # sample_list_of_vectors_ignoring_NAs
      na_omit_column_vec <- stats::na.omit(do.call(c, column_vec))
      if (length(na_omit_column_vec) != 0) {
        na_omit_column_vec <- stats::na.omit(do.call(c, column_vec))
        if (length(na_omit_column_vec) != 0) {
          column_vecs_w_NAs <- lapply(
            column_vec, function(value) value
          )
          column_vecs_w_NAs <- stats::na.omit(do.call(c, column_vecs_w_NAs))
          sample(column_vecs_w_NAs, N_samples, replace = TRUE)
        }
        # column_vec_with_numerics_replicated <- lapply(
        #   column_vec,
        #   function(value) {
        #     if (is.numeric(value) & length(value) == 1) {
        #       rep(value, N_samples)
        #     } else {
        #       value
        #     }
        #   })
        # na_omit_column_vec <- stats::na.omit(do.call(c, column_vec_with_numerics_replicated))
        # sample(na_omit_column_vec, N_samples, replace = TRUE)
      } else {
        rep(0, N_samples)
      }
    })
    sampled_adj_matrices <- apply(column_samples, 1, function(row_vec) matrix(row_vec, nrow = n_nodes, ncol = n_nodes), simplify = FALSE)
  }

  sampled_adj_matrices
}


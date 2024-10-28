
################################################################################
# infer_and_simulate_fcm.R
#
# These functions help with fcm inference estimation. Note that some are
# intended for developer use only.
#
#   - infer_fcm
#   - infer_conventional_fcm
#   - infer_ivfn_or_tfn_fcm
#   - equalize_baseline_and_scenario_outputs
#   - simulate_fcm
#   - simulate_conventional_fcm
#   - simulate_ivfn_or_tfn_fcm
#   - calculate_next_conventional_fcm_state_vector
#   - calculate_next_fuzzy_set_fcm_state_vector
#   - squash
#   - defuzz
#   - convert_element_to_ivfn_or_tfn_if_numeric
#   - convert_fuzzy_set_elements_in_matrix_to_distributions
#   - clean_simulation_output
#   - check_simulation_inputs
#   - print.infer_conventional_fcm
#   - print.infer_ivfn_or_tfn_fcm
#
################################################################################

#' Infer FCM
#'
#' @description
#' This compares the baseline simulation of an fcm with the input scenario (scenario vector)
#' to estimate how outputs change compared to the structural or expected behavior
#' of the system.
#'
#' @details
#' This function performs two fcm simulations and compares the output between the two.
#' The first simulation considers the baseline activity where no nodes are "clamped" and the
#' system behaves without any outside inputs. The second simulation considers a scenario where
#' one or multiple nodes are "clamped" so that the system is reactive to additional inputs.
#' The function returns the difference in simulation results between the scenario and baseline
#' activity to understand how system manipulations compare to structural expectations of the system.
#'
#' This function produces the same output as mental modeler for the following inputs:
#'  - initial_state_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param fuzzy_set_samples The number of samples to represent ivfn or tfn as. Use to create finer scale results.s
#'
#' @returns A list of fcm inference results (including baseline and simulation outputs)
#'
#' @export
#' @examples man/examples/examples-infer_and_simulate_fcm/infer_fcm.R
infer_fcm <- function(adj_matrix = matrix(),
                      initial_state_vector = c(),
                      clamping_vector = c(),
                      activation = "kosko",
                      squashing = "tanh",
                      lambda = 1,
                      max_iter = 100,
                      min_error = 1e-5,
                      fuzzy_set_samples = 1000) {

  check_simulation_inputs(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)

  fcm_class <- get_adj_matrices_input_type(adj_matrix)$object_types_in_list[1]
  if (!(fcm_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input adj_matrix must be an adjacency matrix with edges represented as
         numeric values, ivfns, or tfns")
  }

  if (fcm_class == "conventional") {
    inference <- infer_conventional_fcm(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  } else if (fcm_class %in% c("ivfn", "tfn")) {
    inference <- infer_ivfn_or_tfn_fcm(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error, fuzzy_set_samples)
  }

  inference
}


#' Infer (Conventional) FCM
#'
#' @description
#' This compares the baseline simulation of an fcm with the input scenario (scenario vector)
#' to estimate how outputs change compared to the structural or expected behavior
#' of the system.
#'
#' @details
#' This function performs two fcm simulations and compares the output between the two.
#' The first simulation considers the baseline activity where no nodes are "clamped" and the
#' system behaves without any outside inputs. The second simulation considers a scenario where
#' one or multiple nodes are "clamped" so that the system is reactive to additional inputs.
#' The function returns the difference in simulation results between the scenario and baseline
#' activity to understand how system manipulations compare to structural expectations of the system.
#'
#' This function produces the same output as mental modeler for the following inputs:
#'  - initial_state_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#'
#' @returns A list of (conventional) fcm inference results (including baseline
#' and simulation outputs)
#'
#' @export
#' @examples man/examples/examples-infer_and_simulate_fcm/examples-infer_and_simulate_fcm-infer_conventional_fcm.R
infer_conventional_fcm <- function(adj_matrix = matrix(),
                                   initial_state_vector = c(),
                                   clamping_vector = c(),
                                   activation = "kosko",
                                   squashing = "tanh",
                                   lambda = 1,
                                   max_iter = 100,
                                   min_error = 1e-5) {

  iter <- NULL # for R CMD Check, does not impact logic

  check_simulation_inputs(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  fcm_class <- get_adj_matrices_input_type(adj_matrix)$object_types_in_list[1]
  if (!(fcm_class %in% c("conventional"))) {
    stop("Input adj_matrix must be an adjacency matrix with edges represented as
         numeric values only")
  }

  # Get baseline simulation
  baseline_initial_state_vector <- rep(1, length(initial_state_vector))
  baseline_clamping_vector <- rep(0, length(clamping_vector))
  baseline_simulation <- simulate_fcm(adj_matrix, baseline_initial_state_vector, baseline_clamping_vector, activation, squashing, lambda, max_iter, min_error)

  # Get scenario simulation
  scenario_initial_state_vector <- initial_state_vector
  scenario_clamping_vector <- clamping_vector
  scenario_simulation <- simulate_fcm(adj_matrix, scenario_initial_state_vector, scenario_clamping_vector, activation, squashing, lambda, max_iter, min_error)

  equalized_state_vector_dfs <- equalize_baseline_and_scenario_outputs(baseline_simulation$state_vectors, scenario_simulation$state_vectors)
  baseline_simulation$state_vectors <- equalized_state_vector_dfs$baseline
  scenario_simulation$state_vectors <- equalized_state_vector_dfs$scenario

  inference_state_vectors <- data.frame(scenario_simulation$state_vectors - baseline_simulation$state_vectors)
  inference_state_vectors$iter <- 0:(nrow(inference_state_vectors) - 1)
  rownames(inference_state_vectors) <- 0:(nrow(inference_state_vectors) - 1)

  inference_values <- inference_state_vectors[nrow(inference_state_vectors),]
  inference_values <- subset(inference_values, select = -c(iter))
  rownames(inference_values) <- 1


  inference_plot_data <- data.frame(
    node = colnames(inference_values),
    value = unlist(inference_values)
  )

  structure(
    .Data = list(
      inference = inference_values,
      inference_for_plotting = inference_plot_data,
      inference_state_vectors = inference_state_vectors,
      scenario_simulation = scenario_simulation,
      baseline_simulation = baseline_simulation
    ),
    class = "infer_conventional_fcm"
  )
}


#' Infer (IVFN or TFN) FCM
#'
#' @description
#' This compares the baseline simulation of an fcm with the input scenario (scenario vector)
#' to estimate how outputs change compared to the structural or expected behavior
#' of the system.
#'
#' @details
#' This function performs two fcm simulations and compares the output between the two.
#' The first simulation considers the baseline activity where no nodes are "clamped" and the
#' system behaves without any outside inputs. The second simulation considers a scenario where
#' one or multiple nodes are "clamped" so that the system is reactive to additional inputs.
#' The function returns the difference in simulation results between the scenario and baseline
#' activity to understand how system manipulations compare to structural expectations of the system.
#'
#' This function produces the same output as mental modeler for the following inputs:
#'  - initial_state_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param fuzzy_set_samples The number of samples to represent ivfn or tfn as. Use to create finer scale results.s
#'
#' @returns A list of (ivfn or tfn) fcm inference results (including baseline
#' and simulation outputs)
#'
#' @export
#' @examples man/examples/examples-infer_and_simulate_fcm/examples-infer_and_simulate_fcm-infer_ivfn_or_tfn_fcm.R
infer_ivfn_or_tfn_fcm <- function(adj_matrix = matrix(),
                                  initial_state_vector = c(),
                                  clamping_vector = c(),
                                  activation = "kosko",
                                  squashing = "tanh",
                                  lambda = 1,
                                  max_iter = 100,
                                  min_error = 1e-5,
                                  fuzzy_set_samples = 1000) {
  # Perform checks
  checks <- check_simulation_inputs(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  fcm_class <- get_adj_matrices_input_type(adj_matrix)$object_types_in_list[1]
  if (!(fcm_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input adj_matrix must be an adjacency matrix with edges represented as
         numeric values, ivfns, or tfns")
  }
  concepts <- colnames(adj_matrix)

  # Get baseline simulation
  baseline_initial_state_vector <- initial_state_vector
  baseline_clamping_vector <- rep(0, length(clamping_vector))
  baseline_simulation <- simulate_fcm(adj_matrix, baseline_initial_state_vector, baseline_clamping_vector, activation, squashing, lambda, max_iter, min_error)

  # Get scenario simulation
  scenario_initial_state_vector <- initial_state_vector
  scenario_clamping_vector <- clamping_vector
  scenario_simulation <- simulate_fcm(adj_matrix, scenario_initial_state_vector, scenario_clamping_vector, activation, squashing, lambda, max_iter, min_error)

  equalized_fuzzy_set_state_vector_dfs <- equalize_baseline_and_scenario_outputs(baseline_simulation$state_vectors, scenario_simulation$state_vectors)
  baseline_simulation$state_vectors <- equalized_fuzzy_set_state_vector_dfs$baseline
  scenario_simulation$state_vectors <- equalized_fuzzy_set_state_vector_dfs$scenario

  equalized_crisp_state_vector_dfs <- equalize_baseline_and_scenario_outputs(baseline_simulation$crisp_state_vectors, scenario_simulation$crisp_state_vectors)
  baseline_simulation$crisp_state_vectors <- equalized_crisp_state_vector_dfs$baseline
  scenario_simulation$crisp_state_vectors <- equalized_crisp_state_vector_dfs$scenario

  baseline_state_vectors_as_distributions <- convert_fuzzy_set_elements_in_matrix_to_distributions(baseline_simulation$state_vectors, fcm_class, fuzzy_set_samples)
  scenario_state_vectors_as_distributions <- convert_fuzzy_set_elements_in_matrix_to_distributions(scenario_simulation$state_vectors, fcm_class, fuzzy_set_samples)


  inference_state_vectors_as_distributions <- baseline_state_vectors_as_distributions
  for (i in 1:nrow(baseline_state_vectors_as_distributions)) {
    for (j in 1:ncol(baseline_state_vectors_as_distributions)) {
      inference_state_vectors_as_distributions[i, j] <- list(unlist(scenario_state_vectors_as_distributions[i, j]) - unlist(baseline_state_vectors_as_distributions[i, j]))
    }
  }

  crisp_inference_state_vectors <- data.frame(
    apply(inference_state_vectors_as_distributions, c(1, 2), function(element) mean(element[[1]]), simplify = TRUE)
  )

  if (fcm_class == "ivfn") {
    inference_state_vectors <- data.frame(
      apply(
        inference_state_vectors_as_distributions, c(1, 2),
        function(element) {
          ivfn(min(element[[1]]), max(element[[1]]))
        }
      ))
  } else if (fcm_class == "tfn") {
    inference_state_vectors <- data.frame(
      apply(
        inference_state_vectors_as_distributions, c(1, 2),
        function(element) {
          tfn(min(element[[1]]), mean(element[[1]]),  max(element[[1]]))
        }
      ))
  }

  inference_state_vectors <- clean_simulation_output(inference_state_vectors, concepts)
  crisp_inference_state_vectors <- clean_simulation_output(crisp_inference_state_vectors, concepts)

  final_inference_state_vectors <- inference_state_vectors[nrow(inference_state_vectors),][, -1]
  final_inference_crisp_state_vectors <- crisp_inference_state_vectors[nrow(crisp_inference_state_vectors),][, -1]

  if (fcm_class == "ivfn") {
    final_inference_df <- data.frame(
      concepts = concepts,
      crisp = t(final_inference_crisp_state_vectors),
      lower = vapply(final_inference_state_vectors, function(x) x[[1]]$lower, numeric(1)),
      upper = vapply(final_inference_state_vectors, function(x) x[[1]]$upper, numeric(1))
    )
    colnames(final_inference_df) <- c("concept", "crisp", "lower", "upper")
    rownames(final_inference_df) <- NULL
  } else if (fcm_class == "tfn") {
    final_inference_df <- data.frame(
      concepts = concepts,
      crisp = t(final_inference_crisp_state_vectors),
      lower = vapply(final_inference_state_vectors, function(x) x[[1]]$lower, numeric(1)),
      mode = vapply(final_inference_state_vectors, function(x) x[[1]]$mode, numeric(1)),
      upper = vapply(final_inference_state_vectors, function(x) x[[1]]$upper, numeric(1))
    )
    colnames(final_inference_df) <- c("concept", "crisp", "lower", "mode", "upper")
    rownames(final_inference_df) <- NULL
  }

  final_inference_plot_data <- tidyr::pivot_longer(final_inference_df, cols = 2:ncol(final_inference_df))

  structure(
    .Data = list(
      inference = final_inference_df,
      inference_for_plotting = final_inference_plot_data,
      inference_state_vectors = inference_state_vectors,
      crisp_inference_state_vectors = crisp_inference_state_vectors,
      scenario_simulation = scenario_simulation,
      baseline_simulation = baseline_simulation
    ),
    class = "infer_ivfn_or_tfn_fcm"
  )
}



#' [INTENDED FOR DEVELOPER USE ONLY] Equalize Baseline Simulation and Scenario Simulation DataFrames
#'
#' @description
#' Ensures that the data frames for the baseline and scenario state vectors have
#' the same number of rows. If they do not, extends the final row of the data frame
#' with the lower number of rows (simulation iterations) until both data frames are
#' the same size.
#'
#' @details
#' INTENDED FOR DEVELOPER USE ONLY
#'
#' @param baseline_state_vectors State vectors (normal or crisp) from a
#' simulate_fcm object for a baseline simulation
#' @param scenario_state_vectors State vectors (normal or crisp) from a
#' simulate_fcm object for a scenario simulation
#'
#' @returns A named list of dataframes (baseline and scenario) that have the
#' same number of rows
#'
#' @export
#' @examples NULL
equalize_baseline_and_scenario_outputs <- function(baseline_state_vectors,
                                                   scenario_state_vectors) {
  n_iters_baseline <- nrow(baseline_state_vectors)
  n_iters_scenario <- nrow(scenario_state_vectors)

  if (n_iters_baseline == n_iters_scenario) {
    baseline_state_vectors <- baseline_state_vectors[, -1]
    scenario_state_vectors <- scenario_state_vectors[, -1]
  } else if (n_iters_baseline < n_iters_scenario) {
    extended_baseline_state_vectors <- apply(scenario_state_vectors, 2, function(iter) c(iter, rep(iter[n_iters_baseline], n_iters_scenario - n_iters_baseline)))
    baseline_state_vectors <- extended_baseline_state_vectors
    scenario_state_vectors <- scenario_state_vectors
  } else if (n_iters_scenario < n_iters_baseline) {
    extended_scenario_state_vectors <- apply(scenario_state_vectors, 2, function(iter) c(iter, rep(iter[n_iters_scenario], n_iters_baseline - n_iters_scenario)))
    baseline_state_vectors <- baseline_state_vectors
    scenario_state_vectors <- extended_scenario_state_vectors
  }

  # Here in case given inputs from an ivfn or tfn simulation
  if (identical(methods::is(baseline_state_vectors), c("list", "vector"))) {
    scenario_state_vectors <- data.frame(do.call(cbind, baseline_state_vectors))
  }
  if (identical(methods::is(scenario_state_vectors), c("list", "vector"))) {
    scenario_state_vectors <- data.frame(do.call(cbind, scenario_state_vectors))
  }

  list(
    baseline = baseline_state_vectors,
    scenario = scenario_state_vectors
  )
}



#' Simulate FCM
#'
#' @description
#' This simulates an fcm (conventional, ivfn, or tfn) based on its adjacency matrix.
#'
#' @details
#' This function performs two fcm simulations and compares the output between the two.
#' The first simulation considers the baseline activity where no nodes are "clamped" and the
#' system behaves without any outside inputs. The second simulation considers a scenario where
#' one or multiple nodes are "clamped" so that the system is reactive to additional inputs.
#' The function returns the difference in simulation results between the scenario and baseline
#' activity to understand how system manipulations compare to structural expectations of the system.
#'
#' This function produces the same output as mental modeler for the following inputs:
#'  - initial_state_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#'
#' @returns (Conventional, IVFN, or TFN) FCM simulation results
#'
#' @export
#' @examples man/examples/examples-infer_and_simulate_fcm/examples-infer_and_simulate_fcm-simulate_fcm.R
simulate_fcm <- function(adj_matrix = matrix(),
                         initial_state_vector = c(),
                         clamping_vector = c(),
                         activation = "kosko",
                         squashing = "tanh",
                         lambda = 1,
                         max_iter = 100,
                         min_error = 1e-5) {

  check_simulation_inputs(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)

  fcm_class <- get_adj_matrices_input_type(adj_matrix)$object_types_in_list[1]
  if (!(fcm_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input adj_matrix must be an adjacency matrix with edges represented as
         numeric values, ivfns, or tfns")
  }

  if (fcm_class == "conventional") {
    simulation <- simulate_conventional_fcm(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  } else if (fcm_class %in% c("ivfn", "tfn")) {
    simulation <- simulate_ivfn_or_tfn_fcm(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  }

  simulation
}



#' Simulate (Conventional) FCM
#'
#' @description
#' This simulates a conventional fcm based on its adjacency matrix.
#'
#' @details
#' This function performs two fcm simulations and compares the output between the two.
#' The first simulation considers the baseline activity where no nodes are "clamped" and the
#' system behaves without any outside inputs. The second simulation considers a scenario where
#' one or multiple nodes are "clamped" so that the system is reactive to additional inputs.
#' The function returns the difference in simulation results between the scenario and baseline
#' activity to understand how system manipulations compare to structural expectations of the system.
#'
#' This function produces the same output as mental modeler for the following inputs:
#'  - initial_state_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#'
#' @returns (Conventional) FCM simulation results
#'
#' @export
#' @examples man/examples/examples-infer_and_simulate_fcm/examples-infer_and_simulate_fcm-simulate_conventional_fcm.R
simulate_conventional_fcm <- function(adj_matrix = matrix(),
                                      initial_state_vector = c(),
                                      clamping_vector = c(),
                                      activation = "kosko",
                                      squashing = "tanh",
                                      lambda = 1,
                                      max_iter = 100,
                                      min_error = 1e-5) {

  concept_names <- colnames(adj_matrix)

  state_vectors <- data.frame(matrix(data = numeric(), nrow = max_iter + 1, ncol = length(initial_state_vector)))
  state_vectors[1, ] <- initial_state_vector
  errors <-  data.frame(matrix(data = numeric(), nrow = max_iter, ncol = length(initial_state_vector)))
  errors[1, ] <- 0

  for (i in 2:(max_iter + 1)) {
    state_vector <- state_vectors[i - 1, ]
    next_state_vector <- calculate_next_conventional_fcm_state_vector(adj_matrix, state_vector, activation)
    normalized_state_vector <- squash(next_state_vector, squashing = squashing, lambda = lambda)
    normalized_state_vector[clamping_vector != 0] <- clamping_vector[clamping_vector != 0]
    state_vectors[i, ] <- normalized_state_vector
    errors[i, ] <- abs(as.matrix(state_vectors[i - 1,]) - as.matrix(state_vectors[i, ]))
    total_error <- sum(errors[i, ])
    if (total_error < min_error) {
      state_vectors <- stats::na.omit(state_vectors)
      errors <- stats::na.omit(errors)
      break
    }
  }
  if (i >= max_iter) {
    warning(
      "\tThe simulation reached the maximum number of iterations before
        achieving the minimum allowable error. This may signal that
        the fcm has reached a limit-cycle or is endlessly chaotic.

        It is also possible that the fcm simply requires more iterations
        to converge within the input minimum error.

        Try increasing the max_iter or min_error inputs."
    )
  }

  state_vectors <- clean_simulation_output(state_vectors, concept_names)
  errors <- clean_simulation_output(errors, concept_names)

  structure(
    .Data = list(
      state_vectors = state_vectors,
      errors = errors,
      params = list(
        adj_matrix = adj_matrix,
        initial_state_vector = initial_state_vector,
        activation = activation,
        squashing = squashing,
        lambda = lambda,
        max_iter = max_iter,
        min_error = min_error,
        concepts = concept_names
      )
    ),
    class = "fcm_simulation"
  )
}



#' Simulate (IVFN or TFN) FCM
#'
#' @description
#' This simulates a (IVFN or TFN) fcm based on its adjacency matrix.
#'
#' @details
#' This function performs two fcm simulations and compares the output between the two.
#' The first simulation considers the baseline activity where no nodes are "clamped" and the
#' system behaves without any outside inputs. The second simulation considers a scenario where
#' one or multiple nodes are "clamped" so that the system is reactive to additional inputs.
#' The function returns the difference in simulation results between the scenario and baseline
#' activity to understand how system manipulations compare to structural expectations of the system.
#'
#' This function produces the same output as mental modeler for the following inputs:
#'  - initial_state_vector = c(1, 1, ..., 1)
#'  - activation = "kosko"
#'  - squashing = either "sigmoid" or "tanh"
#'  - lambda = 1
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#'
#' @returns (IVFN or TFN) FCM simulation results
#'
#' @export
#' @examples man/examples/examples-infer_and_simulate_fcm/examples-infer_and_simulate_fcm-simulate_ivfn_or_tfn_fcm.R
simulate_ivfn_or_tfn_fcm <- function(adj_matrix = matrix(),
                                     initial_state_vector = c(),
                                     clamping_vector = c(),
                                     activation = "kosko",
                                     squashing = "tanh",
                                     lambda = 1,
                                     max_iter = 100,
                                     min_error = 1e-5) {

  fcm_class <- get_adj_matrices_input_type(adj_matrix)$object_types_in_list[1]
  if (!(fcm_class %in% c("ivfn", "tfn"))) {
    stop("Input adj_matrix must be an adjacency matrix with edges represented as
         ivfns, or tfns to call simulate_ivfn_or_tfn_fcm")
  }
  concept_names <- colnames(adj_matrix)

  # Convert elements in initial_state_vectors, and clamping_vectors to
  # ivfn or tfn objects to streamline data management in simulation
  formatted_initial_state_vector <- vapply(initial_state_vector, function(x) list(convert_element_to_ivfn_or_tfn_if_numeric(x, desired_class = fcm_class)), list(1))
  clamped_node_locs <- which(clamping_vector != 0)
  formatted_clamped_nodes <- vapply(clamping_vector[clamped_node_locs], function(x) list(convert_element_to_ivfn_or_tfn_if_numeric(x, desired_class = fcm_class)), list(1))

  # Generate empty output objects prior to looping to improve runtime speed
  fuzzy_set_state_vectors <- vector(mode = "list", length = max_iter)
  fuzzy_set_state_vectors[[1]] <- formatted_initial_state_vector

  fuzzy_set_errors <- vector(mode = "list", length = max_iter)
  fuzzy_set_errors[[1]] <- rep(list(tfn(0, 0, 0)), length(initial_state_vector))

  crisp_state_vectors <- data.frame(matrix(data = numeric(), nrow = max_iter, ncol = length(initial_state_vector)))
  crisp_state_vectors[1, ] <- initial_state_vector

  crisp_errors <- data.frame(matrix(data = numeric(), nrow = max_iter, ncol = length(initial_state_vector)))
  crisp_errors[1, ] <- 0

  # Perform simulation
  for (i in 2:(max_iter + 1)) {
    # Calculate simulation step
    fuzzy_set_state_vector <- fuzzy_set_state_vectors[[i - 1]]
    crisp_state_vector <- crisp_state_vectors[i - 1, ]
    next_fuzzy_set_state_vector <- calculate_next_fuzzy_set_fcm_state_vector(adj_matrix, fuzzy_set_state_vector, crisp_state_vector, activation, fcm_class)
    normalized_next_fuzzy_set_state_vector <- lapply(
      next_fuzzy_set_state_vector,
      function(element) {
        if (fcm_class == "ivfn") {
          ivfn(squash(element$lower, squashing, lambda), squash(element$upper, squashing, lambda))
        } else if (fcm_class == "tfn") {
          tfn(squash(element$lower, squashing, lambda), squash(element$mode, squashing, lambda), squash(element$upper, squashing, lambda))
        }
      }
    )
    normalized_next_fuzzy_set_state_vector[clamped_node_locs] <- formatted_clamped_nodes
    crisp_normalized_next_state_vector <- lapply(normalized_next_fuzzy_set_state_vector, defuzz)

    # Store result in output objects
    fuzzy_set_state_vectors[[i]] <- normalized_next_fuzzy_set_state_vector
    crisp_state_vectors[i, ] <- crisp_normalized_next_state_vector
    fuzzy_set_errors[[i]] <- mapply(
      function(state_vector, next_state_vector) {
        if (fcm_class == "ivfn") {
          data.frame(
            error_in_lower = abs(state_vector$lower - next_state_vector$lower),
            error_in_upper = abs(state_vector$upper - next_state_vector$upper)
          )
        } else if (fcm_class == "tfn") {
          data.frame(
            error_in_lower = abs(state_vector$lower - next_state_vector$lower),
            error_in_mode = abs(state_vector$mode - next_state_vector$mode),
            error_in_upper = abs(state_vector$upper - next_state_vector$upper)
          )
        }
      },
      state_vector = fuzzy_set_state_vector,
      next_state_vector = normalized_next_fuzzy_set_state_vector,
      SIMPLIFY = FALSE
    )
    crisp_errors[i, ] <- abs(crisp_state_vector - crisp_normalized_next_state_vector)
    total_error <- sum(crisp_errors[i, ])
    if (total_error < min_error) {
      break
    }
  }
  if (i >= max_iter) {
    warning(
      "\tThe simulation reached the maximum number of iterations before
        achieving the minimum allowable error. This may signal that
        the fcm has reached a limit-cycle or is endlessly chaotic.

        It is also possible that the fcm simply requires more iterations
        to converge within the input minimum error.

        Try increasing the max_iter or min_error inputs."
    )
  }

  # Clean output objects
  fuzzy_set_state_vectors <- clean_simulation_output(fuzzy_set_state_vectors, concept_names)
  fuzzy_set_errors <- clean_simulation_output(fuzzy_set_errors, concept_names)
  crisp_state_vectors <- clean_simulation_output(crisp_state_vectors, concept_names)
  crisp_errors <- clean_simulation_output(crisp_errors, concept_names)

  structure(
    .Data = list(
      state_vectors = fuzzy_set_state_vectors,
      crisp_state_vectors = crisp_state_vectors,
      errors = fuzzy_set_errors,
      crisp_errors = crisp_errors,
      params = list(
        adj_matrix = adj_matrix,
        initial_state_vector = initial_state_vector,
        clamping_vector = clamping_vector,
        activation = activation,
        squashing = squashing,
        lambda = lambda,
        max_iter = max_iter,
        min_error = min_error,
        concepts = concept_names
      )
    ),
    class = "ivfn_or_tfn_simulation"
  )
}


#' [INTENDED FOR DEVELOPER USE ONLY] Calculate Next (Conventional) FCM State
#' Vector
#'
#' @description
#' This calculates the next iteration of a state vector in an fcm simulation
#' based on the kosko, modified-kosko, or rescale activation functions
#'
#' @details
#' INTENDED FOR DEVELOPER USE ONLY
#'
#' The state of the art of fcm typically applies one of three activation functions
#' in calculating iterative state vector values: kosko, modified-kosko, and
#' rescale (as identified in Gonzales et al. 2018 - https://doi.org/10.1142/S0218213018600102).
#'
#' kosko: Only considers the current iteration (Kosko, 1986 - https://doi.org/10.1016/S0020-7373(86)80040-2)
#'
#' modified-kosko: The previous value of a node influences its future value (Stylio & Groumpos, 2004 - https://doi.org/10.1109/TSMCA.2003.818878)
#'
#' rescale: Like modified-kosko, but assigns nodes with no value with a
#' value of 0.5 to reduce the influence that a lack of initial state information
#' can have on the simulation output (rescale, 2011 - https://doi.org/10.1016/j.asoc.2009.12.010)=
#'
#' Use vignette("fcm-class") for more information.
#'
#' @references Kosko, 1986
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#'
#' @returns The (i + 1) iteration of the input state_vector based on the
#' adj_matrix and activation function
#'
#' @export
calculate_next_conventional_fcm_state_vector <- function(adj_matrix = matrix(),
                                                         state_vector = c(),
                                                         activation = c("kosko", "modified-kosko", "rescale")) {
  adj_matrix <- as.matrix(adj_matrix)
  state_vector <- as.matrix(state_vector)

  if (dim(state_vector)[2] != unique(dim(adj_matrix))) {
    state_vector <- t(state_vector)
  }

  if (activation == "kosko") {
    next_state_vector <- state_vector %*% adj_matrix
  } else if (activation == "modified-kosko") {
    next_state_vector <- state_vector %*% adj_matrix + state_vector
  } else if (activation == "rescale") {
    next_state_vector <- (2*state_vector - 1) %*% adj_matrix + (2*state_vector - 1)
  }
  next_state_vector
}


#' [INTENDED FOR DEVELOPER USE ONLY] Calculate Next (IVFN-FCM or TFN-FCM) State
#' Vector
#'
#' @description
#' This calculates the next iteration of a state vector in an fcm simulation
#' based on the kosko, modified-kosko, or rescale activation functions
#'
#' @details
#' INTENDED FOR DEVELOPER USE ONLY
#'
#' The state of the art of fcm typically applies one of three activation functions
#' in calculating iterative state vector values: kosko, modified-kosko, and
#' rescale (as identified in Gonzales et al. 2018 - https://doi.org/10.1142/S0218213018600102).
#'
#' kosko: Only considers the current iteration (Kosko, 1986 - https://doi.org/10.1016/S0020-7373(86)80040-2)
#'
#' modified-kosko: The previous value of a node influences its future value (Stylio & Groumpos, 2004 - https://doi.org/10.1109/TSMCA.2003.818878)
#'
#' rescale: Like modified-kosko, but assigns nodes with no value with a
#' value of 0.5 to reduce the influence that a lack of initial state information
#' can have on the simulation output (rescale, 2011 - https://doi.org/10.1016/j.asoc.2009.12.010)=
#'
#' @param fuzzy_set_adj_matrix An n x n adjacency matrix that represents an FCM
#' and every element in the matrix is a tfn.
#' @param fuzzy_set_state_vector A list of state values as tfn objects
#' @param crisp_state_vector A list of state values as defuzzed tfn objects
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param fcm_class Class of edges in fuzzy_set_adj_matrix. Either 'ivfn' or 'tfn'
#'
#' @returns The (i + 1) iteration of the input state_vector based on the
#' adj_matrix and activation function
#'
#' @export
calculate_next_fuzzy_set_fcm_state_vector <- function(fuzzy_set_adj_matrix = matrix(),
                                                      fuzzy_set_state_vector = c(),
                                                      crisp_state_vector = c(),
                                                      activation = c("kosko", "modified-kosko", "rescale"),
                                                      fcm_class = c("ivfn", "tfn")) {

  next_fuzzy_set_state_vector <- vector(mode = "list", length = length(fuzzy_set_state_vector))
  for (col in seq_along(fuzzy_set_adj_matrix)) {
    dot_product_multiplication_only <- mapply(
      function(coefficient, column_vector) {
        if (activation == "rescale") coefficient <- 2*coefficient - 1
        #browser()
        if (coefficient >= 0) {
          if (fcm_class == "ivfn") {
            ivfn(coefficient*column_vector$lower, coefficient*column_vector$upper)
          } else if (fcm_class == "tfn") {
            tfn(coefficient*column_vector$lower, coefficient*column_vector$mode, coefficient*column_vector$upper)
          }
        } else {
          if (fcm_class == "ivfn") {
            ivfn(coefficient*column_vector$upper, coefficient*column_vector$lower)
          } else if (fcm_class == "tfn") {
            tfn(coefficient*column_vector$upper, coefficient*column_vector$mode, coefficient*column_vector$lower)
          }
        }
      },
      coefficient = crisp_state_vector,
      column_vector = fuzzy_set_adj_matrix[, col]
    )
    dot_product <- apply(dot_product_multiplication_only, 1, function(row) sum(unlist(row)))
    if (fcm_class == "ivfn") {
      next_fuzzy_set_state_vector_column <- ivfn(dot_product[1], dot_product[2])
    } else if (fcm_class == "tfn") {
      next_fuzzy_set_state_vector_column <- tfn(dot_product[1], dot_product[2], dot_product[3])
    }
    next_fuzzy_set_state_vector[[col]] <- next_fuzzy_set_state_vector_column
  }

  if (activation == "kosko") {
    next_fuzzy_set_state_vector <-  next_fuzzy_set_state_vector
  } else if (activation == "modified-kosko") {
    next_fuzzy_set_state_vector <- mapply(
      function(fuzzy_set_1, fuzzy_set_2) {
        if (fcm_class == "ivfn") {
          ivfn(fuzzy_set_1$lower + fuzzy_set_2$lower, fuzzy_set_1$upper + fuzzy_set_2$upper)
        } else if (fcm_class == "tfn") {
          tfn(fuzzy_set_1$lower +  fuzzy_set_2$lower, fuzzy_set_1$mode +  fuzzy_set_2$mode, fuzzy_set_1$upper +  fuzzy_set_2$upper)
        }
      },
      fuzzy_set_1 = fuzzy_set_state_vector,
      fuzzy_set_2 = next_fuzzy_set_state_vector,
      SIMPLIFY = FALSE
    )
  } else if (activation == "rescale") {
    next_fuzzy_set_state_vector <- mapply(
      function(fuzzy_set_1, fuzzy_set_2) {
        if (fcm_class == "ivfn") {
          ivfn((2*fuzzy_set_1$lower - 1) + fuzzy_set_2$lower, (2*fuzzy_set_1$upper - 1) + fuzzy_set_2$upper)
        } else if (fcm_class == "tfn") {
          tfn((2*fuzzy_set_1$lower - 1) +  fuzzy_set_2$lower, (2*fuzzy_set_1$mode - 1) +  fuzzy_set_2$mode, (2*fuzzy_set_1$upper - 1) +  fuzzy_set_2$upper)
        }
      },
      fuzzy_set_1 = fuzzy_set_state_vector,
      fuzzy_set_2 = next_fuzzy_set_state_vector,
      SIMPLIFY = FALSE
    )
  }

  next_fuzzy_set_state_vector
}


#' Squash
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
squash <- function(value = numeric(),
                   squashing = c("sigmoid", "tanh", "bivalent", "saturation", "trivalent"),
                   lambda = 1) {
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



#' Defuzz
#'
#' @description
#' Convert a fuzzy number to a crisp value. For IVFNs, return the average of the
#' upper and lower bounds. For TFNs, return the average of the lower bound, the
#' mode, and the upper bound.
#'
#' @param fuzzy_number A fuzzy number object. Either an ivfn or tfn
#'
#' @returns A crisp value representative of the input IVFN or TFN
#'
#' @export
defuzz <- function(fuzzy_number) {
  fuzzy_class <- methods::is(fuzzy_number)[1]
  if (fuzzy_class == "numeric" | fuzzy_class == "integer") {
    crisp_value <- fuzzy_number
  } else if (fuzzy_class == "ivfn") {
    crisp_value <- (fuzzy_number$lower + fuzzy_number$upper)/2
  } else if (fuzzy_class == "tfn") {
    crisp_value <- (fuzzy_number$lower + fuzzy_number$mode + fuzzy_number$upper)/3
  } else {
    stop("Cannot defuzz input fuzzy_number. Must be either an ivfn or tfn. (Accepts numerics but does nothing with them.)")
  }
  crisp_value
}


#' Convert Value to IVFN or TFN if Value is Numeric
#'
#' @description
#' This checks whether the input element is an ordinary number or a triangular number.
#' If it is an ivfn or tfn, it returns the input, but if it is a numeric type
#' object (ordinary number), it will convert that number into an ivfn or tfn
#'
#' @param element An element in a matrix
#' @param desired_class Transform the element into an 'ivfn' or 'tfn'
#'
#' @returns An IVFN or TFN representation of a crisp, numeric value
#'
#' @export
convert_element_to_ivfn_or_tfn_if_numeric <- function(element, desired_class = c("ivfn", "tfn")) {
  numeric_class <- methods::is(numeric())
  if (identical(methods::is(element), numeric_class) & identical(desired_class, "ivfn")) {
    converted_element <- ivfn(element, element)
  } else if (identical(methods::is(element), numeric_class) & identical(desired_class, "tfn")) {
    converted_element <- tfn(element, element, element)
  }
  converted_element
}


#' Convert IVFN or TFN Elements in Adj. Matrix to Distributions (i.e. sets)
#'
#' @description
#' Given a list of adjacency matrices which include either ivfns or
#' tfns, convert those objects to their corresponding
#' distributions representative of those values.
#'
#' @details
#' This function assists with subtracting the baseline from the scenario
#' simulation when calling infer_fcm with IVFN-FCMs or TFN-FCMs.
#'
#' @param fuzzy_set_matrix A matrix that contains fuzzy sets as elements
#' @param object_class Values are represented either as ivfns or tfns. Options: 'ivfn' or 'tfn'
#' @param N_samples The number of samples to draw from the corresponding distribution
#'
#' @returns An adj. matrix of IVFNs or TFNs represented as lists (sets) of their
#' representative distributions
#'
#' @export
convert_fuzzy_set_elements_in_matrix_to_distributions <- function(fuzzy_set_matrix = matrix(),
                                                                  object_class = c("ivfn", "tfn"),
                                                                  N_samples = integer()) {

  if (!(object_class %in% c("ivfn", "tfn"))) {
    stop("Input object_class must be either 'ivfn' or 'tfn'")
  }

  if (object_class == "ivfn") {
    fuzzy_set_matrix_w_distributions <- apply(
      fuzzy_set_matrix, c(1, 2),
      function(element) {
        if (identical(methods::is(element[[1]]), "ivfn")) {
          element <- list(stats::runif(N_samples, element[[1]]$lower, element[[1]]$upper))
        } else {
          element[[1]]
        }
      }
    )
  } else if (object_class == "tfn") {
    fuzzy_set_matrix_w_distributions <- apply(
      fuzzy_set_matrix, c(1, 2),
      function(element) {
        if (identical(methods::is(element[[1]]), "tfn")) {
          element <- list(rtri(N_samples, lower = element[[1]]$lower, mode = element[[1]]$mode, upper = element[[1]]$upper))
        } else {
          element[[1]]
        }
      }
    )
  }

  fuzzy_set_matrix_w_distributions
}


#' [INTENDED FOR DEVELOPER USE ONLY] Clean Simulation Output
#'
#' @description
#' INTENDED FOR DEVELOPER USE ONLY
#'
#' This adds quality-of-life improvements and detail to simulation output objects
#' such as adding column names and an iter column
#'
#' @param output_obj An fcm_w_fcm_w_tfn simulation output object
#' @param concepts A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @returns A cleaned up simulation output
#'
#' @export
clean_simulation_output <- function(output_obj, concepts) {
  #browser()
  if (identical(methods::is(data.frame()), methods::is(output_obj))) {
    # output_obj is a data.frame
    clean_output_obj <- stats::na.omit(output_obj)
  } else {
    # output_obj is a list of lists
    clean_output_obj <- data.frame(do.call(rbind, output_obj))
  }

  if ("iter" %in% colnames(output_obj)) {
    clean_output_obj$iter <- 0:(nrow(clean_output_obj) - 1)
  } else {
    colnames(clean_output_obj) <- concepts
    clean_output_obj <- cbind(iter = 0:(nrow(clean_output_obj) - 1), clean_output_obj)
  }

  clean_output_obj
}


#' [INTENDED FOR DEVELOPER USE ONLY] Check Simulation Inputs
#'
#' @description
#' Confirm that all inputs will work with the simulation function and return
#' appropriate error messages where necessary
#'
#' @details
#' INTENDED FOR DEVELOPER USE ONLY
#'
#' This checks that all inputs for a simulation function are of an appropriate
#' format, and also fills in missing inputs for initial_state_vector, clamping_vector,
#' and IDs when appropriate.
#'
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param fuzzy_set_samples The number of samples to represent ivfn or tfn as. Use to create finer scale results.s
#'
#' @export
check_simulation_inputs <- function(adj_matrix = matrix(),
                                    initial_state_vector = c(),
                                    clamping_vector = c(),
                                    activation = c("kosko"),
                                    squashing = c("sigmoid"),
                                    lambda = 1,
                                    max_iter = 100,
                                    min_error = 1e-4,
                                    fuzzy_set_samples = 100) {

  # Check adj_matrix ----
  rows <- nrow(adj_matrix)
  cols <- ncol(adj_matrix)
  if (rows != cols) {
    stop("Failed Input Validation: Input adjacency matrix must be a square (n x n) matrix")
  }
  data_types_in_adj_matrix <- unique(do.call(list, (apply(adj_matrix, c(1, 2), function(x) list(methods::is(x[[1]]))))))
  if (length(data_types_in_adj_matrix) > 1) {
    stop("Failed Input Validation: Input adjacency matrix must contain objects of the same type. Either numerics, ivfns, or tfns.")
  }
  n_nodes <- unique(dim(adj_matrix))

  # ----

  # Check initial_state_vector ----
  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
    initial_state_vector <- rep(1, nrow(adj_matrix))
  }
  if (length(initial_state_vector) != n_nodes) {
    stop("Failed Input Validation: Input initial_state_vector must have the same length as the number of nodes in the adj. matrix.")
  }
  data_types_in_initial_state_vector <- unlist(unique(sapply(initial_state_vector, methods::is, simplify = FALSE)))
  if (!identical(data_types_in_initial_state_vector, methods::is(numeric()))) {
    stop("Failed Input Validation: Input initial_state_vector must only contain numeric values.")
  }

  # ----

  # Check clamping_vector ----
  if (identical(clamping_vector, c())) {
    warning("No clamping_vector input given. Assuming no values are clamped.")
    clamping_vector <- rep(0, length(initial_state_vector))
  }
  if (length(clamping_vector) != n_nodes) {
    stop("Failed Input Validation: Input clamping_vector must have the same length as the number of nodes in the adj. matrix.")
  }
  data_types_in_clamping_vector <- unlist(unique(sapply(clamping_vector, methods::is, simplify = FALSE)))
  if (!identical(data_types_in_clamping_vector, methods::is(numeric()))) {
    stop("Failed Input Validation: Input clamping_vector must only contain numeric values.")
  }
  # ----

  # Check activation and squashing ----
  if (!(activation %in% c("kosko", "modified-kosko", "rescale"))) {
    stop("Failed Input Validation: Input activation must be one of the following: 'kosko', 'modified-kosko', or 'rescale'")
  }

  if (!(squashing %in% c("sigmoid", "tanh", "bivalent", "saturation", "trivalent"))) {
    stop('Failed Input Validation: Input squashing must be one of the following: "sigmoid", "tanh", "bivalent", "saturation", "trivalent"')
  }
  if (activation == "rescale" & squashing != "sigmoid") {
    stop(
      paste0(
        "   !!!Please use the sigmoid squashing function with the rescale activation function!!!

          The rescale activation function is designed to optimize performance
          with the sigmoid squashing function. Results are unreliable if
          using a different squashing function.\n",

        "\n          Input squashing function: ", squashing)
    )
  }
  # ----

  # Check lambda ----
  if (!is.numeric(lambda)) {
    stop("Failed Input Validation: Input lambda must be numeric")
  }
  if (lambda <= 0) {
    stop("Failed Input Validation: Input lambda must be greater than 0")
  }
  # ----

  # Check max_iter ----
  if (!is.numeric(max_iter)) {
    stop("Failed Input Validation: Input max_iter must be numeric")
  }
  if (!(max_iter == round(max_iter))) {
    stop("Failed Input Validation: Input max_iter must be a positive integer")
  }
  if (max_iter <= 0) {
    stop("Failed Input Validation: Input max_iter must be a positive integer")
  }
  # ----

  # Check min_error ----
  if (!is.numeric(min_error)) {
    stop("Failed Input Validation: Input min_error must be numeric")
  }
  if (min_error <= 0) {
    stop("Failed Input Validation: Input min_error must be greater than 0")
  }
  # ----

  # Check fuzzy_set_samples ----
  if (!is.numeric(fuzzy_set_samples)) {
    stop("Failed Input Validation: Input fuzzy_set_samples must be numeric")
  }
  if (!(fuzzy_set_samples == round(fuzzy_set_samples))) {
    stop("Failed Input Validation: Input fuzzy_set_samples must be a positive integer")
  }
  if (fuzzy_set_samples <= 0) {
    stop("Failed Input Validation: Input fuzzy_set_samples must be a positive integer")
  }
  # ----


  list(
    initial_state_vector = initial_state_vector,
    clamping_vector = clamping_vector
  )
}


#' Print method for infer_conventional_fcm objects
#'
#' @param x an infer_conventional_fcm object
#' @param ... additional inputs
#' @export
print.infer_conventional_fcm <- function(x, ...) {
  cat(paste0("fcmconfr: ", "conventional"),
      "\n $inference\n",
      paste0("  ", colnames(x$inference), ": ", round(x$inference, digits = 2), sep = "\n"),
      "$inference_for_plotting\n",
      paste0("  - inference data transformed to streamline plotting with ggplot"),
      "\n $inference_state_vectors\n",
      paste0("  - inferences across all iterations of the simulation"),
      "\n $scenario_simulation\n",
      "$baseline_simulation"
  )
}


#' Print method for infer_ivfn_or_tfn_fcm objects
#'
#' @param x an infer_ivfn_or_tfn_fcm object
#' @param ... additional inputs
#'
#' @export
print.infer_ivfn_or_tfn_fcm <- function(x, ...) {
  cat(paste0("fcmconfr: ", "ivfn or tfn"),
      "\n $inference_df\n",
      paste0("  ", x$inference_df$concept, ": [", round(x$inference_df$lower, 2), ", ", round(x$inference_df$upper, 2), "] (", round(x$inference_df$crisp, 2), ")", sep = "\n"),
      "$inference_for_plotting\n",
      paste0("  - inference data transformed to streamline plotting with ggplot"),
      "\n $inference_state_vectors\n",
      paste0("  - inferences as fuzzy sets across all iterations of the simulation"),
      "\n $crisp_inference_state_vectors\n",
      paste0("  - inferences as crisp_values across all iterations of the simulation"),
      "\n $scenario_simulation\n",
      "$baseline_simulation"
  )
}

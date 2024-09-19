
#' infer_fcm_w_fcm_w_tfn_with_clamping
#'
#' @description
#' This confers with a baseline simulation of an fcm_w_fcm_w_tfn and a scenario (scenario vector)
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
#' Use vignette("fcm_w_fcm_w_tfn-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param fcm_w_tfn_adj_matrix An n x n adjacency matrix that represents an FCM
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
#' @param CI Confidence Interval to estimate lower and upper bounds of final inference
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @export
infer_fcm_w_tfn_with_clamping <- function(fcm_w_tfn_adj_matrix = matrix(),
                                     initial_state_vector = c(),
                                     clamping_vector = c(),
                                     activation = "kosko", # Problems when activation == "rescale",
                                     squashing = "tanh",
                                     lambda = 1,
                                     max_iter = 100,
                                     min_error = 1e-5,
                                     CI = 0.95,
                                     IDs = c()) {

  # Perform checks
  checks <- check_simulation_inputs(fcm_w_tfn_adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error, IDs)
  initial_state_vector <- checks$initial_state_vector
  clamping_vector <- checks$clamping_vector
  IDs <- checks$IDs

  # Get baseline simulation
  baseline_initial_state_vector <- initial_state_vector
  baseline_clamping_vector <- rep(0, length(clamping_vector))
  baseline_simulation <- simulate_fcm_w_tfn_with_clamping_or_pulse_only(fcm_w_tfn_adj_matrix, baseline_initial_state_vector, baseline_clamping_vector, activation, squashing, lambda, max_iter, min_error, IDs)

  # Get scenario simulation
  scenario_initial_state_vector <- initial_state_vector
  scenario_clamping_vector <- clamping_vector
  scenario_simulation <- simulate_fcm_w_tfn_with_clamping_or_pulse_only(fcm_w_tfn_adj_matrix, scenario_initial_state_vector, scenario_clamping_vector, activation, squashing, lambda, max_iter, min_error, IDs)

  equalized_dfs <- equalize_fcm_w_tfn_simulation_state_vectors_dataframes(baseline_simulation, scenario_simulation)
  baseline_simulation$state_vectors <- equalized_dfs$baseline$state_vectors
  baseline_simulation$defuzzed_state_vectors <- equalized_dfs$baseline$defuzzed_state_vectors
  scenario_simulation$state_vectors <- equalized_dfs$scenario$state_vectors
  scenario_simulation$defuzzed_state_vectors <- equalized_dfs$scenario$defuzzed_state_vectors

  baseline_state_vectors_as_distributions <- convert_fuzzy_elements_in_matrix_to_distributions(baseline_simulation$state_vectors, "fcm_w_tfn", 1000)# [, -1]
  scenario_state_vectors_as_distributions <- convert_fuzzy_elements_in_matrix_to_distributions(scenario_simulation$state_vectors, "fcm_w_tfn", 1000)# [, -1]

  # browser()
  inference_state_vectors_as_distributions <- baseline_state_vectors_as_distributions
  for (i in 1:nrow(baseline_state_vectors_as_distributions)) {
    for (j in 1:ncol(baseline_state_vectors_as_distributions)) {
      inference_state_vectors_as_distributions[i, j] <- list(unlist(scenario_state_vectors_as_distributions[i, j]) - unlist(baseline_state_vectors_as_distributions[i, j]))
    }
  }

  inference_CI_state_vectors <- data.frame(
    apply(inference_state_vectors_as_distributions, c(1, 2),
          function(element) {
            tfn(
              lower = stats::quantile(element[[1]], 0.5 - CI/2),
              mode = mean(element[[1]]),
              upper = stats::quantile(element[[1]], 0.5 + CI/2)
            )}))

  defuzzed_inference_CI_state_vectors <- data.frame(
    apply(inference_CI_state_vectors, c(1, 2),
          function(element) {
            defuzz_fcm_w_tfn(element[[1]], "cog")}))

  inference_CI_state_vectors <- clean_fcm_w_fcm_w_tfn_simulation_output(inference_CI_state_vectors, IDs)
  defuzzed_inference_CI_state_vectors <- clean_fcm_w_fcm_w_tfn_simulation_output(defuzzed_inference_CI_state_vectors, IDs)

  inference_values <- defuzzed_inference_CI_state_vectors[nrow(defuzzed_inference_CI_state_vectors),][, -1]
  final_inference_CI_state_vectors <- inference_CI_state_vectors[nrow(inference_CI_state_vectors), ][, -1]
  final_inference_df <- data.frame(
    node = IDs,
    defuzzed = t(inference_values),
    lower = vapply(final_inference_CI_state_vectors, function(x) x[[1]]$lower, numeric(1)),
    mode = vapply(final_inference_CI_state_vectors, function(x) x[[1]]$mode, numeric(1)),
    upper = vapply(final_inference_CI_state_vectors, function(x) x[[1]]$upper, numeric(1))
  )
  colnames(final_inference_df) <- c("node", "defuzz", "lower", "mode", "upper")
  rownames(final_inference_df) <- NULL

  final_inference_plot_data <- tidyr::pivot_longer(final_inference_df, cols = 2:ncol(final_inference_df))

  structure(
    .Data = list(
      inference = final_inference_df,
      inference_for_plotting = final_inference_plot_data,
      inference_state_vectors = inference_CI_state_vectors,
      defuzzed_inference_state_vectors = defuzzed_inference_CI_state_vectors,
      scenario_simulation = scenario_simulation,
      baseline_simulation = baseline_simulation
    ),
    class = "fcm_w_fcm_w_tfn_inference_with_clamping"
  )
}



#' equalize_fcm_w_tfn_simulation_state_vectors_dataframes
#'
#' @description
#' This reformats the dataframes of the simulation with the least number of iterations
#' (rows) and repeats the final row until dataframes across the baseline and scenario
#' simulations have the same number of iterations (rows)
#'
#' Use vignette("fcm_w_tfn-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param baseline The output of a fcm_w_fcm_w_tfn simulation using the baseline inputs
#' @param scenario The output of a fcm_w_fcm_w_tfn simulation using the scenario inputs
equalize_fcm_w_tfn_simulation_state_vectors_dataframes <- function(baseline, scenario) {
  n_iters_baseline <- nrow(baseline$state_vectors)
  n_iters_scenario <- nrow(scenario$state_vectors)

  if (n_iters_baseline == n_iters_scenario) {
    baseline_state_vectors <- baseline$state_vectors[, -1]
    scenario_state_vectors <- scenario$state_vectors[, -1]
    baseline_defuzzed_state_vectors <- baseline$defuzzed_state_vectors[, -1]
    scenario_defuzzed_state_vectors <- scenario$defuzzed_state_vectors[, -1]
  } else if (n_iters_baseline < n_iters_scenario) {
    extended_baseline_state_vectors <- data.frame(do.call(cbind, apply(baseline$state_vectors, 2, function(sim) c(sim, rep(sim[n_iters_baseline], n_iters_scenario - n_iters_baseline)))))
    baseline_state_vectors <- extended_baseline_state_vectors
    scenario_state_vectors <- scenario$state_vectors
    extended_baseline_defuzzed_state_vectors <- data.frame(apply(baseline$defuzzed_state_vectors, 2, function(sim) c(sim, rep(sim[n_iters_baseline], n_iters_scenario - n_iters_baseline))))
    baseline_defuzzed_state_vectors <- extended_baseline_defuzzed_state_vectors[, -1]
    scenario_defuzzed_state_vectors <- scenario$defuzzed_state_vectors[, -1]
  } else if (n_iters_scenario < n_iters_baseline) {
    extended_scenario_state_vectors <- data.frame(do.call(cbind, apply(scenario$state_vectors, 2, function(sim) c(sim, rep(sim[n_iters_scenario], n_iters_baseline - n_iters_scenario)))))
    baseline_state_vectors <- baseline$state_vectors
    scenario_state_vectors <- extended_scenario_state_vectors
    extended_scenario_defuzzed_state_vectors <- data.frame(apply(scenario$defuzzed_state_vectors, 2, function(sim) c(sim, rep(sim[n_iters_scenario], n_iters_baseline - n_iters_scenario))))
    baseline_defuzzed_state_vectors <- baseline$defuzzed_state_vectors[, -1]
    scenario_defuzzed_state_vectors <- extended_scenario_defuzzed_state_vectors[, -1]
  }

  list(
    baseline = list(
      state_vectors = baseline_state_vectors,
      defuzzed_state_vectors = baseline_defuzzed_state_vectors
    ),
    scenario = list(
      state_vectors = scenario_state_vectors,
      defuzzed_state_vectors = scenario_defuzzed_state_vectors
    )
  )
}



#' simulate_fcm_w_tfn_with_pulse
#'
#' @description
#' This calculates a sequence of iterations of a simulation over a fcm_w_tfn m object
#' given an initial state vector along with the activation, squashing, and lambda
#' parameters. Additional variables may be defined to control simulation length,
#' column names, and lambda optimization.
#'
#' @details
#' This simulates how an fcm_w_tfn reacts to an input initial state vector. There is a
#' multi-decadal long body of work that has explored numerous activation and squashing
#' functions as well as algorithms to optimize the lambda value for the
#' sigmoid and tanh squashing functions.
#'
#' Use vignette("fcm_w_tfn-class") for more information about each of these
#' functions/algorithms alongside their originating sources.
#'
#' @param fcm_w_tfn_adj_matrix An n x n adjacency matrix that represents an FCM
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
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @export
simulate_fcm_w_tfn_with_clamping_or_pulse_only <- function(fcm_w_tfn_adj_matrix = matrix(),
                                                     initial_state_vector = c(),
                                                     clamping_vector = c(),
                                                     activation = "kosko", # Problems when activation == "rescale",
                                                     squashing = "tanh",
                                                     lambda = 1,
                                                     max_iter = 100,
                                                     min_error = 1e-5,
                                                     IDs = c()) {
  # Perform checks
  checks <- check_simulation_inputs(fcm_w_tfn_adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error, IDs)
  initial_state_vector <- checks$initial_state_vector
  clamping_vector <- checks$clamping_vector
  IDs <- checks$IDs

  # Convert elements in adj_matrix, initial_state_vectors, and clamping_vectors to
  # tfn objects to streamline data management in simulation
  formatted_fcm_w_tfn_adj_matrix <- data.frame(apply(fcm_w_tfn_adj_matrix, c(1, 2), convert_element_to_tfn_if_numeric))
  formatted_initial_state_vector <- vapply(initial_state_vector, convert_element_to_tfn_if_numeric, list(1))
  clamped_node_locs <- which(clamping_vector != 0)
  formatted_clamped_nodes <- vapply(clamping_vector[clamped_node_locs], convert_element_to_tfn_if_numeric, list(1))

  # Generate empty output objects prior to looping to improve runtime speed
  fcm_w_tfn_state_vectors <- vector(mode = "list", length = max_iter)
  fcm_w_tfn_errors <- vector(mode = "list", length = max_iter)
  defuzzed_fcm_w_tfn_state_vectors <- data.frame(matrix(data = numeric(), nrow = max_iter, ncol = length(initial_state_vector)))
  defuzzed_fcm_w_tfn_errors <- data.frame(matrix(data = numeric(), nrow = max_iter, ncol = length(initial_state_vector)))

  fcm_w_tfn_state_vectors[[1]] <- formatted_initial_state_vector
  fcm_w_tfn_errors[[1]] <- rep(list(tfn(0, 0, 0)), length(initial_state_vector))
  defuzzed_fcm_w_tfn_state_vectors[1, ] <- unlist(lapply(formatted_initial_state_vector, defuzz_fcm_w_tfn, "cog"))
  defuzzed_fcm_w_tfn_errors[1, ] <- 0

  for (i in 2:(max_iter + 1)) {
    # Calculate simulation step
    fcm_w_tfn_state_vector <- fcm_w_tfn_state_vectors[[i - 1]]
    defuzzed_fcm_w_tfn_state_vector <- defuzzed_fcm_w_tfn_state_vectors[i - 1, ]
    next_fcm_w_tfn_state_vector <- calculate_next_fcm_w_tfn_state_vector(formatted_fcm_w_tfn_adj_matrix, fcm_w_tfn_state_vector, defuzzed_fcm_w_tfn_state_vector, activation)
    normalized_next_fcm_w_tfn_state_vector <- lapply(
      next_fcm_w_tfn_state_vector,
      function(element) {
        tfn(squash(element$lower, squashing, lambda), squash(element$mode, squashing, lambda), squash(element$upper, squashing, lambda))
      }
    )
    normalized_next_fcm_w_tfn_state_vector[clamped_node_locs] <- formatted_clamped_nodes
    defuzzed_normalized_next_fcm_w_tfn_state_vector <- unlist(lapply(normalized_next_fcm_w_tfn_state_vector, defuzz_fcm_w_tfn, "cog"))
    # Store result in output objects
    fcm_w_tfn_state_vectors[[i]] <- normalized_next_fcm_w_tfn_state_vector
    defuzzed_fcm_w_tfn_state_vectors[i, ] <- defuzzed_normalized_next_fcm_w_tfn_state_vector
    fcm_w_tfn_errors[[i]] <- mapply(
      function(state_vector, next_state_vector) {
        data.frame(
          error_in_lower = abs(state_vector$lower - next_state_vector$lower),
          error_in_mode = abs(state_vector$mode - next_state_vector$mode),
          error_in_upper = abs(state_vector$upper - next_state_vector$upper)
        )
      },
      state_vector = fcm_w_tfn_state_vector,
      next_state_vector = normalized_next_fcm_w_tfn_state_vector,
      SIMPLIFY = FALSE
    )
    defuzzed_fcm_w_tfn_errors[i, ] <- abs(defuzzed_fcm_w_tfn_state_vector - defuzzed_normalized_next_fcm_w_tfn_state_vector)
    total_error <- sum(defuzzed_fcm_w_tfn_errors[i, ])
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
  fcm_w_tfn_state_vectors <- clean_fcm_w_fcm_w_tfn_simulation_output(fcm_w_tfn_state_vectors, IDs)
  fcm_w_tfn_errors <- clean_fcm_w_fcm_w_tfn_simulation_output(fcm_w_tfn_errors, IDs)
  defuzzed_fcm_w_tfn_state_vectors <- clean_fcm_w_fcm_w_tfn_simulation_output(defuzzed_fcm_w_tfn_state_vectors, IDs)
  defuzzed_fcm_w_tfn_errors <- clean_fcm_w_fcm_w_tfn_simulation_output(defuzzed_fcm_w_tfn_errors, IDs)

  structure(
    .Data = list(
      state_vectors = fcm_w_tfn_state_vectors,
      defuzzed_state_vectors = defuzzed_fcm_w_tfn_state_vectors,
      errors = fcm_w_tfn_errors,
      defuzzed_errors = defuzzed_fcm_w_tfn_errors,
      params = list(
        adj_matrix = fcm_w_tfn_adj_matrix,
        initial_state_vector = initial_state_vector,
        activation = activation,
        squashing = squashing,
        lambda = lambda,
        max_iter = max_iter,
        min_error = min_error,
        IDs = IDs
      )
    ),
    class = "fcm_w_fcm_w_tfn_simulation"
  )
}


#' convert_element_to_tfn_if_numeric
#'
#' @description
#' This checks whether the input element is an ordinary number or a triangular number.
#' If it is a triangular number, it returns the input, but if it is a numeric type
#' object (ordinary number), it will convert that number into a triangular number
#'
#' @param element An element in a matrix
#'
#' @export
convert_element_to_tfn_if_numeric <- function(element) {
  ifelse(identical(methods::is(element[[1]]), "tfn"),
         element,
         list(tfn(element[[1]], element[[1]], element[[1]])))
}



#' clean_tfcm_simulation_output
#'
#' @description
#' This adds quality-of-life improvements and detail to simulation output objects
#' such as adding column names and an iter column
#'
#' @param output_obj An fcm_w_fcm_w_tfn simulation output object
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @export
clean_fcm_w_fcm_w_tfn_simulation_output <- function(output_obj, IDs) {
  if (identical(methods::is(data.frame()), methods::is(output_obj))) {
    # output_obj is a data.frame
    output_obj <- stats::na.omit(output_obj)
  } else {
    # output_obj is a list of lists
    output_obj <- data.frame(do.call(rbind, output_obj))
  }
  colnames(output_obj) <- IDs
  output_obj <- cbind(iter = 0:(nrow(output_obj) - 1), output_obj)
  output_obj
}


#' calculate_next_fcm_w_tfn_state_vector
#'
#' @description
#' This calculates the next iteration of a state vector in an fcm simulation
#' based on the kosko, modified-kosko, or rescale activation functions
#'
#' @details
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
#' @param formatted_fcm_w_tfn_adj_matrix An n x n adjacency matrix that represents an FCM
#' and every element in the matrix is a tfn.
#' @param fcm_w_tfn_state_vector A list of state values as tfn objects
#' @param defuzzed_fcm_w_tfn_state_vector A list of state values as defuzzed tfn objects
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#'
#' @export
calculate_next_fcm_w_tfn_state_vector <- function(formatted_fcm_w_tfn_adj_matrix = matrix(),
                                            fcm_w_tfn_state_vector = c(),
                                            defuzzed_fcm_w_tfn_state_vector = c(),
                                            activation = c("kosko", "modified-kosko", "rescale"),
                                            squashing = c("sigmoid", "tanh")) {

    next_fcm_w_tfn_state_vector <- vector(mode = "list", length = length(defuzzed_fcm_w_tfn_state_vector))
    for (col in seq_along(formatted_fcm_w_tfn_adj_matrix)) {
      dot_product_multiplication_only <- mapply(
        function(coefficient, fcm_w_tfn) {
          fcm_w_tfn <- fcm_w_tfn[[1]]
          if (coefficient >= 0) {
            tfn(coefficient*fcm_w_tfn$lower, coefficient*fcm_w_tfn$mode, coefficient*fcm_w_tfn$upper)
          } else {
            tfn(coefficient*fcm_w_tfn$upper, coefficient*fcm_w_tfn$mode, coefficient*fcm_w_tfn$lower)
          }
        },
        coefficient = defuzzed_fcm_w_tfn_state_vector,
        fcm_w_tfn = formatted_fcm_w_tfn_adj_matrix[, col]
      )
      dot_product <- apply(dot_product_multiplication_only, 1, function(row) sum(unlist(row)))
      next_fcm_w_tfn_state_vector[[col]] <- tfn(dot_product[1], dot_product[2], dot_product[3])
    }

    if (activation == "kosko") {
      next_fcm_w_tfn_state_vector <-  next_fcm_w_tfn_state_vector
    } else if (activation == "modified-kosko") {
      next_fcm_w_tfn_state_vector <- mapply(
        function(fcm_w_tfn_1, fcm_w_tfn_2) tfn(fcm_w_tfn_1$lower +  fcm_w_tfn_2$lower, fcm_w_tfn_1$mode +  fcm_w_tfn_2$mode, fcm_w_tfn_1$upper +  fcm_w_tfn_2$upper),
        fcm_w_tfn_1 = fcm_w_tfn_state_vector,
        fcm_w_tfn_2 = next_fcm_w_tfn_state_vector,
        SIMPLIFY = FALSE
      )
    }

    next_fcm_w_tfn_state_vector
}


defuzz_fcm_w_tfn <- function(fcm_w_tfn = tfn(), method = c("cog", "distance")) {
  if (method == "cog") {
    defuzzified_value <- (fcm_w_tfn$lower + fcm_w_tfn$mode + fcm_w_tfn$upper)/3
  } # else if (method == "distance") {
    # x_centroid <- (fcm_w_tfn$lower + fcm_w_tfn$mode + fcm_w_tfn$upper)/3
    # # peak density = 2/(fcm_w_tfn$upper - fcm_w_tfn$lower) from Area Tri = (1/2)*base*height = 1
    # y_centroid <- (1/3)*(2/(fcm_w_tfn$upper - fcm_w_tfn$lower))
    # defuzzified_value <- sqrt(x_centroid^2 + y_centroid^2)
  # }
}



#' confirm_input_vector_is_compatible_with_fcm_w_tfn_adj_matrix
#'
#' @description
#' Confirm that an initial state vector is algorithmically compatible with a triangular adjacency matrix
#'
#' @details
#' Boolean. TRUE if the number of entries in the initial
#' state vector match the number of rows/columns in the adjacency matrix and 2. The
#' datatypes stored within each object are the same (i.e. "numeric" vs "tfn"),
#' FALSE if not
#'
#' Intended for developer use only to improve package readability.
#'
#' @param fcm_w_tfn_adj_matrix An n x n triangular adjacency matrix that represents an FCM
#' @param initial_state_vector An n-length list of the initial states of each node in an fcm simulation
confirm_input_vector_is_compatible_with_fcm_w_tfn_adj_matrix <- function(fcm_w_tfn_adj_matrix = matrix(), initial_state_vector = c()) {
  if (length(initial_state_vector) != unique(dim(fcm_w_tfn_adj_matrix))) {
    stop("Length of input initial_state_vector is does not comply with the dimensions of the input adjacency matrix", .call = FALSE)
  } else {
    TRUE
  }

  data_types <- unique(vapply(initial_state_vector, class, character(1)))
  both_numeric_or_tfn_data_types <- identical(data_types, c("numeric", "tfn")) | identical(data_types, c("tfn", "numeric"))
  only_numeric_data_types <- identical(data_types, "numeric")
  only_tfn_data_types <- identical(data_types, "tfn")

  if (both_numeric_or_tfn_data_types | only_numeric_data_types | only_tfn_data_types) {
    TRUE
  } else {
    stop("Input initial state vector must contain only numeric or tfn values")
  }
}


#' make_adj_matrix_w_tfns
#'
#' @description
#' This constructs a triangular adjacency matrix from an adjacency matrix of the lower
#' limits of edges in an FCM, an adjacency matrix of the averages or expected values
#' of edges, and an adjacency matrix of the upper limits of edges in an FCM.
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
#' @param lower An n x n adjacency matrix that represents the lower limits of
#'              edges in an FCM
#' @param mode An n x n adjacency matrix that represents the modes of edges in an FCM
#' @param upper An n x n adjacency matrix that represents the upper limits of
#'              edges in an FCM
#'
#' @export
#' @examples
#' make_adj_matrix_w_tfns(
#'  lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#'  mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
#'  upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#' )
make_adj_matrix_w_tfns <- function(lower = matrix(),
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

  if ((!all(lower <= mode) | !all(mode <= upper))) {
    offense_locs <- unique(rbind(which(!lower <= mode, arr.ind = TRUE), which(!mode <= upper, arr.ind = TRUE)))
    offenses_df <- data.frame(
      row = offense_locs[, 1],
      col = offense_locs[, 2],
      lower = apply(offense_locs, 1,function(locs) lower[locs[1], locs[2]]),
      mode = apply(offense_locs, 1,function(locs) mode[locs[1], locs[2]]),
      upper = apply(offense_locs, 1,function(locs) upper[locs[1], locs[2]])
    )
    rownames(offenses_df) <- NULL
    writeLines("\n\nERROR: Failed to create triangular adj. matrix from input.\nCheck:")
    print(offenses_df)
    stop(
      "All lower values must be less than or equal to mode values which in turn, \n  must be less than or equal to upper values."
    )
  }

  adj_matrix_w_tfns <- as.data.frame(matrix(data = list(0), nrow = size, ncol = size))
  colnames(adj_matrix_w_tfns) <- IDs
  rownames(adj_matrix_w_tfns) <- IDs

  for (i in 1:length(IDs)) {
    for (j in 1:length(IDs)) {
      adj_matrix_w_tfns[[j]][[i]] <- tfn(
        # [[j]][[i]] instead of [[i]][[j]]
        # because this notation is
        # [[col]][[row]] for data.frames
        lower = lower[i, j],
        mode = mode[i, j],
        upper = upper[i, j]
      )
    }
  }

  class(adj_matrix_w_tfns) <- c("adj_matrix_w_tfns", methods::is(adj_matrix_w_tfns))

  adj_matrix_w_tfns
}



#' tfn S3 class
#'
#' @description
#' This class is an organization scheme for Triangular Numbers (See
#' ____, XXXX for example).
#'
#' @details
#' Triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The tfn class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' Use vignette("fcm_w_fcm_w_tfn-class") for more information.
#'
#' @param lower lower limit of a Triangular Number set (the lower value must be less than or equal to the upper value)
#' @param mode the most likely value of a Triangular Number set
#' @param upper upper limit of a Triangular Number set (the upper value must be greater or equal to the lower value)
#'
#' @export
#' @examples
#' tfn(lower = 0, mode = 0.5, upper = 1)
tfn <- function(lower = double(), mode = double(), upper = double()) {
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
    class = "tfn"
  )
}


#' print.tfn
#'
#' @description
#' This improves the readability of the output
#'
#' @details
#' triangular Numbers represent intervals within a possibility space (typically
#' [0, 1] or [-1, 1]) that contains the true value of an object.
#'
#' The tfn class does not perform any operations on its input, rather
#' it checks whether the input follows the defining criteria of triangular numbers
#'
#' tfns are constructed using vctrs::new_vctr instead of the typical call to
#' structure() because ...
#'
#' Use vignette("fcm_w_fcm_w_tfn-class") for more information.
#'
#' @param x a tfn object
#' @param ... additional inputs
#'
#' @export
#' @examples
#' tfn(lower = 0, upper = 1)
print.tfn <- function(x, ...) {
  cat(class(x), ": [", x$lower, ", ", x$mode, ", ", x$upper, "]", sep = "")
}


#' c.tfn
#'
#' @description
#' This forces the output of c() to the equivalent of list() only for inputs of
#' type tfn
#'
#' @details
#' For tfn objects, c() combines all of the lower and upper data into
#' a single tfn object, but list() returns the expected output of a
#' list of distinct tfn objects.
#'
#' @param ... a set of tfn objects
#'
#' @export
#' @examples
#' c(tfn(0, 1), tfn(0.2, 0.5))
c.tfn <- function(...) {
  list(...)
}



#' rtri
#'
#' @description
#' This pulls n samples from a triangular distribution described by shape parameters
#' defined by a lower limit, upper limit, and mode
#'
#' @details
#'
#' Use vignette("fcmcmr-class") for more information.
#'
#' @param lower lower limit or minimum of the sample space
#' @param upper upper limit or maximum of the sample space
#' @param mode peak of the sample space
#' @param n number of samples to draw from the triangular distribution
#'
#' @export
rtri <- function(n = integer(), lower = double(), mode = double(), upper = double()) {
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




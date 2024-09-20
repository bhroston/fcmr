
infer_fcm <- function(adj_matrix = matrix(),
                      initial_state_vector = c(),
                      clamping_vector = c(),
                      activation = "kosko",
                      squashing = "tanh",
                      lambda = 1,
                      max_iter = 100,
                      min_error = 1e-5,
                      CI = 0.95) {

  check_simulation_inputs(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)

  fcm_class <- get_adj_matrices_input_type(adj_matrix)$object_types_in_list[1]
  if (!(fcm_class %in% c("conventional", "ivfn", "tfn"))) {
    stop("Input adj_matrix must be an adjacency matrix with edges represented as
         numeric values, ivfns, or tfns")
  }

  if (fcm_class == "conventional") {
    inference <- infer_conventional_fcm(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  } else if (fcm_class %in% c("ivfn", "tfn")) {
    inference <- infer_ivfn_or_tfn_fcm(adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error, CI)
  }

  inference
}


infer_conventional_fcm <- function(adj_matrix = matrix(),
                                   initial_state_vector = c(),
                                   clamping_vector = c(),
                                   activation = "kosko",
                                   squashing = "tanh",
                                   lambda = 1,
                                   max_iter = 100,
                                   min_error = 1e-5) {

}

infer_ivfn_or_tfn_fcm <- function(adj_matrix = matrix(),
                                  initial_state_vector = c(),
                                  clamping_vector = c(),
                                  activation = "kosko",
                                  squashing = "tanh",
                                  lambda = 1,
                                  max_iter = 100,
                                  min_error = 1e-5,
                                  CI = 0.95) {

}

equalize_baseline_and_scenario_outputs <- function(baseline, scenario) {

}



#' simulate_fcm
#'
#' @description
#' This confers with a baseline simulation of an fcm and a scenario (scenario vector)
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
#' @param IDs A list of names for each node (must have n items). If empty, will use
#' column names of adjacancy matrix (if given).
#'
#' @export
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
    next_state_vector <- calculate_next_conventional_fcm_state_vector(adj_matrix, state_vector, activation, squashing)
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


#' calculate_next_conventional_fcm_state_vector
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
#' @param adj_matrix An n x n adjacency matrix that represents an FCM
#' @param state_vector A list state values at a particular iteration in an fcm simulation
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#'
#' @export
calculate_next_conventional_fcm_state_vector <- function(adj_matrix = matrix(),
                                                         state_vector = c(),
                                                         activation = c("kosko", "modified-kosko", "rescale"),
                                                         squashing = c("sigmoid", "tanh")) {
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
    if (squashing != "sigmoid") {
      stop(
        paste0(
          "     !!!Please use the sigmoid squashing function with the rescale activation function!!!

          The rescale activation function is designed to optimize performance
          with the sigmoid squashing function. Results are unreliable if
          using a different squashing function.\n",

          "\n          Input squashing function: ", squashing)
      )
    }
    next_state_vector <- (2*state_vector - 1) %*% adj_matrix + (2*state_vector - 1)
  }
  next_state_vector
}


#' calculate_next_fuzzy_set_fcm_state_vector
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
#' @param fuzzy_set_adj_matrix An n x n adjacency matrix that represents an FCM
#' and every element in the matrix is a tfn.
#' @param fuzzy_set_state_vector A list of state values as tfn objects
#' @param crisp_state_vector A list of state values as defuzzed tfn objects
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'rescale'.
#' @param fcm_class Class of edges in fuzzy_set_adj_matrix. Either 'ivfn' or 'tfn'
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



#' defuzz
#'
#' @description
#' Convert a fuzzy number to a crisp value. For IVFNs, return the average of the
#' upper and lower bounds. For TFNs, return the average of the lower bound, the
#' mode, and the upper bound.
#'
#' @param fuzzy_number A fuzzy number object. Either an ivfn or tfn
#'
#' @export
defuzz <- function(fuzzy_number) {
  fuzzy_class <- methods::is(fuzzy_number)
  if (fuzzy_class == "ivfn") {
    crisp_value <- (fuzzy_number$lower + fuzzy_number$upper)/2
  } else if (fuzzy_class == "tfn") {
    crisp_value <- (fuzzy_number$lower + fuzzy_number$mode + fuzzy_number$upper)/3
  } else {
    stop("Cannot defuzz input fuzzy_number. Must be either an ivfn or tfn")
  }
  crisp_value
}


#' convert_element_to_ivfn_or_tfn_if_numeric
#'
#' @description
#' This checks whether the input element is an ordinary number or a triangular number.
#' If it is an ivfn or tfn, it returns the input, but if it is a numeric type
#' object (ordinary number), it will convert that number into an ivfn or tfn
#'
#' @param element An element in a matrix
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



#' clean_simulation_output
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
clean_simulation_output <- function(output_obj, IDs) {
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

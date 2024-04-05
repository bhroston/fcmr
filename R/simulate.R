
#' simulate_fgcm
#'
#' @description
#' This simulates how an fgcm reacts to an initial state vector
#'
#' @details
#' X
#'
#' #' Use vignette("fgcmr-class") for more information.
#'
#' @param grey_adj_matrix An adjacency matrix representing an fgcm, made up of
#'                        grey_number type objects
#' @param initial_state_vector A list of initial state values for an fgcm
#' @param iter Number of iterations to perform
#' @param f_squashing Squashing function
#'
#' @export
#' @examples
#' NULL
simulate_fgcm <- function(grey_adj_matrix = matrix(),
                          initial_state_vector = vector(),
                          iter = 10,
                          f_squashing = "sigmoid") {
  if (methods::is(grey_adj_matrix) != "grey_number") {
    stop("Input grey_adj_matrix should be of type grey_number. Use
         simulate_fcm for a numeric matrix or simulate_ifcm for an
         intuitionistic_fuzzy_set matrix.")
  }

  if (!identical(unique(dim(grey_adj_matrix)), length(initial_state_vector))) {
    stop("The dimensions of input grey_adj_matrix must be equivalent to the
         number of entries (length) in input initial_state_vector (and vice versa")
  }

  state_vectors_by_iter <- vector(mode = "list", length = iter + 1)
  names(state_vectors_by_iter) <- c("initial", paste0("iter", as.character(1:iter)))
  state_vectors_by_iter[[1]] <- initial_state_vector

  for (i in 2:(iter + 1)) {
    next_state_vector <- rep(list(0), length(initial_state_vector))
    current_state_vector <- lapply(
      state_vectors_by_iter[[i - 1]],
      function(x) {
        if (!identical(methods::is(x), "grey_number") & identical(class(x), "numeric")) {
          grey_number(lower = x, upper = x)
        } else {
          x
        }
      }
    )
    for (j in 1:ncol(grey_adj_matrix)) {
      grey_adj_matrix_col_vector <- grey_adj_matrix[, j]
      grey_adj_matrix_col_vector <- lapply(
        grey_adj_matrix_col_vector,
        function(x) {
          if (!identical(methods::is(x), "grey_number") & identical(class(x), "numeric")) {
            grey_number(lower = x, upper = x)
          } else {
            x
          }
        }
      )

      adj_matrix_and_state_vector_products <- mapply(
        function(g1, g2) {
          grey_number(
            lower = min(g1$lower*g2$lower, g1$lower*g2$upper,
                        g1$upper*g2$lower, g1$upper*g2$upper
                        ),
            upper = max(g1$lower*g2$lower, g1$lower*g2$upper,
                        g1$upper*g2$lower, g1$upper*g2$upper
                        )
          )
        },
        g1 = current_state_vector,
        g2 = grey_adj_matrix_col_vector
      )
      adj_matrix_and_state_vector_dot_product <- grey_number(
        lower = sum(unlist(adj_matrix_and_state_vector_products[1, ])),
        upper = sum(unlist(adj_matrix_and_state_vector_products[2, ]))
      )

      next_state_vector_value_lower <- current_state_vector[[j]]$lower + adj_matrix_and_state_vector_dot_product$lower
      next_state_vector_value_upper <- current_state_vector[[j]]$upper + adj_matrix_and_state_vector_dot_product$upper

      if (f_squashing == "sigmoid") {
        next_state_vector[[j]] <- grey_number(
          lower = 1/(1 + exp(-next_state_vector_value_lower)),
          upper = 1/(1 + exp(-next_state_vector_value_upper))
        )
      }

      #next_state_vector[[j]] <- grey_number(
      #  lower = current_state_vector[[j]]$lower + adj_matrix_and_state_vector_dot_product$lower,
      #  upper = current_state_vector[[j]]$upper + adj_matrix_and_state_vector_dot_product$upper
      #)
    }

    state_vectors_by_iter[[i]] <- next_state_vector

    #next_state_vector <- "x"
    #normalized_next_state_vector <- "x"
    #state_vectors_by_iter[[i]] <- normalized_next_state_vector
  }

  state_vectors_by_iter
}

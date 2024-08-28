
#' help_fcmconfr_inputs
#'
#' @description
#' A short description...
#'
#' @export
help_fcmconfr_inputs <- function() {
  shiny::runApp(appDir = system.file('shiny', package = 'fcmconfr'))

  #browser()

  user_input <- list(
    adj_matrix_list = session_variables$adj_matrix_list,
    mc_samples = session_variables$monte_carlo_samples,
    include_zeroes_in_mc_sampling = session_variables$include_zeroes_in_mc_sampling,
    include_zeroes_in_aggregation_fun = session_variables$include_zeroes_in_aggregation
  )

  assign(
    x = "fcmconfr_inputs",
    user_input,
    .GlobalEnv
  )


  # fcmconfr(
  #   adj_matrices <- test_fcms,
  #   samples = 5000,
  #   sampling = "nonparametric",
  #   include_zeroes_in_aggregation = FALSE,
  #   aggregation_fun = "mean",
  #   initial_state_vector <- c(1, 1, 1, 1),
  #   clamping_vector <- c(1, 0, 0, 0),
  #   lambda = 1,
  #   activation = "kosko",
  #   squashing = "sigmoid",
  #   bootstrap_CI = 0.95,
  #   bootstrap_reps = 5000,
  #   bootstrap_draws_per_rep = 5000,
  #   max_iter = 100,
  #   min_error = 1e-5,
  #   n_cores = 2
  # )
  # print(session_variables)
}


#' help_fcmconfr_inputs
#'
#' @description
#' A short description...
#'
#' @export
help_fcmconfr_inputs <- function() {
  shiny::runApp(appDir = system.file('shiny', package = 'fcmconfr'))

  if (!exists("session_variables$clamping_vector")) {
    session_variables$clamping_vector <- rep(0, length(session_variables$initial_state_vector))
  }

  session_variables$include_zeroes_in_mc_sampling <- ifelse(session_variables$include_zeroes_in_mc_sampling == "Yes", TRUE, FALSE)
  session_variables$include_zeroes_in_aggregation <- ifelse(session_variables$include_zeroes_in_aggregation == "Yes", TRUE, FALSE)
  session_variables$aggregation_fun <- tolower(session_variables$aggregation_fun)
  session_variables$bootstrap_inference_means_samples <- ifelse(session_variables$bootstrap_inference_means_samples == "Yes", TRUE, FALSE)
  session_variables$show_progress <- ifelse(session_variables$show_progress == "Yes", TRUE, FALSE)
  paralell = session_variables$parallel <- ifelse(session_variables$parallel == "Yes", TRUE, FALSE)

  #browser()

  # adj_matrix_list = list(matrix()),
  # samples = 1000,
  # include_zeroes_in_aggregation = TRUE,
  # aggregation_fun = c("mean", "median"),
  # initial_state_vector = c(),
  # clamping_vector = c(),
  # activation = c("kosko", "modified-kosko", "rescale"),
  # squashing = c("sigmoid", "tanh"),
  # lambda = 1,
  # max_iter = 100,
  # min_error = 1e-5,
  # bootstrap_inference_means = TRUE,
  # bootstrap_CI = 0.95,
  # bootstrap_reps = 5000,
  # bootstrap_draws_per_rep = 5000,
  # show_progress = TRUE,
  # parallel = TRUE,
  # n_cores = integer(),
  # IDs = c(),
  # include_simulations_in_output = FALSE

  # browser()

  user_input <- list(
    adj_matrix_list = session_variables$adj_matrix_list,
    mc_samples = session_variables$monte_carlo_samples,
    include_zeroes_in_mc_sampling = session_variables$include_zeroes_in_mc_sampling,
    include_zeroes_in_aggregation_fun = session_variables$include_zeroes_in_aggregation,
    aggregation_function = session_variables$aggregation_fun,
    initial_state_vector = session_variables$initial_state_vector,
    clamping_vector = session_variables$clamping_vector,
    activation = session_variables$activation,
    squashing = session_variables$squashing,
    lambda = session_variables$lambda,
    max_iter = session_variables$max_iter,
    min_error = session_variables$min_error,
    bootstrap_inference_means = session_variables$bootstrap_inference_means_samples,
    bootstrap_CI = session_variables$bootstrap_CI,
    bootstrap_reps = session_variables$bootstrap_reps,
    bootstrap_draws_per_rep = session_variables$bootstrap_draws_per_rep,
    show_progress = session_variables$show_progress,
    paralell = session_variables$parallel,
    n_cores = session_variables$n_cores,
    IDs = session_variables$concepts
  )

  assign(
    x = "user_input",
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

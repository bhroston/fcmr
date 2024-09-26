
#' fcmconfr_gui
#'
#' @description
#' A short description...
#'
#' @export
fcmconfr_gui <- function() {
  shiny::runApp(appDir = system.file('shiny', package = 'fcmconfr'))

  if (identical(session_variables$adj_matrix_list, "")) {
    stop("No adj matrix list was selected. Please call gui again for selection.")
  }

  if (!exists("session_variables$clamping_vector")) {
    session_variables$clamping_vector <- rep(0, length(session_variables$initial_state_vector))
  }

  if (!exists("session_variables$fuzzy_set_samples")) {
    session_variables$fuzzy_set_samples <- NA
  }

  session_variables$initial_state_vector <- paste0("c(", paste(session_variables$initial_state_vector, collapse = ", "), ")")
  session_variables$clamping_vector <- paste0("c(", paste(session_variables$clamping_vector, collapse = ", "), ")")
  session_variables$concepts <- paste0("c('", paste(session_variables$concepts, collapse = "', '"), "')")

  cat(
    "user_input <- list(", "\n",
    "  adj_matrices = ", session_variables$adj_matrix_list, ",\n",
    "  # Aggregation and Monte Carlo Sampling", "\n",
    "  aggregation_function = ", paste0("'", session_variables$aggregation_fun, "'"), ",\n",
    "  monte_carlo_sampling_draws = ", session_variables$monte_carlo_samples, ",\n",
    "  # Simulation", "\n",
    "  initial_state_vector = ", session_variables$initial_state_vector, ",\n",
    "  clamping_vector = ", session_variables$clamping_vector, ",\n",
    "  activation = ", paste0("'", session_variables$activation, "'"), ",\n",
    "  squashing = ",  paste0("'", session_variables$squashing, "'"), ",\n",
    "  lambda = ", session_variables$lambda, ",\n",
    "  max_iter = ", session_variables$max_iter, ",\n",
    "  min_error = ", session_variables$min_error, ",\n",
    "  fuzzy_set_samples = ", session_variables$fuzzy_set_samples, ",\n",
    "  # Inference Estimation (bootstrap)", "\n",
    "  inference_estimation_CI = ", session_variables$bootstrap_CI, ",\n",
    "  inference_estimation_bootstrap_reps = ", session_variables$bootstrap_reps, ",\n",
    "  inference_estimation_bootstrap_draws_per_rep = ", session_variables$bootstrap_draws_per_rep, ",\n",
    "  # Runtime Options", "\n",
    "  show_progress = ", session_variables$show_progress, ",\n",
    "  parallel = ", session_variables$parallel, ",\n",
    "  n_cores = ", session_variables$n_cores, ",\n",
    "  # Additional Options", "\n",
    "  include_zero_weighted_edges_in_aggregation_and_mc_sampling = ", session_variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling, ",\n",
    "  include_monte_carlo_FCM_simulations_in_output = TRUE", ",\n",
    "  estimate_inference_CI_w_bootstrap = ", session_variables$bootstrap_inference_means_samples, "\n",
    ")", sep = ""
  )

  # cat(
  #   "fcmconfr", "\n",
  #   "  adj_matrix_list = ", session_variables$adj_matrix_list, ",\n",
  #   "  mc_samples = ", session_variables$monte_carlo_samples, ",\n",
  #   "  include_zeroes_in_mc_samples = ", session_variables$include_zeroes_in_mc_sampling, ",\n",
  #   "  include_zeroes_in_aggregation_fun = ", session_variables$include_zeroes_in_aggregation, ",\n",
  #   "  aggregation_function = ", paste0("'", session_variables$aggregation_fun, "'"), ",\n",
  #   "  initial_state_vector = ", session_variables$initial_state_vector, ",\n",
  #   "  clamping_vector = ", session_variables$clamping_vector, ",\n",
  #   "  activation = ", paste0("'", session_variables$activation, "'"), ",\n",
  #   "  squashing = ",  paste0("'", session_variables$squashing, "'"), ",\n",
  #   "  lambda = ", session_variables$lambda, ",\n",
  #   "  max_iter = ", session_variables$max_iter, ",\n",
  #   "  min_error = ", session_variables$min_error, ",\n",
  #   "  bootstrap_inference_means = ", session_variables$bootstrap_inference_means_samples, ",\n",
  #   "  bootstrap_CI = ", session_variables$bootstrap_CI, ",\n",
  #   "  bootstrap_reps = ", session_variables$bootstrap_reps, ",\n",
  #   "  bootstrap_draws_per_rep = ", session_variables$bootstrap_draws_per_rep, ",\n",
  #   "  show_progress = ", session_variables$show_progress, ",\n",
  #   "  parallel = ", session_variables$parallel, ",\n",
  #   "  n_cores = ", session_variables$n_cores, ",\n",
  #   "  IDs = ", session_variables$concepts, "\n",
  #   ")", sep = ""
  # )


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

  # user_input <- list(
  #   adj_matrix_list = session_variables$adj_matrix_list,
  #   mc_samples = session_variables$monte_carlo_samples,
  #   include_zeroes_in_mc_sampling = session_variables$include_zeroes_in_mc_sampling,
  #   include_zeroes_in_aggregation_fun = session_variables$include_zeroes_in_aggregation,
  #   aggregation_function = session_variables$aggregation_fun,
  #   initial_state_vector = session_variables$initial_state_vector,
  #   clamping_vector = session_variables$clamping_vector,
  #   activation = session_variables$activation,
  #   squashing = session_variables$squashing,
  #   lambda = session_variables$lambda,
  #   max_iter = session_variables$max_iter,
  #   min_error = session_variables$min_error,
  #   bootstrap_inference_means = session_variables$bootstrap_inference_means_samples,
  #   bootstrap_CI = session_variables$bootstrap_CI,
  #   bootstrap_reps = session_variables$bootstrap_reps,
  #   bootstrap_draws_per_rep = session_variables$bootstrap_draws_per_rep,
  #   show_progress = session_variables$show_progress,
  #   paralell = session_variables$parallel,
  #   n_cores = session_variables$n_cores,
  #   IDs = session_variables$concepts
  # )




  # Can't 'assign' in an R package, need to change how the user gets this
  # assign(
  #  x = "user_input",
  #  user_input,
  #  .GlobalEnv
  #)


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

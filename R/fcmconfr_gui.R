
#' fcmconfr_gui
#'
#' @description
#' A short description...
#'
#' @export
fcmconfr_gui <- function() {

  # assignInNamespace(
  #   "collapse_icon",
  #   function() {
  #     bsicons::bs_icon(
  #       "info-circle-fill", class = "collapse-icon", size = NULL
  #     )
  #   },
  #   ns = "bslib"
  # )

  shiny::runApp(appDir = system.file('shiny', package = 'fcmconfr'))

  # browser()

  if (identical(session_variables$adj_matrices, "")) {
    stop("No adj matrix list was selected. Please call gui again for selection.")
  }

  if (!exists("session_variables$clamping_vector")) {
    session_variables$clamping_vector <- rep(0, length(session_variables$initial_state_vector))
  }

  if (!exists("session_variables$aggregation_function")) {
    session_variables$aggregation_function <- "mean"
  }

  if (!exists("session_variables$monte_carlo_samples")) {
    session_variables$monte_carlo_samples <- 1000
  }

  if (!exists("session_variables$fuzzy_set_samples")) {
    session_variables$fuzzy_set_samples <- NA
  }

  if (!exists("session_variables$mc_inference_estimation_CI")) {
    session_variables$mc_inference_estimation_CI <- 0.95
  }

  if (!exists("session_variables$mc_inference_bootstrap_reps")) {
    session_variables$mc_inference_bootstrap_reps <- 1000
  }

  if (!exists("session_variables$mc_inference_bootstrap_draws_per_rep")) {
    session_variables$mc_inference_bootstrap_draws_per_rep <- 1000
  }

  if (!exists("session_variables$n_cores")) {
    session_variables$n_cores <- NA
  }

  if (!exists("session_variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling")) {
    session_variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling <- FALSE
  }

  if (!exists("session_variables$include_monte_carlo_FCM_simulations_in_output")) {
    session_variables$include_monte_carlo_FCM_simulations_in_output <- FALSE
  }

  session_variables$initial_state_vector <- paste0("c(", paste(session_variables$initial_state_vector, collapse = ", "), ")")
  session_variables$clamping_vector <- paste0("c(", paste(session_variables$clamping_vector, collapse = ", "), ")")
  session_variables$concepts <- paste0("c('", paste(session_variables$concepts, collapse = "', '"), "')")

  cat(
    "user_input <- list(", "\n",
    "  adj_matrices = ", session_variables$adj_matrices, ",\n",
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
    "  inference_estimation_CI = ", session_variables$mc_inference_estimation_CI, ",\n",
    "  inference_estimation_bootstrap_reps = ", session_variables$mc_inference_bootstrap_reps, ",\n",
    "  inference_estimation_bootstrap_draws_per_rep = ", session_variables$mc_inference_bootstrap_draws_per_rep, ",\n",
    "  # Runtime Options", "\n",
    "  show_progress = ", session_variables$show_progress, ",\n",
    "  parallel = ", session_variables$parallel, ",\n",
    "  n_cores = ", session_variables$n_cores, ",\n",
    "  # Additional Options", "\n",
    "  perform_aggregate_analysis = ", session_variables$perform_aggregation, ",\n",
    "  perform_monte_carlo_analysis = ", session_variables$perform_monte_carlo, ",\n",
    "  perform_monte_carlo_inference_bootstrap_analysis = ", session_variables$perform_inference_bootstrap, ",\n",
    "  include_zero_weighted_edges_in_aggregation_and_mc_sampling = ", session_variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling, ",\n",
    "  include_monte_carlo_FCM_simulations_in_output = ",  session_variables$include_monte_carlo_FCM_simulations_in_output, "\n",
    ")", sep = ""
  )
}



################################################################################
# utils-plot.fcmconfr.R
#
# This functions is involved with fcmconfr_gui()
#
#   - print.fcmconfr_gui_input
#
################################################################################


#' Print fcmconfr_gui_input
#'
#' @description
#' This function prints a clean, copy-and-paste-able call to \code{\link{fcmconfr}}
#' with the inputs selected by the user in the shiny app.
#'
#' @param x an fcmconfr object
#' @param ... additional inputs
#'
#' @returns A pasteable output to the console of a call to fcmconfr with the
#' selected variables
#'
#' @export
#' @examples
#' NULL
print.fcmconfr_gui_input <- function(x, ...) {

  performed_aggregation <- x$perform_aggregation
  performed_mc <- x$perform_monte_carlo
  performed_bootstrap <- x$perform_inference_bootstrap

  if (x$parallel == "FALSE") {
    x$n_cores <- "integer()"
  }

  if (performed_aggregation & !performed_mc & !performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Aggregation", "\n",
      "  agg_function = ", paste0("'", x$aggregation_fun, "'"), ",\n",
      "  # Simulation", "\n",
      "  initial_state_vector = ", x$initial_state_vector, ",\n",
      "  clamping_vector = ", x$clamping_vector, ",\n",
      "  activation = ", paste0("'", x$activation, "'"), ",\n",
      "  squashing = ",  paste0("'", x$squashing, "'"), ",\n",
      "  lambda = ", x$lambda, ",\n",
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
      "  # Runtime Options", "\n",
      "  show_progress = ", x$show_progress, ",\n",
      "  # Additional Options", "\n",
      "  run_agg_calcs = ", x$perform_aggregation, ",\n",
      "  run_mc_calcs = ", x$perform_monte_carlo, ",\n",
      "  run_ci_calcs = ", x$perform_inference_bootstrap, ",\n",
      "  include_zeroes_in_sampling = ", x$include_zeroes_in_sampling, "\n",
      ")", sep = ""
    )
  } else if (performed_aggregation & performed_mc & !performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Aggregation and Monte Carlo Sampling", "\n",
      "  agg_function = ", paste0("'", x$aggregation_fun, "'"), ",\n",
      "  num_mc_fcms = ", x$monte_carlo_samples, ",\n",
      "  # Simulation", "\n",
      "  initial_state_vector = ", x$initial_state_vector, ",\n",
      "  clamping_vector = ", x$clamping_vector, ",\n",
      "  activation = ", paste0("'", x$activation, "'"), ",\n",
      "  squashing = ",  paste0("'", x$squashing, "'"), ",\n",
      "  lambda = ", x$lambda, ",\n",
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
      "  # Runtime Options", "\n",
      "  show_progress = ", x$show_progress, ",\n",
      "  parallel = ", x$parallel, ",\n",
      "  n_cores = ", x$n_cores, ",\n",
      "  # Additional Options", "\n",
      "  run_agg_calcs = ", x$perform_aggregation, ",\n",
      "  run_mc_calcs = ", x$perform_monte_carlo, ",\n",
      "  run_ci_calcs = ", x$perform_inference_bootstrap, ",\n",
      "  include_zeroes_in_sampling = ", x$include_zeroes_in_sampling, ",\n",
      "  include_sims_in_output = ",  x$include_sims_in_output, "\n",
      ")", sep = ""
    )
  } else if (performed_aggregation & performed_mc & performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Aggregation and Monte Carlo Sampling", "\n",
      "  agg_function = ", paste0("'", x$aggregation_fun, "'"), ",\n",
      "  num_mc_fcms = ", x$monte_carlo_samples, ",\n",
      "  # Simulation", "\n",
      "  initial_state_vector = ", x$initial_state_vector, ",\n",
      "  clamping_vector = ", x$clamping_vector, ",\n",
      "  activation = ", paste0("'", x$activation, "'"), ",\n",
      "  squashing = ",  paste0("'", x$squashing, "'"), ",\n",
      "  lambda = ", x$lambda, ",\n",
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
      "  # Inference Estimation (bootstrap)", "\n",
      "  ci_centering_function = ", paste0("'", x$mc_ci_centering_function, "'"), ",\n",
      "  confidence_interval = ", x$mc_confidence_interval, ",\n",
      "  num_ci_bootstraps = ", x$mc_inference_bootstrap_reps, ",\n",
      "  # Runtime Options", "\n",
      "  show_progress = ", x$show_progress, ",\n",
      "  parallel = ", x$parallel, ",\n",
      "  n_cores = ", x$n_cores, ",\n",
      "  # Additional Options", "\n",
      "  run_agg_calcs = ", x$perform_aggregation, ",\n",
      "  run_mc_calcs = ", x$perform_monte_carlo, ",\n",
      "  run_ci_calcs = ", x$perform_inference_bootstrap, ",\n",
      "  include_zeroes_in_sampling = ", x$include_zeroes_in_sampling, ",\n",
      "  include_sims_in_output = ",  x$include_sims_in_output, "\n",
      ")", sep = ""
    )
  } else if (!performed_aggregation & performed_mc & !performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Monte Carlo Sampling", "\n",
      "  num_mc_fcms = ", x$monte_carlo_samples, ",\n",
      "  # Simulation", "\n",
      "  initial_state_vector = ", x$initial_state_vector, ",\n",
      "  clamping_vector = ", x$clamping_vector, ",\n",
      "  activation = ", paste0("'", x$activation, "'"), ",\n",
      "  squashing = ",  paste0("'", x$squashing, "'"), ",\n",
      "  lambda = ", x$lambda, ",\n",
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
      "  # Runtime Options", "\n",
      "  show_progress = ", x$show_progress, ",\n",
      "  parallel = ", x$parallel, ",\n",
      "  n_cores = ", x$n_cores, ",\n",
      "  # Additional Options", "\n",
      "  run_agg_calcs = ", x$perform_aggregation, ",\n",
      "  run_mc_calcs = ", x$perform_monte_carlo, ",\n",
      "  run_ci_calcs = ", x$perform_inference_bootstrap, ",\n",
      "  include_zeroes_in_sampling = ", x$include_zeroes_in_sampling, ",\n",
      "  include_sims_in_output = ",  x$include_sims_in_output, "\n",
      ")", sep = ""
    )
  } else if (!performed_aggregation & performed_mc & performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Monte Carlo Sampling", "\n",
      "  num_mc_fcms = ", x$monte_carlo_samples, ",\n",
      "  # Simulation", "\n",
      "  initial_state_vector = ", x$initial_state_vector, ",\n",
      "  clamping_vector = ", x$clamping_vector, ",\n",
      "  activation = ", paste0("'", x$activation, "'"), ",\n",
      "  squashing = ",  paste0("'", x$squashing, "'"), ",\n",
      "  lambda = ", x$lambda, ",\n",
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
      "  # Inference Estimation (bootstrap)", "\n",
      "  ci_centering_function = ", paste0("'", x$mc_ci_centering_function, "'"), ",\n",
      "  confidence_interval = ", x$mc_confidence_interval, ",\n",
      "  num_ci_bootstraps = ", x$mc_inference_bootstrap_reps, ",\n",
      "  # Runtime Options", "\n",
      "  show_progress = ", x$show_progress, ",\n",
      "  parallel = ", x$parallel, ",\n",
      "  n_cores = ", x$n_cores, ",\n",
      "  # Additional Options", "\n",
      "  run_agg_calcs = ", x$perform_aggregation, ",\n",
      "  run_mc_calcs = ", x$perform_monte_carlo, ",\n",
      "  run_ci_calcs = ", x$perform_inference_bootstrap, ",\n",
      "  include_zeroes_in_sampling = ", x$include_zeroes_in_sampling, ",\n",
      "  include_sims_in_output = ",  x$include_sims_in_output, "\n",
      ")", sep = ""
    )
  } else if (!performed_aggregation & !performed_mc & !performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Simulation", "\n",
      "  initial_state_vector = ", x$initial_state_vector, ",\n",
      "  clamping_vector = ", x$clamping_vector, ",\n",
      "  activation = ", paste0("'", x$activation, "'"), ",\n",
      "  squashing = ",  paste0("'", x$squashing, "'"), ",\n",
      "  lambda = ", x$lambda, ",\n",
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
      "  # Runtime Options", "\n",
      "  show_progress = ", x$show_progress, ",\n",
      "  # Additional Options", "\n",
      "  run_agg_calcs = ", x$perform_aggregation, ",\n",
      "  run_mc_calcs = ", x$perform_monte_carlo, ",\n",
      "  run_ci_calcs = ", x$perform_inference_bootstrap, "\n",
      ")", sep = ""
    )
  }
}

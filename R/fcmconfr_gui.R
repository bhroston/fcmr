
#' fcmconfr_gui
#'
#' @description
#' This function opens a shiny application window designed to help users
#' determine inputs for the \code{\link{fcmconfr}} function.
#'
#' @returns On exit, this function outputs a copy-and-paste-able sample script
#' to call \code{\link{fcmconfr}} with the selected inputs.
#'
#' @export
#' @examples
#' NULL
fcmconfr_gui <- function() {

  # These bslib and shinyWidgets calls are only here to be acknowledged in
  # R CMD Check, they have no impact on the rest of the function and can be
  # ignored.
  bslib::versions()
  shinyWidgets::animations

  shiny_env_check <- 1

  shiny::runApp(appDir = system.file('shiny', package = 'fcmconfr'))

  if (identical(fcmconfr_gui_input$adj_matrices, "")) {
    stop("No adj matrix list was selected. Please call gui again for selection.")
  }

  if (!exists("fcmconfr_gui_input$clamping_vector")) {
    fcmconfr_gui_input$clamping_vector <- rep(0, length(fcmconfr_gui_input$initial_state_vector))
  }

  if (!exists("fcmconfr_gui_input$aggregation_function")) {
    fcmconfr_gui_input$aggregation_function <- "mean"
  }

  if (!exists("fcmconfr_gui_input$monte_carlo_samples")) {
    fcmconfr_gui_input$monte_carlo_samples <- 1000
  }

  if (!exists("fcmconfr_gui_input$fuzzy_set_samples")) {
    fcmconfr_gui_input$fuzzy_set_samples <- 1000
  }

  if (!exists("fcmconfr_gui_input$mc_inference_estimation_function")) {
    fcmconfr_gui_input$mc_inference_estimation_function <- "mean"
  }

  if (!exists("fcmconfr_gui_input$mc_inference_estimation_CI")) {
    fcmconfr_gui_input$mc_inference_estimation_CI <- 0.95
  }

  if (!exists("fcmconfr_gui_input$mc_inference_bootstrap_reps")) {
    fcmconfr_gui_input$mc_inference_bootstrap_reps <- 1000
  }

  if (!exists("fcmconfr_gui_input$mc_inference_bootstrap_draws_per_rep")) {
    fcmconfr_gui_input$mc_inference_bootstrap_draws_per_rep <- 1000
  }

  if (!exists("fcmconfr_gui_input$n_cores")) {
    fcmconfr_gui_input$n_cores <- 1
  }

  if (!exists("fcmconfr_gui_input$include_zero_weighted_edges_in_aggregation_and_mc_sampling")) {
    fcmconfr_gui_input$include_zero_weighted_edges_in_aggregation_and_mc_sampling <- FALSE
  }

  if (!exists("fcmconfr_gui_input$include_monte_carlo_FCM_simulations_in_output")) {
    fcmconfr_gui_input$include_monte_carlo_FCM_simulations_in_output <- FALSE
  }

  fcmconfr_gui_input$initial_state_vector <- paste0("c(", paste(fcmconfr_gui_input$initial_state_vector, collapse = ", "), ")")
  fcmconfr_gui_input$clamping_vector <- paste0("c(", paste(fcmconfr_gui_input$clamping_vector, collapse = ", "), ")")
  fcmconfr_gui_input$concepts <- paste0("c('", paste(fcmconfr_gui_input$concepts, collapse = "', '"), "')")

  cat(
    "fcmconfr(", "\n",
    "  adj_matrices = ", fcmconfr_gui_input$adj_matrices, ",\n",
    "  # Aggregation and Monte Carlo Sampling", "\n",
    "  aggregation_function = ", paste0("'", fcmconfr_gui_input$aggregation_fun, "'"), ",\n",
    "  monte_carlo_sampling_draws = ", fcmconfr_gui_input$monte_carlo_samples, ",\n",
    "  # Simulation", "\n",
    "  initial_state_vector = ", fcmconfr_gui_input$initial_state_vector, ",\n",
    "  clamping_vector = ", fcmconfr_gui_input$clamping_vector, ",\n",
    "  activation = ", paste0("'", fcmconfr_gui_input$activation, "'"), ",\n",
    "  squashing = ",  paste0("'", fcmconfr_gui_input$squashing, "'"), ",\n",
    "  lambda = ", fcmconfr_gui_input$lambda, ",\n",
    "  max_iter = ", fcmconfr_gui_input$max_iter, ",\n",
    "  min_error = ", fcmconfr_gui_input$min_error, ",\n",
    "  fuzzy_set_samples = ", fcmconfr_gui_input$fuzzy_set_samples, ",\n",
    "  # Inference Estimation (bootstrap)", "\n",
    "  inference_estimation_function = ", fcmconfr_gui_input$mc_inference_estimation_function, ",\n",
    "  inference_estimation_CI = ", fcmconfr_gui_input$mc_inference_estimation_CI, ",\n",
    "  inference_estimation_bootstrap_reps = ", fcmconfr_gui_input$mc_inference_bootstrap_reps, ",\n",
    "  inference_estimation_bootstrap_draws_per_rep = ", fcmconfr_gui_input$mc_inference_bootstrap_draws_per_rep, ",\n",
    "  # Runtime Options", "\n",
    "  show_progress = ", fcmconfr_gui_input$show_progress, ",\n",
    "  parallel = ", fcmconfr_gui_input$parallel, ",\n",
    "  n_cores = ", fcmconfr_gui_input$n_cores, ",\n",
    "  # Additional Options", "\n",
    "  perform_aggregate_analysis = ", fcmconfr_gui_input$perform_aggregation, ",\n",
    "  perform_monte_carlo_analysis = ", fcmconfr_gui_input$perform_monte_carlo, ",\n",
    "  perform_monte_carlo_inference_bootstrap_analysis = ", fcmconfr_gui_input$perform_inference_bootstrap, ",\n",
    "  include_zero_weighted_edges_in_aggregation_and_mc_sampling = ", fcmconfr_gui_input$include_zero_weighted_edges_in_aggregation_and_mc_sampling, ",\n",
    "  include_monte_carlo_FCM_simulations_in_output = ",  fcmconfr_gui_input$include_monte_carlo_FCM_simulations_in_output, "\n",
    ")", sep = ""
  )

}


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
  cat(
    "fcmconfr(", "\n",
    "  adj_matrices = ", x$adj_matrices, ",\n",
    "  # Aggregation and Monte Carlo Sampling", "\n",
    "  aggregation_function = ", paste0("'", x$aggregation_fun, "'"), ",\n",
    "  monte_carlo_sampling_draws = ", x$monte_carlo_samples, ",\n",
    "  # Simulation", "\n",
    "  initial_state_vector = ", x$initial_state_vector, ",\n",
    "  clamping_vector = ", x$clamping_vector, ",\n",
    "  activation = ", paste0("'", x$activation, "'"), ",\n",
    "  squashing = ",  paste0("'", x$squashing, "'"), ",\n",
    "  lambda = ", x$lambda, ",\n",
    "  max_iter = ", x$max_iter, ",\n",
    "  min_error = ", x$min_error, ",\n",
    "  fuzzy_set_samples = ", x$fuzzy_set_samples, ",\n",
    "  # Inference Estimation (bootstrap)", "\n",
    "  inference_estimation_function = ", x$mc_inference_estimation_function, ",\n",
    "  inference_estimation_CI = ", x$mc_inference_estimation_CI, ",\n",
    "  inference_estimation_bootstrap_reps = ", x$mc_inference_bootstrap_reps, ",\n",
    "  inference_estimation_bootstrap_draws_per_rep = ", x$mc_inference_bootstrap_draws_per_rep, ",\n",
    "  # Runtime Options", "\n",
    "  show_progress = ", x$show_progress, ",\n",
    "  parallel = ", x$parallel, ",\n",
    "  n_cores = ", x$n_cores, ",\n",
    "  # Additional Options", "\n",
    "  perform_aggregate_analysis = ", x$perform_aggregation, ",\n",
    "  perform_monte_carlo_analysis = ", x$perform_monte_carlo, ",\n",
    "  perform_monte_carlo_inference_bootstrap_analysis = ", x$perform_inference_bootstrap, ",\n",
    "  include_zero_weighted_edges_in_aggregation_and_mc_sampling = ", x$include_zero_weighted_edges_in_aggregation_and_mc_sampling, ",\n",
    "  include_monte_carlo_FCM_simulations_in_output = ",  x$include_monte_carlo_FCM_simulations_in_output, "\n",
    ")", sep = ""
  )
}

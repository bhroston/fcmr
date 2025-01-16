
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

  # This is here to pass the user-selected inputs from the gui into this
  # function's (fcmconfr_gui) environment, which can then be passed to the
  # console.
  shiny_env_check <- 1

  shiny::runApp(appDir = system.file(file.path('shiny', 'fcmconfr_gui'), package = 'fcmconfr'))
  # shiny_app <- shiny::shinyAppDir(appDir = system.file('shiny', package = 'fcmconfr'))
  #shiny::runGadget(shiny_app, viewer = shiny::dialogViewer("", width = 1400, height = 1200))

  fcmconfr_gui_vars <- names(fcmconfr_gui_input)

  if (identical(fcmconfr_gui_input$adj_matrices, "")) {
    stop(cli::format_error(c(
      "x" = "Error: No adj matrix list was selected.",
      "+++++ Please call fcmconfr_gui() again to make a selection."
    )))
  }

  if (!("clamping_vector" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$clamping_vector <- rep(0, length(fcmconfr_gui_input$initial_state_vector))
  }

  if (!("aggregation_function" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$aggregation_function <- "mean"
  }

  if (!("monte_carlo_samples" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$monte_carlo_samples <- 1000
  }

  if (!("mc_inference_estimation_function" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$mc_inference_estimation_function <- "mean"
  }

  if (!("mc_inference_estimation_CI" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$mc_inference_estimation_CI <- 0.95
  }

  if (!("mc_inference_bootstrap_reps" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$mc_inference_bootstrap_reps <- 1000
  }

  if (!("n_cores" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$n_cores <- 2
  }

  if (!("include_zero_weighted_edges_in_aggregation_and_mc_sampling" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$include_zero_weighted_edges_in_aggregation_and_mc_sampling <- FALSE
  }

  if (!("include_monte_carlo_FCM_simulations_in_output" %in% fcmconfr_gui_vars)) {
    fcmconfr_gui_input$include_monte_carlo_FCM_simulations_in_output <- FALSE
  }

  fcmconfr_gui_input$initial_state_vector <- paste0("c(", paste(fcmconfr_gui_input$initial_state_vector, collapse = ", "), ")")
  fcmconfr_gui_input$clamping_vector <- paste0("c(", paste(fcmconfr_gui_input$clamping_vector, collapse = ", "), ")")
  fcmconfr_gui_input$concepts <- paste0("c('", paste(fcmconfr_gui_input$concepts, collapse = "', '"), "')")

  fcmconfr_gui_inputs <- structure(
    .Data = fcmconfr_gui_input,
    class = "fcmconfr_gui_input"
  )

  fcmconfr_gui_inputs
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

  performed_aggregation <- x$perform_aggregation
  performed_mc <- x$perform_monte_carlo
  performed_bootstrap <- x$perform_inference_bootstrap

  # browser()

  if (performed_aggregation & !performed_mc & !performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Aggregation", "\n",
      "  aggregation_function = ", paste0("'", x$aggregation_fun, "'"), ",\n",
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
      "  perform_aggregate_analysis = ", x$perform_aggregation, ",\n",
      "  perform_monte_carlo_analysis = ", x$perform_monte_carlo, ",\n",
      "  perform_monte_carlo_inference_bootstrap_analysis = ", x$perform_inference_bootstrap, ",\n",
      "  include_zero_weighted_edges_in_aggregation_and_mc_sampling = ", x$include_zero_weighted_edges_in_aggregation_and_mc_sampling, "\n",
      ")", sep = ""
    )
  } else if (performed_aggregation & performed_mc & !performed_bootstrap) {
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
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
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
  } else if (performed_aggregation & performed_mc & performed_bootstrap) {
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
      "  point_of_inference = ", paste0("'", x$point_of_inference, "'"), ",\n",
      "  max_iter = ", x$max_iter, ",\n",
      "  min_error = ", x$min_error, ",\n",
      "  # Inference Estimation (bootstrap)", "\n",
      "  inference_estimation_function = ", paste0("'", x$mc_inference_estimation_function, "'"), ",\n",
      "  inference_estimation_CI = ", x$mc_inference_estimation_CI, ",\n",
      "  inference_estimation_bootstrap_reps = ", x$mc_inference_bootstrap_reps, ",\n",
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
  } else if (!performed_aggregation & performed_mc & !performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Monte Carlo Sampling", "\n",
      "  monte_carlo_sampling_draws = ", x$monte_carlo_samples, ",\n",
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
      "  perform_aggregate_analysis = ", x$perform_aggregation, ",\n",
      "  perform_monte_carlo_analysis = ", x$perform_monte_carlo, ",\n",
      "  perform_monte_carlo_inference_bootstrap_analysis = ", x$perform_inference_bootstrap, ",\n",
      "  include_zero_weighted_edges_in_aggregation_and_mc_sampling = ", x$include_zero_weighted_edges_in_aggregation_and_mc_sampling, ",\n",
      "  include_monte_carlo_FCM_simulations_in_output = ",  x$include_monte_carlo_FCM_simulations_in_output, "\n",
      ")", sep = ""
    )
  } else if (!performed_aggregation & performed_mc & performed_bootstrap) {
    cat(
      "fcmconfr(", "\n",
      "  adj_matrices = ", x$adj_matrices, ",\n",
      "  # Monte Carlo Sampling", "\n",
      "  monte_carlo_sampling_draws = ", x$monte_carlo_samples, ",\n",
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
      "  inference_estimation_function = ", paste0("'", x$mc_inference_estimation_function, "'"), ",\n",
      "  inference_estimation_CI = ", x$mc_inference_estimation_CI, ",\n",
      "  inference_estimation_bootstrap_reps = ", x$mc_inference_bootstrap_reps, ",\n",
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
      "  perform_aggregate_analysis = ", x$perform_aggregation, ",\n",
      "  perform_monte_carlo_analysis = ", x$perform_monte_carlo, ",\n",
      "  perform_monte_carlo_inference_bootstrap_analysis = ", x$perform_inference_bootstrap, ",\n",
      ")", sep = ""
    )
  }
}

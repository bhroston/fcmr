

#' fcmconfr
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' Use vignette("fcmconfr") for more information.
#' @param adj_matrices A list of adjacency matrices (n x n) representing FCMs. This
#' can also be an individual adjacency matrix.Adj. Matrices can be conventional FCMs,
#' FCMs with edge weights as Interval Value Fuzzy Numbers (IVFNs) or FCMs with edge
#' weights as Triangular Fuzzy Numbers (TFNs)
#' @param aggregation_function Aggregate the adj. matrices into a single FCM by taking
#' either the mean or median of the edge weights for edges included in multiple maps
#' @param monte_carlo_sampling_draws The number of FCMs to generate via monte carlo
#' sampling from the input adj. matrices
#' @param initial_state_vector A list state values at the start of an fcm simulation
#' @param clamping_vector A list of values representing specific actions taken to
#' control the behavior of an FCM. Specifically, non-zero values defined in this vector
#' will remain constant throughout the entire simulation as if they were "clamped" at those values.
#' @param activation The activation function to be applied. Must be one of the following:
#' 'kosko', 'modified-kosko', or 'papageorgiou'.
#' @param squashing A squashing function to apply. Must be one of the following:
#' 'bivalent', 'saturation', 'trivalent', 'tanh', or 'sigmoid'.
#' @param lambda A numeric value that defines the steepness of the slope of the
#' squashing function when tanh or sigmoid are applied
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param fuzzy_set_samples The size (n) of the distributions represented by IVFNs or TFNs (only
#' used when IVFNs or TFNs in adj_matrices input)
#' @param inference_estimation_CI The confidence interval to estimate for the inferences
#' of each concept across all monte carlo FCMs (via bootstrap)
#' @param inference_estimation_bootstrap_reps The number of bootstraps to perform in
#' estimating the confidence interval for the inferences of each concept across all monte
#' carlo FCMs
#' @param inference_estimation_bootstrap_draws_per_rep The number of draws to sample
#' (with replacement) from the estimates for the inferences of each concept across
#' all monte carlo FCMs per bootstrap
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param include_zero_weighted_edges_in_aggregation_and_mc_sampling TRUE/FALSE
#' Whether to incorporate zeroes as intentionally-defined edge weights or ignore
#' them when aggregating adj. matrices and sampling for monte carlo FCMs
#' @param include_monte_carlo_FCM_simulations_in_output TRUE/FALSE
#' Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
#' about the size of the output of fcmconfr (simulations are necessary and will run regardless)
#' @param estimate_mc_inference_CI_w_bootstrap TRUE/FALSE
#' Whether to estimate confidence intervals for the inferences of each concept across
#' all monte carlo FCMs via bootstrap. Switch to FALSE if concerned about runtime
#' and/or want to perform bootstrapping manually.
#'
#' @export
fcmconfr <- function(adj_matrices = list(matrix()),
                     # Aggregation and Monte Carlo Sampling
                     aggregation_function = c("mean", "median"),
                     monte_carlo_sampling_draws = 1000,
                     # Simulation
                     initial_state_vector = c(),
                     clamping_vector = c(),
                     activation = c("kosko", "modified-kosko", "rescale"),
                     squashing = c("sigmoid", "tanh"),
                     lambda = 1,
                     max_iter = 100,
                     min_error = 1e-5,
                     fuzzy_set_samples = 1000,
                     # Inference Estimation (bootstrap)
                     inference_estimation_CI = 0.95,
                     inference_estimation_bootstrap_reps = 5000,
                     inference_estimation_bootstrap_draws_per_rep = 5000,
                     # Runtime Options
                     show_progress = TRUE,
                     parallel = TRUE,
                     n_cores = integer(),
                     # Additional Options
                     # do monte_carlo
                     # do aggregate
                     include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
                     include_monte_carlo_FCM_simulations_in_output = TRUE,
                     estimate_mc_inference_CI_w_bootstrap = TRUE) {

  # Perform Checks ----
  #browser()
  adj_matrices_dims <- lapply(adj_matrices, dim)
  if (length(unique(unlist(adj_matrices_dims))) > 1) {
    stop("Input Validation Error: Input adj. matrices are either different sizes or contain non-square matrices
       !! Call standardize_adj_matrices() to standardize the size of the list of adj. matrices")
  }
  n_nodes <- unique(unlist(adj_matrices_dims))

  adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices)
  fcm_class <- adj_matrices_input_type$object_types_in_list[1]
  if (!adj_matrices_input_type$adj_matrices_input_is_list) {
    adj_matrices <- list(adj_matrices)
  }

  if (identical(initial_state_vector, c())) {
    warning("No initial_state_vector input given. Assuming all nodes have an initial state of 1.")
    initial_state_vector <- rep(1, n_nodes)
  }
  if (identical(clamping_vector, c())) {
    warning("No clamping_vector input given. Assuming no values are clamped.")
    clamping_vector <- rep(0, n_nodes)
  }

  # Perform checks
  checks <- lapply(adj_matrices, check_simulation_inputs, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error)
  identified_concepts <- unique(lapply(adj_matrices, colnames))
  if (length(identified_concepts) != 1) {
    stop("All input adjacency matrices must have the same concepts")
  } else {
    concepts <- unlist(identified_concepts)
  }

  # ----

  # Confirm necessary packages are available. If not, warn user and change run options
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)

  # Aggregation and Monte Carlo Simulations ----


  if (length(adj_matrices) == 1) {
    stop("Input adj_matrices must be a list of multiple adjacency matrices. Use infer_fcm for a single adjacency matrix.")
  }

  # Build aggregate adj_matrix
  aggregate_adj_matrix <- aggregate_fcms(adj_matrices, aggregation_function, include_zero_weighted_edges_in_aggregation_and_mc_sampling)
  # Infer aggregate adj_matrix
  aggregate_fcm_inference <- infer_fcm(aggregate_adj_matrix$adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, max_iter, min_error, fuzzy_set_samples)

  # Build monte carlo models
  mc_adj_matrices <- build_monte_carlo_fcms(adj_matrices, monte_carlo_sampling_draws, include_zero_weighted_edges_in_aggregation_and_mc_sampling, show_progress)
  mc_adj_matrices <- lapply(mc_adj_matrices,
                            function(sampled_adj_matrix) {
                              colnames(sampled_adj_matrix) <- concepts
                              rownames(sampled_adj_matrix) <- concepts
                              sampled_adj_matrix
                            })

  # Infer monte carlo models with clamping
  mc_simulations <- infer_monte_carlo_fcm_set(
    mc_adj_matrices = mc_adj_matrices,
    initial_state_vector = initial_state_vector,
    clamping_vector = clamping_vector,
    activation = activation,
    squashing = squashing,
    lambda = lambda,
    max_iter = max_iter,
    min_error = min_error,
    fuzzy_set_samples = fuzzy_set_samples,
    parallel = parallel,
    show_progress = show_progress,
    n_cores = n_cores
  )

  if (estimate_mc_inference_CI_w_bootstrap) {
    CIs_of_mc_simulation_inferences <- get_mc_simulations_inference_CIs_w_bootstrap(mc_simulations$inference, inference_estimation_CI, inference_estimation_bootstrap_reps, inference_estimation_bootstrap_draws_per_rep, parallel, n_cores, show_progress)
  }

  # ----

  # Organize Output
  env_variables <- as.list(environment())
  confr_output <- organize_fcmconfr_output(env_variables)

  confr_output
}



#' organize_fcmconfr_output
#'
#' @description
#' This arranges fcmconfr inputs and outputs into a neatly-arranged list of lists
#'
#' @details
#' A standardized way to organize the fcmconfr into easily navigable data structures.
#'
#' Use vignette("fcmconfr-class") for more information.
#'
#' @param ... additional inputs; typically environmental variables
#'
#' @export
organize_fcmconfr_output <- function(...) {
  variables <- as.list(...)

  params <- list(
    fcm_class = variables$fcm_class,
    adj_matrices = variables$adj_matrices,
    aggregation_and_mc_sampling_opts = list(aggregation_function = variables$aggregation_function,
                                            monte_carlo_sampling_draws = variables$monte_carlo_sampling_draws),
    simulation_opts = list(initial_state_vector = variables$initial_state_vector,
                           clamping_vector = variables$clamping_vector,
                           activation = variables$activation,
                           squashing = variables$squashing,
                           lambda = variables$lambda,
                           max_iter = variables$max_iter,
                           min_error = variables$min_error,
                           fuzzy_set_samples = variables$fuzzy_set_samples),
    runtime_opts = list(parallel = variables$parallel,
                        n_cores = variables$n_cores,
                        show_progress = variables$show_progress),
    additional_opts = list(include_zero_weighted_edges_in_aggregation_and_mc_sampling = variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling,
                           include_monte_carlo_FCM_simulations_in_output = variables$include_monte_carlo_FCM_simulations_in_output,
                           estimate_mc_inference_CI_w_bootstrap = variables$estimate_mc_inference_CI_w_bootstrap)
  )

  fcmconfr_output <- structure(
    .Data = list(
      inference = variables$mc_simulations$inference,
      params = params
    ),
    class = "fcmconfr"
  )

  if (variables$estimate_mc_inference_CI_w_bootstrap) {
    fcmconfr_output$params$mc_confidence_intervals_opts = list(inference_estimation_CI = variables$inference_estimation_CI,
                                                               inference_estimation_bootstrap_reps = variables$inference_estimation_bootstrap_reps,
                                                               inference_estimation_bootstrap_draws_per_rep = variables$inference_estimation_bootstrap_draws_per_rep)
    fcmconfr_output$bootstrap = list(
      CI_by_node = variables$CIs_of_mc_simulation_inferences$CI_by_node,
      bootstrap_means = variables$CIs_of_mc_simulation_inferences$bootstrap_means
    )
  }

  fcmconfr_output
}



#' print.fcmconfr
#'
#' @description
#' This improves the readability of the fcmconfr
#'
#' @details
#' Show the objects listed in the fcmconfr output $inference and $params, as well
#' as $bootstrap if present in output. Additionally, this prints descriptions/summaries
#' of objects within each sub-list like inference_opts, bootstrap_input_opts, etc.
#'
#' Use vignette("fcm-class") for more information.
#'
#' @param x an fgcm_simulation object
#' @param ... additional inputs
#'
#' @export
print.fcmconfr <- function(x, ...) {
  n_input_fcm <- length(x$params$adj_matrices)
  n_mc_sims <- x$params$aggregation_and_mc_sampling_opts$monte_carlo_sampling_draws

  if ("bootstrap" %in% names(x)) {
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - Inferences of ", n_mc_sims, " fcm constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" - CI_by_node: ", x$params$mc_confidence_intervals_opts$inference_estimation_CI, "% CI of means of inference\n"),
        paste0(" - bootstrap_means: ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_reps, " actualizations of the avg inference of ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_draws_per_rep, " draws with replacement"),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        "\n  - additional_opts"
    )
  } else {
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - Inferences of ", n_mc_sims, " fcm constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        "\n  - additional_opts"
    )
  }
}


#' plot.fcmconfr
#'
#' @export
plot.fcmconfr <- function(x, ...) {
  if (x$fcm_class == "conventional") {
    fcm_class_subtitle <- "Conventional FCM"
  }

  if (x$params$simulation_opts$squashing == "tanh" | x$params$simulation_opts$squashing == "trivalent") {
    domain_min <- -1
    domain_max <- 1
  } else {
    domain_min <- 0
    domain_max <- 1
  }

  if (x$fcm_class == "conventional") {
    input_fcms_inferences_longer <- tidyr::pivot_longer(x$inferences$input_fcms$inferences, cols = 2:ncol(x$inferences$input_fcms$inferences))
    input_fcms_inferences_longer$analysis_source <- "input"
    mc_inferences_longer <- tidyr::pivot_longer(x$inferences$monte_carlo_fcms$all_inferences, cols = 1:ncol(x$inferences$monte_carlo_fcms$all_inferences))
    mc_inferences_longer$analysis_source <- "mc"
    aggregate_inferences_longer <- tidyr::pivot_longer(x$inferences$aggregate_fcm$inferences, cols = 1:ncol(x$inferences$aggregate_fcm$inferences))
    aggregate_inferences_longer$analysis_source <- "aggregate"

    results <- rbind(input_fcms_inferences_longer[, -1], mc_inferences_longer)
    results$analysis_source <- factor(results$analysis_source, levels = c("mc", "input"))

    fcmconfr_plot <- ggplot() +
      geom_boxplot(data = input_fcms_inferences_longer, aes(x = value, y = name, color = analysis_source), position = position_nudge(y = 0.18), width = 0.1) +
      geom_boxplot(data = mc_inferences_longer, aes(x = value, y = name, color = analysis_source), position = position_nudge(y = -0.18), width = 0.4, fill = "lightgrey") +
      geom_point(data = aggregate_inferences_longer, aes(x = value, y = name, shape = analysis_source), position = position_nudge(y = -0.18), size = 2, fill = "darkgray") +
      geom_point(data = results, aes(x = value, y = name, alpha = analysis_source), position = position_jitterdodge(jitter.width = 0.3), stroke = NA) +
      ggtitle("FCMconfR Inferences", subtitle = fcm_class_subtitle) +
      scale_x_continuous(breaks = seq(domain_min, domain_max, by = 0.2), limits = c(domain_min, domain_max), expand = c(0, 0)) +
      #scale_fill_manual(values = c(input = "white", mc = "lightgrey"), labels = c(input = "Input", mc = "Monte Carlo")) +
      scale_color_manual(values = c(input = "darkgrey", mc = "black"), labels = c(input = "Input", mc = "Monte Carlo")) +
      scale_shape_manual(values = c("triangle"), labels = c("Aggregate")) +
      scale_alpha_manual(values = c(input = 0.5, mc = 0.1)) +
      guides(alpha = "none", color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
      xlab("Inference Value") +
      ylab("Concept (Node)") +
      theme_classic() +
      theme(
        plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.spacing = unit(0.001, 'cm'),
        legend.text = element_text(size = 10)
      )
  }

  fcmconfr_plot


  # ggplot() +
  #   geom_boxplot(data = input_fcms_inferences_longer, aes(x = value, y = name), width = 0.5, fill = "white") +
  #   geom_jitter(data = input_fcms_inferences_longer, aes(x = value, y = name), height = 0.1, alpha = 0.1) +
  #   ggtitle("Title") +
  #   scale_x_continuous(breaks = seq(domain_min, domain_max, by = 0.2), limits = c(domain_min, domain_max), expand = c(0, 0)) +
  #   xlab("Inference Value") +
  #   ylab("Concept (Node)") +
  #   theme_classic() +
  #   theme(
  #     plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
  #     plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
  #     axis.title = element_text(size = 12),
  #     axis.title.x = element_text(margin = margin(t = 10)),
  #     axis.title.y = element_text(margin = margin(r = 10)),
  #     axis.text = element_text(size = 12)
  #   )



  # # barplot_fig <- barplot(
  # #   apply(x$inferences$input_fcms$inferences[,2:ncol(x$inferences$input_fcms$inferences)], 2, median)
  # # )
  # boxplot(
  #   x$inferences$input_fcms$inferences[,2:ncol(x$inferences$input_fcms$inferences)],
  #   ylim = c(domain_min, domain_max),
  #   col = NULL,
  #   las = 1,
  #   horizontal = TRUE
  # )
  # points(y = factor(input_fcms_inferences_longer$name), x = input_fcms_inferences_longer$value)
  # ggplot() +
  #   geom_col(data = input_fcms_inferences_longer, aes(x = name, y = value))
  #
  # ggplot() +
  #   geom_jitter(data = input_fcms_inferences_longer, aes(x = name, y = value)) +
  #   geom_crossbar(data = bootstrapped_means, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), fill = "red", color = "red", size = 0.1)
}

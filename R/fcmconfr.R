

#' fcmconfr
#'
#' @description
#' This is the primary function of the fcmconfr package. This function performs
#' up to three different analyses of a set of (or an individual) input FCM(s).
#' Call \code{\link{fcmconfr_gui}}. for assistance with inputs.
#'
#' \enumerate{
#'    \item FCM Simulation: Simulate the (raw/unmodified) input FCMs to estimate
#'          their inferences. Inferences are calculated by comparing the simulation
#'          output of a particular scenario with the simulation output of the
#'          baseline (i.e. the natural behavior of the network without external
#'          perturbation), (Ozesmi & Ozesmi, 2003) and help distinguish
#'          decision-making impacts with the structural or
#'          expected, steady-state of the system.
#'    \item Aggregate Analysis: Generate an aggregate adj. matrix from a list of
#'          adj. matrices. FCM aggregation works by calculating the mean/median
#'          edge weight for all edges across the input adj. matrices (i.e. the
#'          mean/median of the edge weight connecting A -> B across all maps,
#'          the mean/median of the edge weight connecting B -> C across all
#'          maps, and so on) (Aminpour et al., 2020). The user may dictate
#'          whether to incorporate 0-valued edge weights in the mean/median
#'          calculations.
#'    \item Monte Carlo Analysis: Generate empirical adj. matrices whose edge
#'          weights are drawn from edge weights from the input FCMs using
#'          monte carlo sampling methods.These empirical FCMs are then simulated
#'          in bulk using the FCM Simulation method previously described. This
#'          outputs a dataframe of inferences across all empirical FCMs that
#'          represent the possibility space of inferences representative of the
#'          collective. This function also uses bootstrapping methods to
#'          estimate confidence intervals about the inferences generated from
#'          the empirical FCMs (may be toggled off if wanting to reduce runtime).
#' }
#'
#' This function accepts three different types of FCMs which differ in how they
#' represent edge weights. Note: All input FCMs must be the same type (i.e. a
#' list of input FCMs must all be of one type or another, but cannot contain
#' multiple types of FCMs in the same input set).
#'
#' \enumerate{
#'    \item Conventional FCMs: These represent edge weights as fuzzy numbers and
#'          represent FCMs in thir traditional form (Stylios, 1997)
#'    \item Interval-Valued Fuzzy Number (IVFN) FCMs: An extension of Conventional
#'          FCMs, these represent edge weights as interval-valued fuzzy numbers
#'          (IVFNs) which describe a range of values and are defined by a lower
#'          and upper bound (e.g. [0.2, 0.8] represents the set of values
#'          between 0.2 and 0.8 which may be represented as a uniform distribution
#'          via runif(N, min = 0.2, max = 0.8)) (Moore & Lodwick, 2003)
#'    \item Triangular Fuzzy Number (TFN) FCMs: An extension of Conventional
#'          FCMs, these represent edge weights as triangular fuzzy numbers (TFNs)
#'          which describe a bounded range of values with a greater density of
#'          values at some point in between, such that the probability density
#'          function of the distribution appears triangular (Moore & Lodwick, 2003).
#'          These are defined by their lower and upper bounds as well as a mode
#'          value, and can be called within this package via the
#'          \code{\link{rtriangular_dist}} function or with other packages
#'          such as EnvStats::rtri().
#' }
#'
#' @references \insertRef{ozesmiParticipatoryApproachEcosystem2003}{fcmconfr}
#' @references \insertRef{aminpourWisdomStakeholderCrowds2020}{fcmconfr}
#' @references \insertRef{styliosIntroducingTheoryFuzzy1997}{fcmconfr}
#' @references \insertRef{mooreIntervalAnalysisFuzzy2003}{fcmconfr}
#'
#' @details
#' Call \code{\link{fcmconfr_gui}}. for assistance with inputs.
#'
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
#' @param point_of_inference The point along the simulation time-series to be
#' identified as the inference. Must be one of the following: 'peak' or 'final'
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param inference_estimation_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param inference_estimation_CI The confidence interval to estimate for the inferences
#' of each concept across all monte carlo FCMs (via bootstrap)
#' @param inference_estimation_bootstrap_reps The number of bootstraps to perform in
#' estimating the confidence interval for the inferences of each concept across all monte
#' carlo FCMs
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param perform_aggregate_analysis TRUE/FALSE Run the code to generate and simulate an aggregate FCM generated from the input adj_matrices
#' @param perform_monte_carlo_analysis TRUE/FALSE Run the code to generate and simulate monte carlo-generated FCM sampled from the input adj_matrices
#' @param perform_monte_carlo_inference_bootstrap_analysis TRUE/FALSE Run the code to estimate the 95 percent CI bounds about the means of the inferences of the monte carlo adj matrices
#' @param include_zero_weighted_edges_in_aggregation_and_mc_sampling TRUE/FALSE Whether to incorporate zeroes as intentionally-defined edge weights or ignore
#' them when aggregating adj. matrices and sampling for monte carlo FCMs
#' @param include_monte_carlo_FCM_simulations_in_output TRUE/FALSE Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
#' about the size of the output of fcmconfr (simulations are necessary and will run regardless)
#'
#' @importFrom Rdpack reprompt
#'
#' @returns A list of outputs generated from the input_fcms simulations,
#'          aggregate_fcm analysis, and monte_carlo_fcms analysis. Bootstrap
#'          estimates of inferences from monte carlo analysis are included, as
#'          well as function inputs.
#'
#' @export
#' @example man/examples/ex-fcmconfr.R
fcmconfr <- function(adj_matrices = list(matrix()),
                     # Aggregation and Monte Carlo Sampling
                     aggregation_function = c("mean", "median"),
                     monte_carlo_sampling_draws = 1000,
                     # Simulation
                     initial_state_vector = c(),
                     clamping_vector = c(),
                     activation = c("kosko", "modified-kosko", "rescale"),
                     squashing = c("sigmoid", "tanh", "bivalent", "saturation", "trivalent"),
                     lambda = 1,
                     point_of_inference = c("peak", "final"),
                     max_iter = 100,
                     min_error = 1e-5,
                     # Inference Estimation (bootstrap)
                     inference_estimation_function = c("mean", "median"),
                     inference_estimation_CI = 0.95,
                     inference_estimation_bootstrap_reps = 5000,
                     # Runtime Options
                     show_progress = TRUE,
                     parallel = TRUE,
                     n_cores = integer(),
                     # Additional Options
                     perform_aggregate_analysis = TRUE,
                     perform_monte_carlo_analysis = TRUE,
                     perform_monte_carlo_inference_bootstrap_analysis = TRUE,
                     include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
                     include_monte_carlo_FCM_simulations_in_output = TRUE) {

  # Perform input checks ----
  checks <- check_fcmconfr_inputs(
    adj_matrices,
    # Aggregation and Monte Carlo Sampling
    aggregation_function, monte_carlo_sampling_draws,
    # Simulation
    initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error,
    # Inference Estimation (bootstrap)
    inference_estimation_function, inference_estimation_CI, inference_estimation_bootstrap_reps,
    # Runtime Options
    show_progress, parallel, n_cores, perform_aggregate_analysis,
    # Additional Options
    perform_monte_carlo_analysis, perform_monte_carlo_inference_bootstrap_analysis, include_zero_weighted_edges_in_aggregation_and_mc_sampling, include_monte_carlo_FCM_simulations_in_output
  )
  fcm_class <- checks$fcm_class
  adj_matrix <- checks$adj_matrices
  concepts <- checks$concept_names
  aggregation_function <- checks$aggregation_function
  initial_state_vector <- checks$initial_state_vector
  clamping_vector <- checks$clamping_vector
  activation <- checks$activation
  squashing <- checks$squashing
  point_of_inference <- checks$point_of_inference
  inference_estimation_function <- checks$inference_estimation_function
  show_progress <- checks$show_progress
  parallel <- checks$parallel
  perform_aggregate_analysis <- checks$perform_aggregate_analysis
  perform_monte_carlo_analysis <- checks$perform_monte_carlo_analysis
  perform_monte_carlo_inference_bootstrap_analysis <- checks$perform_monte_carlo_inference_bootstrap_analysis
  # ----

  # Individual Adj. Matrices Simulations ----
  print("Simulating Input FCMs")
  if (show_progress) {
    individual_adj_matrices_inferences <- pbapply::pblapply(adj_matrices, infer_fcm, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error)
  } else {
    individual_adj_matrices_inferences <- lapply(adj_matrices, infer_fcm, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error)
  }

  if (fcm_class == "conventional") {
    individual_adj_matrices_inferences_df <- do.call(rbind, lapply(individual_adj_matrices_inferences, function(inference) inference$inference))
    individual_adj_matrices_inferences_df <- cbind(input = paste0("adj_matrix_", 1:length(adj_matrices)), individual_adj_matrices_inferences_df)
  } else if (fcm_class %in% c("ivfn", "tfn")) {
    individual_adj_matrices_inferences_df <- lapply(individual_adj_matrices_inferences, function(inference) inference$inference)
    names(individual_adj_matrices_inferences_df) <- paste0("adj_matrix_", 1:length(adj_matrices))
  }
  names(individual_adj_matrices_inferences) <- paste0("adj_matrix_", 1:length(adj_matrices))
  # ----

  # Aggregation Analysis ----
  if (perform_aggregate_analysis) {
    # Build aggregate adj_matrix
    aggregate_adj_matrix <- aggregate_fcms(adj_matrices, aggregation_function, include_zero_weighted_edges_in_aggregation_and_mc_sampling)
    # Infer aggregate adj_matrix
    aggregate_fcm_inference <- infer_fcm(aggregate_adj_matrix$adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error)
  }

  # Monte Carlo Analysis
  if (perform_monte_carlo_analysis) {
    # Build monte carlo models
    mc_adj_matrices <- build_monte_carlo_fcms(adj_matrices, monte_carlo_sampling_draws, include_zero_weighted_edges_in_aggregation_and_mc_sampling, show_progress)
    mc_adj_matrices <- lapply(
      mc_adj_matrices,
      function(sampled_adj_matrix) {
        colnames(sampled_adj_matrix) <- concepts
        rownames(sampled_adj_matrix) <- concepts
        sampled_adj_matrix
      })

    mc_inferences <- infer_monte_carlo_fcm_set(
      mc_adj_matrices = mc_adj_matrices,
      initial_state_vector = initial_state_vector,
      clamping_vector = clamping_vector,
      activation = activation,
      squashing = squashing,
      lambda = lambda,
      point_of_inference = point_of_inference,
      max_iter = max_iter,
      min_error = min_error,
      parallel = parallel,
      show_progress = show_progress,
      n_cores = n_cores,
      include_simulations_in_output = include_monte_carlo_FCM_simulations_in_output
    )

    if (perform_monte_carlo_inference_bootstrap_analysis) {
      CIs_of_expected_values_of_mc_simulation_inferences <- get_mc_simulations_inference_CIs_w_bootstrap(mc_inferences$inference, inference_estimation_function, inference_estimation_CI, inference_estimation_bootstrap_reps, parallel, n_cores, show_progress)
    }
  }
  # ----

  # Organize Output
  env_variables <- as.list(environment())
  fcmconfr_output <- organize_fcmconfr_output(env_variables)

  fcmconfr_output
}


#' [INTENDED FOR DEVELOPER USE ONLY] Check fcmconfr Inputs
#'
#' @description
#' Confirm that all inputs will work with the \code{\link{fcmconfr}} function and return
#' appropriate error messages where necessary
#'
#' @details
#' INTENDED FOR DEVELOPER USE ONLY
#'
#' This checks that all inputs for the \code{\link{fcmconfr}} function are of an appropriate
#' format, and also fills in missing inputs for initial_state_vector, clamping_vector,
#' and IDs when appropriate.
#'
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
#' @param point_of_inference The point along the simulation time-series to be
#' identified as the inference. Must be one of the following: 'peak' or 'final'
#' @param max_iter The maximum number of iterations to run if the minimum error value is not achieved
#' @param min_error The lowest error (sum of the absolute value of the current state
#' vector minus the previous state vector) at which no more iterations are necessary
#' and the simulation will stop
#' @param inference_estimation_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param inference_estimation_CI The confidence interval to estimate for the inferences
#' of each concept across all monte carlo FCMs (via bootstrap)
#' @param inference_estimation_bootstrap_reps The number of bootstraps to perform in
#' estimating the confidence interval for the inferences of each concept across all monte
#' carlo FCMs
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param perform_aggregate_analysis TRUE/FALSE Run the code to generate and simulate an aggregate FCM generated from the input adj_matrices
#' @param perform_monte_carlo_analysis TRUE/FALSE Run the code to generate and simulate monte carlo-generated FCM sampled from the input adj_matrices
#' @param perform_monte_carlo_inference_bootstrap_analysis TRUE/FALSE Run the code to estimate the 95 percent CI bounds about the means of the inferences of the monte carlo adj matrices
#' @param include_zero_weighted_edges_in_aggregation_and_mc_sampling TRUE/FALSE Whether to incorporate zeroes as intentionally-defined edge weights or ignore
#' them when aggregating adj. matrices and sampling for monte carlo FCMs
#' @param include_monte_carlo_FCM_simulations_in_output TRUE/FALSE Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
#' about the size of the output of fcmconfr (simulations are necessary and will run regardless)
#'
#' @returns A list of resolved inputs to pass to \code{\link{fcmconfr}}
#'
#' @export
check_fcmconfr_inputs <- function(adj_matrices = list(matrix()),
                                  # Aggregation and Monte Carlo Sampling
                                  aggregation_function = c("mean", "median"),
                                  monte_carlo_sampling_draws = 1000,
                                  # Simulation
                                  initial_state_vector = c(),
                                  clamping_vector = c(),
                                  activation = c("kosko", "modified-kosko", "rescale"),
                                  squashing = c("sigmoid", "tanh", "bivalent", "saturation", "trivalent"),
                                  lambda = 1,
                                  point_of_inference = c("peak", "final"),
                                  max_iter = 100,
                                  min_error = 1e-5,
                                  # Inference Estimation (bootstrap)
                                  inference_estimation_function = c("mean", "median"),
                                  inference_estimation_CI = 0.95,
                                  inference_estimation_bootstrap_reps = 5000,
                                  # Runtime Options
                                  show_progress = TRUE,
                                  parallel = TRUE,
                                  n_cores = integer(),
                                  # Additional Options
                                  perform_aggregate_analysis = TRUE,
                                  perform_monte_carlo_analysis = TRUE,
                                  perform_monte_carlo_inference_bootstrap_analysis = TRUE,
                                  include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
                                  include_monte_carlo_FCM_simulations_in_output = TRUE) {

  # Perform Checks ----
  adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices)
  fcm_class <- adj_matrices_input_type$fcm_class
  if (!adj_matrices_input_type$adj_matrices_input_is_list) {
    adj_matrices <- list(adj_matrices)
  }

  # Check Additional Options ----
  if (!is.logical(perform_aggregate_analysis)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var perform_aggregate_analysis} must be logical (TRUE/FALSE)",
      "+++++> Input {.var perform_aggregate_analysis} was {perform_aggregate_analysis}"
    )))
  }
  if (!is.logical(perform_monte_carlo_analysis)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var perform_monte_carlo_analysis} must be logical (TRUE/FALSE)",
      "+++++> Input {.var perform_monte_carlo_analysis} was {perform_monte_carlo_analysis}"
    )))
  }
  if (!is.logical(perform_monte_carlo_inference_bootstrap_analysis)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var perform_monte_carlo_inference_bootstrap_analysis} must be logical (TRUE/FALSE)",
      "+++++> Input {.var perform_monte_carlo_inference_bootstrap_analysis} was {perform_monte_carlo_inference_bootstrap_analysis}"
    )))
  }
  if (!is.logical(include_zero_weighted_edges_in_aggregation_and_mc_sampling)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var include_zero_weighted_edges_in_aggregation_and_mc_sampling} must be logical (TRUE/FALSE)",
      "+++++> Input {.var include_zero_weighted_edges_in_aggregation_and_mc_sampling} was {include_zero_weighted_edges_in_aggregation_and_mc_sampling}"
    )))
  }
  if (!is.logical(include_monte_carlo_FCM_simulations_in_output)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var include_monte_carlo_FCM_simulations_in_output} must be logical (TRUE/FALSE)",
      "+++++> Input {.var include_monte_carlo_FCM_simulations_in_output} was {include_monte_carlo_FCM_simulations_in_output}"
    )))
  }
  # ----

  # Confirm Monte Carlo and Bootstrap Function Calls are Viable ----
  if (fcm_class == "conventional" & length(adj_matrices) == 1 & (perform_monte_carlo_analysis | perform_aggregate_analysis)) {
    warning(cli::format_warning(c(
      "!" = "Warning: Cannot aggregate or generate monte carlo samples from a single (conventional) adj. matrix",
      "~~~~~ Skipping aggregate analysis; i.e. setting {.var perform_aggregate_analysis} to FALSE",
      "~~~~~ Skipping monte carlo analysis; i.e. setting {.var perform_monte_carlo_analysis} to FALSE"
    )))
    perform_monte_carlo_analysis = FALSE
    perform_aggregate_analysis = FALSE
  }
  else if ((fcm_class %in% c("ivfn", "tfn")) & length(adj_matrices) == 1 & (perform_aggregate_analysis)) {
    warning(cli::format_warning(c(
      "!" = "Warning: Cannot generate aggregate fcm from a single adj. matrix",
      "~~~~~ Skipping aggregate analysis; i.e. setting {.var perform_aggregate_analysis} to FALSE"
    )))
    perform_aggregate_analysis = FALSE
  }

  if (!perform_monte_carlo_analysis & perform_monte_carlo_inference_bootstrap_analysis) {
    warning(cli::format_warning(c(
      "!" = "Warning: Cannot estimate CIs of monte carlo inferences if monte carlo analysis is not being performed",
      "~~~~~ Skipping CI bound estimation; i.e. setting {.var perform_monte_carlo_inference_bootstrap_analysis} to FALSE"
    )))
    perform_monte_carlo_inference_bootstrap_analysis <- FALSE
  }

  if (perform_monte_carlo_analysis & perform_monte_carlo_inference_bootstrap_analysis) {
    if (identical(inference_estimation_function, c("mean", "median"))) {
      warning(cli::format_warning(c(
        "!" = "Warning: No {.var inference_estimation_function} given",
        "~~~~~ Assuming {.var inference_estimation_function} is 'mean'"
      )))
    }
    inference_estimation_function <- 'mean'
  }
  # ----

  # Check adj_matrices ----
  adj_matrices_dims <- lapply(adj_matrices, dim)
  if (length(unique(unlist(adj_matrices_dims))) > 1) {
    stop(cli::format_error(c(
      "x" = "Error: {.var adj_matrices} are either different sizes or contain non-square matrices",
      "+++++> Call standardize_adj_matrices() to standardize the sizes of {.var adj. matrices}"
    )))
  }
  n_nodes <- unique(unlist(adj_matrices_dims))
  dummy_adj_matrix <- matrix(0, n_nodes, n_nodes)

  identified_concepts <- unique(lapply(adj_matrices, colnames))
  if (length(identified_concepts) != 1) {
    stop(cli::format_error(c(
      "x" = "Error: {.var adj_matrices} must have the same concepts",
      "+++++> Call standardize_adj_matrices() to standardize concepts across {.var adj. matrices}"
    )))
  } else {
    concept_names <- unlist(identified_concepts)
  }

  if (identical(adj_matrices_input_type$object_types_in_list, c("conventional", "sparseMatrix"))) {
    adj_matrices <- lapply(adj_matrices, as.matrix)
    warning(cli::format_warning(c(
      "!" = "Warning: Changed {.var adj_matrices} from sparseMatrix to an ordinary matrix (i.e. using as.matrix)"
    )))
  }
  # ----

  # Check Simulation Inputs ----
  sim_checks <- check_simulation_inputs(dummy_adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error)
  initial_state_vector <- sim_checks$initial_state_vector
  clamping_vector <- sim_checks$clamping_vector
  activation <- sim_checks$activation
  squashing <- sim_checks$squashing
  point_of_inference <- sim_checks$point_of_inference
  # ----

  # Aggregation ----
  if (perform_aggregate_analysis) {
    if (identical(aggregation_function, c("mean", "median"))) {
      warning(cli::format_warning(c(
        "!" = "Warning: No {.var aggregation_function} given",
        "~~~~~ Assuming {.var aggregation_function} is 'mean'"
      )))
      aggregation_function <- "mean"
    }
    if (!(aggregation_function %in% c("mean", "median"))) {
      stop(cli::format_error(c(
        "x" = "Error: {.var aggregation_function} must be one of the following: 'mean' or 'median'",
        "+++++> Input {.var aggregation_function} was {aggregation_function}"
      )))
    }
  }
  # ----

  # Monte Carlo ----
  if (perform_monte_carlo_analysis) {
    if (!is.numeric(monte_carlo_sampling_draws)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var monte_carlo_sampling_draws} must be a positive integer",
        "+++++ Input {.var monte_carlo_sampling_draws} was {monte_carlo_sampling_draws}"
      )))
    }
    if (!(monte_carlo_sampling_draws == round(monte_carlo_sampling_draws))) {
      stop(cli::format_error(c(
        "x" = "Error: {.var monte_carlo_sampling_draws} must be a positive integer",
        "+++++ Input {.var monte_carlo_sampling_draws} was {monte_carlo_sampling_draws}"
      )))
    }
    if (monte_carlo_sampling_draws <= 0) {
      stop(cli::format_error(c(
        "x" = "Error: {.var monte_carlo_sampling_draws} must be a positive integer",
        "+++++ Input {.var monte_carlo_sampling_draws} was {monte_carlo_sampling_draws}"
      )))
    }
  }
  # ----

  # Check Monte Carlo and Bootstrap Inputs ----
  if (perform_monte_carlo_inference_bootstrap_analysis) {
    mcbs_checks <- check_monte_carlo_bootstrap_inputs(inference_estimation_function, inference_estimation_CI, inference_estimation_bootstrap_reps, parallel, n_cores, show_progress)
    inference_estimation_function <- mcbs_checks$inference_estimation_function
  }
  # ----

  # Confirm necessary packages are available. If not, warn user and change run options; Also Check n_cores ----
  show_progress <- check_if_local_machine_has_access_to_show_progress_functionalities(parallel, show_progress)
  parallel <- check_if_local_machine_has_access_to_parallel_processing_functionalities(parallel, show_progress)
  if (!is.numeric(n_nodes)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var n_nodes} must be a positive integer",
      "+++++ Input {.var n_nodes} was {n_nodes}"
    )))
  }
  if (!(n_nodes == round(n_nodes))) {
    stop(cli::format_error(c(
      "x" = "Error: {.var n_nodes} must be a positive integer",
      "+++++ Input {.var n_nodes} was {n_nodes}"
    )))
  }
  if (n_nodes <= 0) {
    stop(cli::format_error(c(
      "x" = "Error: {.var n_nodes} must be a positive integer",
      "+++++ Input {.var n_nodes} was {n_nodes}"
    )))
  }
  # ----

  list(
    fcm_class = fcm_class,
    adj_matrices = adj_matrices,
    concept_names = concept_names,
    aggregation_function = aggregation_function,
    initial_state_vector = initial_state_vector,
    clamping_vector = clamping_vector,
    activation = activation,
    squashing = squashing,
    point_of_inference = point_of_inference,
    inference_estimation_function = inference_estimation_function,
    show_progress = show_progress,
    parallel = parallel,
    perform_aggregate_analysis = perform_aggregate_analysis,
    perform_monte_carlo_analysis = perform_monte_carlo_analysis,
    perform_monte_carlo_inference_bootstrap_analysis = perform_monte_carlo_inference_bootstrap_analysis
  )
}



#' [INTENDED FOR DEVELOPER USE ONLY] Organize fcmconfr Output
#'
#' @description
#' This arranges fcmconfr inputs and outputs into a neatly-arranged list of lists,
#' and critically, neatly-arranged data structures to clean code written in
#' the \code{\link{fcmconfr}} function.
#'
#' @details
#' [INTENDED FOR DEVELOPER USE ONLY]
#'
#' @param ... additional inputs; typically environmental variables
#'
#' @returns An organzed list output of fcmconfr
#'
#' @export
#' @examples
#' NULL
organize_fcmconfr_output <- function(...) {
  variables <- as.list(...)

  fcmconfr_output <- structure(
    .Data = list(
      fcm_class = variables$fcm_class,
      inferences = list(
        input_fcms = list(
          inferences = variables$individual_adj_matrices_inferences_df,
          simulations = variables$individual_adj_matrices_inferences
        )
      ),
      params = list(
        fcm_class = variables$fcm_class,
        adj_matrices = variables$adj_matrices,
        simulation_opts = list(initial_state_vector = variables$initial_state_vector,
                               clamping_vector = variables$clamping_vector,
                               activation = variables$activation,
                               squashing = variables$squashing,
                               lambda = variables$lambda,
                               max_iter = variables$max_iter,
                               min_error = variables$min_error),
        additional_opts = list(perform_monte_carlo_inference_bootstrap_analysis = variables$perform_monte_carlo_inference_bootstrap_analysis,
                               perform_aggregate_analysis = variables$perform_aggregate_analysis,
                               perform_monte_carlo_analysis = variables$perform_monte_carlo_analysis)
      )
    ),
    class = "fcmconfr"
  )

  if (variables$perform_aggregate_analysis) {
    fcmconfr_output$aggregate_adj_matrix <- variables$aggregate_adj_matrix
    fcmconfr_output$inferences$aggregate_fcm = list(
      inferences = variables$aggregate_fcm_inference$inference,
      simulation = variables$aggregate_fcm_inference
    )
    fcmconfr_output$params$aggregation_function = variables$aggregation_function
    fcmconfr_output$params$additional_opts <- list(
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling,
      perform_monte_carlo_inference_bootstrap_analysis = variables$perform_monte_carlo_inference_bootstrap_analysis,
      perform_aggregate_analysis = variables$perform_aggregate_analysis,
      perform_monte_carlo_analysis = variables$perform_monte_carlo_analysis
    )
  }


  if (variables$perform_monte_carlo_analysis) {
    fcmconfr_output$inferences$monte_carlo_fcms = list(
      all_inferences = variables$mc_inferences$inference,
      simulations = variables$mc_inferences$sims
    )
    fcmconfr_output$params$aggregation_function = variables$aggregation_function
    fcmconfr_output$params$monte_carlo_sampling_draws = variables$monte_carlo_sampling_draws
    fcmconfr_output$params$runtime_opts = list(parallel = variables$parallel,
                               n_cores = variables$n_cores,
                               show_progress = variables$show_progress)
    fcmconfr_output$params$additional_opts = list(
      include_zero_weighted_edges_in_aggregation_and_mc_sampling = variables$include_zero_weighted_edges_in_aggregation_and_mc_sampling,
      include_monte_carlo_FCM_simulations_in_output = variables$include_monte_carlo_FCM_simulations_in_output,
      perform_monte_carlo_inference_bootstrap_analysis = variables$perform_monte_carlo_inference_bootstrap_analysis,
      perform_aggregate_analysis = variables$perform_aggregate_analysis,
      perform_monte_carlo_analysis = variables$perform_monte_carlo_analysis
    )
  }

  if (variables$perform_monte_carlo_analysis & variables$perform_monte_carlo_inference_bootstrap_analysis) {
    fcmconfr_output$inferences$monte_carlo_fcms$bootstrap = list(
      CI_estimation_function = variables$inference_estimation_function,
      CIs_and_quantiles_by_node = variables$CIs_of_expected_values_of_mc_simulation_inferences$CIs_and_quantiles_by_node,
      bootstrapped_expected_values = variables$CIs_of_expected_values_of_mc_simulation_inferences$bootstrap_expected_values
    )
    fcmconfr_output$params$mc_confidence_intervals_opts = list(
      inference_estimation_CI = variables$inference_estimation_CI,
      inference_estimation_bootstrap_reps = variables$inference_estimation_bootstrap_reps
    )
  }

  fcmconfr_output
}



#' print.fcmconfr
#'
#' @description
#' This improves the readability of the fcmconfr output
#'
#' @details
#' Show the objects listed in the fcmconfr output \code{$inference} and \code{$params},
#' as well as \code{$bootstrap} if present in output. Additionally, this prints
#' descriptions/summaries of objects within each sub-list like inference_opts,
#' bootstrap_input_opts, etc.
#'
#' @param x an fcmconfr object
#' @param ... additional inputs
#'
#' @returns A console printout of fcmconfr results
#'
#' @export
#' @examples
#' NULL
print.fcmconfr <- function(x, ...) {
  performed_aggregate <- x$params$additional_opts$perform_aggregate_analysis
  performed_mc <- x$params$additional_opts$perform_monte_carlo_analysis
  performed_bootstrap <- x$params$additional_opts$perform_monte_carlo_inference_bootstrap_analysis

  n_input_fcm <- length(x$params$adj_matrices)

  if (performed_aggregate & performed_mc & performed_bootstrap) {
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$aggregation_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" - CIs_about_means_and_quantiles_by_node: ", x$params$mc_confidence_intervals_opts$inference_estimation_CI, "% CI of means of inferences and quantiles by node\n"),
        paste0(" - bootstrapped_means: ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_reps),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (performed_aggregate & performed_mc & !performed_bootstrap) {
    # browser()
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$aggregation_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (!performed_aggregate & performed_mc & performed_bootstrap) {
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" - CIs_about_means_and_quantiles_by_node: ", x$params$mc_confidence_intervals_opts$inference_estimation_CI, "% CI of means of inferences and quantiles by node\n"),
        paste0(" - bootstrapped_means: ", x$params$mc_confidence_intervals_opts$inference_estimation_bootstrap_reps),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (!performed_aggregate & performed_mc & !performed_bootstrap) {
    # browser()
    n_mc_sims <- x$params$monte_carlo_sampling_draws

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (performed_aggregate & !performed_mc) {
    # browser()
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$aggregation_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  } else if (!performed_aggregate & !performed_mc) {
    # browser()
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - input_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$perform_aggregate_analysis, "; Perform MC Analysis = ", x$params$additional_opts$perform_monte_carlo_analysis)
    )
  }
}

#'
#' #' Plot fcmconfr Output
#' #'
#' #' @description
#' #' This function plots the primary results of the \code{\link{fcmconfr}}
#' #' output. The plot features the results of the input fcm inferences,
#' #' aggregate analysis inferences, and the monte carlo analysis bulk inference
#' #' confidence intervals. This function takes \code{\link{fcmconfr}} outputs
#' #' generated from Conventional FCMs, Interval-Value Fuzzy Number (IVFN) FCMs,
#' #' and Triangular Fuzzy Number (TFN) FCMs as inputs.
#' #'
#' #' @details
#' #' Plots generated using ggplot2
#' #'
#' #' @param fcmconfr_output Output directly from fcmconfr
#' #' @param swap_axes Switch x and y axes. Preferable when concepts have longer names
#' #' @param aggregate_point_fill Hex code value for the aggregate point fill
#' #' @param monte_carlo_col_fill Hex code value for the columns/bars representing monte carlo data
#' #' @param monte_carlo_col_alpha Between 0 and 1, the opacity of the monte_carlo_col_fill
#' #'
#' #' @returns A plot of the results generated from fcmconfr
#' #'
#' #' @export
#' #' @example man/examples/ex-fcmconfr_plot.R
#' fcmconfr_plot <- function(fcmconfr_output,
#'                           swap_axes = FALSE,
#'                           aggregate_point_fill = "#fb0009",
#'                           monte_carlo_col_fill = "#fcdbd0",
#'                           monte_carlo_col_alpha = 0.6) {
#'
#'   if (fcmconfr_output$fcm_class == "conventional") {
#'     fcmconfr_plot <- plot_conventional_fcmconfr(fcmconfr_output, swap_axes, aggregate_point_fill, monte_carlo_col_fill, monte_carlo_col_alpha)
#'   } else if (fcmconfr_output$fcm_class == "ivfn" | fcmconfr_output$fcm_class == "tfn") {
#'     fcmconfr_plot <- plot_ivfn_or_tfn_fcmconfr(fcmconfr_output, swap_axes, aggregate_point_fill, monte_carlo_col_fill, monte_carlo_col_alpha)
#'   }
#'
#'   fcmconfr_plot
#' }

#'
#' #' Plot fcmconfr Output (Conventional FCMs)
#' #'
#' #' @description
#' #' This function plots the primary results of the \code{\link{fcmconfr}}
#' #' output. The plot features the results of the input fcm inferences,
#' #' aggregate analysis inferences, and the monte carlo analysis bulk inference
#' #' confidence intervals. This function takes \code{\link{fcmconfr}} outputs
#' #' generated from Conventional FCMs.
#' #'
#' #' @details
#' #' Plots generated using ggplot2
#' #'
#' #' @param conventional_fcmconfr_output Output directly from fcmconfr (for conventional fcms as inputs)
#' #' @param swap_axes Switch x and y axes. Preferable when concepts have longer names
#' #' @param aggregate_point_fill Hex code value for the aggregate point fill
#' #' @param monte_carlo_col_fill Hex code value for the columns/bars representing monte carlo data
#' #' @param monte_carlo_col_alpha Between 0 and 1, the opacity of the monte_carlo_col_fill
#' #'
#' #' @importFrom ggplot2 aes ggplot geom_col geom_segment geom_errorbar geom_point
#' #' scale_fill_manual scale_color_manual scale_y_continuous guides guide_legend
#' #' expand_limits ggtitle labs xlab ylab theme_classic theme margin element_text
#' #' element_blank unit scale_x_discrete coord_flip
#' #'
#' #' @returns A plot of the results generated from fcmconfr for a set of
#' #' Conventional FCMs
#' #'
#' #' @export
#' #' @example man/examples/ex-plot_conventional_fcmconfr.R
#' plot_conventional_fcmconfr <- function(conventional_fcmconfr_output,
#'                                        swap_axes = FALSE,
#'                                        aggregate_point_fill = "#fb0009",
#'                                        monte_carlo_col_fill = "#fcdbd0",
#'                                        monte_carlo_col_alpha = 0.6) {
#'   #browser()
#'
#'   # Pre-defining values for R CMD Check. Does no effect logic.
#'   node <- NULL
#'   name <- NULL
#'   value <- NULL
#'   expected_value <- NULL
#'   analysis_source <- NULL
#'   concept <- NULL
#'   lower <- NULL
#'   upper <- NULL
#'   lower_0.025 <- NULL
#'   upper_0.975 <- NULL
#'   #
#'
#'   fcm_class_subtitle <- "Conventional FCMs"
#'   fcm_clamping_vector <- conventional_fcmconfr_output$params$simulation_opts$clamping_vector
#'   fcm_nodes <- unique(lapply(conventional_fcmconfr_output$params$adj_matrices, colnames))[[1]]
#'   clamped_nodes <- fcm_nodes[fcm_clamping_vector != 0]
#'
#'   agg_function <- conventional_fcmconfr_output$params$aggregation_function
#'   if (agg_function == "mean") {
#'     fcm_agg_function_subtitle <- "Aggregate and Monte Carlo Generated from MEAN of Input FCMs"
#'   } else if (agg_function == "median") {
#'     fcm_agg_function_subtitle <- "Aggregate and Monte Carlo Generated from MEDIAN of Input FCMs"
#'   }
#'
#'   aggregate_inferences_longer <- tidyr::pivot_longer(conventional_fcmconfr_output$inferences$aggregate_fcm$inferences, cols = 1:ncol(conventional_fcmconfr_output$inferences$aggregate_fcm$inferences))
#'   aggregate_inferences_longer$analysis_source <- "aggregate"
#'   mc_CIs <- conventional_fcmconfr_output$inferences$monte_carlo_fcms$bootstrap$CIs_by_node
#'   mc_CIs$analysis_source <- "mc"
#'
#'   # Remove clamped_nodes from plot data frames
#'   aggregate_inferences_longer <- aggregate_inferences_longer[!(aggregate_inferences_longer$name %in% clamped_nodes), ]
#'   mc_CIs <- mc_CIs[!(mc_CIs$node %in% clamped_nodes), ]
#'
#'   max_y <- max(max(mc_CIs$upper_0.975), max(aggregate_inferences_longer$value))
#'   max_y <- (ceiling(max_y*1000)/1000)
#'   min_y <- min(min(mc_CIs$lower_0.025), min(aggregate_inferences_longer$value))
#'   min_y <- (floor(min_y*1000)/1000)
#'
#'   aggregate_inferences_longer <- aggregate_inferences_longer[aggregate_inferences_longer$value >= 0.001, ]
#'   mc_CIs <- mc_CIs[mc_CIs$node %in% aggregate_inferences_longer$name, ]
#'
#'   fcmconfr_plot <- ggplot() +
#'     geom_col(data = mc_CIs, aes(x = node, y = expected_value, fill = analysis_source, color = monte_carlo_col_fill), width = 0.5, alpha = monte_carlo_col_alpha, linewidth = 0.3) +
#'     geom_errorbar(data = mc_CIs, aes(x = node, ymin = lower_0.025, ymax = upper_0.975, color = analysis_source), width = 0.5, linewidth = 0.5) +
#'     geom_point(data = aggregate_inferences_longer, aes(x = name, y = value, fill = analysis_source), color = "black", shape = 21, size = 3, stroke = 0.5) +
#'     scale_fill_manual(values = c(mc = monte_carlo_col_fill, aggregate = aggregate_point_fill), labels = c(mc = "Monte Carlo Average", aggregate = "Aggregate")) +
#'     scale_color_manual(values = c(mc = "black"), labels = c(mc = "Monte Carlo CIs")) +
#'     scale_y_continuous(expand = c(0, 0), limits = c(min_y, max_y)) +
#'     guides(alpha = "none", color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
#'     expand_limits(y = 0) +
#'     ggtitle("FCMconfR Inferences", subtitle = paste(fcm_class_subtitle)) +
#'     labs(caption = fcm_agg_function_subtitle) +
#'     xlab("Inference Value") +
#'     ylab("Concept (Node)") +
#'     theme_classic() +
#'     theme(
#'       plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
#'       plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
#'       plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
#'       plot.caption = element_text(hjust = 0.5, margin = margin(t = 15)),
#'       axis.title = element_text(size = 12),
#'       axis.title.x = element_text(margin = margin(t = 10)),
#'       axis.title.y = element_text(margin = margin(r = 10)),
#'       axis.text = element_text(size = 12),
#'       legend.position = "bottom",
#'       legend.title = element_blank(),
#'       legend.spacing = unit(0.001, 'cm'),
#'       legend.text = element_text(size = 10)
#'     )
#'
#'   if (swap_axes) {
#'     fcmconfr_plot <- fcmconfr_plot + scale_x_discrete(limits = rev) + coord_flip()
#'   }
#'
#'   fcmconfr_plot
#' }
#'
#'
#'
#' #' Plot fcmconfr Output (IVFN FCMs or TFN FCMs)
#' #'
#' #' @description
#' #' This function plots the primary results of the \code{\link{fcmconfr}}
#' #' output. The plot features the results of the input fcm inferences,
#' #' aggregate analysis inferences, and the monte carlo analysis bulk inference
#' #' confidence intervals. This function takes \code{\link{fcmconfr}} outputs
#' #' generated from Interval-Value Fuzzy Number (IVFN) FCMs,
#' #' and Triangular Fuzzy Number (TFN) FCMs as inputs.
#' #'
#' #' @details
#' #' Plots generated using ggplot2
#' #'
#' #' @param ivfn_or_tfn_fcmconfr_output Output directly from fcmconfr (for ivfn or tfn fcms as inputs)
#' #' @param swap_axes Switch x and y axes. Preferable when concepts have longer names
#' #' @param aggregate_point_fill Hex code value for the aggregate point fill
#' #' @param monte_carlo_col_fill Hex code value for the columns/bars representing monte carlo data
#' #' @param monte_carlo_col_alpha Between 0 and 1, the opacity of the monte_carlo_col_fill
#' #'
#' #' @importFrom ggplot2 aes ggplot geom_col geom_segment geom_errorbar geom_point
#' #' scale_fill_manual scale_color_manual scale_y_continuous guides guide_legend
#' #' expand_limits ggtitle labs xlab ylab theme_classic theme margin element_text
#' #' element_blank unit scale_x_discrete coord_flip
#' #'
#' #' @returns A plot of the results generated from fcmconfr for a set of
#' #' IVFN or TFN FCMs
#' #'
#' #' @export
#' #' @example man/examples/ex-plot_ivfn_or_tfn_fcmconfr.R
#' plot_ivfn_or_tfn_fcmconfr <- function(ivfn_or_tfn_fcmconfr_output,
#'                                       swap_axes = FALSE,
#'                                       aggregate_point_fill = "#fb0009",
#'                                       monte_carlo_col_fill = "#fcdbd0",
#'                                       monte_carlo_col_alpha = 0.6) {
#'   # browser()
#'   # Pre-defining values for R CMD Check. Does no effect logic.
#'   node <- NULL
#'   name <- NULL
#'   value <- NULL
#'   expected_value <- NULL
#'   analysis_source <- NULL
#'   concept <- NULL
#'   lower <- NULL
#'   upper <- NULL
#'   lower_0.025 <- NULL
#'   upper_0.975 <- NULL
#'   #
#'
#'   fcm_class <- ivfn_or_tfn_fcmconfr_output$fcm_class
#'   if (fcm_class == "ivfn") {
#'     fcm_class_subtitle <- "Interval-Value Fuzzy Number FCMs"
#'   } else if (fcm_class == "tfn") {
#'     fcm_class_subtitle <- "Triangular Fuzzy Number FCMs"
#'   }
#'
#'   agg_function <- ivfn_or_tfn_fcmconfr_output$params$aggregation_function
#'   if (agg_function == "mean") {
#'     fcm_agg_function_subtitle <- "Aggregate and Monte Carlo Generated from MEAN of Input FCMs"
#'   } else if (agg_function == "median") {
#'     fcm_agg_function_subtitle <- "Aggregate and Monte Carlo Generated from MEDIAN of Input FCMs"
#'   }
#'
#'   #aggregate_inferences_longer <- tidyr::pivot_longer(ivfn_or_tfn_fcmconfr_output$inferences$aggregate_fcm$inferences, cols = 3:ncol(ivfn_or_tfn_fcmconfr_output$inferences$aggregate_fcm$inferences))
#'   aggregate_inferences <- ivfn_or_tfn_fcmconfr_output$inferences$aggregate_fcm$inferences
#'   aggregate_inferences$analysis_source <- "aggregate"
#'   mc_CIs <- ivfn_or_tfn_fcmconfr_output$inferences$monte_carlo_fcms$bootstrap$CIs_by_node
#'   mc_CIs$analysis_source <- "mc"
#'
#'   max_y <- max(max(mc_CIs$upper_0.975), max(aggregate_inferences$upper))
#'
#'   # browser()
#'
#'   fcmconfr_plot <- ggplot() +
#'     geom_col(data = mc_CIs, aes(x = node, y = expected_value, fill = analysis_source, color = monte_carlo_col_fill), width = 0.5, alpha = monte_carlo_col_alpha, linewidth = 0.3) +
#'     geom_segment(data = aggregate_inferences, aes(x = concept, xend = concept, y = lower, yend = upper, color = analysis_source), lineend = "round", linewidth = 2) +
#'     geom_errorbar(data = mc_CIs, aes(x = node, ymin = lower_0.025, ymax = upper_0.975, color = analysis_source), width = 0.5, linewidth = 0.5) +
#'     scale_fill_manual(values = c(mc = monte_carlo_col_fill, aggregate = aggregate_point_fill), labels = c(mc = "Monte Carlo Average", aggregate = "Aggregate")) +
#'     scale_color_manual(values = c(mc = "black", aggregate = aggregate_point_fill), labels = c(mc = "Monte Carlo CIs", aggregate = "Aggregate")) +
#'     scale_y_continuous(expand = c(0, 0), limits = c(0, max_y + 0.05)) +
#'     guides(alpha = "none", color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
#'     expand_limits(y = 0) +
#'     ggtitle("FCMconfR Inferences", subtitle = paste(fcm_class_subtitle)) +
#'     labs(caption = fcm_agg_function_subtitle) +
#'     xlab("Inference Value") +
#'     ylab("Concept (Node)") +
#'     theme_classic() +
#'     theme(
#'       plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
#'       plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
#'       plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
#'       plot.caption = element_text(hjust = 0.5, margin = margin(t = 15)),
#'       axis.title = element_text(size = 12),
#'       axis.title.x = element_text(margin = margin(t = 10)),
#'       axis.title.y = element_text(margin = margin(r = 10)),
#'       axis.text = element_text(size = 12),
#'       legend.position = "bottom",
#'       legend.title = element_blank(),
#'       legend.spacing = unit(0.001, 'cm'),
#'       legend.text = element_text(size = 10)
#'     )
#'
#'   if (swap_axes) {
#'     fcmconfr_plot <- fcmconfr_plot + scale_x_discrete(limits = rev) + coord_flip()
#'   }
#'
#'   fcmconfr_plot
#' }



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
#' @param agg_function Aggregate the adj. matrices into a single FCM by taking
#' either the mean or median of the edge weights for edges included in multiple maps
#' @param num_mc_fcms The number of FCMs to generate via monte carlo
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
#' @param ci_centering_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param confidence_interval The confidence interval to estimate for the inferences
#' of each concept across all monte carlo FCMs (via bootstrap)
#' @param num_ci_bootstraps The number of bootstraps to perform in
#' estimating the confidence interval for the inferences of each concept across all monte
#' carlo FCMs
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param run_agg_calcs TRUE/FALSE Run the code to generate and simulate an aggregate FCM generated from the input adj_matrices
#' @param run_mc_calcs TRUE/FALSE Run the code to generate and simulate monte carlo-generated FCM sampled from the input adj_matrices
#' @param run_ci_calcs TRUE/FALSE Run the code to estimate the 95 percent CI bounds about the means of the inferences of the monte carlo adj matrices
#' @param include_zeroes_in_sampling TRUE/FALSE Whether to incorporate zeroes as intentionally-defined edge weights or ignore
#' them when aggregating adj. matrices and sampling for monte carlo FCMs
#' @param mc_sims_in_output TRUE/FALSE Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
#' about the size of the output of fcmconfr (simulations are necessary and will run regardless)
#'
#' @importFrom Rdpack reprompt
#'
#' @returns A list of outputs generated from the individual_fcms simulations,
#'          aggregate_fcm analysis, and monte_carlo_fcms analysis. Bootstrap
#'          estimates of inferences from monte carlo analysis are included, as
#'          well as function inputs.
#'
#' @export
#' @example man/examples/ex-fcmconfr.R
fcmconfr <- function(adj_matrices = list(matrix()),
                     # Aggregation and Monte Carlo Sampling
                     agg_function = c("mean", "median"),
                     num_mc_fcms = 1000,
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
                     ci_centering_function = c("mean", "median"),
                     confidence_interval = 0.95,
                     num_ci_bootstraps = 5000,
                     # Runtime Options
                     show_progress = TRUE,
                     parallel = FALSE,
                     n_cores = integer(),
                     # Additional Options
                     run_agg_calcs = TRUE,
                     run_mc_calcs = TRUE,
                     run_ci_calcs = TRUE,
                     include_zeroes_in_sampling = FALSE,
                     mc_sims_in_output = TRUE) {

  # Perform input checks ----
  checks <- check_fcmconfr_inputs(
    adj_matrices,
    # Aggregation and Monte Carlo Sampling
    agg_function, num_mc_fcms,
    # Simulation
    initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error,
    # Inference Estimation (bootstrap)
    ci_centering_function, confidence_interval, num_ci_bootstraps,
    # Runtime Options
    show_progress, parallel, n_cores, run_agg_calcs,
    # Additional Options
    run_mc_calcs, run_ci_calcs, include_zeroes_in_sampling, mc_sims_in_output
  )
  fcm_class <- checks$fcm_class
  adj_matrices <- checks$adj_matrices
  concepts <- checks$concept_names
  agg_function <- checks$agg_function
  initial_state_vector <- checks$initial_state_vector
  clamping_vector <- checks$clamping_vector
  activation <- checks$activation
  squashing <- checks$squashing
  point_of_inference <- checks$point_of_inference
  ci_centering_function <- checks$ci_centering_function
  show_progress <- checks$show_progress
  parallel <- checks$parallel
  run_agg_calcs <- checks$run_agg_calcs
  run_mc_calcs <- checks$run_mc_calcs
  run_ci_calcs <- checks$run_ci_calcs
  # ----

  # Individual Adj. Matrices Simulations ----
  print("Simulating Input FCMs", quote = FALSE)
  individual_adj_matrices_inferences <- infer_fcm_set(adj_matrices, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error, parallel, n_cores, show_progress, mc_sims_in_output = TRUE)$sims
  # } else if (show_progress & !parallel) {
  #   individual_adj_matrices_inferences <- pbapply::pblapply(adj_matrices, infer_fcm, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error)
  # } else {
  #   individual_adj_matrices_inferences <- lapply(adj_matrices, infer_fcm, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error)
  # }

  if (fcm_class == "conventional") {
    individual_adj_matrices_inferences_df <- do.call(rbind, lapply(individual_adj_matrices_inferences, function(inference) inference$inferences))
    individual_adj_matrices_inferences_df <- cbind(input = paste0("adj_matrix_", 1:length(adj_matrices)), individual_adj_matrices_inferences_df)
    rownames(individual_adj_matrices_inferences_df) <- NULL
  } else if (fcm_class %in% c("ivfn", "tfn")) {
    individual_adj_matrices_inferences_df <- lapply(individual_adj_matrices_inferences, function(inference) inference$inferences)
    names(individual_adj_matrices_inferences_df) <- paste0("adj_matrix_", 1:length(adj_matrices))
  }
  names(individual_adj_matrices_inferences) <- paste0("adj_matrix_", 1:length(adj_matrices))
  # ----

  # Aggregation Analysis ----
  if (run_agg_calcs) {
    # Build aggregate adj_matrix
    aggregate_adj_matrix <- aggregate_fcms(adj_matrices, agg_function, include_zeroes_in_sampling)
    # Infer aggregate adj_matrix
    aggregate_fcm_inference <- infer_fcm(aggregate_adj_matrix$adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error)
  }

  # Monte Carlo Analysis
  if (run_mc_calcs) {
    # Build monte carlo models
    mc_adj_matrices <- build_monte_carlo_fcms(adj_matrices, num_mc_fcms, include_zeroes_in_sampling, show_progress)
    mc_adj_matrices <- lapply(
      mc_adj_matrices,
      function(sampled_adj_matrix) {
        colnames(sampled_adj_matrix) <- concepts
        rownames(sampled_adj_matrix) <- concepts
        sampled_adj_matrix
      })

    mc_inferences <- infer_fcm_set(
      adj_matrices = mc_adj_matrices,
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
      mc_sims_in_output = mc_sims_in_output
    )

    if (run_ci_calcs) {
      CIs_of_expected_values_of_mc_simulation_inferences <- get_mc_simulations_inference_CIs_w_bootstrap(mc_inferences$inference, ci_centering_function, confidence_interval, num_ci_bootstraps, parallel, n_cores, show_progress)
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
#' @param agg_function Aggregate the adj. matrices into a single FCM by taking
#' either the mean or median of the edge weights for edges included in multiple maps
#' @param num_mc_fcms The number of FCMs to generate via monte carlo
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
#' @param ci_centering_function Estimate confidence intervals about the "mean" or "median" of
#' inferences from the monte carlo simulations
#' @param confidence_interval The confidence interval to estimate for the inferences
#' of each concept across all monte carlo FCMs (via bootstrap)
#' @param num_ci_bootstraps The number of bootstraps to perform in
#' estimating the confidence interval for the inferences of each concept across all monte
#' carlo FCMs
#' @param show_progress TRUE/FALSE Show progress bar when creating fmcm. Uses pbmapply
#' from the pbapply package as the underlying function.
#' @param parallel TRUE/FALSE Whether to utilize parallel processing
#' @param n_cores Number of cores to use in parallel processing. If no input given,
#' will use all available cores in the machine.
#' @param run_agg_calcs TRUE/FALSE Run the code to generate and simulate an aggregate FCM generated from the input adj_matrices
#' @param run_mc_calcs TRUE/FALSE Run the code to generate and simulate monte carlo-generated FCM sampled from the input adj_matrices
#' @param run_ci_calcs TRUE/FALSE Run the code to estimate the 95 percent CI bounds about the means of the inferences of the monte carlo adj matrices
#' @param include_zeroes_in_sampling TRUE/FALSE Whether to incorporate zeroes as intentionally-defined edge weights or ignore
#' them when aggregating adj. matrices and sampling for monte carlo FCMs
#' @param mc_sims_in_output TRUE/FALSE Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
#' about the size of the output of fcmconfr (simulations are necessary and will run regardless)
#'
#' @returns A list of resolved inputs to pass to \code{\link{fcmconfr}}
#'
#' @export
#' @examples
#' NULL
check_fcmconfr_inputs <- function(adj_matrices = list(matrix()),
                                  # Aggregation and Monte Carlo Sampling
                                  agg_function = c("mean", "median"),
                                  num_mc_fcms = 1000,
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
                                  ci_centering_function = c("mean", "median"),
                                  confidence_interval = 0.95,
                                  num_ci_bootstraps = 5000,
                                  # Runtime Options
                                  show_progress = TRUE,
                                  parallel = TRUE,
                                  n_cores = integer(),
                                  # Additional Options
                                  run_agg_calcs = TRUE,
                                  run_mc_calcs = TRUE,
                                  run_ci_calcs = TRUE,
                                  include_zeroes_in_sampling = FALSE,
                                  mc_sims_in_output = TRUE) {

  # Perform Checks ----
  adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices)
  fcm_class <- adj_matrices_input_type$fcm_class
  if (!adj_matrices_input_type$adj_matrices_input_is_list) {
    adj_matrices <- list(adj_matrices)
  }

  # Check Additional Options ----
  if (!is.logical(run_agg_calcs)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var run_agg_calcs} must be logical (TRUE/FALSE)",
      "+++++> Input {.var run_agg_calcs} was {run_agg_calcs}"
    )))
  }
  if (!is.logical(run_mc_calcs)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var run_mc_calcs} must be logical (TRUE/FALSE)",
      "+++++> Input {.var run_mc_calcs} was {run_mc_calcs}"
    )))
  }
  if (!is.logical(run_ci_calcs)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var run_ci_calcs} must be logical (TRUE/FALSE)",
      "+++++> Input {.var run_ci_calcs} was {run_ci_calcs}"
    )))
  }
  if (!is.logical(include_zeroes_in_sampling)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var include_zeroes_in_sampling} must be logical (TRUE/FALSE)",
      "+++++> Input {.var include_zeroes_in_sampling} was {include_zeroes_in_sampling}"
    )))
  }
  if (!is.logical(mc_sims_in_output)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var mc_sims_in_output} must be logical (TRUE/FALSE)",
      "+++++> Input {.var mc_sims_in_output} was {mc_sims_in_output}"
    )))
  }
  # ----

  # Confirm Monte Carlo and Bootstrap Function Calls are Viable ----
  if (fcm_class == "conventional" & length(adj_matrices) == 1 & (run_mc_calcs | run_agg_calcs)) {
    warning(cli::format_warning(c(
      "!" = "Warning: Cannot aggregate or generate monte carlo samples from a single (conventional) adj. matrix",
      "~~~~~ Skipping aggregate analysis; i.e. setting {.var run_agg_calcs} to FALSE",
      "~~~~~ Skipping monte carlo analysis; i.e. setting {.var run_mc_calcs} to FALSE"
    )))
    run_mc_calcs <- FALSE
    run_agg_calcs <- FALSE
  }
  else if ((fcm_class %in% c("ivfn", "tfn")) & length(adj_matrices) == 1 & (run_agg_calcs)) {
    warning(cli::format_warning(c(
      "!" = "Warning: Cannot generate aggregate fcm from a single adj. matrix",
      "~~~~~ Skipping aggregate analysis; i.e. setting {.var run_agg_calcs} to FALSE"
    )))
    run_agg_calcs = FALSE
  }

  if (!run_mc_calcs & run_ci_calcs) {
    warning(cli::format_warning(c(
      "!" = "Warning: Cannot estimate CIs of monte carlo inferences if monte carlo analysis is not being performed",
      "~~~~~ Skipping CI bound estimation; i.e. setting {.var run_ci_calcs} to FALSE"
    )))
    run_ci_calcs <- FALSE
  }

  if (run_mc_calcs & run_ci_calcs) {
    if (identical(ci_centering_function, c("mean", "median"))) {
      warning(cli::format_warning(c(
        "!" = "Warning: No {.var ci_centering_function} given",
        "~~~~~ Assuming {.var ci_centering_function} is 'mean'"
      )))
    }
    ci_centering_function <- 'mean'
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
  if (run_agg_calcs) {
    if (identical(agg_function, c("mean", "median"))) {
      warning(cli::format_warning(c(
        "!" = "Warning: No {.var agg_function} given",
        "~~~~~ Assuming {.var agg_function} is 'mean'"
      )))
      agg_function <- "mean"
    }
    if (!(agg_function %in% c("mean", "median"))) {
      stop(cli::format_error(c(
        "x" = "Error: {.var agg_function} must be one of the following: 'mean' or 'median'",
        "+++++> Input {.var agg_function} was {agg_function}"
      )))
    }
  }
  # ----

  # Monte Carlo ----
  if (run_mc_calcs) {
    if (!is.numeric(num_mc_fcms)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var num_mc_fcms} must be a positive integer",
        "+++++ Input {.var num_mc_fcms} was {num_mc_fcms}"
      )))
    }
    if (!(num_mc_fcms == round(num_mc_fcms))) {
      stop(cli::format_error(c(
        "x" = "Error: {.var num_mc_fcms} must be a positive integer",
        "+++++ Input {.var num_mc_fcms} was {num_mc_fcms}"
      )))
    }
    if (num_mc_fcms <= 0) {
      stop(cli::format_error(c(
        "x" = "Error: {.var num_mc_fcms} must be a positive integer",
        "+++++ Input {.var num_mc_fcms} was {num_mc_fcms}"
      )))
    }
  }
  # ----

  # Check Monte Carlo and Bootstrap Inputs ----
  if (run_ci_calcs) {
    mcbs_checks <- check_monte_carlo_bootstrap_inputs(ci_centering_function, confidence_interval, num_ci_bootstraps, parallel, n_cores, show_progress)
    ci_centering_function <- mcbs_checks$ci_centering_function
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
    agg_function = agg_function,
    initial_state_vector = initial_state_vector,
    clamping_vector = clamping_vector,
    activation = activation,
    squashing = squashing,
    point_of_inference = point_of_inference,
    ci_centering_function = ci_centering_function,
    show_progress = show_progress,
    parallel = parallel,
    run_agg_calcs = run_agg_calcs,
    run_mc_calcs = run_mc_calcs,
    run_ci_calcs = run_ci_calcs
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
        individual_fcms = list(
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
        additional_opts = list(run_ci_calcs = variables$run_ci_calcs,
                               run_agg_calcs = variables$run_agg_calcs,
                               run_mc_calcs = variables$run_mc_calcs)
      )
    ),
    class = "fcmconfr"
  )

  if (variables$run_agg_calcs) {
    fcmconfr_output$aggregate_adj_matrix <- variables$aggregate_adj_matrix
    fcmconfr_output$inferences$aggregate_fcm <- variables$aggregate_fcm_inference
    fcmconfr_output$params$agg_function <- variables$agg_function
    fcmconfr_output$params$additional_opts <- list(
      include_zeroes_in_sampling = variables$include_zeroes_in_sampling,
      run_ci_calcs = variables$run_ci_calcs,
      run_agg_calcs = variables$run_agg_calcs,
      run_mc_calcs = variables$run_mc_calcs
    )
  }

  if (variables$run_mc_calcs) {
    fcmconfr_output$mc_adj_matrices = variables$mc_adj_matrices
    fcmconfr_output$inferences$monte_carlo_fcms = list(
      all_inferences = variables$mc_inferences$inference,
      simulations = variables$mc_inferences$sims
    )
    fcmconfr_output$params$agg_function = variables$agg_function
    fcmconfr_output$params$num_mc_fcms = variables$num_mc_fcms
    fcmconfr_output$params$runtime_opts = list(parallel = variables$parallel,
                               n_cores = variables$n_cores,
                               show_progress = variables$show_progress)
    fcmconfr_output$params$additional_opts = list(
      include_zeroes_in_sampling = variables$include_zeroes_in_sampling,
      mc_sims_in_output = variables$mc_sims_in_output,
      run_ci_calcs = variables$run_ci_calcs,
      run_agg_calcs = variables$run_agg_calcs,
      run_mc_calcs = variables$run_mc_calcs
    )
  }

  if (variables$run_mc_calcs & variables$run_ci_calcs) {
    fcmconfr_output$inferences$monte_carlo_fcms$bootstrap = list(
      CI_estimation_function = variables$ci_centering_function,
      CIs_and_quantiles_by_node = variables$CIs_of_expected_values_of_mc_simulation_inferences$CIs_and_quantiles_by_node,
      bootstrapped_expected_values = variables$CIs_of_expected_values_of_mc_simulation_inferences$bootstrap_expected_values
    )
    fcmconfr_output$params$mc_confidence_intervals_opts = list(
      confidence_interval = variables$confidence_interval,
      num_ci_bootstraps = variables$num_ci_bootstraps
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
  performed_aggregate <- x$params$additional_opts$run_agg_calcs
  performed_mc <- x$params$additional_opts$run_mc_calcs
  performed_bootstrap <- x$params$additional_opts$run_ci_calcs

  n_input_fcm <- length(x$params$adj_matrices)

  if (performed_aggregate & performed_mc & performed_bootstrap) {
    n_mc_sims <- x$params$num_mc_fcms

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - individual_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$agg_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" - CIs_about_means_and_quantiles_by_node: ", x$params$mc_confidence_intervals_opts$confidence_interval, "% CI of means of inferences and quantiles by node\n"),
        paste0(" - bootstrapped_means: ", x$params$mc_confidence_intervals_opts$num_ci_bootstraps),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$run_agg_calcs, "; Perform MC Analysis = ", x$params$additional_opts$run_mc_calcs)
    )
  } else if (performed_aggregate & performed_mc & !performed_bootstrap) {
    # browser()
    n_mc_sims <- x$params$num_mc_fcms

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - individual_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$agg_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$run_agg_calcs, "; Perform MC Analysis = ", x$params$additional_opts$run_mc_calcs)
    )
  } else if (!performed_aggregate & performed_mc & performed_bootstrap) {
    n_mc_sims <- x$params$num_mc_fcms

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - individual_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$bootstrap\n",
        paste0(" - CIs_about_means_and_quantiles_by_node: ", x$params$mc_confidence_intervals_opts$confidence_interval, "% CI of means of inferences and quantiles by node\n"),
        paste0(" - bootstrapped_means: ", x$params$mc_confidence_intervals_opts$num_ci_bootstraps),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$run_agg_calcs, "; Perform MC Analysis = ", x$params$additional_opts$run_mc_calcs)
    )
  } else if (!performed_aggregate & performed_mc & !performed_bootstrap) {
    # browser()
    n_mc_sims <- x$params$num_mc_fcms

    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - individual_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - monte_carlo_fcms: Inferences of data from the ", n_mc_sims, " fcms constructed from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$run_agg_calcs, "; Perform MC Analysis = ", x$params$additional_opts$run_mc_calcs)
    )
  } else if (performed_aggregate & !performed_mc) {
    # browser()
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - individual_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices.\n"),
        paste0(" - aggregate_fcm: Inferences and data from the aggregate (", x$params$agg_function, ") of the ",  n_input_fcm, " input fcm adj. matrices.\n"),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$run_agg_calcs, "; Perform MC Analysis = ", x$params$additional_opts$run_mc_calcs)
    )
  } else if (!performed_aggregate & !performed_mc) {
    # browser()
    cat(paste0("fcmconfr: ", n_input_fcm, " input adj. matrices (", x$params$fcm_class, ")"),
        "\n$inference\n",
        paste0(" - individual_fcms: Inferences and data from the ", n_input_fcm, " input fcm adj. matrices."),
        "\n$params\n",
        " - simulation_opts:",
        paste0("act = ", x$params$simulation_opts$activation, "; squash = ", x$params$simulation_opts$squashing, "; lambda = ", x$params$simulation_opts$lambda),
        paste0("\n  - additional_opts: ", "Perform Aggregate Analysis = ", x$params$additional_opts$run_agg_calcs, "; Perform MC Analysis = ", x$params$additional_opts$run_mc_calcs)
    )
  }
}

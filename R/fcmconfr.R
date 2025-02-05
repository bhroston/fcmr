

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
#' @param include_sims_in_output TRUE/FALSE Whether to include simulations of monte carlo FCMs. Switch to FALSE if concerned
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
                     include_sims_in_output = TRUE) {

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
    run_mc_calcs, run_ci_calcs, include_zeroes_in_sampling, include_sims_in_output
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
  individual_adj_matrices_inferences <- infer_fcm_set(adj_matrices, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error, parallel, n_cores, show_progress, include_sims_in_output = TRUE, skip_checks = TRUE)

  if (fcm_class == "conventional") {
    # individual_adj_matrices_inferences_df <- do.call(rbind, lapply(individual_adj_matrices_inferences$inferences, function(inference) inference$inferences))
    individual_adj_matrices_inferences_df <- cbind(input = paste0("adj_matrix_", 1:length(adj_matrices)), individual_adj_matrices_inferences$inferences)
    rownames(individual_adj_matrices_inferences_df) <- NULL
  } else if (fcm_class %in% c("ivfn", "tfn")) {
    # individual_adj_matrices_inferences_df <- lapply(individual_adj_matrices_inferences$inferences, function(inference) inference$inferences)
    individual_adj_matrices_inferences_df <- individual_adj_matrices_inferences$inferences
    names(individual_adj_matrices_inferences_df) <- paste0("adj_matrix_", 1:length(adj_matrices))
  }
  names(individual_adj_matrices_inferences$simulations) <- paste0("adj_matrix_", 1:length(adj_matrices))
  # ----

  # Aggregation Analysis ----
  if (run_agg_calcs) {
    # Build aggregate adj_matrix
    aggregate_adj_matrix <- aggregate_fcms(adj_matrices, agg_function, include_zeroes_in_sampling)
    # Infer aggregate adj_matrix
    aggregate_fcm_inference <- infer_fcm(aggregate_adj_matrix$adj_matrix, initial_state_vector, clamping_vector, activation, squashing, lambda, point_of_inference, max_iter, min_error, skip_checks = TRUE)
  }

  # Monte Carlo Analysis
  if (run_mc_calcs) {
    # Build monte carlo models
    mc_adj_matrices <- build_monte_carlo_fcms(adj_matrices, num_mc_fcms, include_zeroes_in_sampling, show_progress, skip_checks = TRUE)
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
      include_sims_in_output = include_sims_in_output
    )

    if (run_ci_calcs) {
      CIs_of_expected_values_of_mc_simulation_inferences <- get_mc_simulations_inference_CIs_w_bootstrap(mc_inferences$inference, ci_centering_function, confidence_interval, num_ci_bootstraps, parallel, n_cores, show_progress, skip_checks = TRUE)
    }
  }
  # ----

  # Organize Output
  env_variables <- as.list(environment())
  fcmconfr_output <- organize_fcmconfr_output(env_variables)

  fcmconfr_output
}

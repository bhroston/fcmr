
test_that("streamlined fcmconfr works", {

  test_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  test_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.25, 0, 0, 0.25),
    "C" = c(0, 0.25, 0, 0),
    "D" = c(0, 0, 0.25, 0)
  )
  test_adj_matrix_3 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, 0.75, 0)
  )
  test_adj_matrix_4 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.5, 0, 0, 0.5),
    "C" = c(0, 0.5, 0, 0),
    "D" = c(0, 0, 0.5, 0)
  )
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)

  aggregate_fcms(test_fcms, "mean")

  fcmconfr(
    adj_matrices = test_adj_matrix_1,
    # Aggregation and Monte Carlo Sampling
    aggregation_function = 'mean',
    monte_carlo_sampling_draws = 1000,
    # Simulation
    initial_state_vector = c(1, 1, 1, 1),
    clamping_vector = c(0, 0, 0, 0),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05,
    # Inference Estimation (bootstrap)
    inference_estimation_CI = 0.95,
    inference_estimation_bootstrap_reps = 1000,
    inference_estimation_bootstrap_draws_per_rep = 1000,
    # Runtime Options
    show_progress = TRUE,
    parallel = FALSE,
    n_cores = 10,
    # Additional Options
    include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE,
    include_monte_carlo_FCM_simulations_in_output = TRUE,
    estimate_inference_CI_w_bootstrap = TRUE
  )

  adj_matrices = test_adj_matrix_1
  # Aggregation and Monte Carlo Sampling
  aggregation_function = 'mean'
  monte_carlo_sampling_draws = 1000
  # Simulation
  initial_state_vector = c(1, 1, 1, 1)
  clamping_vector = c(0, 0, 0, 0)
  activation = 'kosko'
  squashing = 'sigmoid'
  lambda = 1
  max_iter = 100
  min_error = 1e-05
  # Inference Estimation (bootstrap)
  inference_estimation_CI = 0.95
  inference_estimation_bootstrap_reps = 1000
  inference_estimation_bootstrap_draws_per_rep = 1000
  # Runtime Options
  show_progress = TRUE
  parallel = FALSE
  n_cores = 10
  # Additional Options
  include_zero_weighted_edges_in_aggregation_and_mc_sampling = FALSE
  include_monte_carlo_FCM_simulations_in_output = TRUE
  estimate_inference_CI_w_bootstrap = TRUE


})



test_that("fcmconfr works", {
  # sampling = "uniform"
  # samples = 10
  # clamping_vector = c(1, 0, 0, 0)
  # activation = "kosko"
  # squashing = "sigmoid"
  # lambda = 1
  # max_iter = 100
  # min_error = 1e-5
  # bootstrap_inference_means = TRUE
  # bootstrap_CI = 0.95
  # show_progress = TRUE
  # parallel = TRUE
  # n_cores = integer()
  # IDs = c()
  # include_simulations_in_output = TRUE


  test_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  test_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.25, 0, 0, 0.25),
    "C" = c(0, 0.25, 0, 0),
    "D" = c(0, 0, 0.25, 0)
  )
  test_adj_matrix_3 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, 0.75, 0)
  )
  test_adj_matrix_4 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.5, 0, 0, 0.5),
    "C" = c(0, 0.5, 0, 0),
    "D" = c(0, 0, 0.5, 0)
  )
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
  expect_no_error(
    test <- fcmconfr(
      adj_matrices <- test_fcms,
      samples = 100,
      include_zeroes_in_aggregation = FALSE,
      aggregation_fun = "mean",
      initial_state_vector <- c(1, 1, 1, 1),
      clamping_vector <- c(1, 0, 0, 0),
      lambda = 1,
      activation = "kosko",
      squashing = "sigmoid",
      bootstrap_CI = 0.95,
      bootstrap_reps = 1000,
      bootstrap_draws_per_rep = 1000,
      max_iter = 100,
      min_error = 1e-5,
      n_cores = 2
    )
  )


  lower_adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.25, 0, 0, 0.25),
    "C" = c(0, 0.25, 0, 0),
    "D" = c(0, 0, 0.25, 0)
  )
  upper_adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, 0.75, 0)
  )
  gm_1 <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
  gm_2 <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix*1.2, upper_adj_matrix*1.2)
  gm_3 <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix*0.8, upper_adj_matrix*0.8)
  fgcm_adj_matrices <- list(gm_1, gm_2, gm_3)
  expect_no_error(
    test <- fcmconfr(
      adj_matrices <- fgcm_adj_matrices,
      samples = 1000,
      include_zeroes_in_aggregation = FALSE,
      aggregation_fun = "mean",
      initial_state_vector <- c(1, 1, 1, 1),
      clamping_vector <- c(1, 0, 0, 0),
      lambda = 1,
      activation = "kosko",
      squashing = "sigmoid",
      bootstrap_CI = 0.95,
      bootstrap_reps = 1000,
      bootstrap_draws_per_rep = 1000,
      max_iter = 100,
      min_error = 1e-5,
      n_cores = 2
    )
  )



  tri_matrix_1 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
    lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
  )
  tri_matrix_2 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
    lower = matrix(data = c(0, 0.4, 0, 0.1), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.6, 0, 0.3), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.8, 0, 0.4), nrow = 2, ncol = 2)
  )
  tri_matrix_3 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
    lower = matrix(data = c(0, 0.4, 0.1, 0.1), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.6, 0.2, 0.3), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.8, 0.3, 0.4), nrow = 2, ncol = 2)
  )
  triangular_adj_matrices <- list(tri_matrix_1, tri_matrix_2, tri_matrix_3)
  expect_no_error(
    test <- fcmconfr(
      adj_matrices <- triangular_adj_matrices,
      samples = 1000,
      include_zeroes_in_aggregation = FALSE,
      aggregation_fun = "mean",
      initial_state_vector <- c(1, 1),
      clamping_vector <- c(1, 0),
      lambda = 1,
      activation = "kosko",
      squashing = "sigmoid",
      bootstrap_CI = 0.95,
      bootstrap_reps = 1000,
      bootstrap_draws_per_rep = 1000,
      max_iter = 100,
      min_error = 1e-5,
      n_cores = 2
    )
  )



#
#   upper_quantile_inferences <- get_quantile_of_fmcm_inference(test$inference, quantile = 0.975)
#   lower_quantile_inferences <- get_quantile_of_fmcm_inference(test$inference, quantile = 0.025)
#
#   bootstrapped_means <- get_means_of_fmcm_inference(fmcm_inference = test$inference, get_bootstrapped_means = TRUE, confidence_interval = 0.95, bootstrap_reps = 5000, bootstrap_samples_per_rep = 5000)


  # ggplot() +
  #   geom_jitter(data = test$inference_for_plotting, aes(x = node, y = value)) +
  #   geom_crossbar(data = bootstrapped_means, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), fill = "red", color = "red", size = 0.1)
})


#
#
# test_that("fgcmconfr works", {
#   lower_adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.25, 0, 0, 0.25),
#     "C" = c(0, 0.25, 0, 0),
#     "D" = c(0, 0, 0.25, 0)
#   )
#   upper_adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.75, 0, 0, 0.75),
#     "C" = c(0, 0.75, 0, 0),
#     "D" = c(0, 0, 0.75, 0)
#   )
#   gm_1 <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
#   gm_2 <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix*1.2, upper_adj_matrix*1.2)
#   gm_3 <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix*0.8, upper_adj_matrix*0.8)
#   fgcm_adj_matrices <- list(gm_1, gm_2, gm_3)
#
#   fgcm_adj_matrices[[1]][1, 3][[1]] <- list(grey_number(0.2, 0.8))
#
#   mm_1 <- (lower_adj_matrix + upper_adj_matrix)/2
#   mm_2 <- mm_1*1.2
#   mm_3 <- mm_1*0.8
#   mode_adj_matrices <- list(mm_1, mm_2, mm_3)
#
#   samples = 1000
#   aggregation_fun = "mean"
#   include_zeroes = TRUE
#   clamping_vector = c(1, 0, 0, 0)
#   activation = "kosko"
#   squashing = "sigmoid"
#   lambda = 1
#   max_iter = 100
#   min_error = 1e-5
#   bootstrap_inference_means = TRUE
#   bootstrap_CI = 0.95
#   show_progress = TRUE
#   parallel = TRUE
#   n_cores = integer()
#   IDs = c()
#   include_simulations_in_output = TRUE
#
#
# })
#
#
#
#
#
# test_that("ftcmconfr works", {
#   tri_matrix_1 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#     lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#     mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
#     upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#   )
#   tri_matrix_2 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#     lower = matrix(data = c(0, 0.4, 0, 0.1), nrow = 2, ncol = 2),
#     mode = matrix(data = c(0, 0.6, 0, 0.3), nrow = 2, ncol = 2),
#     upper = matrix(data = c(0, 0.8, 0, 0.4), nrow = 2, ncol = 2)
#   )
#   tri_matrix_3 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#     lower = matrix(data = c(0, 0.4, 0.1, 0.1), nrow = 2, ncol = 2),
#     mode = matrix(data = c(0, 0.6, 0.2, 0.3), nrow = 2, ncol = 2),
#     upper = matrix(data = c(0, 0.8, 0.3, 0.4), nrow = 2, ncol = 2)
#   )
#
#   triangular_adj_matrices <- list(tri_matrix_1, tri_matrix_2, tri_matrix_3)
#
#   ftcmconfr(
#     ftcm_adj_matrices = triangular_adj_matrices,
#     samples = 1000,
#     include_zeroes_in_aggregation = TRUE,
#     aggregation_fun = "mean",
#     initial_state_vector = c(),
#     clamping_vector = c(),
#     activation = "kosko",
#     squashing = "sigmoid",
#     lambda = 1,
#     max_iter = 100,
#     min_error = 1e-5,
#     bootstrap_inference_means = TRUE,
#     bootstrap_CI = 0.95,
#     bootstrap_reps = 5000,
#     bootstrap_draws_per_rep = 5000,
#     show_progress = TRUE,
#     parallel = TRUE,
#     n_cores = integer(),
#     IDs = c(),
#     include_simulations_in_output = FALSE
#   )
# })
#
# test_that("build_ftcmconfr_models works", {
#   tri_matrix_1 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#     lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
#     mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
#     upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
#   )
#   tri_matrix_2 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#     lower = matrix(data = c(0, 0.4, 0, 0.1), nrow = 2, ncol = 2),
#     mode = matrix(data = c(0, 0.6, 0, 0.3), nrow = 2, ncol = 2),
#     upper = matrix(data = c(0, 0.8, 0, 0.4), nrow = 2, ncol = 2)
#   )
#   tri_matrix_3 <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
#     lower = matrix(data = c(0, 0.4, 0.1, 0.1), nrow = 2, ncol = 2),
#     mode = matrix(data = c(0, 0.6, 0.2, 0.3), nrow = 2, ncol = 2),
#     upper = matrix(data = c(0, 0.8, 0.3, 0.4), nrow = 2, ncol = 2)
#   )
#
#   triangular_adj_matrices <- list(tri_matrix_1, tri_matrix_2, tri_matrix_3)
#   nodes <- c("C1", "C2")
#
#   expect_no_error(build_ftcmconfr_models(triangular_adj_matrices, samples = 1000, aggregation_fun = "mean", include_zeroes = TRUE, nodes = nodes))
# })

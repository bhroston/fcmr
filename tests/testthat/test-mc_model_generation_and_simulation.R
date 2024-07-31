
test_that("build_conventional_fcmconfr_models works", {
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
    nonparametric_models <- build_conventional_fcmconfr_models(test_fcms, "nonparametric", 1000)
  )
  expect_no_error(
    uniform_models <- build_conventional_fcmconfr_models(test_fcms, "uniform", 1000)
  )
  expect_no_error(
    triangular_models <- build_conventional_fcmconfr_models(test_fcms, "triangular", 1000)
  )

  # Check visualizations to confirm distribution shapes
  # hist(unlist(lapply(nonparametric_models, function(x) x[1, 2])))
  # hist(unlist(lapply(uniform_models, function(x) x[1, 2])))
  # hist(unlist(lapply(triangular_models, function(x) x[1, 2])))
})


test_that("build_unconventional_fcmconfr_models works", {
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
    fgcm_models <- build_unconventional_fcmconfr_models(fgcm_adj_matrices, "mean", 1000)
  )
  expect_no_error(
    fgcm_models <- build_unconventional_fcmconfr_models(fgcm_adj_matrices, "mean", 1000, include_zeroes = TRUE)
  )
  expect_no_error(
    fgcm_models <- build_unconventional_fcmconfr_models(fgcm_adj_matrices, "median", 1000)
  )
  expect_no_error(
    fgcm_models <- build_unconventional_fcmconfr_models(fgcm_adj_matrices, "median", 1000, include_zeroes = TRUE)
  )

  expect_no_error(
    ftcm_models <- build_unconventional_fcmconfr_models(triangular_adj_matrices, "mean", 1000)
  )
  expect_no_error(
    ftcm_models <- build_unconventional_fcmconfr_models(triangular_adj_matrices, "mean", 1000, include_zeroes = TRUE)
  )
  expect_no_error(
    ftcm_models <- build_unconventional_fcmconfr_models(triangular_adj_matrices, "median", 1000)
  )
  expect_no_error(
    ftcm_models <- build_unconventional_fcmconfr_models(triangular_adj_matrices, "median", 1000, include_zeroes = TRUE)
  )

  # Check visualizations to confirm distribution shapes
  # hist(unlist(lapply(fgcm_models, function(x) x[1, 2])))
  # hist(unlist(lapply(ftcm_models, function(x) x[2, 1])))
})


test_that("infer_fmcm_with_clamping works", {
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
  sampled_adj_matrices <- build_unconventional_fcmconfr_models(fgcm_adj_matrices, "mean", 1000)
  expect_no_error(
    test_fmcm_inference <- infer_fmcm_with_clamping(
      simulated_adj_matrices = sampled_adj_matrices,
      initial_state_vector <- c(1, 1, 1, 1),
      clamping_vector <- c(1, 0, 0, 0),
      activation = "kosko",
      squashing = "sigmoid",
      lambda = 1,
      max_iter = 1000,
      min_error = 1e-5,
      parallel = TRUE,
      show_progress = TRUE,
      n_cores = 2
    )
  )


  # initial_state_vector <- c(1, 1, 1, 1)
  # clamping_vector <- c(1, 0, 0, 0)
  # activation = "kosko"
  # squashing = "tanh"
  # lambda = 1
  # max_iter = 1000
  # min_error = 1e-5
  # lambda_optimization = "koutsellis"
  # IDs = c()
  # parallel = TRUE
  # show_progress = TRUE
  #
  # n_simulations <- 100
})


test_that("get_means_of_fmcm_inference", {
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
  sampled_adj_matrices <- build_unconventional_fcmconfr_models(fgcm_adj_matrices, "mean", 1000)
  test_fmcm_inference <- infer_fmcm_with_clamping(
    simulated_adj_matrices = sampled_adj_matrices,
    initial_state_vector <- c(1, 1, 1, 1),
    clamping_vector <- c(1, 0, 0, 0),
    activation = "kosko",
    squashing = "sigmoid",
    lambda = 1,
    max_iter = 1000,
    min_error = 1e-5,
    parallel = TRUE,
    show_progress = TRUE,
    n_cores = 2
  )

  test_means <- round(apply(test_fmcm_inference$inference, 2, mean), 1)
  names(test_means) <- NULL
  expect_equal(test_means, c(0.5, 0.1, 0, 0))

  bootstrap_mean_CIs <- get_means_of_fmcm_inference(test_fmcm_inference$inference, n_cores = 2)

  test_nobootstrap <- get_means_of_fmcm_inference(
    test_fmcm_inference$inference,
    get_bootstrapped_means = FALSE
  )
  test_bootstrap_parallel_progress <- get_means_of_fmcm_inference(
    test_fmcm_inference$inference,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = TRUE,
    n_cores = 2,
    show_progress = TRUE
  )
  test_bootstrap_parallel_noprogress <- get_means_of_fmcm_inference(
    test_fmcm_inference$inference,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = TRUE,
    n_cores = 2,
    show_progress = FALSE
  )
  test_bootstrap_noparallel_progress <- get_means_of_fmcm_inference(
    test_fmcm_inference$inference,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = FALSE,
    show_progress = TRUE
  )
  test_bootstrap_noparallel_noprogress <- get_means_of_fmcm_inference(
    test_fmcm_inference$inference,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = FALSE,
    show_progress = FALSE
  )

  Lower_CIs_parallel_progress <- round(test_bootstrap_parallel_progress$mean_CI_by_node$lower_0.025, 2)
  Lower_CIs_parallel_noprogress <- round(test_bootstrap_parallel_noprogress$mean_CI_by_node$lower_0.025, 2)
  Lower_CIs_noparallel_progress <- round(test_bootstrap_noparallel_progress$mean_CI_by_node$lower_0.025, 2)
  Lower_CIs_noparallel_noprogress <- round(test_bootstrap_noparallel_noprogress$mean_CI_by_node$lower_0.025, 2)

  acceptable_random_error <- 0.02

  expect_true(all(abs(Lower_CIs_parallel_progress - Lower_CIs_parallel_progress) < acceptable_random_error))
  expect_true(all(abs(Lower_CIs_noparallel_progress - Lower_CIs_parallel_progress) < acceptable_random_error))
  expect_true(all(abs(Lower_CIs_parallel_progress - Lower_CIs_parallel_noprogress) < acceptable_random_error))
  expect_true(all(abs(Lower_CIs_noparallel_progress - Lower_CIs_parallel_noprogress) < acceptable_random_error))

  # Perform visual check
  # x <- test_bootstrap_noparallel_noprogress
  # x <- x[x$node != "A",]
  # ggplot() +
  #   geom_crossbar(data = x, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), fill = "red", color = "red") +
  #   geom_text(data = x, aes(x = node, y = lower_0.025 - 0.05, label = round(lower_0.025, 2))) +
  #   geom_text(data = x, aes(x = node, y = upper_0.975 + 0.05, label = round(upper_0.975, 2))) +
  #   ylim(0, 1) +
  #   theme_classic()
})


test_that("get_quantiles_of_simulated_values_across_iters works", {
  initial_state_vector <- c(1, 1, 1, 1)
  clamping_vector <- c(1, 0, 0, 0)
  activation = "kosko"
  squashing = "tanh"
  lambda = 1
  max_iter = 100
  min_error = 1e-5
  IDs = c()
  parallel = TRUE
  n_cores = integer()
  show_progress = TRUE
  include_simulations_in_output <- TRUE

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
  sampled_adj_matrices <- build_unconventional_fcmconfr_models(fgcm_adj_matrices, "mean", 1000)
  test_fmcm_inference <- infer_fmcm_with_clamping(
    simulated_adj_matrices = sampled_adj_matrices,
    initial_state_vector <- c(1, 1, 1, 1),
    clamping_vector <- c(1, 0, 0, 0),
    activation = "kosko",
    squashing = "tanh",
    lambda = 1,
    max_iter = 1000,
    min_error = 1e-5,
    parallel = TRUE,
    show_progress = TRUE,
    n_cores = 2
  )

  test_lower_quantile <- get_quantile_of_fmcm_inference(test_fmcm_inference$inference, quantile = 0.25)
  test_upper_quantile <- get_quantile_of_fmcm_inference(test_fmcm_inference$inference, quantile = 0.75)

  matches_expected_lower_quantile_A <- test_lower_quantile$X1 == 1
  matches_expected_lower_quantile_B <- test_lower_quantile$X2 < 0.5 & test_lower_quantile$X2 > 0.3
  matches_expected_lower_quantile_C <- test_lower_quantile$X3 < 0.4 & test_lower_quantile$X3 > 0.2
  matches_expected_lower_quantile_D <- test_lower_quantile$X4 < 0.3 & test_lower_quantile$X4 > 0.05

  expect_true(all(matches_expected_lower_quantile_A, matches_expected_lower_quantile_B,
                  matches_expected_lower_quantile_C, matches_expected_lower_quantile_D))

  # Perform visual check
  # test_lower_quantile_long <- tidyr::pivot_longer(test_lower_quantile, cols = 2:ncol(test_lower_quantile))
  # test_upper_quantile_long <- tidyr::pivot_longer(test_upper_quantile, cols = 2:ncol(test_upper_quantile))
  # ggplot() +
  #   geom_line(data = test_lower_quantile_long, aes(x = iter, y = value, color = name)) +
  #   geom_line(data = test_upper_quantile_long, aes(x = iter, y = value, color = name))
})




#
# test_that("confer_fmcm works with get_means_of_fmcm_inference plot", {
#   mode_adj_matrix <- adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.5, 0, 0, 0.5),
#     "C" = c(0, 0.5, 0, 0),
#     "D" = c(0, 0, 0.5, 0)
#   )
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
#   initial_state_vector <- c(1, 1, 1, 1)
#   clamping_vector <- c(1, 0, 0, 0)
#   activation = "kosko"
#   squashing = "tanh"
#   lambda = 1
#   max_iter = 1000
#   min_error = 1e-5
#   lambda_optimization = "koutsellis"
#   IDs = c()
#   parallel = TRUE
#   show_progress = TRUE
#
#   n_simulations <- 100
#
#   test_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
#   uniform_fmcm_models <- build_fmcm_models_from_grey_adj_matrix(test_grey_adj_matrix, distribution = "uniform", n_sims = n_simulations)
#   triangular_fmcm_models <- build_fmcm_models_from_grey_adj_matrix(test_grey_adj_matrix, mode_adj_matrix, distribution = "triangular", n_sims = n_simulations)
#
#   test_confer_fmcm <- confer_fmcm(uniform_fmcm_models, initial_state_vector, clamping_vector, activation = "kosko", squashing = "tanh",
#                                   lambda = 0.5, max_iter = 100, parallel = TRUE, n_cores = 2, show_progress = TRUE, include_simulations_in_output = TRUE)
#
#   expect_equal(colnames(test_confer_fmcm$inference), c("A", "B", "C", "D"))
#   expect_equal(nrow(test_confer_fmcm$inference), n_simulations)
#   expect_equal(colnames(test_confer_fmcm$inference_for_plotting), c("node", "value"))
#
#   # Perform visual check
#   # bootstrap_mean_CIs <- get_means_of_fmcm_inferences(test_confer_fmcm$inferences)
#   # bootstrap_mean_CIs <- bootstrap_mean_CIs[bootstrap_mean_CIs$node != "A", ]
#   # bootstrap_mean_CIs_plotting <- tidyr::pivot_longer(bootstrap_mean_CIs, cols = 2:3)
#   # bootstrap_mean_CIs_plotting <- bootstrap_mean_CIs_plotting[bootstrap_mean_CIs_plotting$node != "A", ]
#   #  x <- test_confer_fmcm$inferences_for_plotting[test_confer_fmcm$inferences_for_plotting$node != "A", ]
#   #  ggplot() +
#   #    geom_jitter(data = x, aes(x = node, y = value), alpha = 0.025) +
#   #    # geom_crossbar(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), color = "red", fill = "red") +
#   #    geom_errorbar(data = bootstrap_mean_CIs_plotting, aes(x = node, ymin = value, ymax = value, group = name)) +
#   #    geom_text(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025 - 0.05, label = round(lower_0.025, 2))) +
#   #    geom_text(data = bootstrap_mean_CIs, aes(x = node, y = upper_0.975 + 0.05, label = round(upper_0.975, 2))) +
#   #    ylim(0, 1) +
#   #    theme_classic()
# })
#


# test_that("fmcm works", {
#   edgelist_test <- data.frame(
#     source = c("A", "B"),
#     target = c("B", "C"),
#     weight = c(0.4, 0.6)
#   )
#
#   edgelist_lower <- data.frame(
#     source = c("A", "B"),
#     target = c("B", "C"),
#     weight = c(0.2, 0.2)
#   )
#
#   edgelist_upper <- data.frame(
#     source = c("A", "B"),
#     target = c("B", "C"),
#     weight = c(0.8, 0.8)
#   )
#
#   edgelist_mode <- data.frame(
#     source = c("A", "B"),
#     target = c("B", "C"),
#     weight = c(0.5, 0.5)
#   )
#
#   test_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_test)
#   l_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_lower)
#   u_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_upper)
#   m_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_mode)
#
#   # Accept if additional_inputs is a list
#   expect_no_error(fmcm(test_adj_matrix, ... = list(lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)))
#
#   # Throws error when additional inputs do not have matching dims
#   expect_error(fmcm(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = matrix(data = NA, nrow = 5, ncol = 5)))
#   # Throws error when additional inputs have incorrect edge data
#   expect_error(fmcm(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = u_adj_matrix + 0.2))
#
#   # IF distribution == "uniform"
#   # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
#   expect_error(fmcm(test_adj_matrix, l = l_adj_matrix, u = u_adj_matrix))
#   # Throws error when values in lower_adj_matrix are greater than values in upper_adj_matrix
#   expect_error(fmcm(test_adj_matrix, lower_adj_matrix = u_adj_matrix, upper_adj_matrix = l_adj_matrix))
#   # Throws error if too many additional inputs present
#   expect_error(fmcm(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix))
#
#   uniform_fmcm <- fmcm(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
#   expect_equal(names(uniform_fmcm$edgelist), c("source", "target", "weight", "lower", "upper"))
#   expect_equal(uniform_fmcm$distribution_params$lower, l_adj_matrix)
#   expect_equal(uniform_fmcm$distribution_params$upper, u_adj_matrix)
#
#   # IF distribution == "triangular"
#   # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
#   expect_error(fmcm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, u = u_adj_matrix))
#   # Throws warning when no mode_adj_matrix input given
#   expect_warning(fmcm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix))
#   # Throws error when values in lower_adj_matrix are less than values in mode_adj_matrix
#   expect_error(fmcm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*0.01))
#   # Throws error when values in upper_adj_matrix are greater than values in mode_adj_matrix
#   expect_error(fmcm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*2))
#
#   triangular_fmcm <- fmcm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix)
#
#   expect_equal(names(triangular_fmcm$edgelist), c("source", "target", "weight", "lower", "upper", "mode"))
#   expect_equal(triangular_fmcm$distribution_params$lower, l_adj_matrix)
#   expect_equal(triangular_fmcm$distribution_params$upper, u_adj_matrix)
#   expect_equal(triangular_fmcm$distribution_params$mode, m_adj_matrix)
# })
#




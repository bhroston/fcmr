
# microbenchmark(
#   build_monte_carlo_fcms_from_conventional_adj_matrices(adj_matrix_list, N_samples, include_zeroes = TRUE, show_progress),
#   build_monte_carlo_fcms_from_conventional_adj_matrices(adj_matrix_list, N_samples, include_zeroes = FALSE, show_progress)
# )
#
# microbenchmark(
#   build_monte_carlo_fcms_from_fuzzy_adj_matrices(adj_matrix_list, adj_matrix_list_class, N_samples, include_zeroes = TRUE, show_progress),
#   build_monte_carlo_fcms_from_fuzzy_adj_matrices(adj_matrix_list, adj_matrix_list_class, N_samples, include_zeroes = FALSE, show_progress)
# )


test_that("infer_monte_carlo_fcm_set works with ivfn fcms", {
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
  ivfn_mat_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  ivfn_mat_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix*1.2, upper_adj_matrix*1.2)
  ivfn_mat_3 <- make_adj_matrix_w_ivfns(lower_adj_matrix*0.8, upper_adj_matrix*0.8)
  adj_matrices <- list(ivfn_mat_1, ivfn_mat_2, ivfn_mat_3)
  invisible(capture.output(
    test_mc_fcms <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 50, include_zeroes = FALSE)
  ))

  # Check parallel and show_progress run
  invisible(capture.output(
    expect_no_error(
      test_fmcm_inference_p_sp <- infer_monte_carlo_fcm_set(
        mc_adj_matrices = test_mc_fcms,
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
  ))

  # Check parallel and show_progress = FALSE run
  invisible(capture.output(
    expect_no_error(
      test_fmcm_inference_p_and_no_sp <- infer_monte_carlo_fcm_set(
        mc_adj_matrices = test_mc_fcms,
        initial_state_vector <- c(1, 1, 1, 1),
        clamping_vector <- c(1, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 1000,
        min_error = 1e-5,
        parallel = TRUE,
        show_progress = FALSE,
        n_cores = 2
      )
    )
  ))

  # Check parallel = FALSE and show_progress run
  invisible(capture.output(
    expect_warning(
      test_fmcm_inference_no_p_and_sp <- infer_monte_carlo_fcm_set(
        mc_adj_matrices = test_mc_fcms,
        initial_state_vector <- c(1, 1, 1, 1),
        clamping_vector <- c(1, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 1000,
        min_error = 1e-5,
        parallel = FALSE,
        show_progress = TRUE,
        n_cores = 2
      )
    )
  ))

  # Check parallel = FALSE and show_progress = FALSE run
  invisible(capture.output(
    expect_no_error(
      test_fmcm_inference_no_p_and_no_sp <- infer_monte_carlo_fcm_set(
        mc_adj_matrices = test_mc_fcms,
        initial_state_vector <- c(1, 1, 1, 1),
        clamping_vector <- c(1, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 1000,
        min_error = 1e-5,
        parallel = FALSE,
        show_progress = FALSE
      )
    )
  ))

  # Confirm all methods produce same output
  max_error <- 1e-5
  expect_true(all(abs(test_fmcm_inference_p_sp$inference - test_fmcm_inference_no_p_and_sp$inference) <= max_error))
  expect_true(all(abs(test_fmcm_inference_no_p_and_sp$inference - test_fmcm_inference_p_and_no_sp$inference) <= max_error))
  expect_true(all(abs(test_fmcm_inference_p_and_no_sp$inference - test_fmcm_inference_no_p_and_no_sp$inference) <= max_error))
})


test_that("infer_monte_carlo_fcm_set catches invalid parallel processing inputs", {
  invisible(capture.output(
    expect_warning( # When no n_cores input given but parallel processing is intended
      infer_monte_carlo_fcm_set(
        salinization_conventional_fcms,
        initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
        clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 100,
        min_error = 1e-5,
        parallel = TRUE
      )
    )
  ))

  invisible(capture.output(
    expect_error( # When no n_cores input is not an integer
      infer_monte_carlo_fcm_set(
        salinization_conventional_fcms,
        initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
        clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 100,
        min_error = 1e-5,
        parallel = TRUE,
        n_cores = 1.5
      )
    )
  ))

  invisible(capture.output(
    expect_no_error( # When n_cores is a positive, odd integer
      infer_monte_carlo_fcm_set(
        salinization_conventional_fcms,
        initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
        clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 100,
        min_error = 1e-5,
        parallel = TRUE,
        n_cores = 3
      )
    )
  ))

  invisible(capture.output(
    expect_warning( # When n_cores is larger than number of available cores
      infer_monte_carlo_fcm_set(
        salinization_conventional_fcms,
        initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
        clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 100,
        min_error = 1e-5,
        parallel = TRUE,
        n_cores = parallel::detectCores() + 1
      )
    )
  ))

  invisible(capture.output(
    expect_warning( # When n_cores input given but parallel processing is not intended
      infer_monte_carlo_fcm_set(
        salinization_conventional_fcms,
        initial_state_vector = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
        clamping_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
        activation = "kosko",
        squashing = "sigmoid",
        lambda = 1,
        max_iter = 100,
        min_error = 1e-5,
        parallel = FALSE,
        n_cores = 2
      )
    )
  ))
})


# test_that("infer_monte_carlo_fcm_set works with salinization data sets", {
#   mc_adj_matrices <- build_monte_carlo_fcms(salinization_conventional_fcms, N_samples = 100, include_zeroes = FALSE, show_progress = TRUE)
#   test <- infer_monte_carlo_fcm_set(
#     mc_adj_matrices,
#     initial_state_vector = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
#     clamping_vector = c(),
#     activation = "modified-kosko", squashing = "sigmoid", lambda = 1,
#     max_iter = 1000, min_error = 1e-5,
#     parallel = TRUE, n_cores = 2, show_progress = TRUE
#   )
# })


test_that("get_mc_simulations_inference_CIs_w_bootstrap", {
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
  ivfn_mat_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  ivfn_mat_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix*1.2, upper_adj_matrix*1.2)
  ivfn_mat_3 <- make_adj_matrix_w_ivfns(lower_adj_matrix*0.8, upper_adj_matrix*0.8)
  adj_matrices <- list(ivfn_mat_1, ivfn_mat_2, ivfn_mat_3)

  invisible(capture.output(
    test_mc_fcms <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 1000, include_zeroes = FALSE)
  ))
  invisible(capture.output(
    test_mc_fcms_inferences <- infer_monte_carlo_fcm_set(
      mc_adj_matrices = test_mc_fcms,
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
  ))

  # Check for median estimation
  expect_no_error(
    invisible(capture.output(
      mc_CIs_median_p_and_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "median", 0.95, parallel = TRUE, show_progress = TRUE, n_cores = 2),
      mc_CIs_median_p_and_no_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "median", 0.95, parallel = TRUE, show_progress = FALSE, n_cores = 2),
      mc_CIs_median_no_p_and_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "median", 0.95, parallel = FALSE, show_progress = TRUE),
      mc_CIs_median_no_p_and_no_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "median", 0.95, parallel = FALSE, show_progress = FALSE)
    ))
  )

  # Confirm all methods produce similar output (cannot produce exactly the same
  # because of stochasticity in bootstrapping methods)
  expect_true(all(abs(mc_CIs_median_p_and_sp$CI_by_node[, -1] - mc_CIs_median_p_and_no_sp$CI_by_node[, -1]) < 0.3))
  expect_true(all(abs(mc_CIs_median_p_and_no_sp$CI_by_node[, -1] - mc_CIs_median_no_p_and_sp$CI_by_node[, -1]) < 0.3))
  expect_true(all(abs( mc_CIs_median_no_p_and_sp$CI_by_node[, -1] - mc_CIs_median_no_p_and_no_sp$CI_by_node[, -1]) < 0.3))


  # Check for mean estimate
  expect_no_error(
    invisible(capture.output(
      mc_CIs_mean_p_and_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "mean", 0.95, parallel = TRUE, show_progress = TRUE, n_cores = 2),
      mc_CIs_mean_p_and_no_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "mean", 0.95, parallel = TRUE, show_progress = FALSE, n_cores = 2),
      mc_CIs_mean_no_p_and_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "mean", 0.95, parallel = FALSE, show_progress = TRUE),
      mc_CIs_mean_no_p_and_no_sp <- get_mc_simulations_inference_CIs_w_bootstrap(test_mc_fcms_inferences$inference, "mean", 0.95, parallel = FALSE, show_progress = FALSE)
    ))
  )

  # Confirm all methods produce similar output (cannot produce exactly the same
  # because of stochasticity in bootstrapping methods)
  expect_true(all(abs(mc_CIs_mean_p_and_sp$CI_by_node[, -1] - mc_CIs_mean_p_and_no_sp$CI_by_node[, -1]) < 0.3))
  expect_true(all(abs(mc_CIs_mean_p_and_no_sp$CI_by_node[, -1] - mc_CIs_mean_no_p_and_sp$CI_by_node[, -1]) < 0.3))
  expect_true(all(abs( mc_CIs_mean_no_p_and_sp$CI_by_node[, -1] - mc_CIs_mean_no_p_and_no_sp$CI_by_node[, -1]) < 0.3))


  # Check rejects non-dataframe objects
  expect_error(get_mc_simulations_inference_CIs_w_bootstrap("a", "median", 0.95, parallel = FALSE))
})


test_that("get_mc_simulations_inference_CIs_w_bootstrap catches invalid parallel processing inputs", {
  test_mc_fcms <- build_monte_carlo_fcms(salinization_conventional_fcms, N_samples = 100, include_zeroes = TRUE, show_progress = FALSE)
  invisible(capture.output(
    test_mc_fcms_inferences <- infer_monte_carlo_fcm_set(
      mc_adj_matrices = test_mc_fcms,
      initial_state_vector <- c(1, 1, 1, 1, 1, 1, 1, 1, 1),
      clamping_vector <- c(1, 0, 0, 0, 0, 0, 0, 0, 0),
      activation = "kosko",
      squashing = "sigmoid",
      lambda = 1,
      max_iter = 1000,
      min_error = 1e-5,
      parallel = FALSE,
      show_progress = FALSE
    )
  ))

  invisible(capture.output(
    expect_warning( # When no n_cores input given but parallel processing is intended
      get_mc_simulations_inference_CIs_w_bootstrap(
        mc_simulations_inference_df = test_mc_fcms_inferences$inference,
        inference_function = "mean",
        confidence_interval = 0.95,
        bootstrap_reps = 100,
        parallel = TRUE
      )
    )
  ))

  invisible(capture.output(
    expect_error( # When no n_cores input is not an integer
      get_mc_simulations_inference_CIs_w_bootstrap(
        mc_simulations_inference_df = test_mc_fcms_inferences$inference,
        inference_function = "mean",
        confidence_interval = 0.95,
        bootstrap_reps = 100,
        parallel = TRUE,
        n_cores = 1.5
      )
    )
  ))

  invisible(capture.output(
    expect_no_error( # When n_cores is a positive, odd integer
      get_mc_simulations_inference_CIs_w_bootstrap(
        mc_simulations_inference_df = test_mc_fcms_inferences$inference,
        inference_function = "mean",
        confidence_interval = 0.95,
        bootstrap_reps = 100,
        parallel = TRUE,
        n_cores = 3
      )
    )
  ))

  invisible(capture.output(
    expect_warning( # When n_cores is larger than number of available cores
      get_mc_simulations_inference_CIs_w_bootstrap(
        mc_simulations_inference_df = test_mc_fcms_inferences$inference,
        inference_function = "mean",
        confidence_interval = 0.95,
        bootstrap_reps = 100,
        parallel = TRUE,
        n_cores = parallel::detectCores() + 1
      )
    )
  ))

  invisible(capture.output(
    expect_warning( # When n_cores is larger than number of available cores
      get_mc_simulations_inference_CIs_w_bootstrap(
        mc_simulations_inference_df = test_mc_fcms_inferences$inference,
        inference_function = "mean",
        confidence_interval = 0.95,
        bootstrap_reps = 100,
        parallel = FALSE,
        n_cores = 2
      )
    )
  ))
})


test_that("build_monte_carlo_fcms works", {
  test_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0, 0, 0, 1),
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
  invisible(capture.output(
    mc_fcms_sp <- build_monte_carlo_fcms(test_fcms, N_samples = 1000, include_zeroes = FALSE, show_progress = TRUE),
    mc_fcms_no_sp <- build_monte_carlo_fcms(test_fcms, N_samples = 1000, include_zeroes = FALSE, show_progress = FALSE)
  ))
  expect_true(all(unique(unlist(lapply(mc_fcms_sp, function(fcm) fcm[1, 2]))) %in% c(0.5, 0.25, 0.75)))
  expect_true(all(unique(unlist(lapply(mc_fcms_no_sp, function(fcm) fcm[1, 2]))) %in% c(0.5, 0.25, 0.75)))



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
  ivfn_mat_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  ivfn_mat_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix*1.2, upper_adj_matrix*1.2)
  ivfn_mat_3 <- make_adj_matrix_w_ivfns(lower_adj_matrix*0.8, upper_adj_matrix*0.8)
  adj_matrices <- list(ivfn_mat_1, ivfn_mat_2, ivfn_mat_3)

  invisible(capture.output(
    test_mc_fcms_sp <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 100, include_zeroes = FALSE, show_progress = TRUE),
    test_mc_fcms_no_sp <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 100, include_zeroes = FALSE, show_progress = FALSE)
  ))
  mc_samples_row1_col2 <- unlist(lapply(test_mc_fcms_sp, function(x) x[1, 2]))

  expected_samples_row1_col2 <- c(runif(1000, 0.25, 0.75), runif(1000, 0.3, 0.9), runif(1000, 0.2, 0.6))
  diff_in_means <- abs(mean(mc_samples_row1_col2) - mean(expected_samples_row1_col2))
  diff_in_var <- abs(var(mc_samples_row1_col2) - var(expected_samples_row1_col2))

  expect_lt(diff_in_means, 0.05)
  expect_lt(diff_in_var, 0.05)

  # Check when differrent edges across maps
  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0.33, 0, 0),
    "B" = c(0, 0, 0.33, 0),
    "C" = c(0, 0, 0, 0.33),
    "D" = c(0.33, 0, 0, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0.88, 0, 0),
    "B" = c(0, 0, 0.88, 0),
    "C" = c(0, 0, 0, 0.88),
    "D" = c(0.88, 0, 0, 0)
  )
  ivfn_mat_4 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)
  adj_matrices <- list(ivfn_mat_1, ivfn_mat_2, ivfn_mat_3, ivfn_mat_4)

  invisible(capture.output(
    build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 100, include_zeroes = FALSE, show_progress = TRUE)
  ))


  tri_matrix_1 <- make_adj_matrix_w_tfns(
    lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
  )
  tri_matrix_2 <- make_adj_matrix_w_tfns(
    lower = matrix(data = c(0, 0.4, 0, 0.1), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.6, 0, 0.3), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.8, 0, 0.4), nrow = 2, ncol = 2)
  )
  tri_matrix_3 <- make_adj_matrix_w_tfns(
    lower = matrix(data = c(0, 0.4, 0.1, 0.1), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.6, 0.2, 0.3), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.8, 0.3, 0.4), nrow = 2, ncol = 2)
  )
  triangular_adj_matrices <- list(tri_matrix_1, tri_matrix_2, tri_matrix_3)
  invisible(capture.output(
    test_mc_fcms <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(triangular_adj_matrices, "tfn", 1000, include_zeroes = FALSE)
  ))
  mc_samples_row2_col1 <- unlist(lapply(test_mc_fcms, function(x) x[2, 1]))

  expected_samples_row2_col1 <- c(rtriangular_dist(1000, 0.2, 0.3, 0.4), rtriangular_dist(1000, 0.4, 0.6, 0.8), rtriangular_dist(1000, 0.4, 0.6, 0.8))
  diff_in_means <- abs(mean(mc_samples_row2_col1) - mean(expected_samples_row2_col1))
  diff_in_var <- abs(var(mc_samples_row2_col1) - var(expected_samples_row2_col1))

  expect_lt(diff_in_means, 0.02)
  expect_lt(diff_in_var, 0.02)


  # Confirm error with invalid class
  expect_error(build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(triangular_adj_matrices, "invalid_class", 1000, include_zeroes = FALSE))

})


test_that("build_monte_carlo_fcms_from_conventional_adj_matrices works", {
  test_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0, 0, 0, 1),
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
  invisible(capture.output(
    mc_fcms_sp <- build_monte_carlo_fcms(test_fcms, N_samples = 50, include_zeroes = FALSE, show_progress = TRUE),
    mc_fcms_no_sp <- build_monte_carlo_fcms(test_fcms, N_samples = 50, include_zeroes = FALSE, show_progress = FALSE)
  ))
  expect_true(all(unique(unlist(lapply(mc_fcms_sp, function(fcm) fcm[1, 2]))) %in% c(0.5, 0.25, 0.75)))
  expect_true(all(unique(unlist(lapply(mc_fcms_no_sp, function(fcm) fcm[1, 2]))) %in% c(0.5, 0.25, 0.75)))
})


test_that("build_monte_carlo_fcms_from_fuzzy_set_adj_matrices works", {
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
  ivfn_mat_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  ivfn_mat_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix*1.2, upper_adj_matrix*1.2)
  ivfn_mat_3 <- make_adj_matrix_w_ivfns(lower_adj_matrix*0.8, upper_adj_matrix*0.8)
  adj_matrices <- list(ivfn_mat_1, ivfn_mat_2, ivfn_mat_3)

  invisible(capture.output(
    test_mc_fcms_sp <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 100, include_zeroes = FALSE, show_progress = TRUE),
    test_mc_fcms_no_sp <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 100, include_zeroes = FALSE, show_progress = FALSE)
  ))
  mc_samples_row1_col2 <- unlist(lapply(test_mc_fcms_sp, function(x) x[1, 2]))

  expected_samples_row1_col2 <- c(runif(1000, 0.25, 0.75), runif(1000, 0.3, 0.9), runif(1000, 0.2, 0.6))
  diff_in_means <- abs(mean(mc_samples_row1_col2) - mean(expected_samples_row1_col2))
  diff_in_var <- abs(var(mc_samples_row1_col2) - var(expected_samples_row1_col2))

  expect_lt(diff_in_means, 0.05)
  expect_lt(diff_in_var, 0.05)

  # Check when differrent edges across maps
  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0.33, 0, 0),
    "B" = c(0, 0, 0.33, 0),
    "C" = c(0, 0, 0, 0.33),
    "D" = c(0.33, 0, 0, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0.88, 0, 0),
    "B" = c(0, 0, 0.88, 0),
    "C" = c(0, 0, 0, 0.88),
    "D" = c(0.88, 0, 0, 0)
  )
  ivfn_mat_4 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)
  adj_matrices <- list(ivfn_mat_1, ivfn_mat_2, ivfn_mat_3, ivfn_mat_4)

  invisible(capture.output(
    build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(adj_matrices, "ivfn", 100, include_zeroes = FALSE, show_progress = TRUE)
  ))


  tri_matrix_1 <- make_adj_matrix_w_tfns(
    lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
  )
  tri_matrix_2 <- make_adj_matrix_w_tfns(
    lower = matrix(data = c(0, 0.4, 0, 0.1), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.6, 0, 0.3), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.8, 0, 0.4), nrow = 2, ncol = 2)
  )
  tri_matrix_3 <- make_adj_matrix_w_tfns(
    lower = matrix(data = c(0, 0.4, 0.1, 0.1), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.6, 0.2, 0.3), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.8, 0.3, 0.4), nrow = 2, ncol = 2)
  )
  triangular_adj_matrices <- list(tri_matrix_1, tri_matrix_2, tri_matrix_3)
  invisible(capture.output(
    test_mc_fcms <- build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(triangular_adj_matrices, "tfn", 1000, include_zeroes = FALSE)
  ))
  mc_samples_row2_col1 <- unlist(lapply(test_mc_fcms, function(x) x[2, 1]))

  expected_samples_row2_col1 <- c(rtriangular_dist(1000, 0.2, 0.3, 0.4), rtriangular_dist(1000, 0.4, 0.6, 0.8), rtriangular_dist(1000, 0.4, 0.6, 0.8))
  diff_in_means <- abs(mean(mc_samples_row2_col1) - mean(expected_samples_row2_col1))
  diff_in_var <- abs(var(mc_samples_row2_col1) - var(expected_samples_row2_col1))

  expect_lt(diff_in_means, 0.02)
  expect_lt(diff_in_var, 0.02)


  # Confirm error with invalid class
  expect_error(build_monte_carlo_fcms_from_fuzzy_set_adj_matrices(triangular_adj_matrices, "invalid_class", 1000, include_zeroes = FALSE))

})


test_that("monte_carlo_bootstrap_checks works", {

  # Check catches invalid inference_function
  expect_error(
    monte_carlo_bootstrap_checks(
      inference_function = "test",
      confidence_interval = 0.95,
      bootstrap_reps = 1000
    )
  )

  # Check catches invalid confidence interval
  expect_error(
    monte_carlo_bootstrap_checks(
      inference_function = "mean",
      confidence_interval = "a",
      bootstrap_reps = 1000
    )
  )
  expect_error(
    monte_carlo_bootstrap_checks(
      inference_function = "mean",
      confidence_interval = 5,
      bootstrap_reps = 1000
    )
  )

  # Check catches invalid bootstrap_reps
  expect_error(
    monte_carlo_bootstrap_checks(
      inference_function = "mean",
      confidence_interval = 0.95,
      bootstrap_reps = "a"
    )
  )
  expect_error(
    monte_carlo_bootstrap_checks(
      inference_function = "mean",
      confidence_interval = 0.95,
      bootstrap_reps = 10.5
    )
  )
})

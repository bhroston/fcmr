
test_that("infer_fcm works", {


  adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  test_infer <- infer_fcm(adj_matrix,
                          initial_state_vector = c(1, 1, 1, 1, 1, 1),
                          clamping_vector = c(1, 0, 0, 0, 0, 0),
                          activation = "kosko",
                          squashing = "sigmoid",
                          lambda = 1)
  test_inference <- round(test_infer$inference, 2)
  expect_equal(test_inference, data.frame(0.5, -0.1, 0, -0.07, 0.03, 0.02), ignore_attr = TRUE)

  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.2, 0, 0, 0.9, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.3, 0.9, -0.5, 0, -0.5, 0),
    C5 = c(0.5, 0, 0, -0.3, 0, 0),
    C6 = c(0, -0.4, 0, 0, -0.5, 0)
  )
  adj_matrix <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)

  test_sim <- infer_fcm(adj_matrix,
                                    initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                    clamping_vector = c(1, 0, 0, 0, 0, 0),
                                    activation = "kosko",
                                    squashing = "sigmoid",
                                    lambda = 1,
                                    fuzzy_set_samples = 1000)
  test_baseline <- simulate_ivfn_or_tfn_fcm(adj_matrix,
                                            initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                            clamping_vector = c(0, 0, 0, 0, 0, 0),
                                            activation = "kosko",
                                            squashing = "sigmoid",
                                            lambda = 1)
  test_scenario <- simulate_ivfn_or_tfn_fcm(adj_matrix,
                                            initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                            clamping_vector = c(1, 0, 0, 0, 0, 0),
                                            activation = "kosko",
                                            squashing = "sigmoid",
                                            lambda = 1)

  test_baseline_final_state <- test_baseline$state_vectors[nrow(test_baseline$state_vectors),][-1]
  test_scenario_final_state <- test_scenario$state_vectors[nrow(test_scenario$state_vectors),][-1]

  test_baseline_final_state_dists <- convert_fuzzy_set_elements_in_matrix_to_distributions(test_baseline_final_state, "ivfn", 1000)
  test_scenario_final_state_dists <- convert_fuzzy_set_elements_in_matrix_to_distributions(test_scenario_final_state, "ivfn", 1000)

  inferences_as_dists <- vector(mode = "list", length = length(test_baseline_final_state))
  for (i in 1:length(test_baseline_final_state)) {
    inferences_as_dists[[i]] <- test_scenario_final_state_dists[i][[1]][[1]] - test_baseline_final_state_dists[i][[1]][[1]]
  }

  expected_inference_lowers <- unlist(lapply(inferences_as_dists, min))
  expected_inference_uppers <- unlist(lapply(inferences_as_dists, max))

  actual_inference_lowers <- test_sim$inference_df$lower
  actual_inference_uppers <- test_sim$inference_df$upper

  error_in_lowers <- sum(abs(expected_inference_lowers - actual_inference_lowers))
  error_in_uppers <- sum(abs(expected_inference_uppers - actual_inference_uppers))
  total_error <- error_in_lowers + error_in_uppers

  expect_lt(total_error, 0.1)
})


test_that("infer_conventional_fcm works", {
  adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  test_infer <- infer_conventional_fcm(adj_matrix,
                                       initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                       clamping_vector = c(1, 0, 0, 0, 0, 0),
                                       activation = "kosko",
                                       squashing = "sigmoid",
                                       lambda = 1)
  test_inference <- round(test_infer$inference, 2)
  expect_equal(test_inference, data.frame(0.5, -0.1, 0, -0.07, 0.03, 0.02), ignore_attr = TRUE)
})


test_that("infer_ivfn_or_tfn_fcm works", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.2, 0, 0, 0.9, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.3, 0.9, -0.5, 0, -0.5, 0),
    C5 = c(0.5, 0, 0, -0.3, 0, 0),
    C6 = c(0, -0.4, 0, 0, -0.5, 0)
  )
  adj_matrix <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)

  test_sim <- infer_ivfn_or_tfn_fcm(adj_matrix,
                                    initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                    clamping_vector = c(1, 0, 0, 0, 0, 0),
                                    activation = "kosko",
                                    squashing = "sigmoid",
                                    lambda = 1,
                                    fuzzy_set_samples = 1000)
  test_baseline <- simulate_ivfn_or_tfn_fcm(adj_matrix,
                                            initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                            clamping_vector = c(0, 0, 0, 0, 0, 0),
                                            activation = "kosko",
                                            squashing = "sigmoid",
                                            lambda = 1)
  test_scenario <- simulate_ivfn_or_tfn_fcm(adj_matrix,
                                            initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                            clamping_vector = c(1, 0, 0, 0, 0, 0),
                                            activation = "kosko",
                                            squashing = "sigmoid",
                                            lambda = 1)

  test_baseline_final_state <- test_baseline$state_vectors[nrow(test_baseline$state_vectors),][-1]
  test_scenario_final_state <- test_scenario$state_vectors[nrow(test_scenario$state_vectors),][-1]

  test_baseline_final_state_dists <- convert_fuzzy_set_elements_in_matrix_to_distributions(test_baseline_final_state, "ivfn", 1000)
  test_scenario_final_state_dists <- convert_fuzzy_set_elements_in_matrix_to_distributions(test_scenario_final_state, "ivfn", 1000)

  inferences_as_dists <- vector(mode = "list", length = length(test_baseline_final_state))
  for (i in 1:length(test_baseline_final_state)) {
    inferences_as_dists[[i]] <- test_scenario_final_state_dists[i][[1]][[1]] - test_baseline_final_state_dists[i][[1]][[1]]
  }

  expected_inference_lowers <- unlist(lapply(inferences_as_dists, min))
  expected_inference_uppers <- unlist(lapply(inferences_as_dists, max))

  actual_inference_lowers <- test_sim$inference_df$lower
  actual_inference_uppers <- test_sim$inference_df$upper

  error_in_lowers <- sum(abs(expected_inference_lowers - actual_inference_lowers))
  error_in_uppers <- sum(abs(expected_inference_uppers - actual_inference_uppers))
  total_error <- error_in_lowers + error_in_uppers

  expect_lt(total_error, 0.1)
})


test_that("equalize_baseline_and_scenario_outputs works", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.2, 0, 0, 0.9, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.3, 0.9, -0.5, 0, -0.5, 0),
    C5 = c(0.5, 0, 0, -0.3, 0, 0),
    C6 = c(0, -0.4, 0, 0, -0.5, 0)
  )
  adj_matrix <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)

  test_baseline <- simulate_ivfn_or_tfn_fcm(adj_matrix,
                                            initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                            clamping_vector = c(0, 0, 0, 0, 0, 0),
                                            activation = "kosko",
                                            squashing = "sigmoid",
                                            lambda = 1)
  test_scenario <- simulate_ivfn_or_tfn_fcm(adj_matrix,
                                            initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                            clamping_vector = c(1, 0, 1, 1, 0, 0),
                                            activation = "kosko",
                                            squashing = "sigmoid",
                                            lambda = 1)
  equalized_dfs <- equalize_baseline_and_scenario_outputs(test_baseline$state_vectors, test_scenario$state_vectors)
  expect_equal(nrow(equalized_dfs$baseline), nrow(equalized_dfs$scenario))
})


test_that("simulate_fcm works", {
  adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  test_sim <- simulate_fcm(adj_matrix, c(1, 1, 1, 1, 1, 1), clamping_vector = c(1, 0, 0, 0, 0, 0), activation = "kosko", squashing = "sigmoid")
  expect_equal(round(test_sim$state_vectors[2, ], 2), data.frame(1, 1, 0.38, 0.5, 0.11, 0.33, 0.13), ignore_attr = TRUE)
  expect_equal(round(test_sim$state_vectors[nrow(test_sim$state_vectors), ], 2), data.frame(9, 1, 0.31, 0.5, 0.18, 0.49, 0.32), ignore_attr = TRUE)

  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.2, 0, 0, 0.9, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.3, 0.9, -0.5, 0, -0.5, 0),
    C5 = c(0.5, 0, 0, -0.3, 0, 0),
    C6 = c(0, -0.4, 0, 0, -0.5, 0)
  )
  adj_matrix <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  test_sim <- simulate_fcm(adj_matrix, c(1, 1, 1, 1, 1, 1), clamping_vector = c(1, 0, 0, 0, 0, 0), activation = "kosko", squashing = "sigmoid")
  first_iter <- test_sim$state_vectors[2, ][-1]
  first_iter_lowers <- round(vapply(first_iter, function(x) x[[1]]$lower, numeric(1)), 2)
  first_iter_uppers <- round(vapply(first_iter, function(x) x[[1]]$upper, numeric(1)), 2)
  expect_equal(first_iter_lowers, c(1, 0.38, 0.5, 0.11, 0.33, 0.13), ignore_attr = TRUE)
  expect_equal(first_iter_uppers, c(1, 0.67, 0.5, 0.4, 0.55, 0.29), ignore_attr = TRUE)

  final_iter <- test_sim$state_vectors[nrow(test_sim$state_vectors), ][-1]
  final_iter_lowers <- round(vapply(final_iter, function(x) x[[1]]$lower, numeric(1)), 2)
  final_iter_uppers <- round(vapply(final_iter, function(x) x[[1]]$upper, numeric(1)), 2)
  expect_equal(final_iter_lowers, c(1, 0.32, 0.5, 0.18, 0.47, 0.29), ignore_attr = TRUE)
  expect_equal(final_iter_uppers, c(1, 0.51, 0.5, 0.39, 0.6, 0.39), ignore_attr = TRUE)
})


test_that("simulate_conventional_fcm works", {
  adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  test_sim <- simulate_conventional_fcm(adj_matrix, c(1, 1, 1, 1, 1, 1), clamping_vector = c(1, 0, 0, 0, 0, 0), activation = "kosko", squashing = "sigmoid")
  expect_equal(round(test_sim$state_vectors[2, ], 2), data.frame(1, 1, 0.38, 0.5, 0.11, 0.33, 0.13), ignore_attr = TRUE)
  expect_equal(round(test_sim$state_vectors[nrow(test_sim$state_vectors), ], 2), data.frame(9, 1, 0.31, 0.5, 0.18, 0.49, 0.32), ignore_attr = TRUE)
})


test_that("simulate_ivfn_or_tfn_fcm works", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.2, 0, 0, 0.9, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.3, 0.9, -0.5, 0, -0.5, 0),
    C5 = c(0.5, 0, 0, -0.3, 0, 0),
    C6 = c(0, -0.4, 0, 0, -0.5, 0)
  )
  adj_matrix <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  test_sim <- simulate_ivfn_or_tfn_fcm(adj_matrix, c(1, 1, 1, 1, 1, 1), clamping_vector = c(1, 0, 0, 0, 0, 0), activation = "kosko", squashing = "sigmoid")
  first_iter <- test_sim$state_vectors[2, ][-1]
  first_iter_lowers <- round(vapply(first_iter, function(x) x[[1]]$lower, numeric(1)), 2)
  first_iter_uppers <- round(vapply(first_iter, function(x) x[[1]]$upper, numeric(1)), 2)
  expect_equal(first_iter_lowers, c(1, 0.38, 0.5, 0.11, 0.33, 0.13), ignore_attr = TRUE)
  expect_equal(first_iter_uppers, c(1, 0.67, 0.5, 0.4, 0.55, 0.29), ignore_attr = TRUE)

  final_iter <- test_sim$state_vectors[nrow(test_sim$state_vectors), ][-1]
  final_iter_lowers <- round(vapply(final_iter, function(x) x[[1]]$lower, numeric(1)), 2)
  final_iter_uppers <- round(vapply(final_iter, function(x) x[[1]]$upper, numeric(1)), 2)
  expect_equal(final_iter_lowers, c(1, 0.32, 0.5, 0.18, 0.47, 0.29), ignore_attr = TRUE)
  expect_equal(final_iter_uppers, c(1, 0.51, 0.5, 0.39, 0.6, 0.39), ignore_attr = TRUE)
})


test_that("calculate_next_conventional_fcm_state_vector", {
  adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  test_next_state <- calculate_next_conventional_fcm_state_vector(adj_matrix, state_vector = c(1, 0, 0, 0, 0, 0), activation = "kosko")
  expect_equal(test_next_state[1, ], c(0, -0.85, 0, -0.7, 0.1, 0), ignore_attr = TRUE)

  test_next_state <- calculate_next_conventional_fcm_state_vector(adj_matrix, state_vector = c(1, 1, 1, 1, 1, 1), activation = "modified-kosko")
  expect_equal(test_next_state[1, ], c(1, 0.5, 1, -1.1, 0.3, -0.9), ignore_attr = TRUE)

  test_next_state <- calculate_next_conventional_fcm_state_vector(adj_matrix, state_vector = c(1, 1, 1, 1, 1, 1), activation = "rescale")
  expect_equal(test_next_state[1, ], c(1, 0.5, 1, -1.1, 0.3, -0.9), ignore_attr = TRUE)
})


test_that("calculate_next_fuzzy_set_fcm_state_vector", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.2, 0, 0, 0.9, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.3, 0.9, -0.5, 0, -0.5, 0),
    C5 = c(0.5, 0, 0, -0.3, 0, 0),
    C6 = c(0, -0.4, 0, 0, -0.5, 0)
  )
  adj_matrix <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  test_next_state <- calculate_next_fuzzy_set_fcm_state_vector(adj_matrix,
                                                               fuzzy_set_state_vector = c(ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1)),
                                                               crisp_state_vector = c(1, 1, 1, 1, 1, 1),
                                                               activation = "kosko",
                                                               fcm_class = "ivfn")
  expect_equal(test_next_state, c(ivfn(0, 0), ivfn(-0.5, 0.7), ivfn(0, 0), ivfn(-2.1, -0.4), ivfn(-0.7, 0.2), ivfn(-1.9, -0.9)), ignore_attr = TRUE)

  test_next_state <- calculate_next_fuzzy_set_fcm_state_vector(adj_matrix,
                                                               fuzzy_set_state_vector = c(ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1)),
                                                               crisp_state_vector = c(1, 1, 1, 1, 1, 1),
                                                               activation = "modified-kosko",
                                                               fcm_class = "ivfn")
  expect_equal(test_next_state, c(ivfn(1, 1), ivfn(0.5, 1.7), ivfn(1, 1), ivfn(-1.1, 0.6), ivfn(0.3, 1.2), ivfn(-0.9, 0.1)), ignore_attr = TRUE)

  test_next_state <- calculate_next_fuzzy_set_fcm_state_vector(adj_matrix,
                                                               fuzzy_set_state_vector = c(ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1), ivfn(1, 1)),
                                                               crisp_state_vector = c(1, 1, 1, 1, 1, 1),
                                                               activation = "rescale",
                                                               fcm_class = "ivfn")
  expect_equal(test_next_state, c(ivfn(1, 1), ivfn(0.5, 1.7), ivfn(1, 1), ivfn(-1.1, 0.6), ivfn(0.3, 1.2), ivfn(-0.9, 0.1)), ignore_attr = TRUE)
})


test_that("squash works", {
  expect_equal(round(squash(1, "sigmoid", lambda = 1), 5), 0.73106)
  expect_equal(round(squash(1, "sigmoid", lambda = 0.5), 5), 0.62246)

  expect_equal(round(squash(1, "tanh", lambda = 1), 5), 0.76159)
  expect_equal(round(squash(1, "tanh", lambda = 0.5), 5), 0.46212)

  expect_equal(squash(0.5, "bivalent"), 1)
  expect_equal(squash(0.5, "trivalent"), 1)
  expect_equal(squash(-0.5, "trivalent"), -1)
  expect_equal(squash(2, "saturation"), 1)
})


test_that("defuzz_ivfn_or_tfn works", {
  expect_equal(defuzz_ivfn_or_tfn(1), 1)
  expect_equal(defuzz_ivfn_or_tfn(ivfn(0.2, 0.6)), 0.4)
  expect_equal(defuzz_ivfn_or_tfn(tfn(0.1, 0.3, 0.8)), 0.4)
})


test_that("convert_element_to_ivfn_or_tfn_if_numeric works", {
  expect_equal(convert_element_to_ivfn_or_tfn_if_numeric(1, "ivfn"), ivfn(1, 1))
  expect_equal(convert_element_to_ivfn_or_tfn_if_numeric(1, "tfn"), tfn(1, 1, 1))
})


test_that("convert_fuzzy_set_elements_in_matrix_to_distributions works", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.2, 0, 0, 0.9, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.3, 0.9, -0.5, 0, -0.5, 0),
    C5 = c(0.5, 0, 0, -0.3, 0, 0),
    C6 = c(0, -0.4, 0, 0, -0.5, 0)
  )
  adj_matrix <- make_adj_matrix_w_ivfns(lower_adj_matrix, upper_adj_matrix)
  adj_matrix_w_distributions <- convert_fuzzy_set_elements_in_matrix_to_distributions(adj_matrix, "ivfn", 1000)
  test_dist <- adj_matrix_w_distributions[1, 2][[1]][[1]]
  test_dist_based_on <- ivfn(-0.85, -0.2)
  expect_gte(min(test_dist), test_dist_based_on$lower)
  expect_lte(max(test_dist), test_dist_based_on$upper)
  expect_equal(length(test_dist), 1000)

  expect_lt(abs(mean(test_dist) - (test_dist_based_on$lower + test_dist_based_on$upper)/2), 0.1)
  expect_lt(abs(var(test_dist) - ((test_dist_based_on$upper - test_dist_based_on$lower)^2)/12), 0.1)
})


test_that("clean_simulation_output works", {
  NULL
})


test_that("check_simulation_inputs works", {
  test_conventional_fcm <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  test_ivfn_fcm <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  mode_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  test_tfn_fcm <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)

  # Check adj_matrices ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_conventional_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))

  expect_error(check_simulation_inputs(cbind(test_conventional_fcm, test_conventional_fcm)))
  error_ivfn_fcm <- test_ivfn_fcm
  error_ivfn_fcm[2, 2][[1]] <- 1
  expect_error(check_simulation_inputs(adj_matrix = error_ivfn_fcm))
  # ----

  # Check initial_state_vector ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1, 1), clamping_vector = c(1, 0)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, "a"), clamping_vector = c(1, 0)))
  # ----

  # Check clamping_vector ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 1, 1)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, "a")))
  # ----

  # Check activation and squashing ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "kosko"))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "modified-kosko"))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "rescale"))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "wrong"))

  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), squashing = "sigmoid"))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), squashing = "tanh"))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), squashing = "wrong"))

  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "rescale",  squashing = "sigmoid"))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "rescale", squashing = "tanh"))
  # ----

  # Check lambda ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), lambda = 1))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), lambda = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), lambda = -1))
  # ----

  # Check max_iter ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = 100))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = 1.5))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = 0))
  # ----

  # Check min_error ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), min_error = 1e-4))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), min_error = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), min_error = -1))
  # ----

  # Check fuzzy_set_samples ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = 100))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = 1.5))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = 0))
  # ----
})

test_that("trying_triangular_estimation", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  mode_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.65, 0, 0, 0.80, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.6, 0.65, -0.85, 0, -0.95, 0),
    C5 = c(0.6, 0, 0, -0.2, 0, 0),
    C6 = c(0, -0.85, 0.1, 0, -0.75, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.1, 0, 0, 1, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.4, 0.7, -0.7, 0, -0.1, 0),
    C5 = c(0.7, 0, 0, -0.1, 0, 0),
    C6 = c(0, -0.2, 0.25, 0, -0.7, 0)
  )
  tri_adj_matrix <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(lower_adj_matrix, mode_adj_matrix, upper_adj_matrix)
  initial_state_vector <- c(0.25, 0.5, -0.3, -0.2, -0.65, -0.7)

  lower_infer <- infer_fcm_with_clamping(adj_matrix = lower_adj_matrix, initial_state_vector = initial_state_vector, clamping_vector = c(0.25, 0, -0.3, 0, 0, 0), activation = "modified-kosko", squashing = "tanh", lambda = 1, max_iter = 500)
  mode_infer <- infer_fcm_with_clamping(adj_matrix = mode_adj_matrix, initial_state_vector = initial_state_vector, clamping_vector = c(0.25, 0, -0.3, 0, 0, 0), activation = "modified-kosko", squashing = "tanh", lambda = 1, max_iter = 500)
  upper_infer <- infer_fcm_with_clamping(adj_matrix = upper_adj_matrix, initial_state_vector = initial_state_vector, clamping_vector = c(0.25, 0, -0.3, 0, 0, 0), activation = "modified-kosko", squashing = "tanh", lambda = 1, max_iter = 500)

  plot(x = 1:20, y = lower_infer$scenario_simulation$state_vectors$C2[1:20], ylim = c(-1, 1), type = "l")
  lines(x = 1:20, y = mode_infer$scenario_simulation$state_vectors$C2[1:20] )
  lines(x = 1:20, y = upper_infer$scenario_simulation$state_vectors$C2[1:20] )

})

test_that("get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices works", {
  expect_no_error(
    get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
      lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
      mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
      upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
    )
  )
})

test_that("confirm_initial_state_vector_is_compatible_with_triangular_adj_matrix ", {
  tri_adj_matrix <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
    lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
  )
  expect_no_error(
    confirm_input_vector_is_compatible_with_triangular_adj_matrix(
      tri_adj_matrix, c(triangular_number(0, 0.2, 0.8), 0)
    )
  )
})


test_that("get_triangular_distribution_of_values works", {
  expect_no_error(get_triangular_distribution_of_values(lower = 0.25, upper = 0.75, mode = 0.5, n = 100))
  expect_error(get_triangular_distribution_of_values(lower = 0.75, upper = 0.25))

  test_tri_dist <- get_triangular_distribution_of_values(lower = 0.25, upper = 0.75, mode = 0.5, n = 10000)
  # Perform visual check
  # hist(test_tri_dist)

  test_tri_dist <- get_triangular_distribution_of_values(lower = 0.25, upper = 0.75, mode = 0.7, n = 10000)
  # Perform visual check
  # hist(test_tri_dist)
})

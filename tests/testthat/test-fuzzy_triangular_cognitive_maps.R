

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

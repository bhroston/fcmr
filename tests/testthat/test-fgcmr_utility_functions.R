test_that("get_grey_adj_matrix_from_lower_and_upper_adj_matrices works", {
  test_lower_adj_matrix <- data.frame(
    "C1" = c(0, 0, 0.51, -0.9, 0, 0, 0, 0),
    "C2" = c(0, 0, 0, 0.7, -0.52, 0, 0, 0),
    "C3" = c(0.13, 0, 0, 0, 0, 0.3, 0, 0),
    "C4" = c(0.28, 0.6, 0, 0, 0, 0, 0.2, 0),
    "C5" = c(0, 0.5, 0, 0, 0, 0, 0, 0),
    "C6" = c(0, 0, 0, 0, 0, 0, 0, 0.35),
    "C7" = c(0, 0, 0, -0.16, 0, 0, 0, 0),
    "C8" = c(0, 0, 0, 0, 0, 0.43, 0, 0)
  )
  test_upper_adj_matrix <- data.frame(
    "C1" = c(0, 0, 1.00, -0.7, 0, 0, 0, 0),
    "C2" = c(0, 0, 0, 0.9, -0.32, 0, 0, 0),
    "C3" = c(0.43, 0, 0, 0, 0, 0.5, 0, 0),
    "C4" = c(0.48, 0.8, 0, 0, 0, 0, 0.4, 0),
    "C5" = c(0, 0.7, 0, 0, 0, 0, 0, 0),
    "C6" = c(0, 0, 0, 0, 0, 0, 0, 0.85),
    "C7" = c(0, 0, 0, 0.34, 0, 0, 0, 0),
    "C8" = c(0, 0, 0, 0, 0, 0.63, 0, 0)
  )
  test_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(test_lower_adj_matrix, test_upper_adj_matrix)

  testthat::expect_equal(methods::is(test_grey_adj_matrix), "grey_number")
  testthat::expect_equal(nrow(test_grey_adj_matrix), 8)
  testthat::expect_equal(ncol(test_grey_adj_matrix), 8)
  testthat::expect_equal(test_grey_adj_matrix[[3]][[6]], grey_number(0.3, 0.5))
})


test_that("get_grey_adj_matrix_from_list_of_grey_numbers works", {
  test_adj_matrix_values <- c(
    grey_number(0.5, 0.7),
    grey_number(-0.52, -0.32),
    grey_number(0.7, 0.9),
    grey_number(0.6, 0.8),
    grey_number(-0.9, -0.7),
    grey_number(0.28, 0.48),
    grey_number(0.51, 1.0),
    grey_number(0.13, 0.43),
    grey_number(-0.16, 0.34),
    grey_number(0.2, 0.4),
    grey_number(0.3, 0.5),
    grey_number(0.43, 0.63),
    grey_number(0.35, 0.85)
  )

  test_adj_matrix_locs <- c(
    "2-5", "5-2", "4-2", "2-4", "4-1", "1-4", "3-1",
    "1-3", "4-7", "7-4", "6-3", "6-8", "8-6"
  )

  test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
    values = test_adj_matrix_values,
    locs = test_adj_matrix_locs
  )

  testthat::expect_equal(methods::is(test_grey_adj_matrix), "grey_number")
  testthat::expect_equal(nrow(test_grey_adj_matrix), 8)
  testthat::expect_equal(ncol(test_grey_adj_matrix), 8)
  testthat::expect_equal(test_grey_adj_matrix[[3]][[6]], grey_number(0.3, 0.5))
})

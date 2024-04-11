
test_that("fgcmr_simulation works", {
  # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
  # Test 1 (Exact as fcm)
  w5_2 <- grey_number(0.6, 0.6)
  w2_5 <- grey_number(-0.42, -0.42)
  w4_2 <- grey_number(0.8, 0.8)
  w2_4 <- grey_number(0.7, 0.7)
  w4_1 <- grey_number(-0.8, -0.8)
  w1_4 <- grey_number(0.38, 0.38)
  w3_1 <- grey_number(0.755, 0.755)
  #w1_3 <- grey_number(0.13, 0.43) They say greyness is 0.2, but can't be
  #w1_3 <- grey_number(0.33, 0.33)
  w1_3 <- grey_number(0.28, 0.28) # Might be this?
  w4_7 <- grey_number(0.09, 0.09)
  w7_4 <- grey_number(0.3, 0.3)
  w6_3 <- grey_number(0.4, 0.4)
  w6_8 <- grey_number(0.53, 0.53)
  w8_6 <- grey_number(0.6, 0.6)

  test_adj_matrix_edge_weights <- c(w2_5, w5_2, w4_2, w2_4, w4_1, w1_4,
                                    w3_1, w1_3, w4_7, w7_4, w6_3, w6_8,
                                    w8_6)
  test_adj_matrix_edge_indeces <- c("2-5", "5-2", "4-2", "2-4", "4-1", "1-4",
                                    "3-1", "1-3", "4-7", "7-4", "6-3", "6-8",
                                    "8-6")
  test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
    values = test_adj_matrix_edge_weights,
    locs = test_adj_matrix_edge_indeces,
    size = 8
  )


  # Test 2 (Proper grey adjacency matrix)
  # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
  # Does not replicate paper results
  w5_2 <- grey_number(0.5, 0.7)
  w2_5 <- grey_number(-0.52, -0.32)
  w4_2 <- grey_number(0.7, 0.9)
  w2_4 <- grey_number(0.6, 0.8)
  w4_1 <- grey_number(-0.9, -0.7)
  w1_4 <- grey_number(0.28, 0.48)
  w3_1 <- grey_number(0.51, 1.0)
  w1_3 <- grey_number(0.13, 0.43)
  w4_7 <- grey_number(-0.16, 0.34)
  w7_4 <- grey_number(0.2, 0.4)
  w6_3 <- grey_number(0.3, 0.5)
  w6_8 <- grey_number(0.43, 0.63)
  w8_6 <- grey_number(0.35, 0.85)

  test_adj_matrix_edge_weights <- c(w2_5, w5_2, w4_2, w2_4, w4_1, w1_4,
                                    w3_1, w1_3, w4_7, w7_4, w6_3, w6_8,
                                    w8_6)
  test_adj_matrix_edge_indeces <- c("2-5", "5-2", "4-2", "2-4", "4-1", "1-4",
                                    "3-1", "1-3", "4-7", "7-4", "6-3", "6-8",
                                    "8-6")
  test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
    values = test_adj_matrix_edge_weights,
    locs = test_adj_matrix_edge_indeces,
    size = 8
  )

  C1 <- grey_number(0.48, 0.48)
  C2 <- grey_number(0.57, 0.57)
  C3 <- grey_number(0.58, 0.58)
  C4 <- grey_number(0.68, 0.68)
  C5 <- grey_number(0.58, 0.58)
  C6 <- grey_number(0.59, 0.59)
  C7 <- grey_number(0.52, 0.52)
  C8 <- grey_number(0.58, 0.58)
  test_initial_state_vector = c(C1, C2, C3, C4, C5, C6, C7, C8)

  test_fgcmr_simulation <- simulate_fgcmr(test_grey_adj_matrix, test_initial_state_vector, lambda = 1, activation = "modified-kosko", max_iter = 100, min_error = 1e-8)

  test_steady_states <- test_fgcmr_simulation$state_vectors[nrow(test_fgcmr_simulation$state_vectors),]
  lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))

  rounded_test_steady_states <- lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))

  # Test 3 (Grey Initial State Vector with Proper grey adjacency matrix)
  # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
  # Does not replicate paper results
  C1 <- grey_number(0.38, 0.58)
  C2 <- grey_number(0.47, 0.67)
  C3 <- grey_number(0.48, 0.68)
  C4 <- grey_number(0.58, 0.78)
  C5 <- grey_number(0.48, 0.68)
  C6 <- grey_number(0.49, 0.69)
  C7 <- grey_number(0.42, 0.62)
  C8 <- grey_number(0.48, 0.68)
  test_initial_state_vector = c(C1, C2, C3, C4, C5, C6, C7, C8)

  test_fgcmr_simulation <- simulate_fgcmr(test_grey_adj_matrix, test_initial_state_vector, lambda = 1, activation = "modified-kosko", max_iter = 100, min_error = 1e-8)

  test_steady_states <- test_fgcmr_simulation$state_vectors[nrow(test_fgcmr_simulation$state_vectors),]
  lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))

  rounded_test_steady_states <- lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))


})


test_that("fgcmr works", {
  # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
  w2_5 <- grey_number(0.5, 0.7)
  w5_2 <- grey_number(-0.52, -0.32)
  w4_2 <- grey_number(0.7, 0.9)
  w2_4 <- grey_number(0.6, 0.8)
  w4_1 <- grey_number(-0.9, -0.7)
  w1_4 <- grey_number(0.28, 0.48)
  w3_1 <- grey_number(0.51, 1.0)
  w1_3 <- grey_number(0.13, 0.43)
  w4_7 <- grey_number(-0.16, 0.34)
  w7_4 <- grey_number(0.2, 0.4)
  w6_3 <- grey_number(0.3, 0.5)
  w6_8 <- grey_number(0.43, 0.63)
  w8_6 <- grey_number(0.35, 0.85)

  test_adj_matrix_edge_weights <- c(w2_5, w5_2, w4_2, w2_4, w4_1, w1_4,
                                    w3_1, w1_3, w4_7, w7_4, w6_3, w6_8,
                                    w8_6)
  test_adj_matrix_edge_indeces <- c("2-5", "5-2", "4-2", "2-4", "4-1", "1-4",
                                    "3-1", "1-3", "4-7", "7-4", "6-3", "6-8",
                                    "8-6")
  test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
    values = test_adj_matrix_edge_weights,
    locs = test_adj_matrix_edge_indeces,
    size = 8
  )
})


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

  testthat::expect_equal(nrow(test_grey_adj_matrix), 8)
  testthat::expect_equal(ncol(test_grey_adj_matrix), 8)
  testthat::expect_equal(test_grey_adj_matrix[[3]][[6]], grey_number(0.3, 0.5))
})

test_that("get_grey_adj_matrix_from_lower_and_upper_adj_matrices and
          get_grey_adj_matrix_from_list_of_grey_numbers return the same thing", {
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
            test_grey_adj_matrix_1 <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(test_lower_adj_matrix, test_upper_adj_matrix)

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

            test_grey_adj_matrix_2 <- get_grey_adj_matrix_from_list_of_grey_numbers(
              values = test_adj_matrix_values,
              locs = test_adj_matrix_locs
            )

            expect_identical(test_grey_adj_matrix_1, test_grey_adj_matrix_2)
          })


test_that("c.grey_number works", {
  expect_equal(
    c(grey_number(0, 1), grey_number(0.5, 0.6)),
    list(grey_number(0, 1), grey_number(0.5, 0.6))
  )
})


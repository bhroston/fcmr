
test_that("confer_fgcm works", {
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

  grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
  activation_vector <- c(1, 1, 1, 1)
  scenario_vector <- c(1, 0, 0, 0)

  test <- confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
                      "kosko", "tanh", max_iter = 1000)

  rounded_grey_nums <- apply(test$conferred_bounds, 1, function(entry) grey_number(round(as.numeric(entry[2]), 2), round(as.numeric(entry[3]), 2)))
  names(rounded_grey_nums) <- c("A", "B", "C", "D")

  expect_equal(test$conferred_bounds$node, c("A", "B", "C", "D"))

  # Match output with mentalmodeler ONLY when all edges are positive. This
  # is NOT an accurate comparison when there are negative edges.
  expect_equal(rounded_grey_nums$A$upper, 1)
  expect_equal(rounded_grey_nums$A$lower, 1)

  expect_equal(rounded_grey_nums$B$upper, 0.77)
  expect_equal(rounded_grey_nums$B$lower, 0.25)

  expect_equal(rounded_grey_nums$C$upper, 0.52)
  expect_equal(rounded_grey_nums$C$lower, 0.06)

  expect_equal(rounded_grey_nums$D$upper, 0.37)
  expect_equal(rounded_grey_nums$D$lower, 0.02)

  test <- confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
                      "kosko", "sigmoid", max_iter = 1000, algorithm = "salmeron")

  rounded_grey_nums <- apply(test$conferred_bounds, 1, function(entry) grey_number(round(as.numeric(entry[2]), 2), round(as.numeric(entry[3]), 2)))
  names(rounded_grey_nums) <- c("A", "B", "C", "D")

  # Does NOT Match output with mentalmodeler because the sigmoid scenario and baseline
  # possibility spaces intersect, so the minimum values have to be 0
  expect_equal(rounded_grey_nums$A$upper, 0.5)
  expect_equal(rounded_grey_nums$A$lower, 0.5)

  expect_equal(rounded_grey_nums$B$upper, 0.21)
  expect_equal(rounded_grey_nums$B$lower, 0)

  expect_equal(rounded_grey_nums$C$upper, 0.11)
  expect_equal(rounded_grey_nums$C$lower, 0)

  expect_equal(rounded_grey_nums$D$upper, 0.08)
  expect_equal(rounded_grey_nums$D$lower, 0)

  expect_no_error(confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
                              "modified-kosko", "tanh", max_iter = 10000, algorithm = "salmeron"))
  expect_no_error(confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
                              "modified-kosko", "sigmoid", max_iter = 100, algorithm = "salmeron"))
  expect_no_error(confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
                              "rescale", "sigmoid", max_iter = 100, algorithm = "salmeron"))


  # Perform visual check
  # x <- test$inference_for_plotting
  # x <- x[x$node != "A", ]
  # x$node <- c("B", "C", "D")
  # ggplot(x) +
  #   geom_bar(aes(x = node, y = lower_value), stat = "identity", fill = "white") +
  #   geom_crossbar(aes(x = node, y = lower_value, ymin = lower_value, ymax = upper_value), color = "red", fill = "red") +
  #   geom_text(aes(x = node, y = lower_value - 0.05, label = round(lower_value, 2))) +
  #   geom_text(aes(x = node, y = upper_value + 0.05, label = round(upper_value, 2))) +
  #   ylim(0, 1) +
  #   theme_classic()
  #
  # p <- barplot(height = x$value, names.arg = x$name, col = "red")
  # text(x = p, y = x$value + 0.05, labels = round(x$value, 1))
})


test_that("confer_fgcm outputs within fmcm bounds", {
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
  mode_adj_matrix <- (lower_adj_matrix + upper_adj_matrix)/2

  grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)

  fgcm_results <- confer_fgcm(
    grey_adj_matrix,
    initial_state_vector <- c(1, 1, 1, 1),
    clamping_vector <- c(1, 0, 0, 0),
    activation = "modified-kosko",
    squashing = "sigmoid",
    lambda = 1,
    max_iter = 100
  )

  fmcm_sims <- build_fmcm_models(mode_adj_matrix,
                                 n_sims = 100,
                                 parallel = FALSE,
                                 show_progress = TRUE,
                                 distribution = "uniform",
                                 lower_adj_matrix = lower_adj_matrix,
                                 upper_adj_matrix = upper_adj_matrix)

  fmcm_results <- confer_fmcm(fmcm_sims,
                              initial_state_vector = c(1, 1, 1, 1),
                              clamping_vector = c(1, 0, 0, 0),
                              activation = "modified-kosko",
                              squashing = "sigmoid", n_cores = 2,
                              lambda = 1,
                              max_iter = 100,
                              min_error = 1e-5, include_simulations_in_output = TRUE)

  fgcm_bounds <- fgcm_results$conferred_bounds
  min_fmcm_inferences <- apply(fmcm_results$inferences, 2, min)
  max_fmcm_inferences <- apply(fmcm_results$inferences, 2, max)

  expect_lte(fgcm_bounds$lower[1], min_fmcm_inferences[1])
  expect_gte(fgcm_bounds$upper[1], max_fmcm_inferences[1])
  expect_lte(fgcm_bounds$lower[2], min_fmcm_inferences[2])
  expect_gte(fgcm_bounds$upper[2], max_fmcm_inferences[2])
  expect_lte(fgcm_bounds$lower[3], min_fmcm_inferences[3])
  expect_gte(fgcm_bounds$upper[3], max_fmcm_inferences[3])
  expect_lte(fgcm_bounds$lower[4], min_fmcm_inferences[4])
  expect_gte(fgcm_bounds$upper[4], max_fmcm_inferences[4])

  # x <- fmcm_results$inferences
  # x_longer <- tidyr::pivot_longer(x, cols = 1:4)
  # z <- fgcm_results$conferred_bounds
  # z_longer <- tidyr::pivot_longer(z, cols = 2:3)
  # ggplot() +
  #   #geom_jitter(data = x_longer, aes(x = name, y = value), alpha = 1, size = 0.25) +
  #   geom_boxplot(data = x_longer, aes(x = name, y = value)) +
  #   geom_errorbar(data = z_longer, aes(x = node, y = value, ymin = value, ymax = value, group = name), color = "grey") +
  #   theme_classic()
})

test_that("warning pops up if max_iter reached", {
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

  grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
  activation_vector <- c(1, 1, 1, 1)
  scenario_vector <- c(1, 0, 0, 0)

  expect_warning(confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
                      "modified-kosko", "tanh", max_iter = 20))
})

test_that("fgcm works", {
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

  test_fgcm <- fgcm(test_grey_adj_matrix)

  expect_equal(class(test_fgcm), "fgcm")
  expect_equal(test_fgcm$edgelist, get_edgelist_from_grey_adj_matrix(test_grey_adj_matrix))
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




#
#   # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
#   # Test 1 (Exact as fcm)
#   w5_2 <- grey_number(0.6, 0.6)
#   #w2_5 <- grey_number(-0.42, -0.42)
#   w2_5 <- grey_number(0.42, 0.42)
#   w4_2 <- grey_number(0.8, 0.8)
#   w2_4 <- grey_number(0.7, 0.7)
#   w4_1 <- grey_number(-0.8, -0.8)
#   w1_4 <- grey_number(0.38, 0.38)
#   w3_1 <- grey_number(0.755, 0.755)
#   #w1_3 <- grey_number(0.13, 0.43) They say greyness is 0.2, but can't be
#   #w1_3 <- grey_number(0.33, 0.33)
#   w1_3 <- grey_number(0.28, 0.28) # Might be this?
#   w4_7 <- grey_number(0.09, 0.09)
#   w7_4 <- grey_number(0.3, 0.3)
#   w6_3 <- grey_number(0.4, 0.4)
#   w6_8 <- grey_number(0.53, 0.53)
#   w8_6 <- grey_number(0.6, 0.6)
#
#   test_adj_matrix_edge_weights <- c(w2_5, w5_2, w4_2, w2_4, w4_1, w1_4,
#                                     w3_1, w1_3, w4_7, w7_4, w6_3, w6_8,
#                                     w8_6)
#   test_adj_matrix_edge_indeces <- c("2-5", "5-2", "4-2", "2-4", "4-1", "1-4",
#                                     "3-1", "1-3", "4-7", "7-4", "6-3", "6-8",
#                                     "8-6")
#   test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
#     values = test_adj_matrix_edge_weights,
#     locs = test_adj_matrix_edge_indeces,
#     size = 8
#   )
#
#   C1 <- grey_number(0.48, 0.48)
#   C2 <- grey_number(0.57, 0.57)
#   C3 <- grey_number(0.58, 0.58)
#   C4 <- grey_number(0.68, 0.68)
#   C5 <- grey_number(0.58, 0.58)
#   C6 <- grey_number(0.59, 0.59)
#   C7 <- grey_number(0.52, 0.52)
#   C8 <- grey_number(0.58, 0.58)
#   test_initial_state_vector = c(C1, C2, C3, C4, C5, C6, C7, C8)
#
#   C1 <- grey_number(0.48, 0.48)
#   C2 <- grey_number(0.57, 0.57)
#   C3 <- grey_number(0.58, 0.58)
#   C4 <- grey_number(0.68, 0.68)
#   C5 <- grey_number(0.59, 0.59)
#   C6 <- grey_number(0.58, 0.58)
#   C7 <- grey_number(0.59, 0.59)
#   C8 <- grey_number(0.52, 0.52)
#   test_initial_state_vector = c(C1, C2, C3, C4, C5, C6, C7, C8)
#
#   test_fgcmr_simulation <- simulate_fgcmr(test_grey_adj_matrix, test_initial_state_vector, lambda = 1, activation = "modified-kosko", max_iter = 100, min_error = 1e-8)
#
#
#
#
#   # Test 2 (Proper grey adjacency matrix)
#   # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
#   # Does not replicate paper results
# w5_2 <- grey_number(0.5, 0.7)
# w2_5 <- grey_number(-0.52, -0.32)
# w4_2 <- grey_number(0.7, 0.9)
# w2_4 <- grey_number(0.6, 0.8)
# w4_1 <- grey_number(-0.9, -0.7)
# w1_4 <- grey_number(0.28, 0.48)
# w3_1 <- grey_number(0.51, 1.0)
# w1_3 <- grey_number(0.13, 0.43)
# w4_7 <- grey_number(-0.16, 0.34)
# w7_4 <- grey_number(0.2, 0.4)
# w6_3 <- grey_number(0.3, 0.5)
# w6_8 <- grey_number(0.43, 0.63)
# w8_6 <- grey_number(0.35, 0.85)
#
# test_adj_matrix_edge_weights <- c(w2_5, w5_2, w4_2, w2_4, w4_1, w1_4,
#                                   w3_1, w1_3, w4_7, w7_4, w6_3, w6_8,
#                                   w8_6)
# test_adj_matrix_edge_indeces <- c("2-5", "5-2", "4-2", "2-4", "4-1", "1-4",
#                                   "3-1", "1-3", "4-7", "7-4", "6-3", "6-8",
#                                   "8-6")
# test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
#   values = test_adj_matrix_edge_weights,
#   locs = test_adj_matrix_edge_indeces,
#   size = 8
# )
#
# test_lower_adj_matrix <- apply(test_grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$lower, x[[1]]))
# test_upper_adj_matrix <- apply(test_grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$upper, x[[1]]))
# test_mode_adj_matrix <- (test_upper_adj_matrix + test_lower_adj_matrix)/2
#
# test_fgcm <- simulate_fgcm(test_grey_adj_matrix,
#                          activation_vector = c(0.48, 0.57, 0.58, 0.68, 0.58, 0.59, 0.52, 0.58),
#                          scenario_vector = c(0, 0, 0, 0, 0, 0, 0, 0),
#                         activation = "kosko", squashing = "tanh", lambda = 1,
#                         max_iter = 100, min_error = 1e-5, algorithm = "concepcion")
#
# test_uniform_sims <- build_fmccm_models(
#   adj_matrix <- test_mode_adj_matrix, n_sims = 1000, parallel = TRUE,
#   distribution = "uniform", show_progress = TRUE,
#   lower_adj_matrix = test_lower_adj_matrix,
#   upper_adj_matrix = test_upper_adj_matrix
# )
# test_fmccm <- confer_fmccm(test_uniform_sims, scenario_vector = c(1, 0, 0, 0, 0, 0, 0, 0),
#              activation = "kosko", squashing = "tanh", lambda = 1,
#              max_iter = 100, min_error = 1e-5, include_simulations_in_output = TRUE)
#
# test_sim <- simulate_fmccm_models(
#   test_uniform_sims, activation_vector = c(0.48, 0.57, 0.58, 0.68, 0.58, 0.59, 0.52, 0.58),
#   scenario_vector = c(0, 0, 0, 0, 0, 0, 0, 0),
#   activation = "kosko", squashing = "tanh", lambda = 1,
#   max_iter = 100, min_error = 1e-5
# )
#
# final_scenario_states <- lapply(test_fmccm$sims,
#        function(sim) {
#          states <- sim$scenario_simulation$state_vectors
#          n_iters <- nrow(states)
#          states[n_iters, ]
#        })
# final_scenario_states <- data.frame(do.call(rbind, final_scenario_states))
# final_scenario_states <- subset(final_scenario_states, select = -c(iter))
#
# x <- test_fmccm$inferences
#
# x <- final_scenario_states
# x_longer <- tidyr::pivot_longer(x, cols = 1:8)
# z <- test_fgcm$inference_bounds
# z_longer <- tidyr::pivot_longer(z, cols = 2:3)
# ggplot() +
#   #geom_jitter(data = x_longer, aes(x = name, y = value), alpha = 1, size = 0.25) +
#   geom_boxplot(data = x_longer, aes(x = name, y = value)) +
#   geom_errorbar(data = z_longer, aes(x = node, y = value, ymin = value, ymax = value, group = name), color = "grey") +
#   theme_classic()
#
#
#
#   C1 <- grey_number(0.48, 0.48)
#   C2 <- grey_number(0.57, 0.57)
#   C3 <- grey_number(0.58, 0.58)
#   C4 <- grey_number(0.68, 0.68)
#   C5 <- grey_number(0.58, 0.58)
#   C6 <- grey_number(0.59, 0.59)
#   C7 <- grey_number(0.52, 0.52)
#   C8 <- grey_number(0.58, 0.58)
#   test_initial_state_vector = c(C1, C2, C3, C4, C5, C6, C7, C8)
#
#   test_fgcmr_simulation <- simulate_fgcmr(test_grey_adj_matrix, test_initial_state_vector, lambda = 1, activation = "modified-kosko", max_iter = 100, min_error = 1e-8)
#
#   test_steady_states <- test_fgcmr_simulation$state_vectors[nrow(test_fgcmr_simulation$state_vectors),]
#   lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
#
#   rounded_test_steady_states <- lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
#
#   # Test 3 (Grey Initial State Vector with Proper grey adjacency matrix)
#   # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
#   # Does not replicate paper results
#   C1 <- grey_number(0.38, 0.58)
#   C2 <- grey_number(0.47, 0.67)
#   C3 <- grey_number(0.48, 0.68)
#   C4 <- grey_number(0.58, 0.78)
#   C5 <- grey_number(0.48, 0.68)
#   C6 <- grey_number(0.49, 0.69)
#   C7 <- grey_number(0.42, 0.62)
#   C8 <- grey_number(0.48, 0.68)
#   test_initial_state_vector = c(C1, C2, C3, C4, C5, C6, C7, C8)
#
#   test_fgcmr_simulation <- simulate_fgcmr(test_grey_adj_matrix, test_initial_state_vector, lambda = 1, activation = "modified-kosko", max_iter = 100, min_error = 1e-8)
#
#   test_steady_states <- test_fgcmr_simulation$state_vectors[nrow(test_fgcmr_simulation$state_vectors),]
#   lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
#
#   rounded_test_steady_states <- lapply(test_steady_states, function(x) grey_number(round(x[[1]]$lower, 4), round(x[[1]]$upper, 4)))
#
#
#
# test_that("simulate_fgcm works", {
#   lower_adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.25, 0, 0, 0.25),
#     "C" = c(0, 0.25, 0, 0),
#     "D" = c(0, 0, 0.25, 0)
#   )
#
#   upper_adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(0.75, 0, 0, 0.75),
#     "C" = c(0, 0.75, 0, 0),
#     "D" = c(0, 0, 0.75, 0)
#   )
#
#   grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
#   activation_vector <- c(1, 1, 1, 1)
#   scenario_vector <- c(1, 0, 0, 0)
#
#   test_lower_adj_matrix <- apply(grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$lower, x[[1]]))
#   test_upper_adj_matrix <- apply(grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$upper, x[[1]]))
#   test_mode_adj_matrix <- (test_upper_adj_matrix + test_lower_adj_matrix)/2
#
#
#   expect_no_error(simulate_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
#                                 "kosko", "tanh", max_iter = 1000))
#
#   conferred_fgcm <- confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector = c(1, 0, 0, 0),
#                                 activation = "kosko", squashing = "sigmoid", lambda = 1,
#                                 max_iter = 100, min_error = 1e-5)
#   scenario_fgcm <- simulate_fgcm(grey_adj_matrix, activation_vector, scenario_vector,
#                                  activation = "rescale", squashing = "sigmoid", lambda = 1,
#                                  max_iter = 100, min_error = 1e-5)
#
#   z <- data.frame(
#     node = base_fgcm$inference_bounds$node,
#     lower = scenario_fgcm$inference_bounds$lower - base_fgcm$inference_bounds$lower,
#     upper = scenario_fgcm$inference_bounds$upper - base_fgcm$inference_bounds$upper
#   )
#
#   test_fmccm_models <- build_fmccm_models(test_mode_adj_matrix, n_sims = 1000, distribution = "uniform",
#                                           lower_adj_matrix = test_lower_adj_matrix,
#                                           upper_adj_matrix = test_upper_adj_matrix)
#   sim_fmccm <- simulate_fmccm_models(
#     test_fmccm_models,
#     activation_vector,
#     scenario_vector = c(1, 0, 0, 0),
#     activation = "kosko",
#     squashing = "sigmoid",
#     lambda = 1,
#     max_iter = 100,
#     min_error = 1e-5
#   )
#
#   x <- subset(sim_fmccm$final_states_across_sims, select = -c(iter))
#   x_longer <- tidyr::pivot_longer(x, cols = 1:4)
#   z_longer <- conferred_fgcm$conferred_bounds_for_plotting
#   ggplot() +
#     #geom_jitter(data = x_longer, aes(x = name, y = value), alpha = 1, size = 0.25) +
#     geom_boxplot(data = x_longer, aes(x = name, y = value)) +
#     geom_errorbar(data = z_longer, aes(x = node, y = value, ymin = value, ymax = value, group = type), color = "grey") +
#     theme_classic()
#
#
#
#
#   # Test from Salmeron & Papageorgiou, 2024 - http://dx.doi.org/10.1007/978-3-642-39739-4_14
#   # Does not replicate paper results
#   w1_3 <- grey_number(0.13, 0.43)
#   w1_4 <- grey_number(0.28, 0.48)
#   w2_4 <- grey_number(0.6, 0.8)
#   w3_1 <- grey_number(0.51, 1.0)
#   w4_1 <- grey_number(-0.9, -0.7)
#   w8_6 <- grey_number(0.35, 0.85)
#   w4_2 <- grey_number(0.7, 0.9)
#   w4_7 <- grey_number(-0.16, 0.34)
#   w5_2 <- grey_number(-0.52, -0.32)
#   w6_3 <- grey_number(0.3, 0.5)
#   w6_8 <- grey_number(0.43, 0.63)
#   w7_4 <- grey_number(0.2, 0.4)
#
#   adj_matrix_edge_weights <- c(w1_3, w1_4, w2_4, w3_1, w4_1, w8_6, w4_2,
#                                w4_7, w5_2, w6_3, w6_8, w7_4)
#   adj_matrix_edge_indeces <- c("1-3", "1-4", "2-4", "3-1", "4-1", "8-6", "4-2",
#                                "4-7", "5-2", "6-3", "6-8", "7-4")
#   test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
#     values = adj_matrix_edge_weights,
#     locs = adj_matrix_edge_indeces,
#     size = 8
#   )
#   test_lower_adj_matrix <- apply(test_grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$lower, x[[1]]))
#   test_upper_adj_matrix <- apply(test_grey_adj_matrix, c(1, 2), function(x) ifelse(class(x[[1]]) == "grey_number", x[[1]]$upper, x[[1]]))
#   test_mode_adj_matrix <- (test_upper_adj_matrix + test_lower_adj_matrix)/2
#
#   activation_vector <- c(0.48, 0.57, 0.58, 0.68, 0.58, 0.59, 0.52, 0.58)
#
#   test_fgcm <- confer_fgcm(
#     test_grey_adj_matrix,
#     activation_vector,
#     scenario_vector = rep(0, 8),
#     activation = "kosko",
#     squashing = "tanh",
#     lambda = 1,
#     max_iter = 100,
#     min_error = 1e-5,
#     algorithm = "salmeron"
#   )
#
#   test_fmccm_models <- build_fmccm_models(test_mode_adj_matrix, n_sims = 1000, distribution = "uniform",
#                                           lower_adj_matrix = test_lower_adj_matrix,
#                                           upper_adj_matrix = test_upper_adj_matrix)
#   sim_fmccm <- simulate_fmccm_models(
#     test_fmccm_models,
#     activation_vector,
#     scenario_vector = rep(0, 8),
#     activation = "kosko",
#     squashing = "tanh",
#     lambda = 1,
#     max_iter = 100,
#     min_error = 1e-5
#   )
#
#   x <- subset(sim_fmccm$final_states_across_sims, select = -c(iter))
#   x_longer <- tidyr::pivot_longer(x, cols = 1:8)
#   z <- test_fgcm$inference_bounds
#   z_longer <- tidyr::pivot_longer(z, cols = 2:3)
#   ggplot() +
#     #geom_jitter(data = x_longer, aes(x = name, y = value), alpha = 1, size = 0.25) +
#     geom_boxplot(data = x_longer, aes(x = name, y = value)) +
#     geom_errorbar(data = z_longer, aes(x = node, y = value, ymin = value, ymax = value, group = name), color = "grey") +
#     theme_classic()
#
# })


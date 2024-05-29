
test_that("results match mental modeler", {
  test_edgelist <- data.frame(
    source = c("A"),
    target = c("B"),
    weight = c(1)
  )
  test_adj_matrix <- get_adj_matrix_from_edgelist(test_edgelist)
  init_state_vector <- c(1, 0)

  sim <- simulate_fcmr(test_adj_matrix, initial_state_vector = init_state_vector, activation_vector = c(0, 0),
                activation = "modified-kosko", squashing = "tanh",
                lambda = 1, max_iter = 50)
  sim$state_vectors

  data <- sim$state_vectors$B
  plot(data, type = "l")

  fcm::fcm.infer(init_state_vector, test_adj_matrix)

  test_edgelist <- data.frame(
    source = c("A", "B", "C", "D"),
    target = c("B", "C", "D", "B"),
    weight = c(1, 1, 1, 1)
  )
  test_adj_matrix <- get_adj_matrix_from_edgelist(test_edgelist)
  init_state_vector <- c(1, 1, 1, 1)
  sim <- simulate_fcmr(test_adj_matrix, initial_state_vector = c(1, 1, 1, 1),
                       activation_vector = c(1, 0, 0, 0), activation = "kosko",
                       squashing = "tanh", lambda = 1, min_error = 0.00001)
  sim$state_vectors

  fcm::fcm.infer(init_state_vector, test_adj_matrix, transform = "t", infer = "mk")

  /Users/benro/Downloads/test.xlsx
  /Users/benro/Downloads/test.xls

})


test_that("get_edgelist_from_adj_matrix works", {
  test_adj_matrix <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  goal_edgelist <- data.frame(
    "source" = c("C2", "C3", "C4", "C1", "C5", "C1", "C5", "C1"),
    "target" = c("C1", "C1", "C1", "C2", "C2", "C3", "C4", "C5"),
    "weight" = c(0.36, 0.45, -0.90, -0.40, 0.60, -0.25, 0.30, 0.30)
  )

  expect_identical(get_edgelist_from_adj_matrix(test_adj_matrix), goal_edgelist)
})



test_that("get_adj_matrix_from_edgelist works", {
  test_edgelist <- data.frame(
    from = c("A"),
    to = c("B"),
    weight = c(0.55)
  )
  test_adj_matrix <- get_adj_matrix_from_edgelist(test_edgelist, source_colname = "from", target_colname = "to")
  expected_adj_matrix <- data.frame(A = c(0, 0), B = c(0.55, 0))
  rownames(expected_adj_matrix) <- c("A", "B")
  ### Check this test case for error or function pass with "to" and "from" instead of "source" and "target"
  expect_equal(test_adj_matrix, expected_adj_matrix)

  # Test from Koutsellis et al. 2022 - https://doi.org/10.1007/s12351-022-00717-x
  # Confirmed to reproduce results
  test_source_nodes <- c(
    "C1", "C1", "C1", "C2", "C2", "C3", "C3", "C3", "C4", "C4",
    "C4", "C5", "C6", "C7", "C8", "C8", "C9", "C10", "C10", "C11",
    "C12", "C13", "C14", "C14", "C15", "C15", "C15", "C15", "C16", "C16",
    "C17", "C17", "C18", "C19", "C20", "C20", "C21", "C22", "C23", "C23",
    "C24", "C25", "C28", "C29"
  )
  test_target_nodes <- c(
    "C15", "C20", "C23", "C12", "C25", "C14", "C16", "C23", "C15", "C20",
    "C23", "C16", "C15", "C15", "C10", "C12", "C23", "C11", "C13", "C26",
    "C14", "C28", "C13", "C23", "C17", "C18", "C24", "C30", "C15", "C28",
    "C21", "C22", "C19", "C20", "C26", "C28", "C27", "C29", "C19", "C24",
    "C29", "C15", "C27", "C27"
  )
  test_edge_weights <- c(
    -0.442, -0.375, -0.348, 0.549, 0.706, -0.421, -0.132, -0.245, -0.52, 0.261,
    -0.29, 0.651, 0.487, 0.229, 0.776, 0.319, 0.792, 0.319, 0.391, -0.402,
    -0.381, 0.481, 0.097, 0.11, -0.852, 0.902, 0.226, -0.852, -0.719, -0.274,
    0.722, 0.888, -0.521, -0.311, -0.932, -0.419, 0.322, 0.21, 0.9, 0.189,
    0.196, -0.311, 0.481, 0.378
  )

  edgelist <- data.frame(
    "source" = test_source_nodes,
    "target" = test_target_nodes,
    "weight" = test_edge_weights
  )

  # Confirm return the same output as igraph
  # g <- igraph::graph_from_data_frame(edgelist, directed = TRUE)
  # igraph_adj_matrix <- data.frame(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  # test_adj_matrix <- get_adj_matrix_from_edgelist(edgelist)
  # expect_equal(igraph_adj_matrix, test_adj_matrix)
  expect_no_error(get_adj_matrix_from_edgelist(edgelist))
})



test_that("squash works", {
  expect_equal(
    round(squash(1, "sigmoid"), 3), 0.731
  )
  expect_equal(
    round(squash(1, "tanh"), 3), 0.762
  )
})



test_that("confirm_adj_matrix_is_square works", {
  test_adj_matrix_1 <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6, 0),
    "C3" = c(-0.25, 0, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3, 0),
    "C5" = c(0.3, 0, 0, 0, 0, 0)
  )

  test_adj_matrix_2 <- data.frame(
    "C1" = c("a", 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )
  rownames(test_adj_matrix_2) <- colnames(test_adj_matrix_2)

  expect_error(confirm_adj_matrix_is_square(test_adj_matrix_1))
  expect_no_error(confirm_adj_matrix_is_square(test_adj_matrix_2))
})



test_that("confirm_adj_matrices_have_same_concepts works", {
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

  list_of_concepts_in_fcm <- lapply(test_fcms, colnames)

  expect_no_error(
    confirm_adj_matrices_have_same_concepts(list_of_concepts_in_fcm)
  )

  colnames(test_fcms[[2]])[3] <- "asdc"
  list_of_concepts_in_fcm <- lapply(test_fcms, colnames)
  expect_error(
    confirm_adj_matrices_have_same_concepts(list_of_concepts_in_fcm)
  )
})



test_that("confirm_adj_matrices_have_same_dimensions works", {
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
    confirm_adj_matrices_have_same_dimensions(test_fcms)
  )

  test_fcms[[2]]$E <- c(1, 1, 1, 1)
  expect_error(
    confirm_adj_matrices_have_same_dimensions(test_fcms)
  )
})



test_that("get_node_IDs_from_input works", {
  test_adj_matrix <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  expect_identical(get_node_IDs_from_input(test_adj_matrix), paste0("C", 1:nrow(test_adj_matrix)))

  expect_warning(get_node_IDs_from_input(test_adj_matrix, IDs = c("Test1", "Test2", "Test3", "Test4", "Test5")))

  colnames(test_adj_matrix) <- NULL
  expect_identical(get_node_IDs_from_input(test_adj_matrix), paste0("C", 1:nrow(test_adj_matrix)))

  expect_error(get_node_IDs_from_input(test_adj_matrix, IDs = c("C1", "C2", "C3", "C4", "C5", "C6")))
})



test_that("confirm_input_vector_is_compatable_with_adj_matrices works", {
  test_adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  expect_no_error(
    confirm_input_vector_is_compatable_with_adj_matrices(test_adj_matrix, c(1, 1, 1, 1), "fcm")
  )
  expect_error(
    confirm_input_vector_is_compatable_with_adj_matrices(test_adj_matrix, c(1, 1, 1, 1, 1), "fcm")
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
  test_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
  expect_no_error(
    confirm_input_vector_is_compatable_with_adj_matrices(test_grey_adj_matrix, c(1, 1, 1, 1), "fgcm")
  )
  expect_error(
    confirm_input_vector_is_compatable_with_adj_matrices(test_grey_adj_matrix, c(1, 1, 1, 1, 1), "fgcm")
  )

  test_tri_adj_matrix <- get_triangular_adj_matrix_from_lower_mode_and_upper_adj_matrices(
    lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
  )
  expect_no_error(
    confirm_input_vector_is_compatable_with_adj_matrices(test_tri_adj_matrix, c(1, 1), "ftcm")
  )
  expect_error(
    confirm_input_vector_is_compatable_with_adj_matrices(test_tri_adj_matrix, c(1, 1, 1, 1, 1), "ftcm")
  )
})




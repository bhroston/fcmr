
test_that("simulate_fcmr works", {
  test_adj_matrix <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  test_initial_state_vector <- c(0.400, 0.707, 0.612, 0.717, 0.300)

  test_fcmr_1 <- simulate_fcmr(adj_matrix = test_adj_matrix, initial_state_vector = test_initial_state_vector,
                activation = "modified-kosko", squashing = "sigmoid", lambda = 1, max_iter = 10)
  test_fcmr_1_state_vectors <- test_fcmr_1$state_vectors
  final_state_fcmr_1 <- round(test_fcmr_1_state_vectors[nrow(test_fcmr_1_state_vectors), ], digits = 3)
  rownames(final_state_fcmr_1) <- NULL
  expect_identical(final_state_fcmr_1, data.frame("C1" = 0.625, "C2" = 0.708, "C3" = 0.612, "C4" = 0.717, "C5" = 0.711))


})


test_that("optimize_fcmr_lambda works", {
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

  g <- igraph::graph_from_data_frame(edgelist, directed = TRUE)
  test_adj_matrix <- t(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))

  expect_true(optimize_fcmr_lambda(test_adj_matrix, squashing = "tanh", method = "none"))
  expect_equal(0.927, optimize_fcmr_lambda(test_adj_matrix, squashing = "sigmoid", method = "koutsellis"))
  expect_equal(0.421, optimize_fcmr_lambda(test_adj_matrix, squashing = "tanh", method = "koutsellis"))
  expect_error(optimize_fcmr_lambda(test_adj_matrix, squashing = "tanh", method = "asdcads"))
})


test_that("fcmr works", {
  test_adj_matrix <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  test_fcmr <- fcmr(test_adj_matrix)

  expect_identical(test_fcmr$adj_matrix, test_adj_matrix)
  expect_identical(test_fcmr$edgelist, get_edgelist_from_adj_matrix(test_adj_matrix))
  expect_identical(test_fcmr$concepts, colnames(test_adj_matrix))
  expect_identical(class(test_fcmr), "fcmr")
})

test_that("fcmr catches datatype errors", {
  test_adj_matrix <- data.frame(
    "C1" = c("a", 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )
  expect_error(fcmr(test_adj_matrix))
})


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

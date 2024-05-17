
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

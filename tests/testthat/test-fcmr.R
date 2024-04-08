
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

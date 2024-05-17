# test_that("fcmr catches errors", {
#
#   test_adj_matrix_1 <- data.frame(
#     "C1" = c(0, 0.36, 0.45, -0.90, 0, 0),
#     "C2" = c(-0.4, 0, 0, 0, 0.6, 0),
#     "C3" = c(-0.25, 0, 0, 0, 0, 0),
#     "C4" = c(0, 0, 0, 0, 0.3, 0),
#     "C5" = c(0.3, 0, 0, 0, 0, 0)
#   )
#
#   test_adj_matrix_2 <- data.frame(
#     "C1" = c("a", 0.36, 0.45, -0.90, 0),
#     "C2" = c(-0.4, 0, 0, 0, 0.6),
#     "C3" = c(-0.25, 0, 0, 0, 0),
#     "C4" = c(0, 0, 0, 0, 0.3),
#     "C5" = c(0.3, 0, 0, 0, 0)
#   )
#   rownames(test_adj_matrix_2) <- colnames(test_adj_matrix_2)
#
#   expect_error(fcmr(test_adj_matrix_1))
#   expect_error(fcmr(test_adj_matrix_2))
#
#   test_adj_matrix_3 <- data.frame(
#     "C1" = c(0, 0.36, 0.45, -0.90, 0),
#     "C2" = c(-0.4, 0, 0, 0, 0.6),
#     "C3" = c(-0.25, 0, 0, 0, 0),
#     "C4" = c(0, 0, 0, 0, 0.3),
#     "C5" = c(0.3, 0, 0, 0, 0)
#   )
#   colnames(test_adj_matrix_3) <- NULL
#   rownames(test_adj_matrix_3) <- NULL
#
#   test_fcm_3 <- fcmr(test_adj_matrix_3)
# })


test_that("grey_number catches errors", {
  expect_error(grey_number(0.5, 0.4))
  expect_error(grey_number("a", 0.5))
})

test_that("grey_number works", {
  test_grey_number_1 <- grey_number()
  expect_equal(test_grey_number_1$lower, -Inf)
  expect_equal(test_grey_number_1$upper, Inf)
  expect_equal(methods::is(test_grey_number_1), "grey_number")

  test_grey_number_2 <- grey_number(-1, 1)
  expect_equal(test_grey_number_2$lower, -1)
  expect_equal(test_grey_number_2$upper, 1)
})

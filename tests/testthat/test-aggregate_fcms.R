
test_that("aggregate_fcms works", {
  # Check w/ conventional fcms ----
  test_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  test_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  test_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  test_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.5, 0)
  )
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
  test_aggregate <- aggregate_fcms(test_fcms, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.625)

  test_adj_matrix_3[1, 2] <- 0
  test_adj_matrix_2[1, 2] <- 0
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
  test_aggregate <- aggregate_fcms(test_fcms, "mean", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.75)

  test_adj_matrix_2[1, 2] <- 0.6
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
  test_aggregate <- aggregate_fcms(test_fcms, "median", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.6)

  test_aggregate <- aggregate_fcms(test_fcms, "median", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.55)

  # ----
  # Check w/ ivfn fcms ----
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)
  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  adj_matrix_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)
  lower_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.2, 0)
  )
  upper_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_3 <- make_adj_matrix_w_ivfns(lower_adj_matrix_3, upper_adj_matrix_3)
  lower_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.0, 0)
  )
  upper_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  adj_matrix_4 <- make_adj_matrix_w_ivfns(lower_adj_matrix_4, upper_adj_matrix_4)

  test_fcms_w_ivfns <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
  test_aggregate <- aggregate_fcms(test_fcms_w_ivfns, "mean", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.4, 0.65))

  test_aggregate <- aggregate_fcms(test_fcms_w_ivfns, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.3, 0.65))

  test_aggregate <- aggregate_fcms(test_fcms_w_ivfns, "median", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.3, 0.6))

  test_aggregate <- aggregate_fcms(test_fcms_w_ivfns, "median", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.4, 0.6))

  # ----
  # Check w/ tfn fcms ----
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  mode_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_1 <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)
  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  mode_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.7, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  adj_matrix_2 <- make_adj_matrix_w_tfns(lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2)
  lower_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.2, 0)
  )
  mode_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_3 <- make_adj_matrix_w_tfns(lower_adj_matrix_3, mode_adj_matrix_3, upper_adj_matrix_3)
  lower_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.0, 0)
  )
  mode_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0, 0)
  )
  upper_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  adj_matrix_4 <- make_adj_matrix_w_tfns(lower_adj_matrix_4, mode_adj_matrix_4, upper_adj_matrix_4)

  test_fcms_w_tfns <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
  test_aggregate <- aggregate_fcms(test_fcms_w_tfns, "mean", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.4, 0.5, 0.65))

  test_aggregate <- aggregate_fcms(test_fcms_w_tfns, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.3, 0.375, 0.65))

  test_aggregate <- aggregate_fcms(test_fcms_w_tfns, "median", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.3, 0.4, 0.6))

  test_aggregate <- aggregate_fcms(test_fcms_w_tfns, "median", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.4, 0.4, 0.6))
})


test_that("aggregate_conventional_fcms works", {
  test_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  test_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  test_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  test_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.5, 0)
  )
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
  test_aggregate <- aggregate_conventional_fcms(test_fcms, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.625)

  test_adj_matrix_3[1, 2] <- 0
  test_adj_matrix_2[1, 2] <- 0
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
  test_aggregate <- aggregate_conventional_fcms(test_fcms, "mean", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.75)

  test_adj_matrix_2[1, 2] <- 0.6
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
  test_aggregate <- aggregate_conventional_fcms(test_fcms, "median", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.6)

  test_aggregate <- aggregate_conventional_fcms(test_fcms, "median", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2], 0.55)
})


test_that("aggregate_fcms_w_ivfns works", {
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)
  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  adj_matrix_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)
  lower_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.2, 0)
  )
  upper_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_3 <- make_adj_matrix_w_ivfns(lower_adj_matrix_3, upper_adj_matrix_3)
  lower_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.0, 0)
  )
  upper_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  adj_matrix_4 <- make_adj_matrix_w_ivfns(lower_adj_matrix_4, upper_adj_matrix_4)

  test_fcms_w_ivfns <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
  test_aggregate <- aggregate_fcms_w_ivfns(test_fcms_w_ivfns, "mean", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.4, 0.65))

  test_aggregate <- aggregate_fcms_w_ivfns(test_fcms_w_ivfns, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.3, 0.65))

  test_aggregate <- aggregate_fcms_w_ivfns(test_fcms_w_ivfns, "median", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.3, 0.6))

  test_aggregate <- aggregate_fcms_w_ivfns(test_fcms_w_ivfns, "median", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], ivfn(0.4, 0.6))
})


test_that("fcm_w_tfn_aggregation works", {
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  mode_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_1 <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)
  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  mode_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.7, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  adj_matrix_2 <- make_adj_matrix_w_tfns(lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2)
  lower_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.2, 0)
  )
  mode_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_3 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  adj_matrix_3 <- make_adj_matrix_w_tfns(lower_adj_matrix_3, mode_adj_matrix_3, upper_adj_matrix_3)
  lower_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.0, 0)
  )
  mode_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0, 0)
  )
  upper_adj_matrix_4 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  adj_matrix_4 <- make_adj_matrix_w_tfns(lower_adj_matrix_4, mode_adj_matrix_4, upper_adj_matrix_4)

  test_fcms_w_tfns <- list(adj_matrix_1, adj_matrix_2, adj_matrix_3, adj_matrix_4)
  test_aggregate <- aggregate_fcms_w_tfns(test_fcms_w_tfns, "mean", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.4, 0.5, 0.65))

  test_aggregate <- aggregate_fcms_w_tfns(test_fcms_w_tfns, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.3, 0.375, 0.65))

  test_aggregate <- aggregate_fcms_w_tfns(test_fcms_w_tfns, "median", include_zeroes = TRUE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.3, 0.4, 0.6))

  test_aggregate <- aggregate_fcms_w_tfns(test_fcms_w_tfns, "median", include_zeroes = FALSE)
  expect_equal(test_aggregate$adj_matrix[1, 2][[1]], tfn(0.4, 0.4, 0.6))
})


test_that("aggregate_fcms works", {
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

  test_aggregate_1 <- aggregate_fcms(test_fcms, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate_1$adj_matrix[1, 2], 0.625)

  test_fcms[[2]][1, 2] <- 0
  test_aggregate_2 <- aggregate_fcms(test_fcms, "mean", include_zeroes = TRUE)
  test_aggregate_3 <- aggregate_fcms(test_fcms, "mean", include_zeroes = FALSE)
  expect_equal(test_aggregate_2$adj_matrix[1, 2], 0.5625)
  expect_equal(test_aggregate_3$adj_matrix[1, 2], 0.75)


  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  adj_matrix_w_ivfns_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)

  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.15, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.85, 0)
  )
  adj_matrix_w_ivfns_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)

  test_fcms_w_ivfns <- list(adj_matrix_w_ivfns_1, adj_matrix_w_ivfns_2)
  test_aggregate_4 <- aggregate_fcms(test_fcms_w_ivfns, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate_4$adj_matrix[1, 2][[1]], ivfn(0.2, 0.8))


  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  mode_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.5, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  adj_matrix_w_tfns_1 <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)

  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.15, 0)
  )
  mode_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.65, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.85, 0)
  )
  adj_matrix_w_tfns_2 <- make_adj_matrix_w_tfns(lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2)

  test_fcms_w_tfns <- list(adj_matrix_w_tfns_1, adj_matrix_w_tfns_2)
  test_aggregate_5 <- aggregate_fcms(test_fcms_w_tfns, "mean", include_zeroes = TRUE)
  expect_equal(test_aggregate_5$adj_matrix[1, 2][[1]], tfn(0.2, 0.575, 0.8))
})

test_that("fcm_w_ivfn_aggregation works", {
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.25, 0, 0, 0.25),
    "C" = c(0, 0.25, 0, 0),
    "D" = c(0, 0, 0.25, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, 0.75, 0)
  )
  adj_matrix_w_ivfns_1 <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)

  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.15, 0, 0, 0.15),
    "C" = c(0, 0.15, 0, 0),
    "D" = c(0, 0, 0.15, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.85, 0, 0, 0.85),
    "C" = c(0, 0.85, 0, 0),
    "D" = c(0, 0, 0.85, 0)
  )
  adj_matrix_w_ivfns_2 <- make_adj_matrix_w_ivfns(lower_adj_matrix_2, upper_adj_matrix_2)

  test_fcms_w_ivfns <- list(adj_matrix_w_ivfns_1, adj_matrix_w_ivfns_2)
  aggregate_fcms_w_ivfns(test_fcms_w_tfns, "mean")
})

test_that("fcm_w_tfn_aggregation works", {
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.25, 0, 0, 0.25),
    "C" = c(0, 0.25, 0, 0),
    "D" = c(0, 0, 0.25, 0)
  )
  mode_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.5, 0, 0, 0.5),
    "C" = c(0, 0.5, 0, 0),
    "D" = c(0, 0, 0.5, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, 0.75, 0)
  )
  adj_matrix_w_tfns_1 <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)

  lower_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.15, 0, 0, 0.15),
    "C" = c(0, 0.15, 0, 0),
    "D" = c(0, 0, 0.15, 0)
  )
  mode_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.65, 0, 0, 0.65),
    "C" = c(0, 0.65, 0, 0),
    "D" = c(0, 0, 0.65, 0)
  )
  upper_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.85, 0, 0, 0.85),
    "C" = c(0, 0.85, 0, 0),
    "D" = c(0, 0, 0.85, 0)
  )
  adj_matrix_w_tfns_2 <- make_adj_matrix_w_tfns(lower_adj_matrix_2, mode_adj_matrix_2, upper_adj_matrix_2)

  test_fcms_w_tfns <- list(adj_matrix_w_tfns_1, adj_matrix_w_tfns_2)
  aggregate_fcms_w_tfns(test_fcms_w_tfns, "mean")
})

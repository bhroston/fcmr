

test_that("infer_fcm works", {
  test_conventional_fcm <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )

  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  test_ivfn_fcm <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)

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
  test_tfn_fcm <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)


  infer_conventional_fcm(
    adj_matrix = test_conventional_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )

  infer_ivfn_or_tfn_fcm (
    adj_matrix = test_ivfn_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )

  infer_fcm(
    adj_matrix = test_conventional_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )

  infer_ivfn_or_tfn_fcm (
    adj_matrix = test_ivfn_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )

  infer_ivfn_or_tfn_fcm (
    adj_matrix = test_tfn_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )


  infer_fcm(
    adj_matrix = mc_adj_matrices[[1]],
    initial_state_vector = c(1, 1, 1, 1),
    clamping_vector = c(0, 1, 0, 0),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )


})

test_that("simulate_fcm works", {
  test_conventional_fcm <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )

  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.25, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.75, 0)
  )
  test_ivfn_fcm <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)

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
  test_tfn_fcm <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)

  simulate_fcm(
    adj_matrix = test_conventional_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )

  simulate_fcm(
    adj_matrix = test_ivfn_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )

  simulate_fcm(
    adj_matrix = test_tfn_fcm,
    initial_state_vector = c(1, 1),
    clamping_vector = c(0, 1),
    activation = 'kosko',
    squashing = 'sigmoid',
    lambda = 1,
    max_iter = 100,
    min_error = 1e-05
  )

  # adj_matrices = test_conventional_fcms
  # # Simulation
  initial_state_vector = c(1, 1)
  clamping_vector = c(0, 1)
  activation = 'kosko'
  squashing = 'sigmoid'
  lambda = 1
  max_iter = 100
  min_error = 1e-05

})

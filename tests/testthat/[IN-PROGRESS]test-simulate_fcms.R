

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


test_that("calculate_next_conventional_fcm_state_vector", {
  adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  test_next_state <- calculate_next_conventional_fcm_state_vector(adj_matrix, state_vector = c(1, 0, 0, 0, 0, 0), activation = "kosko", squashing = "sigmoid")
  colnames(test_next_state) <- NULL
  expect_equal(test_next_state[1, ], c(0, -0.85, 0, -0.7, 0.1, 0))



  test <- infer_conventional_fcm(adj_matrix,
                                 initial_state_vector = c(1, 1, 1, 1, 1, 1),
                                 clamping_vector = c(1, 0, 0, 0, 0, 0),
                                 activation = "kosko",
                                 squashing = "sigmoid")
  test$inference
})


test_that("calculate_next_fuzzy_set_fcm_state_vector", {

})


test_that("squash works", {
  expect_equal(round(squash(1, "sigmoid", lambda = 1), 5), 0.73106)
  expect_equal(round(squash(1, "sigmoid", lambda = 0.5), 5), 0.62246)

  expect_equal(round(squash(1, "tanh", lambda = 1), 5), 0.76159)
  expect_equal(round(squash(1, "tanh", lambda = 0.5), 5), 0.46212)

  expect_equal(squash(0.5, "bivalent"), 1)
  expect_equal(squash(0.5, "trivalent"), 1)
  expect_equal(squash(-0.5, "trivalent"), -1)
  expect_equal(squash(2, "saturation"), 1)
})


test_that("defuzz works", {
  expect_equal(defuzz(1), 1)
  expect_equal(defuzz(ivfn(0.2, 0.6)), 0.4)
  expect_equal(defuzz(tfn(0.1, 0.3, 0.8)), 0.4)
})


test_that("convert_element_to_ivfn_or_tfn_if_numeric works", {
  expect_equal(convert_element_to_ivfn_or_tfn_if_numeric(1, "ivfn"), ivfn(1, 1))
  expect_equal(convert_element_to_ivfn_or_tfn_if_numeric(1, "tfn"), tfn(1, 1))
})


test_that("clean_simulation_output works", {

})


test_that("check_simulation_inputs works", {
  test_conventional_fcm <- data.frame(
    "A" = c(0, 0),
    "B" = c(1, 0)
  )
  lower_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.4, 0)
  )
  upper_adj_matrix_1 <- data.frame(
    "A" = c(0, 0),
    "B" = c(0.6, 0)
  )
  test_ivfn_fcm <- make_adj_matrix_w_ivfns(lower_adj_matrix_1, upper_adj_matrix_1)
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
  test_tfn_fcm <- make_adj_matrix_w_tfns(lower_adj_matrix_1, mode_adj_matrix_1, upper_adj_matrix_1)

  # Check adj_matrices ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_conventional_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))

  expect_error(check_simulation_inputs(cbind(test_conventional_fcm, test_conventional_fcm)))
  error_ivfn_fcm <- test_ivfn_fcm
  error_ivfn_fcm[2, 2][[1]] <- 1
  expect_error(check_simulation_inputs(adj_matrix = error_ivfn_fcm))
  # ----

  # Check initial_state_vector ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1, 1), clamping_vector = c(1, 0)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, "a"), clamping_vector = c(1, 0)))
  # ----

  # Check clamping_vector ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 1, 1)))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, "a")))
  # ----

  # Check activation and squashing ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "kosko"))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "modified-kosko"))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "rescale"))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "wrong"))

  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), squashing = "sigmoid"))
  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), squashing = "tanh"))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), squashing = "wrong"))

  expect_no_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "rescale",  squashing = "sigmoid"))
  expect_error(check_simulation_inputs(adj_matrix = test_tfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), activation = "rescale", squashing = "tanh"))
  # ----

  # Check lambda ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), lambda = 1))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), lambda = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), lambda = -1))
  # ----

  # Check max_iter ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = 100))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = 1.5))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), max_iter = 0))
  # ----

  # Check min_error ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), min_error = 1e-4))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), min_error = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), min_error = -1))
  # ----

  # Check fuzzy_set_samples ----
  expect_no_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = 100))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = "a"))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = 1.5))
  expect_error(check_simulation_inputs(adj_matrix = test_ivfn_fcm, initial_state_vector = c(1, 1), clamping_vector = c(1, 0), fuzzy_set_samples = 0))
  # ----
})

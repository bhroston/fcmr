

test_that("asdfasd", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  mode_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.65, 0, 0, 0.80, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.6, 0.65, -0.85, 0, -0.95, 0),
    C5 = c(0.6, 0, 0, -0.2, 0, 0),
    C6 = c(0, -0.85, 0.1, 0, -0.75, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.1, 0, 0, 1, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.4, 0.7, -0.7, 0, -0.1, 0),
    C5 = c(0.7, 0, 0, -0.1, 0, 0),
    C6 = c(0, -0.2, 0.25, 0, -0.7, 0)
  )
  fcm_w_tfn_adj_matrix <- get_fcm_w_tfn_adj_matrix_from_lower_mode_and_upper_adj_matrices(lower_adj_matrix, mode_adj_matrix, upper_adj_matrix)
  initial_state_vector <- c(0.25, 0.5, -0.3, -0.2, -0.65, -0.7)
  clamping_vector = c(0.25, 0, 0.3, 0, 0, 0)

  test_new <- infer_fcm_w_tfn_with_clamping(fcm_w_tfn_adj_matrix = fcm_w_tfn_adj_matrix,
                                        initial_state_vector = initial_state_vector,
                                        clamping_vector = clamping_vector,
                                        activation = "kosko",
                                        squashing = "tanh",
                                        lambda = 0.5,
                                        max_iter = 100,
                                        min_error = 1e-3,
                                        IDs = c())
  test_new_inference <- test_new$inference

  mcmc_models <- build_monte_carlo_fcms(list(fcm_w_tfn_adj_matrix), 1000, include_zeroes = TRUE, show_progress = TRUE)
  test_mcmc <- infer_fmcm_with_clamping(simulated_adj_matrices = mcmc_models,
                                        initial_state_vector = initial_state_vector,
                                        clamping_vector = clamping_vector,
                                        activation = "kosko",
                                        squashing = "tanh",
                                        lambda = 0.5,
                                        max_iter = 100,
                                        min_error = 1e-3,
                                        IDs = c(),
                                        parallel = TRUE,
                                        n_cores = integer(),
                                        show_progress = TRUE,
                                        include_simulations_in_output = FALSE)

  lower_quantile_inference <- get_quantile_of_fmcm_inference(test_mcmc$inference, 0.025)
  mean_inference <- apply(test_mcmc$inference, 2, mean)
  upper_quantile_inference <- get_quantile_of_fmcm_inference(test_mcmc$inference, 0.975)

  mcmc_inference <- data.frame(
    nodes = names(lower_quantile_inference),
    lower = unlist(lower_quantile_inference),
    mode = unlist(mean_inference),
    upper = unlist(upper_quantile_inference)
  )

})



test_that("trying_simulate_fcm_w_tfn", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  mode_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.65, 0, 0, 0.80, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.6, 0.65, -0.85, 0, -0.95, 0),
    C5 = c(0.6, 0, 0, -0.2, 0, 0),
    C6 = c(0, -0.85, 0.1, 0, -0.75, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.1, 0, 0, 1, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.4, 0.7, -0.7, 0, -0.1, 0),
    C5 = c(0.7, 0, 0, -0.1, 0, 0),
    C6 = c(0, -0.2, 0.25, 0, -0.7, 0)
  )
  fcm_w_tfn_adj_matrix <- make_adj_matrix_w_tfns(lower_adj_matrix, mode_adj_matrix, upper_adj_matrix)
  initial_state_vector <- c(0.25, 0.5, -0.3, -0.2, -0.65, -0.7)
  clamping_vector = c(0.25, 0, 0.3, 0, 0, 0)
  activation = "kosko"
  squashing = "tanh"
  lambda = 1
  max_iter = 100
  min_error = 1e-3
  IDs = c()


  expect_warning(
    test <- simulate_fcm_w_tfn_with_clamping_or_pulse_only(fcm_w_tfn_adj_matrix = fcm_w_tfn_adj_matrix,
                                                     initial_state_vector = initial_state_vector,
                                                     activation = "kosko",
                                                     squashing = "tanh",
                                                     lambda = 1,
                                                     max_iter = 100,
                                                     min_error = 1e-5,
                                                     IDs = c())
  )

  expect_no_error(
    test <- simulate_fcm_w_tfn_with_clamping_or_pulse_only(fcm_w_tfn_adj_matrix = fcm_w_tfn_adj_matrix,
                                                     initial_state_vector = initial_state_vector,
                                                     clamping_vector = c(0.25, 0, 0.3, 0, 0, 0),
                                                     activation = "modified-kosko",
                                                     squashing = "tanh",
                                                     lambda = 0.5,
                                                     max_iter = 100,
                                                     min_error = 1e-5,
                                                     IDs = c())
  )

  expect_no_error(
    test <- infer_fcm_w_tfn_with_clamping(fcm_w_tfn_adj_matrix = fcm_w_tfn_adj_matrix,
                                     initial_state_vector = initial_state_vector,
                                     clamping_vector = clamping_vector,
                                     activation = "kosko",
                                     squashing = "tanh",
                                     lambda = 0.5,
                                     max_iter = 100,
                                     min_error = 1e-3,
                                     IDs = c())
  )

  #test_longer <- tidyr::pivot_longer(test$defuzzed_state_vectors[1:20,], col = 2:7)
  #test_longer <- tidyr::pivot_longer(test$inference_state_vectors[1:20,], col = 2:7)
  # ggplot(test_longer) +
  #   geom_line(aes(x = iter, y = value, group = name, color = name))

})

test_that("trying_triangular_estimation", {
  lower_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.85, 0, 0, 0.35, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.7, 0.6, -1, 0, -1, 0),
    C5 = c(0.1, 0, 0, -0.8, 0, 0),
    C6 = c(0, -0.95, 0, 0, -0.95, 0)
  )
  mode_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.65, 0, 0, 0.80, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.6, 0.65, -0.85, 0, -0.95, 0),
    C5 = c(0.6, 0, 0, -0.2, 0, 0),
    C6 = c(0, -0.85, 0.1, 0, -0.75, 0)
  )
  upper_adj_matrix <- data.frame(
    C1 = c(0, 0, 0, 0, 0, 0),
    C2 = c(-0.1, 0, 0, 1, 0, 0),
    C3 = c(0, 0, 0, 0, 0, 0),
    C4 = c(-0.4, 0.7, -0.7, 0, -0.1, 0),
    C5 = c(0.7, 0, 0, -0.1, 0, 0),
    C6 = c(0, -0.2, 0.25, 0, -0.7, 0)
  )

  expect_no_error(
    tri_adj_matrix <- get_fcm_w_tfn_adj_matrix_from_lower_mode_and_upper_adj_matrices(lower_adj_matrix, mode_adj_matrix, upper_adj_matrix)
  )
  # initial_state_vector <- c(0.25, 0.5, -0.3, -0.2, -0.65, -0.7)

  # lower_infer <- infer_fcm_with_clamping(adj_matrix = lower_adj_matrix, initial_state_vector = initial_state_vector, clamping_vector = c(0.25, 0, -0.3, 0, 0, 0), activation = "modified-kosko", squashing = "tanh", lambda = 1, max_iter = 500)
  # mode_infer <- infer_fcm_with_clamping(adj_matrix = mode_adj_matrix, initial_state_vector = initial_state_vector, clamping_vector = c(0.25, 0, -0.3, 0, 0, 0), activation = "modified-kosko", squashing = "tanh", lambda = 1, max_iter = 500)
  # upper_infer <- infer_fcm_with_clamping(adj_matrix = upper_adj_matrix, initial_state_vector = initial_state_vector, clamping_vector = c(0.25, 0, -0.3, 0, 0, 0), activation = "modified-kosko", squashing = "tanh", lambda = 1, max_iter = 500)

  # plot(x = 1:20, y = lower_infer$scenario_simulation$state_vectors$C2[1:20], ylim = c(-1, 1), type = "l")
  # lines(x = 1:20, y = mode_infer$scenario_simulation$state_vectors$C2[1:20] )
  # lines(x = 1:20, y = upper_infer$scenario_simulation$state_vectors$C2[1:20] )

})


test_that("get_fcm_w_tfn_adj_matrix_from_lower_mode_and_upper_adj_matrices works", {
  expect_no_error(
    get_fcm_w_tfn_adj_matrix_from_lower_mode_and_upper_adj_matrices(
      lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
      mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
      upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
    )
  )
})


test_that("confirm_initial_state_vector_is_compatible_with_fcm_w_tfn_adj_matrix ", {
  tri_adj_matrix <- get_fcm_w_tfn_adj_matrix_from_lower_mode_and_upper_adj_matrices(
    lower = matrix(data = c(0, 0.2, 0, 0.5), nrow = 2, ncol = 2),
    mode = matrix(data = c(0, 0.3, 0, 0.6), nrow = 2, ncol = 2),
    upper = matrix(data = c(0, 0.4, 0, 0.7), nrow = 2, ncol = 2)
  )
  expect_no_error(
    confirm_input_vector_is_compatible_with_fcm_w_tfn_adj_matrix(
      tri_adj_matrix, c(triangular_number(0, 0.2, 0.8), 0)
    )
  )
})


test_that("get_triangular_distribution_of_values works", {
  expect_no_error(rtri(lower = 0.25, upper = 0.75, mode = 0.5, n = 100))
  expect_error(rtri(lower = 0.75, upper = 0.25))

  test_tri_dist <- rtri(lower = 0.25, upper = 0.75, mode = 0.5, n = 10000)
  # Perform visual check
  # hist(test_tri_dist)

  test_tri_dist <- rtri(lower = 0.25, upper = 0.75, mode = 0.7, n = 10000)
  # Perform visual check
  # hist(test_tri_dist)
})



# test_fun <- function(goal_mean = numeric(), lower_bound = -1, upper_bound = 1, acceptable_error = 0.001, n_vars = c(2, 3), length_out = 100, CI = 0.95) {
#   if (goal_mean < lower_bound | goal_mean > upper_bound) stop("Input goal_mean must be within the lower and upper bounds")
#   if (lower_bound >= upper_bound) stop("Input lower_bound must be less than Input upper_bound")
#   if (lower_bound != -1 & lower_bound != 0) stop("This function is designed with the domain [-1, 1] or [0, 1] in mind. Please use either -1 or 0 as the lower_bound.")
#   if (upper_bound != 1) stop(stop("This function is designed with the domain [-1, 1] or [0, 1] in mind. Please use either 1 as the upper_bound."))
#   if (acceptable_error <= 0) stop("Input acceptable_error must be greater than 0")
#   if (!(n_vars %in% c(2, 3))) stop("Input n_vars must be either 2 or 3")
#   if ((length_out != round(length_out)) | (length_out <= 0)) stop("Input length_out must be a positive integer")
#   if (n_vars == 2) {
#     var_1 <- seq(lower_bound, upper_bound, length.out = length_out)*(1 - abs(rnorm(length_out, 0, 0.001)))
#     var_2 <- seq(lower_bound, upper_bound, length.out = length_out)*(1 - abs(rnorm(length_out, 0, 0.001)))
#     combos <- expand.grid(var_1, var_2)
#     combos <- combos[combos[, 1] <= combos[, 2], ]
#   } else if (n_vars == 3) {
#     var_1 <- seq(lower_bound, upper_bound, length.out = length_out)*(1 - abs(rnorm(length_out, 0, 0.001)))
#     var_2 <- seq(lower_bound, upper_bound, length.out = length_out)*(1 - abs(rnorm(length_out, 0, 0.001)))
#     var_3 <- seq(lower_bound, upper_bound, length.out = length_out)*(1 - abs(rnorm(length_out, 0, 0.001)))
#     combos <- expand.grid(var_1, var_2, var_3)
#     combos <- combos[(combos[, 1] <= combos[, 2] & combos[, 2] <= combos[, 3]), ]
#   }
#   combo_means <- apply(combos, 1, function(row) .Internal(mean(row)))
#   acceptable_combos <- combos[abs(combo_means - chosen_mean) <= error, ]
#   possible_values <- as.vector(t(acceptable_combos))
#   lower_quantile <- quantile(possible_values, (0.5 - CI/2))
#   upper_quantile <- quantile(possible_values, (0.5 + CI/2))
#
#   acceptable_combos
# }

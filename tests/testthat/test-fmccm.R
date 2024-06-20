
test_that("confer_fmccm works", {
  adj_matrix <- adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.5, 0, 0, 0.5),
    "C" = c(0, 0.5, 0, 0),
    "D" = c(0, 0, 0.5, 0)
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
  activation_vector <- c(1, 1, 1, 1)
  scenario_vector <- c(1, 0, 0, 0)
  activation = "kosko"
  squashing = "tanh"
  lambda = 1
  max_iter = 1000
  min_error = 1e-5
  lambda_optimization = "koutsellis"
  IDs = c()
  parallel = TRUE
  show_progress = TRUE

  uniform_fmccm_models <- build_fmccm_models(adj_matrix, n_sims = 1000, distribution = "uniform", lower_adj_matrix = lower_adj_matrix, upper_adj_matrix = upper_adj_matrix)
  # simulated_adj_matrices = uniform_fmccm_models

  test_confer_fmccm <- confer_fmccm(uniform_fmccm_models, activation_vector, scenario_vector, activation = "kosko", squashing = "tanh",
               lambda = 1, max_iter = 1000, parallel = TRUE, n_cores = 2, show_progress = TRUE)
  test_means <- round(apply(test_confer_fmccm$inferences, 2, mean), 1)
  test_means <- test_means[names(test_means) != "iter"]
  expect_equal(test_means, c(A = 1.0, B = 0.5, C = 0.2, D = 0.1))

  #bootstrap_mean_CIs <- get_means_of_fmccm_inferences(test_confer_fmccm$inferences)
  #bootstrap_mean_CIs <- bootstrap_mean_CIs[bootstrap_mean_CIs$node != "A", ]
  #bootstrap_mean_CIs_plotting <- tidyr::pivot_longer(bootstrap_mean_CIs, cols = 2:3)
  #bootstrap_mean_CIs_plotting <- bootstrap_mean_CIs_plotting[bootstrap_mean_CIs_plotting$node != "A", ]

  # Perform visual check
  # x <- test_confer_fmccm$inferences_for_plotting[test_confer_fmccm$inferences_for_plotting$node != "A", ]
  # ggplot() +
  #   geom_jitter(data = x, aes(x = node, y = value), alpha = 0.025) +
  #   geom_crossbar(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), color = "red", fill = "red") +
  #   # geom_errorbar(data = bootstrap_mean_CIs_plotting, aes(x = node, ymin = value, ymax = value, group = name)) +
  #   geom_text(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025 - 0.05, label = round(lower_0.025, 2))) +
  #   geom_text(data = bootstrap_mean_CIs, aes(x = node, y = upper_0.975 + 0.05, label = round(upper_0.975, 2))) +
  #   ylim(0, 1) +
  #   theme_classic()


  test_confer_fmccm <- confer_fmccm(uniform_fmccm_models, activation_vector, scenario_vector, activation = "kosko", squashing = "tanh",
                                    lambda = 1, max_iter = 1000, parallel = TRUE, n_cores = 2, show_progress = FALSE)
  test_means <- round(apply(test_confer_fmccm$inferences, 2, mean), 1)
  test_means <- test_means[names(test_means) != "iter"]
  expect_equal(test_means, c(A = 1.0, B = 0.5, C = 0.2, D = 0.1))

  test_confer_fmccm <- confer_fmccm(uniform_fmccm_models, activation_vector, scenario_vector, activation = "kosko", squashing = "tanh",
                                    lambda = 1, max_iter = 1000, parallel = FALSE, show_progress = TRUE)
  test_means <- round(apply(test_confer_fmccm$inferences, 2, mean), 1)
  test_means <- test_means[names(test_means) != "iter"]
  expect_equal(test_means, c(A = 1.0, B = 0.5, C = 0.2, D = 0.1))

  test_confer_fmccm <- confer_fmccm(uniform_fmccm_models, activation_vector, scenario_vector, activation = "kosko", squashing = "tanh",
                                    lambda = 1, max_iter = 1000, parallel = FALSE, show_progress = FALSE)
  test_means <- round(apply(test_confer_fmccm$inferences, 2, mean), 1)
  test_means <- test_means[names(test_means) != "iter"]
  expect_equal(test_means, c(A = 1.0, B = 0.5, C = 0.2, D = 0.1))

  expect_equal(colnames(test_confer_fmccm$inferences_for_plotting), c("node", "value"))
  expect_equal(names(test_confer_fmccm$inference_state_vectors_by_iter), paste0("iter_", 0:(length(test_confer_fmccm$inference_state_vectors_by_iter) - 1)))
  expect_equal(names(test_confer_fmccm$inference_state_vectors_by_sim), paste0("sim_", 1:length(test_confer_fmccm$inference_state_vectors_by_sim)))
})


test_that("simulate_fmccm_models works", {
  edgelist_test <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.4, 0.6)
  )

  edgelist_lower <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.2, 0.2)
  )

  edgelist_upper <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.8, 0.8)
  )

  edgelist_mode <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.5, 0.5)
  )

  test_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_test)
  l_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_lower)
  u_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_upper)
  m_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_mode)

  # Perform visual checks of model outputs
  uniform_fmccm_models <- build_fmccm_models(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
  test_uniform_sims_kosko_tanh <-  simulate_fmccm_models(
    uniform_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "kosko", squashing = "tanh", lambda = 1,
    max_iter = 50, parallel = FALSE
  )
  test_uniform_sims_kosko_sigmoid <-  simulate_fmccm_models(
    uniform_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "kosko", squashing = "sigmoid", lambda = 1,
    max_iter = 50, parallel = FALSE
  )
  test_uniform_sims_modifiedkosko_tanh <-  simulate_fmccm_models(
    uniform_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "modified-kosko", squashing = "tanh", lambda = 0.5,
    max_iter = 50, parallel = FALSE
  )
  test_uniform_sims_modifiedkosko_sigmoid <-  simulate_fmccm_models(
    uniform_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "modified-kosko", squashing = "sigmoid", lambda = 0.4,
    max_iter = 50, parallel = FALSE
  )
  # Something strange happens with papageourgiou and tanh, getting negative numbers; check later
  # Performs correctly for sigmoid
  expect_error(simulate_fmccm_models(
    uniform_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "rescale", squashing = "tanh", lambda = 0.5,
    max_iter = 50, parallel = FALSE
  ))
  test_uniform_sims_rescale_sigmoid <-  simulate_fmccm_models(
    uniform_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "rescale", squashing = "sigmoid", lambda = 0.5,
    max_iter = 50, parallel = FALSE
  )

  triangular_fmccm_models <- build_fmccm_models(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
  test_triangular_sims_kosko_tanh <-  simulate_fmccm_models(
    triangular_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "kosko", squashing = "tanh", lambda = 1,
    max_iter = 50, parallel = FALSE
  )
  test_triangular_sims_kosko_sigmoid <-  simulate_fmccm_models(
    triangular_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "kosko", squashing = "sigmoid", lambda = 1,
    max_iter = 50, parallel = FALSE
  )
  test_triangular_sims_modifiedkosko_tanh <-  simulate_fmccm_models(
    triangular_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "modified-kosko", squashing = "tanh", lambda = 0.5,
    max_iter = 50, parallel = FALSE
  )
  test_triangular_sims_modifiedkosko_sigmoid <-  simulate_fmccm_models(
    triangular_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "modified-kosko", squashing = "sigmoid", lambda = 0.4,
    max_iter = 50, parallel = FALSE
  )

  # Something strange happens with papageourgiou and tanh, getting negative numbers; check later
  expect_error(test_triangular_sims_rescale_tanh <-  simulate_fmccm_models(
    triangular_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "rescale", squashing = "tanh", lambda = 0.5,
    max_iter = 50, parallel = FALSE
  ))
  test_triangular_sims_rescale_sigmoid <-  simulate_fmccm_models(
    triangular_fmccm_models, activation_vector = c(1, 1, 1), scenario_vector = c(1, 0, 0),
    activation = "rescale", squashing = "sigmoid", lambda = 0.5,
    max_iter = 50, parallel = FALSE
  )
})


test_that("build_fmccm_models works", {
  edgelist_test <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.4, 0.6)
  )

  edgelist_lower <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.2, 0.2)
  )

  edgelist_upper <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.8, 0.8)
  )

  edgelist_mode <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.5, 0.5)
  )

  test_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_test)
  l_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_lower)
  u_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_upper)
  m_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_mode)

  # Accept if additional_inputs is a list
  expect_no_error(build_fmccm_models(test_adj_matrix, ... = list(lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)))

  # Throws error when additional inputs do not have matching dims
  expect_error(build_fmccm_models(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = matrix(data = NA, nrow = 5, ncol = 5)))
  # Throws error when additional inputs have incorrect edge data
  expect_error(build_fmccm_models(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = u_adj_matrix + 0.2))

  # IF distribution == "uniform"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(build_fmccm_models(test_adj_matrix, l = l_adj_matrix, u = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are greater than values in upper_adj_matrix
  expect_error(build_fmccm_models(test_adj_matrix, lower_adj_matrix = u_adj_matrix, upper_adj_matrix = l_adj_matrix))
  # Throws error if too many additional inputs present
  expect_error(build_fmccm_models(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix))

  uniform_fmccm_models <- build_fmccm_models(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
  expect_equal(unique(dim(test_adj_matrix)), unique(unlist(lapply(uniform_fmccm_models, function(model) unique(dim(model))))))
  expect_equal(length(uniform_fmccm_models), 100)
  # Visual check
  hist(unlist(lapply(uniform_fmccm_models, function(model) model[model != 0] [1])))
  hist(unlist(lapply(uniform_fmccm_models, function(model) model[model != 0] [2])))

  # IF distribution == "triangular"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(build_fmccm_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, u = u_adj_matrix))
  # Throws warning when no mode_adj_matrix input given
  expect_warning(build_fmccm_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are less than values in mode_adj_matrix
  expect_error(build_fmccm_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*0.01))
  # Throws error when values in upper_adj_matrix are greater than values in mode_adj_matrix
  expect_error(build_fmccm_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*2))

  triangular_fmccm_models <- build_fmccm_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix)
  expect_equal(unique(dim(test_adj_matrix)), unique(unlist(lapply(triangular_fmccm_models, function(model) unique(dim(model))))))
  expect_equal(length(triangular_fmccm_models), 100)
  # Visual check
  hist(unlist(lapply(triangular_fmccm_models, function(model) model[model != 0] [1])))
  hist(unlist(lapply(triangular_fmccm_models, function(model) model[model != 0] [2])))

  triangular_fmccm_models_upper_skew <- build_fmccm_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = u_adj_matrix*0.9)
  hist(unlist(lapply(triangular_fmccm_models_upper_skew, function(model) model[model != 0] [1])))
  hist(unlist(lapply(triangular_fmccm_models_upper_skew, function(model) model[model != 0] [2])))
})


test_that("fmccm works", {
  edgelist_test <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.4, 0.6)
  )

  edgelist_lower <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.2, 0.2)
  )

  edgelist_upper <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.8, 0.8)
  )

  edgelist_mode <- data.frame(
    source = c("A", "B"),
    target = c("B", "C"),
    weight = c(0.5, 0.5)
  )

  test_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_test)
  l_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_lower)
  u_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_upper)
  m_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_mode)

  # Accept if additional_inputs is a list
  expect_no_error(fmccm(test_adj_matrix, ... = list(lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)))

  # Throws error when additional inputs do not have matching dims
  expect_error(fmccm(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = matrix(data = NA, nrow = 5, ncol = 5)))
  # Throws error when additional inputs have incorrect edge data
  expect_error(fmccm(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = u_adj_matrix + 0.2))

  # IF distribution == "uniform"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(fmccm(test_adj_matrix, l = l_adj_matrix, u = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are greater than values in upper_adj_matrix
  expect_error(fmccm(test_adj_matrix, lower_adj_matrix = u_adj_matrix, upper_adj_matrix = l_adj_matrix))
  # Throws error if too many additional inputs present
  expect_error(fmccm(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix))

  uniform_fmccm <- fmccm(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
  expect_equal(names(uniform_fmccm$edgelist), c("source", "target", "weight", "lower", "upper"))
  expect_equal(uniform_fmccm$distribution_params$lower, l_adj_matrix)
  expect_equal(uniform_fmccm$distribution_params$upper, u_adj_matrix)

  # IF distribution == "triangular"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(fmccm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, u = u_adj_matrix))
  # Throws warning when no mode_adj_matrix input given
  expect_warning(fmccm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are less than values in mode_adj_matrix
  expect_error(fmccm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*0.01))
  # Throws error when values in upper_adj_matrix are greater than values in mode_adj_matrix
  expect_error(fmccm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*2))

  triangular_fmccm <- fmccm(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix)

  expect_equal(names(triangular_fmccm$edgelist), c("source", "target", "weight", "lower", "upper", "mode"))
  expect_equal(triangular_fmccm$distribution_params$lower, l_adj_matrix)
  expect_equal(triangular_fmccm$distribution_params$upper, u_adj_matrix)
  expect_equal(triangular_fmccm$distribution_params$mode, m_adj_matrix)
})


test_that("get_means_of_fmccm_inferences", {
  adj_matrix <- adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
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
  activation_vector <- c(1, 1, 1, 1)
  scenario_vector <- c(1, 0, 0, 0)

  uniform_fmccm_models <- build_fmccm_models(adj_matrix, n_sims = 1000, distribution = "uniform", lower_adj_matrix = lower_adj_matrix, upper_adj_matrix = upper_adj_matrix)
  # simulated_adj_matrices = uniform_fmccm_models

  test_sim_fmccm <- simulate_fmccm_models(uniform_fmccm_models, activation_vector, scenario_vector, activation = "kosko", squashing = "tanh",
                                    lambda = 1, max_iter = 1000, parallel = FALSE, show_progress = TRUE)


  test_nobootstrap <- get_means_of_fmccm_inferences(
    test_sim_fmccm$final_states_across_sims,
    get_bootstrapped_means = FALSE
  )
  test_bootstrap_parallel_progress <- get_means_of_fmccm_inferences(
    test_sim_fmccm$final_states_across_sims,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = TRUE,
    n_cores = 2,
    show_progress = TRUE
  )
  test_bootstrap_parallel_noprogress <- get_means_of_fmccm_inferences(
    test_sim_fmccm$final_states_across_sims,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = TRUE,
    n_cores = 2,
    show_progress = FALSE
  )
  test_bootstrap_noparallel_progress <- get_means_of_fmccm_inferences(
    test_sim_fmccm$final_states_across_sims,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = FALSE,
    show_progress = TRUE
  )
  test_bootstrap_noparallel_noprogress <- get_means_of_fmccm_inferences(
    test_sim_fmccm$final_states_across_sims,
    get_bootstrapped_means = TRUE,
    confidence_interval = 0.95,
    bootstrap_reps = 1000,
    bootstrap_samples_per_rep = 1000,
    parallel = FALSE,
    show_progress = FALSE
  )

  Lower_CIs_parallel_progress <- round(test_bootstrap_parallel_progress$lower_0.025, 2)
  Lower_CIs_parallel_noprogress <- round(test_bootstrap_parallel_noprogress$lower_0.025, 2)
  Lower_CIs_noparallel_progress <- round(test_bootstrap_noparallel_progress$lower_0.025, 2)
  Lower_CIs_noparallel_noprogress <- round(test_bootstrap_noparallel_noprogress$lower_0.025, 2)

  acceptable_random_error <- 0.02

  expect_true(all(abs(Lower_CIs_parallel_progress - Lower_CIs_parallel_progress) < acceptable_random_error))
  expect_true(all(abs(Lower_CIs_noparallel_progress - Lower_CIs_parallel_progress) < acceptable_random_error))
  expect_true(all(abs(Lower_CIs_parallel_progress - Lower_CIs_parallel_noprogress) < acceptable_random_error))
  expect_true(all(abs(Lower_CIs_noparallel_progress - Lower_CIs_parallel_noprogress) < acceptable_random_error))

  # Perform visual check
  # x <- test_bootstrap_noparallel_noprogress
  # x <- x[x$node != "A",]
  # ggplot() +
  #   geom_crossbar(data = x, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), fill = "red", color = "red") +
  #   geom_text(data = x, aes(x = node, y = lower_0.025 - 0.05, label = round(lower_0.025, 2))) +
  #   geom_text(data = x, aes(x = node, y = upper_0.975 + 0.05, label = round(upper_0.975, 2))) +
  #   ylim(0, 1) +
  #   theme_classic()
})


test_that("get_quantiles_of_simulated_values_across_iters works", {
  test_adj_matrix <- adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  l_adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.25, 0, 0, 0.25),
    "C" = c(0, 0.25, 0, 0),
    "D" = c(0, 0, 0.25, 0)
  )
  u_adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, 0.75, 0)
  )

  test_fmccm_models <- build_fmccm_models(test_adj_matrix, n_sims = 1000, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
  test_sims <-  simulate_fmccm_models(
    test_fmccm_models, activation_vector = c(1, 1, 1, 1), scenario_vector = c(1, 0, 0, 0),
    activation = "kosko", squashing = "tanh", lambda = 1,
    max_iter = 1000, parallel = FALSE
  )

  test_lower_quantile <- get_quantile_of_fmccm_state_vectors_at_iter(test_sims$state_vectors_by_iter_across_sims, quantile = 0.25)
  test_upper_quantile <- get_quantile_of_fmccm_state_vectors_at_iter(test_sims$state_vectors_by_iter_across_sims, quantile = 0.75)

  test_lower_quantile_final <- round(test_lower_quantile[nrow(test_lower_quantile),], 2)
  test_upper_quantile_final <- round(test_upper_quantile[nrow(test_upper_quantile),], 2)

  matches_expected_lower_quantile_A <- test_lower_quantile_final$A == 1
  matches_expected_lower_quantile_B <- test_lower_quantile_final$B < 0.5 & test_lower_quantile_final$B > 0.3
  matches_expected_lower_quantile_C <- test_lower_quantile_final$C < 0.4 & test_lower_quantile_final$C > 0.2
  matches_expected_lower_quantile_D <- test_lower_quantile_final$D < 0.3 & test_lower_quantile_final$D > 0.1

  expect_true(all(matches_expected_lower_quantile_A, matches_expected_lower_quantile_B,
                  matches_expected_lower_quantile_C, matches_expected_lower_quantile_D))

  # Perform visual check
  # test_lower_quantile_long <- tidyr::pivot_longer(test_lower_quantile, cols = 2:ncol(test_lower_quantile))
  # test_upper_quantile_long <- tidyr::pivot_longer(test_upper_quantile, cols = 2:ncol(test_upper_quantile))
  # ggplot() +
  #   geom_line(data = test_lower_quantile_long, aes(x = iter, y = value, color = name)) +
  #   geom_line(data = test_upper_quantile_long, aes(x = iter, y = value, color = name))
})


test_that("get_triangular_distribution_of_values works", {
  expect_no_error(get_triangular_distribution_of_values(lower = 0.25, upper = 0.75, mode = 0.5, n = 100))
  expect_error(get_triangular_distribution_of_values(lower = 0.75, upper = 0.25))

  test_tri_dist <- get_triangular_distribution_of_values(lower = 0.25, upper = 0.75, mode = 0.5, n = 10000)
  # Perform visual check
  # hist(test_tri_dist)

  test_tri_dist <- get_triangular_distribution_of_values(lower = 0.25, upper = 0.75, mode = 0.7, n = 10000)
  # Perform visual check
  # hist(test_tri_dist)
})


test_that("get_beta_distribution_of_values works", {
  expect_no_error(get_beta_distribution_of_values(mu = 0.5, sd = 0.1, n = 100))
  expect_error(get_beta_distribution_of_values(mu = 2, sd = 0.1))
  expect_error(get_beta_distribution_of_values(mu = 0.5, sd = 6))

  # Perform visual check
  # test_beta_dist <- get_beta_distribution_of_values(mu = 0.5, sd = 0.1, n = 10000)
  # hist(test_beta_dist)
})




#
# test_quantiles <- get_quantiles_of_simulated_values_across_iters(test$inference_state_vectors_by_sim, get_bootstrapped_means = TRUE, bootstrap_reps = 100, bootstrap_samples_per_rep = 100)
#
# apply(min, 2, test$inference)
#
# tidyr::pivot_longer(apply(test$inference, 2, min), 1:4)
#
# nodes <- colnames(test$inference)
# plotting_data <- list(
#   min_values = data.frame(
#     node = nodes,
#     value = apply(test$inference, 2, min, simplify = TRUE)
#   ),
#   max_values = data.frame(
#     node = nodes,
#     value = apply(test$inference, 2, max, simplify = TRUE)
#   ),
#   quantiles = data.frame(
#     node = nodes,
#     upper_quantile = unlist(lapply(test_quantiles, function(x) x[1])),
#     lower_quantile = unlist(lapply(test_quantiles, function(x) x[2]))
#   )
# )
#
# plotting_data$min_values <- plotting_data$min_values[plotting_data$min_values$value != 1, ]
# plotting_data$max_values <- plotting_data$max_values[plotting_data$max_values$value != 1, ]
# plotting_data$quantiles <- plotting_data$quantiles[plotting_data$quantiles$upper_quantile != 1, ]
#
# ggplot() +
#   geom_bar(data = plotting_data$max_values, aes(x = node, y = value), stat = "identity", fill = "grey") +
#   geom_bar(data = plotting_data$min_values, aes(x = node, y = value), stat = "identity", fill = "white") +
#   geom_errorbar(data = plotting_data$quantiles,
#                 aes(x = node, ymin = lower_quantile, ymax = lower_quantile), color = "red", linetype = 2) +
#   geom_errorbar(data = plotting_data$quantiles,
#                 aes(x = node, ymin = upper_quantile, ymax = upper_quantile), color = "red", linetype = 2) +
#   geom_text(data = plotting_data$quantiles, aes(x = node, y = lower_quantile - 0.025, label = round(lower_quantile, 2))) +
#   geom_text(data = plotting_data$quantiles, aes(x = node, y = upper_quantile + 0.025, label = round(upper_quantile, 2))) +
#   theme_classic()
#
#
# geom_bar(aes(x = node, y = min()), stat = "identity", fill = "white")
#
# geom_boxplot(aes(x = node, y = value), color = "white")
#
#
# geom_bar(aes(x = node, y = lower_value), stat = "identity", fill = "white") +
#   geom_crossbar(aes(x = node, y = lower_value, ymin = lower_value, ymax = upper_value), color = "red", fill = "red") +
#   geom_text(aes(x = node, y = lower_value - 0.05, label = round(lower_value, 2))) +
#   geom_text(aes(x = node, y = upper_value + 0.05, label = round(upper_value, 2))) +
#   theme_classic()
#
#
# uniform_fmccm_models <- build_fmccm_models(adj_matrix, n_sims = 100, distribution = "uniform", lower_adj_matrix = lower_adj_matrix, upper_adj_matrix = upper_adj_matrix)
# simulated_adj_matrices <- uniform_fmccm_models
# test_uniform_sims_kosko_tanh <- simulate_fmccm_models(
#   uniform_fmccm_models, activation_vector, scenario_vector,
#   activation = "kosko", squashing = "tanh", lambda = 1,
#   max_iter = 1000, parallel = FALSE
# )

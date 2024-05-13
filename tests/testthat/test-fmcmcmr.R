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


test_that("simulate_fmcmcmr_models works", {

})

test_that("build_fmcmcmr_models works", {
  # Accept if additional_inputs is a list
  expect_no_error(build_fmcmcmr_models(test_adj_matrix, ... = list(lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)))

  # Throws error when additional inputs do not have matching dims
  expect_error(build_fmcmcmr_models(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = matrix(data = NA, nrow = 5, ncol = 5)))
  # Throws error when additional inputs have incorrect edge data
  expect_error(build_fmcmcmr_models(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = u_adj_matrix + 0.2))

  # IF distribution == "uniform"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(build_fmcmcmr_models(test_adj_matrix, l = l_adj_matrix, u = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are greater than values in upper_adj_matrix
  expect_error(build_fmcmcmr_models(test_adj_matrix, lower_adj_matrix = u_adj_matrix, upper_adj_matrix = l_adj_matrix))
  # Throws error if too many additional inputs present
  expect_error(build_fmcmcmr_models(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix))

  uniform_fmcmcmr_models <- build_fmcmcmr_models(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
  expect_equal(unique(dim(test_adj_matrix)), unique(unlist(lapply(uniform_fmcmcmr_models, function(model) unique(dim(model))))))
  expect_equal(length(uniform_fmcmcmr_models), 100)
  # Visual check
  hist(unlist(lapply(uniform_fmcmcmr_models, function(model) model[model != 0] [1])))
  hist(unlist(lapply(uniform_fmcmcmr_models, function(model) model[model != 0] [2])))

  # IF distribution == "triangular"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(build_fmcmcmr_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, u = u_adj_matrix))
  # Throws warning when no mode_adj_matrix input given
  expect_warning(build_fmcmcmr_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are less than values in mode_adj_matrix
  expect_error(build_fmcmcmr_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*0.01))
  # Throws error when values in upper_adj_matrix are greater than values in mode_adj_matrix
  expect_error(build_fmcmcmr_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*2))

  triangular_fmcmcmr_models <- build_fmcmcmr_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix)
  expect_equal(unique(dim(test_adj_matrix)), unique(unlist(lapply(triangular_fmcmcmr_models, function(model) unique(dim(model))))))
  expect_equal(length(triangular_fmcmcmr_models), 100)
  # Visual check
  hist(unlist(lapply(triangular_fmcmcmr_models, function(model) model[model != 0] [1])))
  hist(unlist(lapply(triangular_fmcmcmr_models, function(model) model[model != 0] [2])))

  triangular_fmcmcmr_models_upper_skew <- build_fmcmcmr_models(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = u_adj_matrix*0.9)
  hist(unlist(lapply(triangular_fmcmcmr_models_upper_skew, function(model) model[model != 0] [1])))
  hist(unlist(lapply(triangular_fmcmcmr_models_upper_skew, function(model) model[model != 0] [2])))
})

test_that("fmcmcmr works", {
  # Accept if additional_inputs is a list
  expect_no_error(fmcmcmr(test_adj_matrix, ... = list(lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)))

  # Throws error when additional inputs do not have matching dims
  expect_error(fmcmcmr(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = matrix(data = NA, nrow = 5, ncol = 5)))
  # Throws error when additional inputs have incorrect edge data
  expect_error(fmcmcmr(test_adj_matrix, upper_adj_matrix = u_adj_matrix, lower_adj_matrix = u_adj_matrix + 0.2))

  # IF distribution == "uniform"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(fmcmcmr(test_adj_matrix, l = l_adj_matrix, u = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are greater than values in upper_adj_matrix
  expect_error(fmcmcmr(test_adj_matrix, lower_adj_matrix = u_adj_matrix, upper_adj_matrix = l_adj_matrix))
  # Throws error if too many additional inputs present
  expect_error(fmcmcmr(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix))

  uniform_fmcmcmr <- fmcmcmr(test_adj_matrix, lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
  expect_equal(names(uniform_fmcmcmr$edgelist), c("source", "target", "weight", "lower", "upper"))
  expect_equal(uniform_fmcmcmr$distribution_params$lower, l_adj_matrix)
  expect_equal(uniform_fmcmcmr$distribution_params$upper, u_adj_matrix)

  # IF distribution == "triangular"
  # Throws error when lower_adj_matrix and upper_adj_matrix inputs not given
  expect_error(fmcmcmr(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, u = u_adj_matrix))
  # Throws warning when no mode_adj_matrix input given
  expect_warning(fmcmcmr(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix))
  # Throws error when values in lower_adj_matrix are less than values in mode_adj_matrix
  expect_error(fmcmcmr(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*0.01))
  # Throws error when values in upper_adj_matrix are greater than values in mode_adj_matrix
  expect_error(fmcmcmr(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix*2))

  triangular_fmcmcmr <- fmcmcmr(test_adj_matrix, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = m_adj_matrix)

  expect_equal(names(triangular_fmcmcmr$edgelist), c("source", "target", "weight", "lower", "upper", "mode"))
  expect_equal(triangular_fmcmcmr$distribution_params$lower, l_adj_matrix)
  expect_equal(triangular_fmcmcmr$distribution_params$upper, u_adj_matrix)
  expect_equal(triangular_fmcmcmr$distribution_params$mode, m_adj_matrix)
})

test_that("get_triangular_distribution_of_values works", {

})

test_that("get_beta_distribution_of_values works", {

})






#
#
# edgelist_test <- data.frame(
#   source = c("A", "B"),
#   target = c("B", "C"),
#   weight = c(0.4, 0.6)
# )
#
# edgelist_lower <- data.frame(
#   source = c("A", "B"),
#   target = c("B", "C"),
#   weight = c(0.2, 0.2)
# )
#
# edgelist_upper <- data.frame(
#   source = c("A", "B"),
#   target = c("B", "C"),
#   weight = c(0.8, 0.8)
# )
#
# test_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_test)
# l_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_lower)
# u_adj_matrix <- get_adj_matrix_from_edgelist(edgelist_upper)
#
# test_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(l_adj_matrix, u_adj_matrix)
# colnames(test_grey_adj_matrix) <- c("A", "B", "C")
# fgcm_sim <- simulate_fgcmr(test_grey_adj_matrix, initial_state_vector = list(1, 0, 0), squashing = "tanh", lambda = 0.5)
#
# lower_bounds <- apply(fgcm_sim$state_vectors, 2, function(grey_num_list) unlist(lapply(grey_num_list, function(grey_num) grey_num$lower)))
# upper_bounds <- apply(fgcm_sim$state_vectors, 2, function(grey_num_list) unlist(lapply(grey_num_list, function(grey_num) grey_num$upper)))
#
# lower_bounds_df <- data.frame(cbind(iter = 1:nrow(lower_bounds), lower_bounds))
# upper_bounds_df <- data.frame(cbind(iter = 1:nrow(upper_bounds), upper_bounds))
#
# lower_bounds_df <- tidyr::pivot_longer(lower_bounds_df, cols = 2:ncol(lower_bounds_df))
# upper_bounds_df <- tidyr::pivot_longer(upper_bounds_df, cols = 2:ncol(upper_bounds_df))
#
# lower_bounds_df$iter_name <- paste0(lower_bounds_df$iter, "_", lower_bounds_df$name)
# upper_bounds_df$iter_name <- paste0(upper_bounds_df$iter, "_", lower_bounds_df$name)
#
# test_that("fmcmcmr uniform sim works", {
#   uniform_models <- build_fmcmcmr_models(test_adj_matrix, n_sims = 1000, distribution = "uniform", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix)
#   sim_models <- simulate_fmcmcmr_models(uniform_models, initial_state_vector = c(1, 0, 0), squashing = "tanh", lambda = 0.5, parallel = FALSE)
#
#   sim_state_vectors <- lapply(sim_models, function(model) model$state_vectors)
#   for (i in seq_along(sim_state_vectors)) {
#     sim_state_vectors[[i]] <- cbind(
#       iter = 1:nrow(sim_state_vectors[[i]]),
#       sim = i,
#       sim_state_vectors[[i]]
#     )
#   }
#
#   sim_state_vectors_df <- data.frame(do.call(rbind, sim_state_vectors))
#   sim_state_vectors_df_long <- tidyr::pivot_longer(sim_state_vectors_df, cols = 3:ncol(sim_state_vectors_df))
#   sim_state_vectors_df_long$sim_name <- paste0(sim_state_vectors_df$sim, "_", sim_state_vectors_df_long$name)
#
#   rows_per_sim <- unique(unlist(lapply(sim_state_vectors, function(sim) nrow(sim))))
#   iter_values_across_sims <- vector(mode = "list", length = rows_per_sim)
#   for (i in 1:rows_per_sim) {
#     iter_values_across_sims[[i]] <- do.call(rbind, lapply(sim_state_vectors, function(sim) sim[i, ]))
#   }
#   names(iter_values_across_sims) <- paste0("iter_", 1:rows_per_sim)
#
#   get_sim_quantiles_df <- function(state_vectors_node_iters, mean_quantiles = c(0.025, 0.975), sd_quantiles = c(0.16, 0.84)) {
#     n_draws <- 100
#     node_means <- lapply(state_vectors_node_iters, function(iter) {
#       mean_of_means <- vector(mode = "list", length = length(iter))
#       for (i in 1:length(iter)) {
#         mean_of_means[[i]] <- mean(sample(iter, n_draws, replace = TRUE))
#       }
#       return(do.call(c, mean_of_means))
#     })
#     mean_quantile_CIs <- do.call(rbind, lapply(node_means, function(iter) quantile(iter, c(mean_quantiles[1], mean_quantiles[2]))))
#     colnames(mean_quantile_CIs) <- c("lower_mean_CI", "upper_mean_CI")
#
#     sd_quantile_CIs <- do.call(rbind, lapply(state_vectors_node_iters, function(iter) quantile(iter, c(sd_quantiles[1], sd_quantiles[2]))))
#     colnames(sd_quantile_CIs) <- c("lower_sd_CI", "upper_sd_CI")
#
#     quantile_CIs <- data.frame(cbind(iter = 1:length(state_vectors_node_iters), mean_quantile_CIs, sd_quantile_CIs))
#
#     return(quantile_CIs)
#   }
#
#   B_iters <- lapply(iter_values_across_sims, function(iter) iter$B)
#   C_iters <- lapply(iter_values_across_sims, function(iter) iter$C)
#
#   B_quantile_CIs <- get_sim_quantiles_df(B_iters)
#   C_quantile_CIs <- get_sim_quantiles_df(C_iters)
#
#   data <- sim_state_vectors_df_long
#
#   ggplot() +
#     geom_line(data = data, aes(x = iter, y = value, group = sim_name, color = name), alpha = 0.01) +
#     geom_line(data = lower_bounds_df, aes(x = iter, y = value, group = name), color = "grey") +
#     geom_line(data = upper_bounds_df, aes(x = iter, y = value, group = name), color = "grey") +
#     #geom_ribbon(data = B_quantile_CIs, aes(x = iter, ymin = lower_mean_CI, ymax = upper_mean_CI), fill = "#B0CBE7FF") +
#     #geom_ribbon(data = B_quantile_CIs, aes(x = iter, ymin = lower_sd_CI, ymax = upper_sd_CI), fill = "#B0CBE7FF", alpha = 0.5) +
#     #geom_ribbon(data = C_quantile_CIs, aes(x = iter, ymin = lower_mean_CI, ymax = upper_mean_CI), fill = "#FEF7C7FF") +
#     #geom_ribbon(data = C_quantile_CIs, aes(x = iter, ymin = lower_sd_CI, ymax = upper_sd_CI), fill = "#FEF7C7FF", alpha = 0.5) +
#     scale_x_continuous(limits = c(0, 30), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
#     scale_color_manual(
#       values = c(
#         #A = "black", B = "#B0CBE7FF", C = "#FEF7C7FF", D = "#EBA07EFF"
#         A = "black", B = "#B0CBE7FF", C = "#FEF7C7FF"
#       )
#     ) +
#     theme_classic() +
#     ggtitle(paste0("Node Values per Iteration across\n", length(sim_state_vectors), " Simulations")) +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       legend.position = "top"
#     ) +
#     guides(color = guide_legend(override.aes = list(alpha = 1))) +
#     labs(
#       x = "Iter",
#       y = "Value",
#       color = NULL
#     )
# })
#
#
# test_that("fmcmcmr triangular sim works", {
#   #triangular_models <- build_fmcmcmr_models(test_adj_matrix, n_sims = 1000, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = test_adj_matrix)
#   mode_adj_matrix <- u_adj_matrix
#   mode_adj_matrix[mode_adj_matrix != 0] <- 0.7
#   triangular_models <- build_fmcmcmr_models(test_adj_matrix, n_sims = 1000, distribution = "triangular", lower_adj_matrix = l_adj_matrix, upper_adj_matrix = u_adj_matrix, mode_adj_matrix = mode_adj_matrix)
#   sim_models <- simulate_fmcmcmr_models(triangular_models, initial_state_vector = c(1, 0, 0), squashing = "tanh", lambda = 0.5, parallel = FALSE)
#
#   sim_state_vectors <- lapply(sim_models, function(model) model$state_vectors)
#   for (i in seq_along(sim_state_vectors)) {
#     sim_state_vectors[[i]] <- cbind(
#       iter = 1:nrow(sim_state_vectors[[i]]),
#       sim = i,
#       sim_state_vectors[[i]]
#     )
#   }
#
#   sim_state_vectors_df <- data.frame(do.call(rbind, sim_state_vectors))
#   sim_state_vectors_df_long <- tidyr::pivot_longer(sim_state_vectors_df, cols = 3:ncol(sim_state_vectors_df))
#   sim_state_vectors_df_long$sim_name <- paste0(sim_state_vectors_df$sim, "_", sim_state_vectors_df_long$name)
#
#   rows_per_sim <- unique(unlist(lapply(sim_state_vectors, function(sim) nrow(sim))))
#   iter_values_across_sims <- vector(mode = "list", length = rows_per_sim)
#   for (i in 1:rows_per_sim) {
#     iter_values_across_sims[[i]] <- do.call(rbind, lapply(sim_state_vectors, function(sim) sim[i, ]))
#   }
#   names(iter_values_across_sims) <- paste0("iter_", 1:rows_per_sim)
#
#   get_sim_quantiles_df <- function(state_vectors_node_iters, mean_quantiles = c(0.025, 0.975), sd_quantiles = c(0.16, 0.84)) {
#     n_draws <- 100
#     node_means <- lapply(state_vectors_node_iters, function(iter) {
#       mean_of_means <- vector(mode = "list", length = length(iter))
#       for (i in 1:length(iter)) {
#         mean_of_means[[i]] <- mean(sample(iter, n_draws, replace = TRUE))
#       }
#       return(do.call(c, mean_of_means))
#     })
#     mean_quantile_CIs <- do.call(rbind, lapply(node_means, function(iter) quantile(iter, c(mean_quantiles[1], mean_quantiles[2]))))
#     colnames(mean_quantile_CIs) <- c("lower_mean_CI", "upper_mean_CI")
#
#     sd_quantile_CIs <- do.call(rbind, lapply(state_vectors_node_iters, function(iter) quantile(iter, c(sd_quantiles[1], sd_quantiles[2]))))
#     colnames(sd_quantile_CIs) <- c("lower_sd_CI", "upper_sd_CI")
#
#     quantile_CIs <- data.frame(cbind(iter = 1:length(state_vectors_node_iters), mean_quantile_CIs, sd_quantile_CIs))
#
#     return(quantile_CIs)
#   }
#
#   B_iters <- lapply(iter_values_across_sims, function(iter) iter$B)
#   C_iters <- lapply(iter_values_across_sims, function(iter) iter$C)
#
#   B_quantile_CIs <- get_sim_quantiles_df(B_iters)
#   C_quantile_CIs <- get_sim_quantiles_df(C_iters)
#
#   data <- sim_state_vectors_df_long
#
#   ggplot() +
#     geom_line(data = data, aes(x = iter, y = value, group = sim_name, color = name), alpha = 0.01) +
#     geom_line(data = lower_bounds_df, aes(x = iter, y = value, group = name), color = "grey") +
#     geom_line(data = upper_bounds_df, aes(x = iter, y = value, group = name), color = "grey") +
#     geom_ribbon(data = B_quantile_CIs, aes(x = iter, ymin = lower_mean_CI, ymax = upper_mean_CI), fill = "#B0CBE7FF") +
#     geom_ribbon(data = B_quantile_CIs, aes(x = iter, ymin = lower_sd_CI, ymax = upper_sd_CI), fill = "#B0CBE7FF", alpha = 0.5) +
#     geom_ribbon(data = C_quantile_CIs, aes(x = iter, ymin = lower_mean_CI, ymax = upper_mean_CI), fill = "#FEF7C7FF") +
#     geom_ribbon(data = C_quantile_CIs, aes(x = iter, ymin = lower_sd_CI, ymax = upper_sd_CI), fill = "#FEF7C7FF", alpha = 0.5) +
#     scale_x_continuous(limits = c(0, 30), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
#     scale_color_manual(
#       values = c(
#         #A = "black", B = "#B0CBE7FF", C = "#FEF7C7FF", D = "#EBA07EFF"
#         A = "black", B = "#B0CBE7FF", C = "#FEF7C7FF"
#       )
#     ) +
#     theme_classic() +
#     ggtitle(paste0("Node Values per Iteration across\n", length(sim_state_vectors), " Simulations")) +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       legend.position = "top"
#     ) +
#     guides(color = guide_legend(override.aes = list(alpha = 1))) +
#     labs(
#       x = "Iter",
#       y = "Density",
#       color = NULL
#     )
# })
#
# #
# # B_1_sd <- do.call(rbind, lapply(B_iters, function(iter) quantile(iter, c(0.16, 0.84))))
# # B_1_sd <- data.frame(cbind(iter = 1:rows_per_sim, B_1_sd))
# # names(B_1_sd) <- c("iter", "lower", "upper")
# #
# # B_means <- lapply(B_iters, function(iter) {
# #   mean_of_means <- vector(mode = "list", length = length(iter))
# #   for (i in 1:length(iter)) {
# #     mean_of_means[[i]] <- mean(sample(iter, 100, replace = TRUE))
# #   }
# #   return(do.call(c, mean_of_means))
# # })
# #
# # B_means_CI <- do.call(rbind, lapply(B_means, function(iter) quantile(iter, c(0.025, 0.975))))
# # B_means_CI <- data.frame(cbind(iter = 1:rows_per_sim, B_means_CI))
# # names(B_means_CI) <- c("iter", "lower", "upper")
#
#
# # test_that("---- works", {
# #   test_adj_matrix_1 <- data.frame(
# #     "C1" = c(0, 0.36, 0.45, -0.90, 0),
# #     "C2" = c(-0.4, 0, 0, 0, 0.6),
# #     "C3" = c(-0.25, 0, 0, 0, 0),
# #     "C4" = c(0, 0, 0, 0, 0.3),
# #     "C5" = c(0.3, 0, 0, 0, 0)
# #   )
# #
# #   test_initial_state_vector_1 <- c(0.400, 0.707, 0.612, 0.717, 0.300)
# # })
# #
# #
# # test_that("simulate_fmcmcmr works", {
#   # test <- data.frame(
#   #   source = c("A", "B"),
#   #   target = c("B", "C"),
#   #   weight = c(0.4, 0.7),
#   #   sd = c(0.1, 0.3)
#   # )
#   #
#   # # t1 <- Sys.time()
#   # test_simulated_adj_matrices <- build_fmcmcmr_models(
#   #   get_adj_matrix_from_edgelist(test),
#   #   get_adj_matrix_from_edgelist(test,value_colname = "sd"),
#   #   n_sims = 100
#   # )
# #   t2 <- Sys.time()
# #   t2 - t1
# #
# #   t1 <- Sys.time()
# #   test_models <- simulate_fmcmcmr_models(
# #     test_simulated_adj_matrices, c(1, 0, 0), parallel = TRUE
# #   )
# #   t2 <- Sys.time()
# #   t2 - t1
# #
# #   t1 <- Sys.time()
# #   test_models <- simulate_fmcmcmr_models(
# #     test_simulated_adj_matrices, c(1, 0, 0), parallel = FALSE
# #   )
# #   t2 <- Sys.time()
# #   t2 - t1
# #
# #   test_models <- lapply(test_models, function(model) {
# #     model$state_vectors <- cbind(iter = 1:nrow(model$state_vector), model$state_vectors)
# #   })
# #
# #   x <- do.call(rbind, test_models)
# #   y <- pivot_longer(x, cols = 2:4)
# #
# #   ggplot(y) +
# #     geom_line(aes(x = iter, y = value, color = name))
# #
# #
# #
# # })
# #
# #
# # test_that("trying something for graphing", {
# #
# #   n_sims <- 1000
# #
# #   # edgelist <- data.frame(
# #   #   source = c("A", "B"),
# #   #   target = c("B", "C"),
# #   #   weight = c(0.4, 0.7),
# #   #   sd = c(0.1, 0.3)
# #   # )
# #   # adj_matrix <- get_adj_matrix_from_edgelist(edgelist)
# #
# #   # adj_matrix <- data.frame(
# #   #   "A" = c(0, 0.36, 0.45, -0.90, 0),
# #   #   "B" = c(-0.4, 0, 0, 0, 0.6),
# #   #   "C" = c(-0.25, 0, 0, 0, 0),
# #   #   "D" = c(0, 0, 0, 0, 0.3),
# #   #   "E" = c(0.3, 0, 0, 0, 0)
# #   # )
# #   # edgelist <- get_edgelist_from_adj_matrix(adj_matrix)
# #
# #   # A -(+)> B -(+)> C -(+)> D
#   edgelist <- data.frame(
#   source = c("A", "B", "C"),
#   target = c("B", "C", "D"),
#   weight = c(0.4, 0.7, 0.1)
#   )
# #   # A -(+)> B -(+)> C -(+)> D -(+)> E
# #   #edgelist <- data.frame(
# #   # source = c("A", "B", "C", "D"),
# #   # target = c("B", "C", "D", "E"),
# #   # weight = c(0.4, 0.7, 0.1, 0.2)
# #   #)
# #
# #   # A -(+)> B -(+)> A
# #   #edgelist <- data.frame(
# #   #  source = c("A", "B"),
# #   #  target = c("B", "A"),
# #   #  weight = c(0.4, 0.7)
# #   #)
# #
# #   edgelist <- data.frame(
# #     source = c("A", "B", "C"),
# #     target = c("B", "C", "B"),
# #     weight = c(0.1, 0.4, 0.6)
# #   )
# #
# #   adj_matrix <- get_adj_matrix_from_edgelist(edgelist)
# #
# #   uniform_distribution_of_weights_per_edge <- vector(mode = "list", length = nrow(edgelist))
# #   for (i in seq_along(uniform_distribution_of_weights_per_edge)) {
# #     uniform_distribution_of_weights_per_edge[[i]] <- runif(n_sims, min = -1, max = 1)
# #   }
# #   #uniform_distribution_of_weights_per_edge <- rep(list(runif(n_sims, min = -1, max = 1)), nrow(edgelist))
# #   possible_weights_per_sim <- do.call(cbind, uniform_distribution_of_weights_per_edge)
# #
# #   unweighted_edgelist <- edgelist[c("source", "target")]
# #
# #   sampled_weights_adj_matrices <- vector(mode = "list", length = n_sims)
# #   for (i in 1:n_sims) {
# #     sampled_weights_edgelist <- cbind(unweighted_edgelist, "weight" = possible_weights_per_sim[i, ])
# #     sampled_weights_adj_matrices[[i]] <- get_adj_matrix_from_edgelist(sampled_weights_edgelist)
# #   }
# #
# #   activation_vector <- c(1, 0, 0)
# #   #activation_vector <- c(1, 0, 0, 0)
# #   sampled_fcmr_sims <- pbapply::pblapply(sampled_weights_adj_matrices, function(fcm) simulate_fcmr(fcm, activation_vector, max_iter = 100, squashing = "tanh", lambda = 0.5)$state_vectors)
# #   names(sampled_fcmr_sims) <- 1:length(sampled_fcmr_sims)
# #
# #   sampled_fcmr_sims_results <- sampled_fcmr_sims
# #   for (i in seq_along(sampled_fcmr_sims_results)) {
# #     sampled_fcmr_sims_results[[i]] <- cbind("sim" = as.character(i), "iter" = as.numeric(0:(nrow(sampled_fcmr_sims_results[[i]]) - 1)), sampled_fcmr_sims_results[[i]])
# #   }
# #   sampled_fcmr_sims_results_df <- do.call(rbind, sampled_fcmr_sims_results)
# #   sampled_fcmr_sims_results_df_long <- tidyr::pivot_longer(sampled_fcmr_sims_results_df, cols = 3:ncol(sampled_fcmr_sims_results_df))
# #
# #   data <- sampled_fcmr_sims_results_df_long %>%
# #     #dplyr::filter(name == c("A", "B", "C", "D"))
# #     dplyr::filter(name == c("A", "B"))
# #   ggplot(data) +
# #     geom_line(aes(x = iter, y = value, group_by = sim, color = name), alpha = 0.2) +
# #     scale_x_continuous(limits = c(0, 30), expand = c(0, 0)) +
# #     scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
# #     scale_color_manual(
# #       values = c(
# #         #A = "black", B = "#B0CBE7FF", C = "#FEF7C7FF", D = "#EBA07EFF"
# #         A = "black", B = "#B0CBE7FF", C = "#FEF7C7FF"
# #       )
# #     ) +
# #     theme_classic() +
# #     ggtitle(paste0("Node Values per Iteration across\n", n_sims, " Simulations")) +
# #     theme(
# #       plot.title = element_text(hjust = 0.5),
# #       legend.position = "top"
# #     ) +
# #     guides(color = guide_legend(override.aes = list(alpha = 1))) +
# #     labs(
# #       x = "Iter",
# #       y = "Density",
# #       color = NULL
# #     )
# #
# #   max_weights_per_node_per_sim <- lapply(sampled_fcmr_sims, function(sim) apply(sim, 2, max))
# #   max_weights_per_node_per_sim <- data.frame(cbind("sim" = 1:n_sims, do.call(rbind, max_weights_per_node_per_sim)))
# #   max_weights_per_node_per_sim_longer <- tidyr::pivot_longer(max_weights_per_node_per_sim, cols = 2:ncol(max_weights_per_node_per_sim), values_to = "max")
# #
# #   min_weights_per_node_per_sim <- lapply(sampled_fcmr_sims, function(sim) apply(sim, 2, min))
# #   min_weights_per_node_per_sim <- data.frame(cbind("sim" = 1:n_sims, do.call(rbind, min_weights_per_node_per_sim)))
# #   min_weights_per_node_per_sim_longer <- tidyr::pivot_longer(min_weights_per_node_per_sim, cols = 2:ncol(min_weights_per_node_per_sim), values_to = "min")
# #
# #   min_max_weights_per_node_per_sim <- merge(max_weights_per_node_per_sim_longer, min_weights_per_node_per_sim_longer)
# #   min_max_weights_per_node_per_sim$peak <- ifelse(
# #     min_max_weights_per_node_per_sim$max > abs(min_max_weights_per_node_per_sim$min),
# #     min_max_weights_per_node_per_sim$max,
# #     min_max_weights_per_node_per_sim$min
# #   )
# #
# #   data <- min_max_weights_per_node_per_sim %>%
# #     dplyr::filter(name == c("B", "C"))
# #
# #   data <- max_weights_per_node_per_sim_longer %>%
# #     dplyr::filter(name == c("B", "C", "D"))
# #   ggplot(data) +
# #     geom_density(aes(x = peak, group = name, fill = name), alpha = 0.75) +
# #     scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
# #     scale_y_continuous(expand = c(0, 0)) +
# #     scale_fill_manual(
# #       values = c(
# #         #B = "#B0CBE7FF", C = "#FEF7C7FF", D = "#EBA07EFF"
# #         B = "#B0CBE7FF", C = "#FEF7C7FF"
# #       )
# #     ) +
# #     theme_classic() +
# #     ggtitle("PDF of Peak Values of Nodes in FCM Simulation") +
# #     theme(
# #       plot.title = element_text(hjust = 0.5),
# #       legend.position = "top"
# #     ) +
# #     labs(
# #       x = "Peak Value",
# #       y = "Density",
# #       fill = NULL
# #     )
# #
# #
# #
# #
# #   blank_weight_edgelist <- edgelist[c("source", "target")]
# #   simulated_edgelists <- rep(list(blank_weight_edgelist), n_sims)
# #   simulated_adj_matrices <- rep(data.frame(matrix()), n_sims)
# #
# #   for (i in seq_along(simulated_edgelists)) {
# #     simulated_edgelists[[i]]$weight <- possible_weights_per_sim[i, ]
# #     simulated_adj_matrices[[i]] <- get_adj_matrix_from_edgelist(simulated_edgelists[[i]])
# #   }
# #
# #   # simulated_adj_matrices <- lapply(simulated_edgelists, function(simulated_edgelist) {
# #   #  simulated_edgelist$weight <- lapply(edgelist$unif_dist, function(dist) sample(dist, 1))
# #   #  get_adj_matrix_from_edgelist(simulated_edgelist)
# #   #})
# #
# #   test_A <- lapply(simulated_adj_matrices, function(fcm) simulate_fcmr(fcm, c(1, 0, 0, 0), max_iter = 100, squashing = "tanh", lambda = 0.5)$state_vectors)
# #   test_A_peaks <- vector(mode = "list", length = n_sims)
# #   names(test_A) <- as.character(1:length(test_A))
# #   names(test_A_peaks) <- names(test_A)
# #   for (i in seq_along(test_A)) {
# #     test_A[[i]] <- cbind("sim" = as.character(i), "iter" = as.numeric(0:(nrow(test_A[[i]]) - 1)), test_A[[i]])
# #     #test_A[[i]] <- test_A[[i]][-1, ]
# #     test_A_peaks[[i]] <- apply(test_A[[i]], 2, max)
# #   }
# #   test_A <- do.call(rbind, test_A)
# #   test_A_long <- tidyr::pivot_longer(test_A, cols = 3:ncol(test_A))
# #
# #   test_A_peaks <- do.call(rbind, test_A_peaks)
# #
# #
# #   ggplot(data = test_A) +
# #     geom_line(aes(x = iter, y = A, group = sim), alpha = 0.1, color = "red") +
# #     geom_line(aes(x = iter, y = B, group = sim), alpha = 0.1, color = "blue") +
# #     geom_line(aes(x = iter, y = C, group = sim), alpha = 0.1, color = "black")
# #
# #   hist(test_A$E)
# #   plot(hist(test_A$A), xlim = c(0, 1))
# #   plot(hist(test_A$B), xlim = c(0, 1))
# #   plot(hist(test_A$C), xlim = c(0, 1))
# #
# #
# #   ggplot(test_A_long) +
# #     geom_line(aes(x = iter, y = value, group_by = sim, color = name), alpha = 0.1)
# #
# #     geom_density(aes(x = value, group = name, color = name))
# #
# #     geom_density(aes(y = name, group = name))
# #
# #     geom_area(aes(x = A)) +
# #
# #
# #
# #
# #   test_A_long <- tidyr::pivot_longer(test_A, cols = 3:ncol(test_A))
# #   data <- test_A %>%
# #     dplyr::select("sim", "iter", "A", "D", "E")
# #   data_long <- tidyr::pivot_longer(data, cols = 3:ncol(data))
# #
# #   ggplot(data = data) +
# #     geom_line(aes(x = iter, y = A, group = sim), alpha = 0.1, color = "red") +
# #     geom_line(aes(x = iter, y = B, group = sim), alpha = 0.1, color = "blue") +
# #     geom_line(aes(x = iter, y = C, group = sim), alpha = 0.1, color = "black")
# #
# #   ggplot(data = data_long, aes(color = name)) +
# #     geom_line(aes(x = iter, y = value, group_by = sim), alpha = 0.05)
# #
# #     geom_line(aes(x = iter, y = A, group = sim), alpha = 0.001, color = "red") +
# #     geom_line(aes(x = iter, y = B, group = sim), alpha = 0.001, color = "blue") +
# #     geom_line(aes(x = iter, y = C, group = sim), alpha = 0.001, color = "black")
# # })
#
# test_fun <- function(x, ...) {
#   list(...)
# }
#

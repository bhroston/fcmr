# test_that("multiplication works", {
#   lower_adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(-0.75, 0, 0, -0.75),
#     "C" = c(0, 0.25, 0, 0),
#     "D" = c(0, 0, 0.25, 0)
#   )
#
#   mode_adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(-0.5, 0, 0, -0.5),
#     "C" = c(0, 0.5, 0, 0),
#     "D" = c(0, 0, 0.5, 0)
#   )
#
#   upper_adj_matrix <- data.frame(
#     "A" = c(0, 0, 0, 0),
#     "B" = c(-0.25, 0, 0, -0.25),
#     "C" = c(0, 0.75, 0, 0),
#     "D" = c(0, 0, 0.75, 0)
#   )
#
#   activation_vector <- c(1, 1, 1, 1)
#   scenario_vector <- c(1, 0, 0, 0)
#   activation <- "kosko"
#   squashing <- "tanh"
#   lambda <- 0.76 # CHANGED THIS
#   max_iter <- 100
#   min_error <- 1e-5
#   lambda_optimization <- "none"
#   IDs <- c()
#
#   lower_fcm_confer <- confer_fcm(
#     adj_matrix = lower_adj_matrix,
#     activation_vector = activation_vector,
#     scenario_vector = scenario_vector,
#     activation = activation,
#     squashing = squashing,
#     lambda = 0.76, ###### CHANGED THIS
#     max_iter = max_iter,
#     min_error = min_error,
#     lambda_optimization = lambda_optimization,
#     IDs = IDs
#   )
#
#   upper_fcm_confer <- confer_fcm(
#     adj_matrix = upper_adj_matrix,
#     activation_vector = activation_vector,
#     scenario_vector = scenario_vector,
#     activation = activation,
#     squashing = squashing,
#     lambda = 0.76, ####### CHANGED THIS
#     max_iter = max_iter,
#     min_error = min_error,
#     lambda_optimization = lambda_optimization,
#     IDs = IDs
#   )
#
#   n_nodes <- length(lower_fcm_confer$inference)
#
#   nrow_lower_baseline <- nrow(lower_fcm_confer$baseline_simulation$state_vectors)
#   nrow_lower_scenario <- nrow(lower_fcm_confer$scenario_simulation$state_vectors)
#   nrow_upper_baseline <- nrow(upper_fcm_confer$baseline_simulation$state_vectors)
#   nrow_upper_scenario <- nrow(upper_fcm_confer$scenario_simulation$state_vectors)
#
#   inference_values <- vector(mode = "list", length = n_nodes)
#   for (i in 2:n_nodes) {
#     lower_baseline_final <- round(lower_fcm_confer$baseline_simulation$state_vectors[nrow_lower_scenario, i], 3)
#     lower_scenario_final <- round(lower_fcm_confer$scenario_simulation$state_vectors[nrow_lower_scenario, i], 3)
#     lower_inference <- lower_scenario_final - lower_baseline_final
#     upper_baseline_final <- round(upper_fcm_confer$baseline_simulation$state_vectors[nrow_upper_baseline, i], 3)
#     upper_scenario_final <- round(upper_fcm_confer$scenario_simulation$state_vectors[nrow_upper_scenario, i], 3)
#     upper_inference <- upper_scenario_final - upper_baseline_final
#     inference_values[[i]] <- grey_number(lower_inference, upper_inference)
#   }
#
#   test_sim_adj_matrices <- build_fmccm_models(mode_adj_matrix, n_sims = 10000, parallel = TRUE, distribution = "uniform", show_progress = TRUE, lower_adj_matrix = lower_adj_matrix, upper_adj_matrix = upper_adj_matrix)
#   test_confer_fmccm <- confer_fmccm(test_sim_adj_matrices,
#                activation_vector = activation_vector,
#                scenario_vector = scenario_vector,
#                activation = activation,
#                squashing = squashing,
#                lambda = 0.76, #### CHANGED THIS
#                max_iter = max_iter,
#                min_error = min_error,
#                IDs = IDs
#                )
#
#   # sim_1291 produces the max output for C
#   which(test_confer_fmccm$inferences$C == max(test_confer_fmccm$inferences$C))
#   z <- test_confer_fmccm$inference_state_vectors_by_sim$sim_1291[14, ]
#
#   max_adj_matrix <- test_sim_adj_matrices$sim_9816
#   #min_adj_matrix <- test_sim_adj_matrices$sim_7955
#   AB_edges <- unlist(lapply(test_sim_adj_matrices, function(x) x[1, 2]))
#   BC_edges <- unlist(lapply(test_sim_adj_matrices, function(x) x[2, 3]))
#   CD_edges <- unlist(lapply(test_sim_adj_matrices, function(x) x[3, 4]))
#   DB_edges <- unlist(lapply(test_sim_adj_matrices, function(x) x[4, 2]))
#
#   hist(AB_edges, xlim = c(-1, 1))
#   abline(v = max_adj_matrix[1, 2], col = "red")
#
#   hist(BC_edges, xlim = c(-1, 1))
#   abline(v = max_adj_matrix[2, 3], col = "red")
#
#   hist(CD_edges, xlim = c(-1, 1))
#   abline(v = max_adj_matrix[3, 4], col = "red")
#
#   hist(DB_edges, xlim = c(-1, 1))
#   abline(v = max_adj_matrix[4, 2], col = "red")
#
#   df <- test_confer_fmccm$inference_state_vectors_by_sim$sim_1696[12, ]
#
#   grey_bounds_df <- data.frame(
#     node = c("A", "B", "C", "D"),
#     lower = c(lower_fcm_confer$inference$A, lower_fcm_confer$inference$B, lower_fcm_confer$inference$C, lower_fcm_confer$inference$D),
#     upper = c(upper_fcm_confer$inference$A, upper_fcm_confer$inference$B, upper_fcm_confer$inference$C, upper_fcm_confer$inference$D)
#   )
#   grey_bounds_df_longer <- tidyr::pivot_longer(grey_bounds_df, cols = 2:3)
#
#   grey_bounds_df <- data.frame(
#     node = c("A", "B", "C", "D"),
#     lower = c(fgcm_test$inference$A[[1]]$lower, fgcm_test$inference$B[[1]]$lower, fgcm_test$inference$C[[1]]$lower, fgcm_test$inference$D[[1]]$lower),
#     upper = c(fgcm_test$inference$A[[1]]$upper, fgcm_test$inference$B[[1]]$upper, fgcm_test$inference$C[[1]]$upper, fgcm_test$inference$D[[1]]$upper)
#   )
#   grey_bounds_df_longer <- tidyr::pivot_longer(grey_bounds_df, cols = 2:3)
#
#   max_C_points_df <- data.frame(
#     node = c("A", "B", "C", "D"),
#     value = unlist(c(z[2], z[3], z[4], z[5]))
#   )
#
#
#  x <- test_confer_fmccm$inferences_for_plotting
#  ggplot() +
#    geom_jitter(data = x, aes(x = node, y = value), alpha = 1) +
#    geom_point(data = max_C_points_df, aes(x = node, y = value), color = "red") +
#    geom_errorbar(data = grey_bounds_df_longer, aes(x = node, y = value, ymin = value, ymax = value, group = name), color = "grey")
#    # geom_crossbar(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), color = "red", fill = "red") +
#    #geom_errorbar(data = bootstrap_mean_CIs_plotting, aes(x = node, ymin = value, ymax = value, group = name)) +
#    #geom_text(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025 - 0.05, label = round(lower_0.025, 2))) +
#    #geom_text(data = bootstrap_mean_CIs, aes(x = node, y = upper_0.975 + 0.05, label = round(upper_0.975, 2))) +
#    ylim(0, 1) +
#    theme_classic()
#
#  min_adj_matrix
#
#  grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_adj_matrix, upper_adj_matrix)
#
#  fgcm_test <- confer_fgcm(grey_adj_matrix, activation_vector, scenario_vector, activation, squashing, lambda, max_iter, min_error, algorithm = "salmeron")
#  fgcm_test$inference
#
#
#
#
#
# })

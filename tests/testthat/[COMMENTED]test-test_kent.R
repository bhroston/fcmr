# # test_that("multiplication works", {
# #   kent_model_csv_path <- "/Users/benro/Downloads/KentM_final.csv"
# #   kent_model_adj_matrix <- read.csv(kent_model_csv_path)
# #   kent_model_adj_matrix <- subset(kent_model_adj_matrix, select = -(X))
# #   kent_model_adj_matrix[is.na(kent_model_adj_matrix)] <- 0
# #   kent_model_adj_matrix[kent_model_adj_matrix != 0] <- as.numeric(kent_model_adj_matrix[kent_model_adj_matrix != 0])
# #
# #   lower_kent_model_adj_matrix <- kent_model_adj_matrix
# #   lower_kent_model_adj_matrix[lower_kent_model_adj_matrix != 0] <- lower_kent_model_adj_matrix[lower_kent_model_adj_matrix != 0] - 0.212
# #   lower_kent_model_adj_matrix[lower_kent_model_adj_matrix < -1] <- -1
# #
# #   upper_kent_model_adj_matrix <- kent_model_adj_matrix
# #   upper_kent_model_adj_matrix[upper_kent_model_adj_matrix != 0] <- upper_kent_model_adj_matrix[upper_kent_model_adj_matrix != 0] + 0.212
# #   upper_kent_model_adj_matrix[upper_kent_model_adj_matrix > 1] <- 1
# #
# #   activation_vector <- rep(1, 22)
# #   scenario_vector <- rep(0, 22)
# #   scenario_vector[3] <- 1
# #
# #   # kent_fmc_confer <- confer_fcm(adj_matrix = kent_model_adj_matrix, activation_vector = activation_vector, scenario_vector = scenario_vector,
# #   #                               activation = "kosko", squashing = "tanh", max_iter = 1000, lambda = 1)
# #   # sim_8_fcm_confer <- confer_fcm(adj_matrix = sim_uniform_kent_models[[8]], activation_vector = activation_vector, scenario_vector = scenario_vector,
# #   #                               activation = "kosko", squashing = "tanh", max_iter = 1000, lambda = 1)
# #
# #   # plot(base_fcm_confer$scenario_simulation$state_vectors$Salinization.of.the.Occoquan.Reservoir)
# #   # points(base_fcm_confer$baseline_simulation$state_vectors$Salinization.of.the.Occoquan.Reservoir)
# #   # lines(base_fcm_confer$scenario_simulation$state_vectors$Salinization.of.the.Occoquan.Reservoir - base_fcm_confer$baseline_simulation$state_vectors$Salinization.of.the.Occoquan.Reservoir)
# #
# #   test_grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(lower_kent_model_adj_matrix, upper_kent_model_adj_matrix)
# #   test_fgcm_confer <- confer_fgcm(test_grey_adj_matrix, activation_vector, scenario_vector,
# #                       "kosko", "tanh", max_iter = 100, lambda = 1)
# #
# #   sim_uniform_kent_models <- build_fmcm_models(
# #     adj_matrix = kent_model_adj_matrix, distribution = "uniform",  n_sims = 1000, parallel = FALSE, show_progress = TRUE,
# #     lower_adj_matrix = lower_kent_model_adj_matrix,
# #     upper_adj_matrix = upper_kent_model_adj_matrix
# #   )
# #
# #   kent_confer_fmcm <- confer_fmcm(sim_uniform_kent_models, activation_vector = activation_vector,
# #                scenario_vector = scenario_vector, squashing = "tanh", activation = "kosko",
# #                lambda_optimization = "none", parallel = TRUE, show_progress = TRUE)
# #
# #   x <- kent_confer_fmcm$inferences[, 1:10]
# #   x_longer <- tidyr::pivot_longer(x, cols = 1:10)
# #   z <- test_fgcm_confer$conferred_bounds[1:10,]
# #   z_longer <- tidyr::pivot_longer(z, cols = 2:3)
# #   ggplot() +
# #     #geom_jitter(data = x_longer, aes(x = name, y = value), alpha = 1) +
# #     geom_boxplot(data = x_longer, aes(x = name, y = value)) +
# #     geom_errorbar(data = z_longer, aes(x = node, y = value, ymin = value, ymax = value, group = name), color = "grey")
# #
# # })
# #
# #
# # # test_mean_of_means <- get_means_of_fmcm_inferences(kent_confer_fmcm$inferences, bootstrap_reps = 10000, bootstrap_samples_per_rep = 10000, parallel = TRUE, show_progress = TRUE)
# # #
# # # grey_bounds_df <- data.frame(
# # #   node = names(x),
# # #   lower = c(test_fgcm_confer$inference$Aquatic.life[[1]]$lower, test_fgcm_confer$inference$Chemicals.added.in.drinking.water.treatment[[1]]$lower,
# # #             test_fgcm_confer$inference$Consensus.building[[1]]$lower, test_fgcm_confer$inference$Deicers..private.[[1]]$lower),
# # #   upper = c(test_fgcm_confer$inference$Aquatic.life[[1]]$upper, test_fgcm_confer$inference$Chemicals.added.in.drinking.water.treatment[[1]]$upper,
# # #             test_fgcm_confer$inference$Consensus.building[[1]]$upper, test_fgcm_confer$inference$Deicers..private.[[1]]$upper)
# # # )
# # #
# # # grey_bounds_df <- data.frame(
# # #   node = colnames(test_fgcm_confer$inference[1:4]),
# # #   lower = c(test_fgcm_confer$inference$Salinization.of.the.Occoquan.Reservoir[[1]]$lower,
# # #             test_fgcm_confer$inference$Deicers..public.[[1]]$lower,
# # #             test_fgcm_confer$inference$Chemicals.added.in.drinking.water.treatment[[1]]$lower,
# # #             test_fgcm_confer$inference$Infrastructure.corrosion[[1]]$lower),
# # #   upper = c(test_fgcm_confer$inference$Salinization.of.the.Occoquan.Reservoir[[1]]$upper,
# # #             test_fgcm_confer$inference$Deicers..public.[[1]]$upper,
# # #             test_fgcm_confer$inference$Chemicals.added.in.drinking.water.treatment[[1]]$upper,
# # #             test_fgcm_confer$inference$Infrastructure.corrosion[[1]]$upper)
# # # )
# # # grey_bounds_df_longer <- tidyr::pivot_longer(grey_bounds_df, cols = 2:3)
# # #
# # # # min(kent_confer_fmcm$inferences$Aquatic.life)
# # # min(kent_confer_fmcm$inferences$Salinization.of.the.Occoquan.Reservoir) # 0.0204
# # # max(kent_confer_fmcm$inferences$Salinization.of.the.Occoquan.Reservoir) # 0.150
# # #
# # # x <- kent_confer_fmcm$inferences[,2:5]
# # # x_longer <- tidyr::pivot_longer(x, cols = 1:4)
# # # ggplot() +
# # #   geom_jitter(data = x_longer, aes(x = name, y = value), alpha = 1) +
# # #   #geom_point(data = max_C_points_df, aes(x = node, y = value), color = "red") +
# # #   geom_errorbar(data = grey_bounds_df_longer, aes(x = node, y = value, ymin = value, ymax = value, group = name), color = "grey")
# # #
# # # # Something's happening where the order of nodes is getting rearranged and so
# # # # the scenario vector is activating the wrong node...
# # #
# # # x <- kent_confer_fmcm$inferences_for_plotting
# # # ggplot() +
# # #   geom_jitter(data = x, aes(x = node, y = value), alpha = 0.025) +
# # #   # geom_crossbar(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), color = "red", fill = "red") +
# # #   geom_errorbar(data = bootstrap_mean_CIs_plotting, aes(x = node, ymin = value, ymax = value, group = name)) +
# # #   geom_text(data = bootstrap_mean_CIs, aes(x = node, y = lower_0.025 - 0.05, label = round(lower_0.025, 2))) +
# # #   geom_text(data = bootstrap_mean_CIs, aes(x = node, y = upper_0.975 + 0.05, label = round(upper_0.975, 2))) +
# # #   ylim(0, 1) +
# # #   theme_classic()
# # #
# # # x <- data.frame(
# # #   Salinization.of.the.Occoquan.Reservoir = unlist(lapply(test, function(x) x$inference$Salinization.of.the.Occoquan.Reservoir)),
# # #   Deicers..public. = unlist(lapply(test, function(x) x$inference$Deicers..public)),
# # #   Chemicals.added.in.drinking.water.treatment = unlist(lapply(test, function(x) x$inference$Chemicals.added.in.drinking.water.treatment)),
# # #   Infrastructure.corrosion = unlist(lapply(test, function(x) x$inference$Infrastructure.corrosion))
# # # )
# # # x_longer <- tidyr::pivot_longer(x, cols = 1:4)
# # # grey_bounds_df <- data.frame(
# # #   node = colnames(test_fgcm_confer$inference)[1:4],
# # #   lower = c(test_fgcm_confer$scenario_simulation$state_vectors_bounds$lower$Salinization.of.the.Occoquan.Reservoir[49],
# # #             test_fgcm_confer$scenario_simulation$state_vectors_bounds$lower$Deicers..public.[49],
# # #             test_fgcm_confer$scenario_simulation$state_vectors_bounds$lower$Chemicals.added.in.drinking.water.treatment[49],
# # #             test_fgcm_confer$scenario_simulation$state_vectors_bounds$lower$Infrastructure.corrosion[49]),
# # #   upper = c(test_fgcm_confer$scenario_simulation$state_vectors_bounds$upper$Salinization.of.the.Occoquan.Reservoir[49],
# # #             test_fgcm_confer$scenario_simulation$state_vectors_bounds$upper$Deicers..public.[49],
# # #             test_fgcm_confer$scenario_simulation$state_vectors_bounds$upper$Chemicals.added.in.drinking.water.treatment[49],
# # #             test_fgcm_confer$scenario_simulation$state_vectors_bounds$upper$Infrastructure.corrosion[49])
# # # )
# # # grey_bounds_df_longer <- tidyr::pivot_longer(grey_bounds_df, cols = 2:3)
# # # ggplot() +
# # #   geom_jitter(data = x_longer, aes(x = name, y = value), alpha = 0.025) +
# # #   geom_errorbar(data = grey_bounds_df_longer, aes(x = node, y = value, ymin = value, ymax = value, group = name), color = "grey")
#
#
# cluster_1_fcms <- readRDS("/Users/benro/Library/CloudStorage/OneDrive-VirginiaTech/Academics/Research/Projects/GCR/Papers/Dissertation Papers/fcmconfr_Paper/testing_for_lauren/RDS Objects/cluster_1_fcms.rds")
# cluster_2_fcms <- readRDS("/Users/benro/Library/CloudStorage/OneDrive-VirginiaTech/Academics/Research/Projects/GCR/Papers/Dissertation Papers/fcmconfr_Paper/testing_for_lauren/RDS Objects/cluster_2_fcms.rds")
# cluster_3_fcms <- readRDS("/Users/benro/Library/CloudStorage/OneDrive-VirginiaTech/Academics/Research/Projects/GCR/Papers/Dissertation Papers/fcmconfr_Paper/testing_for_lauren/RDS Objects/cluster_3_fcms.rds")
#
# initialVector <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
# clampingVector <- c(0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1)
#
#
# cluster_1_fcmconfr <- fcmconfr(
#   adj_matrices = cluster_1_fcms,
#   # Monte Carlo Sampling
#   num_mc_fcms = 100,
#   # Simulation
#   initial_state_vector = initialVector,
#   clamping_vector = clampingVector,
#   activation = 'rescale',
#   squashing = 'sigmoid',
#   lambda = 1,
#   point_of_inference = 'final',
#   max_iter = 100,
#   min_error = 1e-05,
#   # Inference Estimation (bootstrap)
#   ci_centering_function = 'mean',
#   confidence_interval = 0.95,
#   num_ci_bootstraps = 1000,
#   # Runtime Options
#   show_progress = TRUE,
#   parallel = TRUE,
#   n_cores = 10,
#   # Additional Options
#   run_agg_calcs = FALSE,
#   run_mc_calcs = TRUE,
#   run_ci_calcs = TRUE,
#   include_zeroes_in_sampling = FALSE,
#   include_sims_in_output = FALSE
# )
#
# cluster_2_fcmconfr <- fcmconfr(
#   adj_matrices = cluster_2_fcms,
#   # Monte Carlo Sampling
#   num_mc_fcms = 1000,
#   # Simulation
#   initial_state_vector = initialVector,
#   clamping_vector = clampingVector,
#   activation = 'rescale',
#   squashing = 'sigmoid',
#   lambda = 1,
#   point_of_inference = 'final',
#   max_iter = 100,
#   min_error = 1e-05,
#   # Inference Estimation (bootstrap)
#   ci_centering_function = 'mean',
#   confidence_interval = 0.95,
#   num_ci_bootstraps = 1000,
#   # Runtime Options
#   show_progress = TRUE,
#   parallel = TRUE,
#   n_cores = 2,
#   # Additional Options
#   run_agg_calcs = FALSE,
#   run_mc_calcs = TRUE,
#   run_ci_calcs = TRUE,
#   include_zeroes_in_sampling = FALSE,
#   include_sims_in_output = FALSE
# )
#
# cluster_3_fcmconfr <- fcmconfr(
#   adj_matrices = cluster_3_fcms,
#   # Monte Carlo Sampling
#   num_mc_fcms = 1000,
#   # Simulation
#   initial_state_vector = initialVector,
#   clamping_vector = clampingVector,
#   activation = 'rescale',
#   squashing = 'sigmoid',
#   lambda = 1,
#   point_of_inference = 'final',
#   max_iter = 100,
#   min_error = 1e-05,
#   # Inference Estimation (bootstrap)
#   ci_centering_function = 'mean',
#   confidence_interval = 0.95,
#   num_ci_bootstraps = 1000,
#   # Runtime Options
#   show_progress = TRUE,
#   parallel = TRUE,
#   n_cores = 2,
#   # Additional Options
#   run_agg_calcs = FALSE,
#   run_mc_calcs = TRUE,
#   run_ci_calcs = TRUE,
#   include_zeroes_in_sampling = FALSE,
#   include_sims_in_output = FALSE
# )
#
# # Get a list of monte carlo adj. matrices from multiple fcmconfr outputs
# # Notice the fcmconfr outputs are grouped in a call to list(), NOT c()
# all_clusters_mc_fcms <- do.call(c, lapply(
#   list(cluster_1_fcmconfr, cluster_2_fcmconfr, cluster_3_fcmconfr),
#   function(fcmconfr_obj) {
#     fcmconfr_obj$mc_adj_matrices
#   }
# ))
#
# all_clusters_mc_fcms_fcmconfr <- fcmconfr(
#   adj_matrices = all_clusters_mc_fcms,
#   # Monte Carlo Sampling
#   num_mc_fcms = 6000, # This needs to be AT LEAST DOUBLE length(all_clusters_mc_fcms)
#   # Simulation
#   initial_state_vector = initialVector,
#   clamping_vector = clampingVector,
#   activation = 'rescale',
#   squashing = 'sigmoid',
#   lambda = 1,
#   point_of_inference = 'final',
#   max_iter = 100,
#   min_error = 1e-05,
#   # Inference Estimation (bootstrap)
#   ci_centering_function = 'median',
#   confidence_interval = 0.95,
#   num_ci_bootstraps = 1000,
#   # Runtime Options
#   show_progress = TRUE,
#   parallel = TRUE,
#   n_cores = 10,
#   # Additional Options
#   run_agg_calcs = FALSE,
#   run_mc_calcs = TRUE,
#   run_ci_calcs = TRUE,
#   include_zeroes_in_sampling = FALSE,
#   include_sims_in_output = FALSE
# )

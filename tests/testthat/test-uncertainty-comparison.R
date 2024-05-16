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
# adj_matrix_lower <- get_adj_matrix_from_edgelist(edgelist_lower)
# adj_matrix_upper <- get_adj_matrix_from_edgelist(edgelist_upper)
# grey_adj_matrix <- get_grey_adj_matrix_from_lower_and_upper_adj_matrices(adj_matrix_lower, adj_matrix_upper)
#
# sim <- simulate_fgcmr(grey_adj_matrix, initial_state_vector = list(1, 0, 0), squashing = "tanh", lambda = 0.5)
#
# get_var_states <- function(var_states) {
#   data.frame(
#     iter = 1:length(var_states),
#     lower_state = unlist(lapply(var_states, function(grey_num) grey_num$lower)),
#     upper_state = unlist(lapply(var_states, function(grey_num) grey_num$upper))
#   )
# }
#
# A_states <- get_var_states(sim$state_vectors$C1)
# B_states <- get_var_states(sim$state_vectors$C2)
# C_states <- get_var_states(sim$state_vectors$C3)
#
# B_states$mode <- (B_states$lower_state + B_states$upper_state)/2
# B_quantiles <- vector(mode = "list", length = nrow(B_states))
#
# B_limits <- data.frame(lowest = max(B_states$lower_state), highest = max(B_states$upper_state))
# B_dist <- triangle_inv_cdf(1000, lower = B_limits$lowest, upper = B_limits$highest, mode = (B_limits$lowest + B_limits$highest)/2)
# #B_dist_CI <- quantile(B_dist, c(0.025, 0.975))
# B_dist_CI <- quantile(B_dist, c(0.16, 0.84))
#
# B_quantiles <- vector(mode = "list", length = nrow(B_states))
# B_means <- vector(mode = "list", length = nrow(B_states))
# for (i in 1:nrow(B_states)) {
#   triangle_dist_at_iter <- triangle_inv_cdf(100, B_states$lower_state[i], B_states$upper_state[i], B_states$mode[i])
#   B_quantiles[[i]] <- quantile(triangle_dist_at_iter, c(0.16, 0.84))
#
#   B_means_dist <- vector(mode = "numeric", length = nrow(B_states))
#   for (j in seq_along(B_means_dist)) {
#     B_means_dist[j] <- mean(sample(triangle_dist_at_iter, size = 100, replace = TRUE))
#   }
#   B_means[[i]] <- quantile(B_means_dist, c(0.025, 0.975))
# }
#
# B_quantiles <- do.call(rbind, B_quantiles)
# B_means <- do.call(rbind, B_means)
# colnames(B_means) <- c("mean_lower", "mean_upper")
# colnames(B_quantiles) <- c("Q_lower", "Q_upper")
#
# B_states <- cbind(B_states, B_means, B_quantiles)
#
#
#
#
#
# ggplot_theming <- theme_classic() +
#   ggtitle(paste0("Grey Node Values per Iteration")) +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "top"
#   ) +
#   guides(color = guide_legend(override.aes = list(alpha = 1))) +
#   labs(
#     x = "Iter",
#     y = "Value",
#     color = NULL
#   )
#
# # A = "black", B = "#B0CBE7FF", C = "#FEF7C7FF"
# ggplot() +
#   geom_line(data = A_states, aes(x = iter, y = lower_state)) +
#   geom_line(data = A_states, aes(x = iter, y = upper_state)) +
#   geom_line(data = B_states, aes(x = iter, y = lower_state)) +
#   geom_line(data = B_states, aes(x = iter, y = upper_state)) +
#   geom_ribbon(data = B_states, aes(x = iter, ymin = lower_state, ymax = upper_state), fill = "#B0CBE7FF", alpha = 0.3) +
#   geom_ribbon(data = B_states, aes(x = iter, ymin = mean_lower, ymax = mean_upper), fill = "#B0CBE7FF", alpha = 1)+
#   geom_ribbon(data = B_states, aes(x = iter, ymin = Q_lower, ymax = Q_upper), fill = "#B0CBE7FF", alpha = 0.5) +
#   geom_line(data = C_states, aes(x = iter, y = lower_state)) +
#   geom_line(data = C_states, aes(x = iter, y = upper_state)) +
#   geom_ribbon(data = C_states, aes(x = iter, ymin = lower_state, ymax = upper_state), fill = "#FEF7C7FF", alpha = 0.5) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic() +
#   ggtitle(paste0("Grey Node Values per Iteration")) +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "top"
#   ) +
#   guides(color = guide_legend(override.aes = list(alpha = 1))) +
#   labs(
#     x = "Iter",
#     y = "Value",
#     color = NULL
#   )
#
# B_limits <- data.frame(lowest = max(B_states$lower_state), highest = max(B_states$upper_state))
# hist(runif(1000, min = B_limits$lowest, B_limits$highest))
# hist(rtri(1000, min = B_limits$lowest, max = B_limits$highest, mode = (B_limits$lowest + B_limits$highest)/2))
# B_dist_CI <- quantile(B_dist, c(0.16, 0.84))
#
#
# B_dist <- rtri(1000, min = B_limits$lowest, max = B_limits$highest, mode = (B_limits$lowest + B_limits$highest)/2)
# #B_dist_CI <- quantile(B_dist, c(0.025, 0.975))
# B_dist_CI <- quantile(B_dist, c(0.16, 0.84))
#
#
# triangle_inv_cdf <- function(n = 100, lower, upper, mode = c()) {
#   if (identical(mode, c())) {
#     mode <- (lower + upper)/2
#   }
#   inv_cdf <- vector(mode = "numeric", length = n)
#   for (i in 1:n) {
#     x <- i/n
#     if (x <= mode) {
#       inv_cdf[i] <- sqrt(x*(upper - lower)*(mode - lower)) + lower
#     } else if (x > mode) {
#       inv_cdf[i] <- upper - sqrt((-x + 1)*(upper - lower)*(upper - mode))
#     } else {
#       stop("Unknown input")
#     }
#   }
#   return(inv_cdf)
# }
#
# triangle_sd <- function(lower, upper, mode = c()) {
#   if (identical(mode, c())) {
#     mode <- (lower + upper)/2
#   }
#   variance <- (lower^2 + upper^2 + mode^2 - lower*upper - lower*mode - mode*upper)/18
#   return(sqrt(variance))
# }
#
# # trinorm(100, B_limits$lowest, B_limits$highest)
#
# # rtriangle <- function(n = 100, lower, upper, mode = c()) {
# #   if (identical(mode, c())) {
# #     mode <- (lower + upper)/2
# #   }
# #   pdf <- vector(mode = "numeric", length = n)
# #   for (i in seq_along(pdf)) {
# #     x <- i/n
# #     if (x < lower) {
# #       pdf[i] <- 0
# #     } else if (x >= lower & x < mode) {
# #       pdf[i] <- (2*(x - lower))/((upper - lower)*(mode - lower))
# #     } else if (x == mode) {
# #       pdf[i] <- 2/(upper - lower)
# #     } else if (x > mode & x <= upper) {
# #       pdf[i] <- (2*(upper - x))/((upper - lower)*(upper - mode))
# #     } else if (x > upper) {
# #       pdf[i] <- 0
# #     } else {
# #       stop("Unknown input")
# #     }
# #   }
# #   pdf <- pdf/sum(pdf)
# #   return(pdf)
# # }
# #
# triangle_cdf <- function(n = 100, lower, upper, mode = c()) {
#   if (identical(mode, c())) {
#     mode <- (lower + upper)/2
#   }
#   cdf <- vector(mode = "numeric", length = n)
#   for (i in 1:n) {
#     x <- i/n
#     if (x < lower) {
#       cdf[i] <- 0
#     } else if (x >= lower & x < mode) {
#       cdf[i] <- ((x - lower)^2)/((upper - lower)*(mode - lower))
#     } else if (x == mode) {
#       cdf[i] <- ((x - lower)^2)/((upper - lower)*(mode - lower))
#     } else if (x > mode & x <= upper) {
#       cdf[i] <- 1 - ((upper - x)^2)/((upper - lower)*(upper - mode))
#     } else if (x > upper) {
#       cdf[i] <- 1
#     } else {
#       stop("Unknown input")
#     }
#   }
#   return(cdf)
# }
# #
# # # triangle_cdf_value <- function(x, lower, upper, mode = c()) {
# # #   if (identical(mode, c())) {
# # #     mode <- (lower + upper)/2
# # #   }
# # #   if (x <= mode) {
# # #     p <- ((x - lower)^2)/((upper - lower)*(mode - lower))
# # #   } else if (x > mode) {
# # #     p <- 1 - ((upper - x)^2)/((upper - lower)*(upper - mode))
# # #   } else if (x == mode) {
# # #     p <- ((x - lower)^2)/((upper - lower)*(mode - lower))
# # #   } else if (x > mode & x <= upper) {
# # #     p <- 1 - ((upper - x)^2)/((upper - lower)*(upper - mode))
# # #   } else if (x > upper) {
# # #     p <- 1
# # #   } else {
# # #     stop("Unknown input")
# # #   }
# # #   return(p)
# # # }
# #
# #
# #
# #
# # test <- trinorm(10000, lower = 0.2, upper = 0.8)
# # test_cumsum <- cumsum(test)
# # samples <- runif(1000, 0, 1)
# # test[which(sort(c(test_cumsum, samples[1])) == samples[1]) + 1]
# #
# # for (i in seq_along(samples)) {
# #   samples[i] <- test[which(sort(c(test_cumsum, samples[i])) == samples[i]) + 1]
# # }
# #
# #
# #
# # # test[sample(1:100, 100, replace = TRUE)]
# #
# #
# # lower_to_mode_area_fraction <- mode - lower
# # upper_to_mode_area_fraction <- upper - mode
# # height <- 1/(0.5*(mode - lower) + 0.5*(upper - mode))
# #
# # lower_to_mode_slope <- height/(mode - lower)

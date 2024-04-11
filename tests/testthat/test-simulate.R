# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
#
# test_that("fgcm_simulation works", {
#   test_adj_matrix_values <- c(
#     grey_number(0.5, 0.7),
#     grey_number(-0.52, -0.32),
#     grey_number(0.7, 0.9),
#     grey_number(0.6, 0.8),
#     grey_number(-0.9, -0.7),
#     grey_number(0.28, 0.48),
#     grey_number(0.51, 1.0),
#     grey_number(0.13, 0.43),
#     grey_number(-0.16, 0.34),
#     grey_number(0.2, 0.4),
#     grey_number(0.3, 0.5),
#     grey_number(0.43, 0.63),
#     grey_number(0.35, 0.85)
#   )
#
#   test_adj_matrix_locs <- c(
#     "2-5", "5-2", "4-2", "2-4", "4-1", "1-4", "3-1",
#     "1-3", "4-7", "7-4", "6-3", "6-8", "8-6"
#   )
#
#   test_grey_adj_matrix <- get_grey_adj_matrix_from_list_of_grey_numbers(
#     values = test_adj_matrix_values,
#     locs = test_adj_matrix_locs
#   )
#
#   test_initial_state_vector <- c(
#     grey_number(0.48, 0.48),
#     grey_number(0.57, 0.57),
#     grey_number(0.58, 0.58),
#     grey_number(0.68, 0.68),
#     grey_number(0.58, 0.58),
#     grey_number(0.59, 0.59),
#     grey_number(0.52, 0.52),
#     grey_number(0.58, 0.58)
#   )
#
#   test <- simulate_fgcm(test_grey_adj_matrix, test_initial_state_vector)
#
#   ggplotify_test <- lapply(test, function(x) data.frame(do.call(rbind, x)))
#   ggplotify_test <- mapply(function(x, y) cbind(x, "concept" = paste0("C", 1:nrow(x)),  "iter" = rep(y, nrow(x))), x = ggplotify_test, y = 0:(length(ggplotify_test) - 1), SIMPLIFY = FALSE)
#   ggplotify_test <- do.call(rbind, ggplotify_test)
#
#   # ggplot() +
#   #  geom_line(data = ggplotify_test_C1, aes(x = unlist(iter), y = unlist(lower))) +
#   #  geom_line(data = ggplotify_test_C1, aes(x = unlist(iter), y = unlist(upper))) +
#   #  scale_y_continuous(limits = c(0, 1))
#
#   # plot(ggplotify_test_C1$iter, ggplotify_test_C1$lower, type = "l", yli = c(0, 1), xlim = c(0, iter))
#   # lines(ggplotify_test_C1$iter, ggplotify_test_C1$upper, type = "l")
#   # polygon(c(ggplotify_test_C1$iter, rev(ggplotify_test_C1$iter)),
#   #        c(ggplotify_test_C1$lower, rev(ggplotify_test_C1$upper)),
#   #        col = "#6BD7AF")
#
# })

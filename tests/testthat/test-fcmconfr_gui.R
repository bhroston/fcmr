# # test_adj_matrix <- data.frame(
# #   "A" = rep(0, 26),
# #   "B" = rep(0, 26),
# #   "C" = rep(0, 26),
# #   "D" = rep(0, 26),
# #   "E" = rep(0, 26),
# #   "F" = rep(0, 26),
# #   "G" = rep(0, 26),
# #   "H" = rep(0, 26),
# #   "I" = rep(0, 26),
# #   "J" = rep(0, 26),
# #   "K" = rep(0, 26),
# #   "L" = rep(0, 26),
# #   "M" = rep(0, 26),
# #   "N" = rep(0, 26),
# #   "O" = rep(0, 26),
# #   "P" = rep(0, 26),
# #   "Q" = rep(0, 26),
# #   "R" = rep(0, 26),
# #   "S" = rep(0, 26),
# #   "T" = rep(0, 26),
# #   "U" = rep(0, 26),
# #   "V" = rep(0, 26),
# #   "W" = rep(0, 26),
# #   "X" = rep(0, 26),
# #   "Y" = rep(0, 26),
# #   "Z" = rep(0, 26)
# # )
# #
# # test_adj_matrix_1 <- data.frame(
# #   "A" = c(0, 0, 0, 0),
# #   "B" = c(1, 0, 0, 1),
# #   "C" = c(0, 1, 0, 0),
# #   "D" = c(0, 0, 1, 0)
# # )
# # test_adj_matrix_2 <- data.frame(
# #   "A" = c(0, 0, 0, 0),
# #   "B" = c(0.25, 0, 0, 0.25),
# #   "C" = c(0, 0.25, 0, 0),
# #   "D" = c(0, 0, 0.25, 0)
# # )
# # test_adj_matrix_3 <- data.frame(
# #   "A" = c(0, 0, 0, 0),
# #   "B" = c(0.75, 0, 0, 0.75),
# #   "C" = c(0, 0.75, 0, 0),
# #   "D" = c(0, 0, 0.75, 0)
# # )
# # test_adj_matrix_4 <- data.frame(
# #   "A" = c(0, 0, 0, 0),
# #   "B" = c(0.5, 0, 0, 0.5),
# #   "C" = c(0, 0.5, 0, 0),
# #   "D" = c(0, 0, 0.5, 0)
# # )
# # test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3, test_adj_matrix_4)
# #
# # shinytest2::load_app_env(renv = rlang::global_env(), globalrenv = rlang::global_env())
# #
# # shinytest2::record_test("inst/shiny")
# #
# # library(shinytest2)
# #
# #
# #
# #
# # app <- AppDriver$new(app_dir = "inst/shiny")
# # app$set_inputs(test_adj_matrices = test_fcms)
# # app$set_inputs(activation = "modified-kosko")
# #
# # app$get_value(input = "adj_matrices")
# #
# # app$get_values()
#
# test_dir(
#   "tests/testthat",
#   shiny::loadSupport(appDir = "inst/shiny"),
#   reporter = c("progress", "fail")
# )
#
#
#

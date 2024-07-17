
test_that("fcmconfr works", {
  test_adj_matrix_1 <- adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  test_adj_matrix_2 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.25, 0, 0, 0.25),
    "C" = c(0, 0.25, 0, 0),
    "D" = c(0, 0, 0.25, 0)
  )
  test_adj_matrix_3 <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(0.75, 0, 0, 0.75),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, 0.75, 0)
  )
  test_fcms <- list(test_adj_matrix_1, test_adj_matrix_2, test_adj_matrix_3)

  test_fcmconfr <- fcmconfr(
    test_fcms, "nonparametric", samples = 1000,
    initial_state_vector <- c(1, 1, 1, 1), clamping_vector = c(1, 0, 0, 0),
    activation = "kosko", squashing = "sigmoid", lambda = 1, max_iter = 100, min_error = 1e-5
  )
  test_fcmconfr <- fcmconfr(
    test_fcms, "uniform", samples = 1000,
    initial_state_vector <- c(1, 1, 1, 1), clamping_vector = c(1, 0, 0, 0),
    activation = "kosko", squashing = "sigmoid", lambda = 1, max_iter = 100, min_error = 1e-5
  )
  test_fcmconfr <- fcmconfr(
    test_fcms, "triangular", samples = 1000,
    initial_state_vector <- c(1, 1, 1, 1), clamping_vector = c(1, 0, 0, 0),
    activation = "kosko", squashing = "sigmoid", lambda = 1, max_iter = 100, min_error = 1e-5
  )
})

test_that("infer_fcm works", {
  adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(1, 0, 0, 1),
    "C" = c(0, 1, 0, 0),
    "D" = c(0, 0, 1, 0)
  )
  initial_state_vector <- c(1, 1, 1, 1)
  clamping_vector <- c(1, 0, 0, 0)
  squashing = "tanh"
  lambda = 1
  max_iter = 10000
  min_error = 1e-5
  lambda_optimization = "koutsellis"
  IDs = c()

  test_confer <- infer_fcm(adj_matrix, initial_state_vector, clamping_vector, activation = "kosko",
             squashing = "tanh", lambda = 1, max_iter = 10000)

  inference_vals <- round(test_confer$inference, 1)

  # Return same results as mentalmodeler
  expect_equal(unlist(inference_vals), unlist(data.frame(A = 1.0, B = 0.9, C = 0.7, D = 0.6)))
  expect_equal(colnames(test_confer$inference_state_vectors), c("iter", "A", "B", "C", "D"))

  # x <- tidyr::pivot_longer(test_confer$inference, cols = 1:4)
  # x <- test_confer$inference_for_plotting[test_confer$inference_for_plotting$node != "A", ]
  # ggplot(x) + geom_col(aes(x = node, y = value), fill = "red") +
  #  ylim(0, 1) +
  #  theme_classic()

  # p <- barplot(height = x$value, names.arg = x$name, col = "red")
  # text(x = p, y = x$value + 0.05, labels = round(x$value, 1))

  # Test with negative edge weights
  adj_matrix <- data.frame(
    "A" = c(0, 0, 0, 0),
    "B" = c(-0.25, 0, 0, -0.25),
    "C" = c(0, 0.75, 0, 0),
    "D" = c(0, 0, -0.25, 0)
  )
  initial_state_vector <- c(1, 1, 1, 1)
  clamping_vector <- c(0, 1, 0, 0)
  squashing = "tanh"
  lambda = 1
  max_iter = 1000
  min_error = 1e-5
  lambda_optimization = "koutsellis"
  IDs = c()

  test_confer <- infer_fcm(adj_matrix, initial_state_vector, clamping_vector, activation = "kosko",
                            squashing = "tanh", lambda = 1, max_iter = 1000)

  # plot(test_confer$scenario_simulation$state_vectors$C, ylim = c(-1, 1))
  # points(test_confer$baseline_simulation$state_vectors$C)
})


test_that("warning pops up if max_iter reached", {
  test_adj_matrix_1 <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  test_initial_state_vector_1 <- c(0.400, 0.707, 0.612, 0.717, 0.300)

  expect_warning(simulate_fcm(adj_matrix = test_adj_matrix_1, initial_state_vector = test_initial_state_vector_1, clamping_vector = c(0, 0, 0, 0, 0),
                             activation = "modified-kosko", squashing = "sigmoid", lambda = 1, max_iter = 10))
})


test_that("simulate_fcm works", {
  # Test from Stylios & Groumpos, 2000 - J. Intell. Fuzzy Syst. Vol. 8 No. 1 pp.83-98
  # Title: Fuzzy Cognitive Maps in modeling supervisory control systems (no doi available)
  # Confirmed to reproduce results
  test_adj_matrix_1 <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  test_initial_state_vector_1 <- c(0.400, 0.707, 0.612, 0.717, 0.300)

  test_fcm_1 <- simulate_fcm(adj_matrix = test_adj_matrix_1, initial_state_vector = test_initial_state_vector_1, clamping_vector = c(0, 0, 0, 0, 0),
                activation = "modified-kosko", squashing = "sigmoid", lambda = 1, max_iter = 100)
  expect_error(simulate_fcm(adj_matrix = test_adj_matrix_1, initial_state_vector = test_initial_state_vector_1, clamping_vector = c(0, 0, 0, 0, 0),
                             activation = "rescale", squashing = "tanh", lambda = 1, max_iter = 100))
  expect_no_error(simulate_fcm(adj_matrix = test_adj_matrix_1, initial_state_vector = test_initial_state_vector_1, clamping_vector = c(0, 0, 0, 0, 0),
                            activation = "rescale", squashing = "sigmoid", lambda = 1, max_iter = 100))

  test_fcm_1_state_vectors <- test_fcm_1$state_vectors
  final_state_fcm_1 <- round(test_fcm_1_state_vectors[nrow(test_fcm_1_state_vectors), ], digits = 3)
  rownames(final_state_fcm_1) <- NULL
  expect_identical(final_state_fcm_1, data.frame("iter" = 10, "C1" = 0.625, "C2" = 0.708, "C3" = 0.612, "C4" = 0.717, "C5" = 0.711))

  # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
  # Not confirmed to reproduce their results
  # test_adj_matrix_2 <- data.frame(
  #   "C1" = c(0, 0, 0.755, -0.8, 0, 0, 0, 0),
  #   "C2" = c(0, 0, 0, 0.8, 0.6, 0, 0, 0),
  #   "C3" = c(0.28, 0, 0, 0, 0, 0.4, 0, 0),
  #   "C4" = c(0.38, 0.7, 0, 0, 0, 0, 0.3, 0),
  #   "C5" = c(0, -0.42, 0, 0, 0, 0, 0, 0),
  #   "C6" = c(0, 0, 0, 0, 0, 0, 0, 0.6),
  #   "C7" = c(0, 0, 0, 0.09, 0, 0, 0, 0),
  #   "C8" = c(0, 0, 0, 0, 0, 0.53, 0, 0)
  # )
  # test_initial_state_vector_2 <- c(0.48, 0.57, 0.58, 0.68, 0.59, 0.58, 0.59, 0.52)
  # test_fcm_2 <- simulate_fcm(adj_matrix = test_adj_matrix_2, initial_state_vector = test_initial_state_vector_2,
  #                              activation = "modified-kosko", squashing = "sigmoid", lambda = 1, max_iter = 10)
  # test_fcm_2$state_vectors

  # Test from Salmeron & Papageorgiou, 2014 - https://doi.org/10.1007/s10489-013-0511-z
  # Not confirmed to reproduce their results
  # test_adj_matrix_3 <- data.frame(
  #   "C1" = c(0, 0, 0.51, 1.0, 0, 0, 0, 0),
  #   "C2" = c(0, 0, 0, 0.7, 0.5, 0, 0, 0),
  #   "C3" = c(0.13, 0, 0, 0, 0, 0.3, 0, 0),
  #   "C4" = c(0.28, 0.6, 0, 0, 0, 0, 0.2, 0),
  #   "C5" = c(0, -0.52, 0, 0, 0, 0, 0, 0),
  #   "C6" = c(0, 0, 0, 0, 0, 0, 0, 0.35),
  #   "C7" = c(0, 0, 0, -0.16, 0, 0, 0, 0),
  #   "C8" = c(0, 0, 0, 0, 0, 0.43, 0, 0)
  # )
  # test_initial_state_vector_3 <- c(0.48, 0.57, 0.58, 0.68, 0.59, 0.58, 0.59, 0.52)
  # test_fcm_3 <- simulate_fcm(adj_matrix = test_adj_matrix_3, initial_state_vector = test_initial_state_vector_3,
  #                              activation = "modified-kosko", squashing = "sigmoid", lambda = 1, max_iter = 10)
  # test_fcm_3$state_vectors
})

#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph as_adjacency_matrix
test_that("optimize_fcm_lambda works", {
  # Test from Koutsellis et al. 2022 - https://doi.org/10.1007/s12351-022-00717-x
  # Confirmed to reproduce results
  test_source_nodes <- c(
    "C1", "C1", "C1", "C2", "C2", "C3", "C3", "C3", "C4", "C4",
    "C4", "C5", "C6", "C7", "C8", "C8", "C9", "C10", "C10", "C11",
    "C12", "C13", "C14", "C14", "C15", "C15", "C15", "C15", "C16", "C16",
    "C17", "C17", "C18", "C19", "C20", "C20", "C21", "C22", "C23", "C23",
    "C24", "C25", "C28", "C29"
  )
  test_target_nodes <- c(
    "C15", "C20", "C23", "C12", "C25", "C14", "C16", "C23", "C15", "C20",
    "C23", "C16", "C15", "C15", "C10", "C12", "C23", "C11", "C13", "C26",
    "C14", "C28", "C13", "C23", "C17", "C18", "C24", "C30", "C15", "C28",
    "C21", "C22", "C19", "C20", "C26", "C28", "C27", "C29", "C19", "C24",
    "C29", "C15", "C27", "C27"
  )
  test_edge_weights <- c(
    -0.442, -0.375, -0.348, 0.549, 0.706, -0.421, -0.132, -0.245, -0.52, 0.261,
    -0.29, 0.651, 0.487, 0.229, 0.776, 0.319, 0.792, 0.319, 0.391, -0.402,
    -0.381, 0.481, 0.097, 0.11, -0.852, 0.902, 0.226, -0.852, -0.719, -0.274,
    0.722, 0.888, -0.521, -0.311, -0.932, -0.419, 0.322, 0.21, 0.9, 0.189,
    0.196, -0.311, 0.481, 0.378
  )

  edgelist <- data.frame(
    "source" = test_source_nodes,
    "target" = test_target_nodes,
    "weight" = test_edge_weights
  )

  test_adj_matrix <- get_adj_matrix_from_edgelist(edgelist)

  # Reproduce paper results (Koutsellis et al)
  expect_null(optimize_fcm_lambda(test_adj_matrix, squashing = "tanh", method = "none"))
  expect_equal(0.927, optimize_fcm_lambda(test_adj_matrix, squashing = "sigmoid", method = "koutsellis"))
  expect_equal(0.421, optimize_fcm_lambda(test_adj_matrix, squashing = "tanh", method = "koutsellis"))
  expect_error(optimize_fcm_lambda(test_adj_matrix, squashing = "tanh", method = "asdcads"))
})


test_that("fcm works", {
  # Test from Stylios & Groumpos, 2000 - J. Intell. Fuzzy Syst. Vol. 8 No. 1 pp.83-98
  # Title: Fuzzy Cognitive Maps in modeling supervisory control systems (no doi available)
  # Confirmed to reproduce results
  test_adj_matrix <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  test_fcm <- fcm(test_adj_matrix)

  expect_identical(test_fcm$adj_matrix, test_adj_matrix)
  expect_identical(test_fcm$edgelist, get_edgelist_from_adj_matrix(test_adj_matrix))
  expect_identical(test_fcm$concepts, colnames(test_adj_matrix))
  expect_identical(class(test_fcm), "fcm")
})

test_that("fcm catches datatype errors", {
  test_adj_matrix <- data.frame(
    "C1" = c("a", 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )
  expect_error(fcm(test_adj_matrix))
})


test_that("get_edgelist_from_adj_matrix works", {
  test_adj_matrix <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )

  goal_edgelist <- data.frame(
    "source" = c("C2", "C3", "C4", "C1", "C5", "C1", "C5", "C1"),
    "target" = c("C1", "C1", "C1", "C2", "C2", "C3", "C4", "C5"),
    "weight" = c(0.36, 0.45, -0.90, -0.40, 0.60, -0.25, 0.30, 0.30)
  )

  expect_identical(get_edgelist_from_adj_matrix(test_adj_matrix), goal_edgelist)
})


test_that("aggregate_fcm works", {
  test_adj_matrix <- data.frame(
    "C1" = c(0, 0.36, 0.45, -0.90, 0),
    "C2" = c(-0.4, 0, 0, 0, 0.6),
    "C3" = c(-0.25, 0, 0, 0, 0),
    "C4" = c(0, 0, 0, 0, 0.3),
    "C5" = c(0.3, 0, 0, 0, 0)
  )
  adj_matrix_list <- list(test_adj_matrix, test_adj_matrix*0.5, test_adj_matrix*0.2)
  expect_no_error(aggregate_fcm(adj_matrix_list, "mean"))
  expect_no_error(aggregate_fcm(adj_matrix_list, "median"))

  adj_matrix_list[[1]]$C1[[2]] <- 0
  expect_no_error(aggregate_fcm(adj_matrix_list, "mean", FALSE))


  # # fcmconfr is a package developed by Ben Roston (author)
  # # install via remotes::install_git("https://github.com/bhroston/fcmr.git", ref = "18-add-fcm-aggregation-functions")
  #
  # indFCMFilepath <- "/Users/benro/Library/CloudStorage/OneDrive-VirginiaTech/Academics/Research/Projects/GCR/Papers/Dissertation Papers/FCM Structural Analysis/raw_data/raw_stakeholder_data/T1_individual_adj_data.xlsx"
  #
  # fileSheets <- readxl::excel_sheets(indFCMFilepath)
  # adj_matrices <- lapply(fileSheets, function(sheet) readxl::read_excel(indFCMFilepath, sheet = sheet))
  # names(adj_matrices) <- fileSheets
  # complete_agg <- fcmconfr::aggregate_fcm(adj_matrices, aggregation_fun = "mean")
  # complete_agg_mat <- complete_agg$adj_matrix
  #
  # known_complete_agg_filepath <- "/Users/benro/Desktop/FCM_Cycle_Analysis_Projects/cycle-partition-analyisis/agg_complete_adj_matrix.csv"
  # known_complete_agg <- read.csv(known_complete_agg_filepath)
  # colnames(known_complete_agg) <- known_complete_agg[1, ]
  # known_complete_agg <- known_complete_agg[,-1]
  #
  # which(complete_agg_mat == 0.668, arr.ind = TRUE)
  # which(known_complete_agg == 0.668, arr.ind = TRUE)
  #
  # aggregate_consensus_filepath <- "/Users/benro/Library/CloudStorage/OneDrive-VirginiaTech/Academics/Research/Projects/GCR/Papers/Dissertation Papers/FCM Structural Analysis/raw_data/raw_group_and_agg_data/T1_group_and_agg_adj_data.xlsx"
  #
})

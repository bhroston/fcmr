


test <- data.frame(
  source = c("A", "B"),
  target = c("B", "C"),
  weight = c(0.4, 0.7),
  sd = c(0.1, 0.3)
)

#simulated_adj_matrices <- build_fmcmcmr_models(get_adj_matrix_from_edgelist(test), get_adj_matrix_from_edgelist(test, value_colname = "sd"))


simulate_fmcmcmr_models <- function(simulated_adj_matrices = list(matrix()),
                                    initial_state_vector = c(),
                                    activation = "modified-kosko",
                                    squashing = "sigmoid",
                                    lambda = 1,
                                    max_iter = 10,
                                    min_error = 1e-5,
                                    lambda_optimization = "none",
                                    IDs = c(),
                                    parallel = TRUE) {
  if (parallel == TRUE) {
    # num_cores <- parallel::detectCores()
    # cluster <- parallel::makeCluster(num_cores)
    # doFuture::registerDoFuture()
    #
    # doParallel::registerDoParallel(cluster)
    # registerDoParallel(makeCluster(no_cores))
    #
    # `%dopar%` <- foreach::`%dopar%`
    # functions_to_export <- c("simulate_fcmr", "confirm_adj_matrix_is_square",
    #                          )

    `%dofuture%` <- doFuture::`%dofuture%`
    future::plan(future::multisession())

    env <- rlang::child_env("base")
    env$simulate_fcmr <- rlang::set_env(simulate_fcmr, env)
    env$confirm_adj_matrix_is_square <- rlang::set_env(confirm_adj_matrix_is_square, env)
    env$confirm_initial_state_vector_is_compatible_with_adj_matrix <- rlang::set_env(confirm_initial_state_vector_is_compatible_with_adj_matrix, env)
    env$get_node_IDs_from_input <- rlang::set_env(get_node_IDs_from_input, env)
    env$optimize_fcmr_lambda <- rlang::set_env(optimize_fcmr_lambda, env)
    env$calculate_next_fcm_state_vector <- rlang::set_env(calculate_next_fcm_state_vector, env)
    env$squash <- rlang::set_env(squash, env)

    fmcmcmr_simulation_results <- foreach::foreach(
      simulated_adj_matrix = simulated_adj_matrices,
      options.future = list(env = env)
    ) %dofuture% {
      env$simulate_fcmr(
        adj_matrix = simulated_adj_matrix,
        initial_state_vector = initial_state_vector,
        activation = activation,
        squashing = squashing,
        lambda = lambda,
        max_iter = max_iter,
        min_error = min_error,
        lambda_optimization = lambda_optimization,
        IDs = IDs
      )
    }
  } else {
    fmcmcmr_simulation_results <- lapply(
      simulated_adj_matrices,
      function(simulated_adj_matrix) {
        simulate_fcmr(
          adj_matrix = simulated_adj_matrix,
          initial_state_vector = initial_state_vector,
          activation = activation,
          squashing = squashing,
          lambda = lambda,
          max_iter = max_iter,
          min_error = min_error,
          lambda_optimization = lambda_optimization,
          IDs = IDs
        )
      }
    )
  }

  fmcmcmr_simulation_results
}



#
#     env$get_adj_matrix_from_edgelist <- rlang::set_env(get_adj_matrix_from_edgelist, env)
#     furrr::future_map(
#       test, get_adj_matrix_from_edgelist, .env_globals = env
#     )
#
#
#
#     simulate_fcmr <- simulate_fcmr
#     confirm_adj_matrix_is_square <- confirm_adj_matrix_is_square
#     confirm_initial_state_vector_is_compatible_with_adj_matrix <- confirm_initial_state_vector_is_compatible_with_adj_matrix
#     get_node_IDs_from_input <- get_node_IDs_from_input
#     optimize_fcmr_lambda <- optimize_fcmr_lambda
#     calculate_next_fcm_state_vector <- calculate_next_fcm_state_vector
#     squash <- squash
#
#     future::FutureGlobals(globals = list(
#       TRUE,
#       add = list(
#         simulate_fcmr <- simulate_fcmr,
#         confirm_adj_matrix_is_square <- confirm_adj_matrix_is_square,
#         confirm_initial_state_vector_is_compatible_with_adj_matrix <- confirm_initial_state_vector_is_compatible_with_adj_matrix,
#         get_node_IDs_from_input <- get_node_IDs_from_input,
#         optimize_fcmr_lambda <- optimize_fcmr_lambda,
#         calculate_next_fcm_state_vector <- calculate_next_fcm_state_vector,
#         squash <- squash
#       )
#     ))
#
#
#     env <- rlang::child_env("base")
#     env$get_adj_matrix_from_edgelist <- rlang::set_env(get_adj_matrix_from_edgelist, env)
#     env$test <- test
#
#
#     future_env <- rlang::env(get_adj_matrix_from_edgelis = get_adj_matrix_from_edgelist)
#
#     furrr::future_map(test, ls(), .env_globals = env)
#
#     fut <- future::future({simulate_fcmr(sim_adj_matrix)}, globals = list(sim_adj_matrix = simulated_adj_matrices))
#     fut <- future::future({get_adj_matrix_from_edgelist(test)}, envir = env)
#     future::value(fut)
#
#     foreach::foreach(
#       #simulated_adj_matrix = simulated_adj_matrices
#       test,
#       options.future = list(globals = structure(list(TRUE, add = list("get_adj_matrix_from_edgelist"))))
#     ) %dofuture% {
#       env$get_adj_matrix_from_edgelist(test)
#       #simulate_fcmr(simulated_adj_matrix, initial_state_vector = c(1, 0, 0))
#     }
#
#     foreach::foreach(
#       simulated_adj_matrix = simulated_adj_matrices,
#       options.future = list(env = env)
#     ) %dofuture% {
#       env$simulate_fcmr(simulated_adj_matrix, c(1, 0, 0))
#     }
#
#     env$squash <- rlang::set_env(squash, env)
#
#      foreach::foreach(
#       #simulated_adj_matrix = simulated_adj_matrices
#        1:5,
#        options.future = list(env = env)
#     ) %dofuture% {
#       env$squash(1)
#       #env$get_adj_matrix_from_edgelist(test)
#       #simulate_fcmr(simulated_adj_matrix, initial_state_vector = c(1, 0, 0))
#     }
#
#     get_adj_matrix_from_edgelist = get_adj_matrix_from_edgelist
#     simulate_fcmr <- simulate_fcmr
#
#     future::FutureGlobals()
#     future::FutureGlobals(globals = list(TRUE, add = list(get_adj_matrix_from_edgelist = get_adj_matrix_from_edgelist)))
#     fut <- future::future({get_adj_matrix_from_edgelist()}, packages = NULL)
#     fut <- future::future({1})
#     future::value(fut)






build_fmcmcmr_models <- function(adj_matrix = matrix(),
                                 sd_adj_matrix = matrix(),
                                 IDs = c(),
                                 n_sims = 100,
                                 parallel = TRUE) {

  # Cleaner way to validate inputs than re-writing the validation tests here
  fmcmcmr_data <- fmcmcmr(adj_matrix, sd_adj_matrix, IDs)
  adj_matrix <- fmcmcmr_data$adj_matrix
  sd_adj_matrix <- fmcmcmr_data$sd_adj_matrix
  edgelist <- fmcmcmr_data$edgelist

  edgelist$beta_dist <- mapply(
    function(mu, sd) get_beta_distribution_of_values(mu, sd, n_sims),
    mu = edgelist$weight, sd = edgelist$sd,
    SIMPLIFY = FALSE
  )

  lapply(edgelist$beta_dist, function(dist) sample(dist, 1))

  blank_weight_edgelist <- edgelist[c("source", "target")]
  simulated_edgelists <- rep(list(blank_weight_edgelist), n_sims)

  simulated_adj_matrices <- lapply(simulated_edgelists, function(simulated_edgelist) {
    simulated_edgelist$weight <- lapply(edgelist$beta_dist, function(dist) sample(dist, 1))
    get_adj_matrix_from_edgelist(simulated_edgelist)
  })

  simulated_adj_matrices
}



#' fmcmcmr (fuzzy monte carlo markov chain cognitive map) S3 class
#'
#' @description
#' This class is an organization scheme for fuzzy cognitive maps that model
#' uncertainty using monte carlo markov chain simulations (See
#' Koasidis et al. 2022 - https://dx.doi.org/10.2139/ssrn.4233987 or
#' Koutsellis et al. 2023 - https://doi.org/10.1016/j.softx.2023.101513 for
#' an application in python.
#'
#' ###It stores the nodes of an FCM and its corresponding adjacency matrix
#' and edgelist.
#'
#' @details
#' fmcmcmr stores fmcmcm data in forms useful to data manipulation, particular
#' regarding pairing with popular network analysis libraries like igraph
#' or visNetwork.
#'
#' Use vignette("fmcmcmr-class") for more information.
#'
#' @param adj_matrix An n x n adjacency matrix that represents the edge weights
#' of an FCM
#' @param sd_adj_matrix An n x n adjacency matrix that represents the standard
#' deviations of the edge weights listed in the adjacecny matrix (adj_matrix)
#' @param IDs A list of names for each node (must have n items)
#'
#' @export
fmcmcmr <- function(adj_matrix = matrix(), sd_adj_matrix = matrix(), IDs = c()) {
  # Validate input
  confirm_adj_matrix_is_square(adj_matrix)
  confirm_adj_matrix_is_square(sd_adj_matrix)
  IDs <- get_node_IDs_from_input(adj_matrix, IDs)

  adj_matrix_data_types <- unique(vapply(adj_matrix, class, character(1)))
  only_numeric_data_types_in_adj_matrix <- identical(adj_matrix_data_types, "numeric")
  if (!only_numeric_data_types_in_adj_matrix) {
    stop("Input adjacency matrix (adj_matrix) must only contain numeric objects, and all
         objects must be numeric")
  }

  sd_adj_matrix_data_types <- unique(vapply(sd_adj_matrix, class, character(1)))
  only_numeric_data_types_in_sd_adj_matrix <- identical(sd_adj_matrix_data_types, "numeric")
  if (!only_numeric_data_types_in_sd_adj_matrix) {
    stop("Input standard deviation adjacency matrix (sd_adj_matrix) must only contain numeric objects, and all
         objects must be numeric")
  }

  if (!identical(unique(dim(adj_matrix)), NA)) {
    adj_matrix_edgelist <- get_edgelist_from_adj_matrix(adj_matrix)
    colnames(adj_matrix_edgelist) <- c("source", "target", "weight")
  } else {
    adj_matrix_edgelist <- matrix()
  }
  if (!identical(unique(dim(sd_adj_matrix)), NA)) {
    sd_adj_matrix_edgelist <- get_edgelist_from_adj_matrix(sd_adj_matrix)
    colnames(sd_adj_matrix_edgelist) <- c("source", "target", "sd")
  } else {
    sd_adj_matrix_edgelist <- matrix()
  }
  edgelist <- merge(adj_matrix_edgelist, sd_adj_matrix_edgelist, all = TRUE)

  structure(
    .Data = list(
      concepts = IDs,
      adj_matrix = adj_matrix,
      sd_adj_matrix = sd_adj_matrix,
      edgelist = edgelist
    ),
    class = "fmcmcmr"
  )
}



#' get_beta_distribution_of_values
#'
#' @description
#' This pulls n samples from a beta distribution described by shape parameters
#' defined by a mean and standard deviation
#'
#' @details
#'
#' Use vignette("fcmcmrr-class") for more information.
#'
#' @param mu mean of the population distribution
#' @param sd standard deviation of the population distribution
#'
#' @export
get_beta_distribution_of_values <- function(mu = double(), sd = double(), n = 1000) {
  a <- ((mu*(1 - mu))/(sd^2)) - mu
  b <- a/mu - a
  values_distribution <- rbeta(n, shape1 = a, shape2 = b)

  values_distribution
}


# get_beta_distribution_of_values_given_CIs <- function(mu = double(), CI_lower = double(), CI_upper = double(), n = 1000) {
#   #uess_sd_from_lower <- ((CI_lower*qchisq(p = 0.05, df = n - 1))/(sqrt(n - 1)))^(1/2)
#   #guess_sd_from_upper <- ((CI_upper*qchisq(p = 0.05, df = n - 1))/(sqrt(n - 1)))^(1/2)
#   #avg_guess_sd <- (guess_sd_from_lower + guess_sd_from_upper)/2
#   #guess_sd <- 1/(1 + exp(-avg_guess_sd))
#
#   guess_sd <- 0.13
#   guess_a <- ((mu*(1 - mu))/(guess_sd^2)) - mu
#   guess_b <- (((1 - mu)^2)*mu)/guess_sd^2
#
#   calculated_CI_lower <- qbeta(0.025, guess_a, guess_b)
#   calculated_CI_upper <- qbeta(0.975, guess_a, guess_b)
#
#   x <- rbeta(100000, guess_a, guess_b)
#   plot(hist(x))
#
#   calculated_CI_lower <- quantile(x, 0.025)
#   calculated_CI_upper <- quantile(x, 0.975)
#   calculated_sd <- sd(x)
#
#   # Find: sd for CI_upper and CI_lower
#   CI_lower_error <- abs(CI_lower - calculated_CI_lower)
#   CI_upper_error <- abs(CI_upper - calculated_CI_upper)
#
#   CI_range <- CI_upper - CI_lower
#   calculated_CI_range <- calculated_CI_upper - calculated_CI_lower
#   CI_range_error <- CI_range - calculated_CI_range
#
#   guess_sd <- guess_sd + 0.5*CI_range_error
#
#   guess_CI_lower <- (CI_lower + calculated_CI_lower)/2
#   guess_CI_upper <- (CI_upper + calculated_CI_upper)/2
#
#
#
#
#   guess_sd <- 0.25
#
#   sd_unknown <- TRUE
#   while (sd_unknown) {
#     a <- ((mu*(1 - mu))/(guess_sd^2)) - mu
#     b <- a/mu - a
#     calc_CI_upper <- qbeta(0.975, a, b)
#     calc_CI_lower <- qbeta(0.025, a, b)
#
#     values_distribution <- rbeta(n, shape1 = a, shape2 = b)
#     actual_CI_upper <- quantile(values_distribution, 0.95)
#
#     actual_sd <- sd(values_distribution)
#     error <- (actual_sd - guess_sd)^2
#     if (error > 1e5) {
#       guess_sd <- (actual_sd + guess_sd)/2
#     } else {
#       sd_value <- actual_sd
#       sd_unknown <- FALSE
#     }
#   }
#
#
#   values_distribution
# }

# If mean = 0.9 and could be 0.5 or 1
# Find a beta distribution whose 95% CIs are 0.5 and 1





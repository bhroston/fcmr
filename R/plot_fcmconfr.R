
#' Get fcmconfr Object Plot Data
#' @export
get_plot_data <- function(fcmconfr_object, filter_limit = 10e-3) {

  # browser()

  fcm_clamping_vector <- fcmconfr_object$params$simulation_opts$clamping_vector
  fcm_nodes <- unique(lapply(fcmconfr_object$params$adj_matrices, colnames))[[1]]
  clamped_nodes <- fcm_nodes[fcm_clamping_vector != 0]

  input_inference_averages <- colSums(fcmconfr_object$inferences$input_fcms$inferences[2:ncol(fcmconfr_object$inferences$input_fcms$inferences)])
  nodes_of_interest_indexes <- which(input_inference_averages > filter_limit)
  nodes_of_interest <- names(nodes_of_interest_indexes)

  input_inferences <- as.data.frame(fcmconfr_object$inferences$input_fcms$inferences)
  aggregate_inferences <- as.data.frame(fcmconfr_object$inferences$aggregate_fcm$inferences)
  mc_inferences <- as.data.frame(fcmconfr_object$inferences$monte_carlo_fcms$all_inferences)

  input_inferences <- fcmconfr_object$inferences$input_fcms$inferences[, c(1, nodes_of_interest_indexes + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
  aggregate_inferences <- data.frame(
    aggregate_inferences[, nodes_of_interest_indexes]
  )
  names(aggregate_inferences) <- nodes_of_interest

  mc_inferences <- data.frame(
    mc_inferences[, nodes_of_interest_indexes]
  )
  colnames(mc_inferences) <- nodes_of_interest

  if (fcmconfr_object$fcm_class == "conventional") {
    fcm_class_subtitle <- "Conventional FCMs"
    input_inferences_longer <- tidyr::pivot_longer(input_inferences, cols = 2:ncol(input_inferences))
    aggregate_inferences_longer <- tidyr::pivot_longer(aggregate_inferences, cols = 1:ncol(aggregate_inferences))
    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences, cols = 1:ncol(mc_inferences))

    if (any(abs(input_inferences_longer$value) > 1) | any(abs(aggregate_inferences_longer$value) > 1) | any(abs(mc_inferences_longer$value) > 1)) {
      warning("Some inferences have a magnitude greater than 1 which suggests that
              the simulations did not converge, and will likely output unclear and/or
              illogical results.. Either increase the max. number of iterations
              (max_iter) or decrease lambda for improved results.")
    }

    max_y <- max(max(input_inferences_longer$value), max(mc_inferences_longer$value), max(aggregate_inferences_longer$value))
    max_y <- (ceiling(max_y*1000))/1000
    min_y <- min(min(input_inferences_longer$value), min(mc_inferences_longer$value), min(aggregate_inferences_longer$value))
    min_y <- (floor(min_y*1000))/1000

  } else if (fcmconfr_object$fcm_class == "ivfn") {
    fcm_class_subtitle <- "IVFN FCM"
    # aggregate_inferences_longer <- tidyr::pivot_longer(fcmconfr_object$inferences$aggregate_fcm$inferences, cols = c(2, ncol(fcmconfr_object$inferences$aggregate_fcm$inferences)))
    aggregate_inferences_longer <- fcmconfr_object$inferences$aggregate_fcm$inferences
    colnames(aggregate_inferences_longer) <- c("name", "crisp", "lower", "upper")
    mc_inferences_longer <- tidyr::pivot_longer(fcmconfr_object$inferences$monte_carlo_fcms$all_inferences, cols = c(1:ncol(fcmconfr_object$inferences$monte_carlo_fcms$all_inferences)))

    aggregate_inferences_longer <- aggregate_inferences_longer[!(aggregate_inferences_longer$name %in% clamped_nodes), ]
    # aggregate_inferences_longer <- aggregate_inferences_longer[aggregate_inferences_longer$value >= 0.001, ]
    mc_inferences_longer <- mc_inferences_longer[!(mc_inferences_longer$name %in% aggregate_inferences_longer$name), ]

    max_y <- max(max(mc_inferences_longer$value), max(aggregate_inferences_longer$lower))
    max_y <- (ceiling(max_y*1000))/1000
    min_y <- min(min(mc_inferences_longer$value), min(aggregate_inferences_longer$lower))
    min_y <- (floor(min_y*1000))/1000
  } else if (fcmconfr_object$fcm_class == "tfn") {
    fcm_class_subtitle <- "TFN FCM"
  }

  input_inferences_longer$analysis_source <- "input"
  aggregate_inferences_longer$analysis_source <- "aggregate"
  mc_inferences_longer$analysis_source <- "mc_inferences"
  # mc_CIs$analysis_source <- "mc"

  # aggregate_inferences_longer <- tidyr::pivot_longer(fcmconfr_object$inferences$aggregate_fcm$inferences, cols = 1:ncol(fcmconfr_object$inferences$aggregate_fcm$inferences))

  # mc_CIs <- fcmconfr_object$inferences$monte_carlo_fcms$bootstrap$CIs_by_node


  # Remove clamped_nodes from plot data frames
  # aggregate_inferences_longer <- aggregate_inferences_longer[!(aggregate_inferences_longer$name %in% clamped_nodes), ]
  # mc_CIs <- mc_CIs[!(mc_CIs$node %in% clamped_nodes), ]
  #
  # aggregate_inferences_longer <- aggregate_inferences_longer[aggregate_inferences_longer$value >= 0.001, ]
  # mc_CIs <- mc_CIs[mc_CIs$node %in% aggregate_inferences_longer$name, ]
  #
  # max_y <- max(max(mc_CIs$upper_0.975), max(aggregate_inferences_longer$value))
  # max_y <- (ceiling(max_y*1000)/1000)
  # min_y <- min(min(mc_CIs$lower_0.025), min(aggregate_inferences_longer$value))
  # min_y <- (floor(min_y*1000)/1000)

  # aggregate_inferences_longer <- aggregate_inferences_longer[!(aggregate_inferences_longer$name %in% clamped_nodes), ]
  # # aggregate_inferences_longer <- aggregate_inferences_longer[aggregate_inferences_longer$value >= 0.001, ]
  # mc_inferences_longer <- mc_inferences_longer[!(mc_inferences_longer$name %in% aggregate_inferences_longer$name), ]

  # browser()



  list(
    fcm_class_subtitle = fcm_class_subtitle,
    clamped_nodes,
    input_inferences = input_inferences_longer,
    aggregate_inferences = aggregate_inferences_longer,
    mc_inferences = mc_inferences_longer,
    # mc_CIs = mc_CIs,
    max_activation = max_y,
    min_activation = min_y
  )
}

#' Autoplot fcmconfr
#' @importFrom ggplot2 autoplot
#' @export
autoplot.fcmconfr <- function(fcmconfr_object,
                              coord_flip = TRUE,
                              inferences_in_plot = c("input", "aggregate", "mc"),
                              aggregate_fill = "#f7e3dd",
                              aggregate_color = "#E1D1CB",
                              aggregate_alpha = 0.7,
                              mc_fill = "#f7e3dd",
                              ...) {

  plot_data <- get_plot_data(fcmconfr_object)

  browser()

  plot_data$mc_inferences$value <- plot_data$mc_inferences$value/2

  if (fcmconfr_object$fcm_class == "conventional") {
    ggplot() +
      ggplot2::geom_boxplot(
        data = plot_data$mc_inferences,
        aes(x = .data$name, y = .data$value), outlier.shape = NA, width = 0.7,
      ) +
      ggplot2::geom_jitter(
        data = plot_data$mc_inferences,
        aes(x = .data$name, y = .data$value), size = 0.5, alpha = 0.05, width = 0.3, shape = 8,
      ) +
      # ggplot2::geom_jitter(
      #   data = plot_data$input_inferences,
      #   aes(x = .data$name, y = .data$value), alpha = 1, shape = 15, size = 3, width = 0,
      # ) +
      ggplot2::geom_point(
        data = plot_data$aggregate_inferences,
        aes(x = .data$name, y = .data$value), shape = 17, color = "red", size = 3,
      ) +
      default_theme() +
      coord_flip()

    # ggplot_main <- ggplot() +
    #   geom_col(
    #     data = plot_data$aggregate_inferences,
    #     aes(x = .data$name, y = .data$value,
    #         fill = .data$analysis_source, color = .data$analysis_source),
    #     color = aggregate_color, alpha = aggregate_alpha,
    #     width = 0.8
    #   ) +
    #   geom_errorbar(
    #     data = plot_data$mc_CIs,
    #     aes(x = .data$node, ymin = .data$lower_0.025, ymax = .data$upper_0.975,
    #         color = plot_data$mc_CIs$analysis_source),
    #     width = 0.5, alpha = 0.7,
    #     linewidth = 0.5
    #   ) +
    #   scale_fill_manual(
    #     values = c(mc = mc_fill, aggregate = aggregate_fill),
    #     labels = c(mc = "Monte Carlo Average", aggregate = "Aggregate")
    #   ) +
    #   scale_color_manual(
    #     values = c(mc = "black"),
    #     labels = c(mc = "Monte Carlo CIs")
    #   ) +
    #   scale_y_continuous(
    #     expand = c(0, 0),
    #     limits = c(plot_data$min_activation, plot_data$max_activation)
    #   ) +
    #   guides(alpha = "none", color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
    #   expand_limits(y = 0) +
    #   labs(
    #     x = "Inference Value",
    #     y = "Concept (Node)"
    #   )
  } else if (fcmconfr_object$fcm_class == "ivfn") {
    ggplot() +
      geom_col(data = plot_data$aggregate_inferences, aes(x = .data$name, y = .data$crisp))

    ggplot() +
      geom_col(data = mc_CIs, aes(x = node, y = expected_value, fill = analysis_source, color = monte_carlo_col_fill), width = 0.5, alpha = monte_carlo_col_alpha, linewidth = 0.3) +
      geom_segment(data = aggregate_inferences, aes(x = concept, xend = concept, y = lower, yend = upper, color = analysis_source), lineend = "round", linewidth = 2) +
      geom_errorbar(data = mc_CIs, aes(x = node, ymin = lower_0.025, ymax = upper_0.975, color = analysis_source), width = 0.5, linewidth = 0.5)
  }




  if (coord_flip) {
    ggplot_main + default_theme() + coord_flip()
  } else {
    ggplot_main + default_theme()
  }
}

#' Plot fcmconfr
#' @importFrom graphics plot
#' @export
plot.fcmconfr <- function(x, ...) {
  print(autoplot(x, ...))
}


#' Custom plot.fcmconfr Theme
#' @importFrom ggplot2 %+replace%
#' @export
theme_custom <- function(...) {
  theme_classic(...) %+replace%
    theme(
      plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10), angle = 90, vjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing = unit(0.001, 'cm')
    )
}

#' Default plot.fcmconfr Theme
#' @export
default_theme <- function() {
  theme_custom()
}


#
# ggplot() +
#   # geom_col(
#   #   data = plot_data$mc_CIs,
#   #   aes(x = .data$node, y = .data$value,
#   #       fill = .data$analysis_source),
#   #   alpha = mc_alpha,
#   #   width = 0.7
#   # ) +
#   ggplot2::geom_crossbar(
#     data = plot_data$mc_CIs,
#     aes(x = .data$node, y = .data$expected_value, ymin = .data$lower_0.025, ymax = .data$upper_0.975,
#         fill = plot_data$mc_CIs$analysis_source),
#     color = mc_fill,
#     width = 0.5,
#     linewidth = 0.2
#   ) +
#   geom_point(
#     data = plot_data$aggregate_inferences,
#     aes(x = .data$name, y = .data$value,
#         fill = plot_data$aggregate_inferences$analysis_source),
#     shape = 21,
#     size = 1,
#     stroke = 0.5
#   ) +
#   scale_fill_manual(
#     values = c(mc = mc_fill, aggregate = aggregate_fill),
#     labels = c(mc = "Monte Carlo Average", aggregate = "Aggregate")
#   ) +
#   # scale_color_manual(
#   #   values = c(mc = "black"),
#   #   labels = c(mc = "Monte Carlo CIs")
#   # ) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = c(plot_data$min_activation, plot_data$max_activation)
#   ) +
#   guides(alpha = "none", color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
#   expand_limits(y = 0) +
#   labs(
#     x = "Inference Value",
#     y = "Concept (Node)",
#     # caption = plot_data$fcm_class_subtitle
#   ) +
#   default_theme() +
#   coord_flip()
#
# # 247, 227, 221



# ggplot() +
#   geom_col(
#     data = plot_data$mc_CIs,
#     aes(x = .data$node, y = .data$expected_value,
#         fill = .data$analysis_source),
#     alpha = mc_alpha,
#     width = 0.7
#   ) +
#   geom_errorbar(
#     data = plot_data$mc_CIs,
#     aes(x = .data$node, ymin = .data$lower_0.025, ymax = .data$upper_0.975,
#         color = plot_data$mc_CIs$analysis_source),
#     width = 0.5,
#     linewidth = 0.5
#   ) +
#   geom_point(
#     data = plot_data$aggregate_inferences,
#     aes(x = .data$name, y = .data$value,
#         fill = plot_data$aggregate_inferences$analysis_source),
#     #color = "black",
#     color = aggregate_fill,
#     shape = 21,
#     size = 2,
#     stroke = 0.5
#   ) +
#   scale_fill_manual(
#     values = c(mc = mc_fill, aggregate = aggregate_fill),
#     labels = c(mc = "Monte Carlo Average", aggregate = "Aggregate")
#   ) +
#   scale_color_manual(
#     values = c(mc = "black"),
#     labels = c(mc = "Monte Carlo CIs")
#   ) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = c(plot_data$min_activation, plot_data$max_activation)
#   ) +
#   guides(alpha = "none", color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
#   expand_limits(y = 0) +
#   labs(
#     x = "Inference Value",
#     y = "Concept (Node)",
#     # caption = plot_data$fcm_class_subtitle
#   ) +
#   default_theme() +
#   coord_flip()

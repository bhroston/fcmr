
#' Get Concepts in fcmconfr Object to Include in Plot
#'
#' @description
#' This determines which concepts should be included in fcmconfr output plot by
#' selecting concepts that either were NOT clamped (if applicable) or did not
#' reach a certain activation level throughout the simulation (i.e. whose
#' 0-value would take up space in the figure.)
#'
#' @details
#' This function removes:
#'     - Clamped Nodes because they would be held at 1 and would increase the
#'     x-axis, potentially diminishing the ability to view other data
#'     - Inactivated Nodes because they do not display on the plot and would
#'     add unnecessary whitespace to the plot.
#'
#' @param fcmconfr_object A direct output of the \code{\link{fcmconfr}} function
#' @param filter_limit Remove concepts whose inferences do not exceed this value
#'
#' @export
get_concepts_to_plot <- function(fcmconfr_object, filter_limit = 10e-10) {
  # browser()

  fcm_clamping_vector <- fcmconfr_object$params$simulation_opts$clamping_vector
  fcm_nodes <- unique(lapply(fcmconfr_object$params$adj_matrices, colnames))[[1]]
  clamped_node_indexes <- which(fcm_clamping_vector != 0)
  clamped_nodes <- fcm_nodes[clamped_node_indexes]

  if (identical(fcmconfr_object$fcm_class, "conventional")) {
    fcmconfr_inferences = list(
      input = fcmconfr_object$inferences$input_fcms$inferences[, -1],
      agg = fcmconfr_object$inferences$aggregate_fcm$inferences,
      mc = fcmconfr_object$inferences$monte_carlo_fcms$all_inferences
    )
  } else if (identical(fcmconfr_object$fcm_class, "ivfn")) {
    raw_input_inferences <- fcmconfr_object$inferences$input_fcms$inferences
    adj_matrix_labels <- names(raw_input_inferences)
    input_inferences_as_lists <- mapply(
      function(adj_matrix_name, input_inference) {
        # browser()
        cbind(adj_matrix = adj_matrix_name, input_inference)
      },
      adj_matrix_name = adj_matrix_labels,
      input_inference = raw_input_inferences, SIMPLIFY = FALSE
    )
    input_inferences <- do.call(rbind, input_inferences_as_lists)
    rownames(input_inferences) <- NULL

    fcmconfr_inferences = list(
      input = input_inferences,
      agg = fcmconfr_object$inferences$aggregate_fcm$inferences,
      mc = fcmconfr_object$inferences$monte_carlo_fcms$all_inferences
    )
  }

  non_null_inference_dfs <- !(unlist(lapply(fcmconfr_inferences, is.null)))
  fcmconfr_inferences_across_analyses <- do.call(rbind, fcmconfr_inferences[non_null_inference_dfs])
  max_inference_by_node <- apply(fcmconfr_inferences_across_analyses, 2, max)

  nodes_to_plot_indexes <-  which(max_inference_by_node > filter_limit & !(fcm_nodes %in% clamped_nodes))
  nodes_to_plot <- fcm_nodes[nodes_to_plot_indexes]

  list(
    name = nodes_to_plot,
    index = nodes_to_plot_indexes
  )
}



#' Get fcmconfr Object Plot Data
#'
#' @description
#' This function parses through an \code{\link{fcmconfr}} output to gather and
#' organize the analysis results into dataframes that are constructed to be
#' plugged directly into a ggplot2 pipeline
#'
#' @details
#' This function produces slightly different outputs for \code{\link{fcmconfr}}
#' outputs generated from conventional, ivfn, and tfn FCMs
#'
#' @param fcmconfr_object A direct output of the \code{\link{fcmconfr}} function
#' @param filter_limit Remove concepts whose inferences do not exceed this value
#'
#' @export
get_plot_data <- function(fcmconfr_object, filter_limit = 10e-3) {

  # browser()
  nodes_to_plot <- get_concepts_to_plot(fcmconfr_object, filter_limit)

  if (length(nodes_to_plot$name) == 0) {
    stop("No inferences are greater than the filter limit, so no plot cannot be drawn.")
  }

  # browser()

  if (identical(fcmconfr_object$fcm_class, "conventional")) {
    input_inferences <- as.data.frame(fcmconfr_object$inferences$input_fcms$inferences)
    input_inferences <- fcmconfr_object$inferences$input_fcms$inferences[, c(1, nodes_to_plot$index + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
  } else if (identical(fcmconfr_object$fcm_class, "ivfn")) {
    raw_input_inferences <- fcmconfr_object$inferences$input_fcms$inferences
    adj_matrix_labels <- names(raw_input_inferences)
    input_inferences_as_lists <- mapply(
      function(adj_matrix_name, input_inference) {
        # browser()
        cbind(adj_matrix = adj_matrix_name, input_inference)
      },
      adj_matrix_name = adj_matrix_labels,
      input_inference = raw_input_inferences, SIMPLIFY = FALSE
    )
    input_inferences <- do.call(rbind, input_inferences_as_lists)
    rownames(input_inferences) <- NULL
  }

  if (fcmconfr_object$params$additional_opts$perform_aggregate_analysis) {
    aggregate_inferences <- as.data.frame(fcmconfr_object$inferences$aggregate_fcm$inferences)
    aggregate_inferences <- data.frame(
      aggregate_inferences[, nodes_to_plot$index]
    )
    names(aggregate_inferences) <- nodes_to_plot$name
  } else {
    aggregate_inferences <- data.frame(
      blank = 0
    )
  }
  if (fcmconfr_object$params$additional_opts$perform_monte_carlo_analysis) {
    mc_inferences <- as.data.frame(fcmconfr_object$inferences$monte_carlo_fcms$all_inferences)
    mc_inferences <- data.frame(
      mc_inferences[, nodes_to_plot$index]
    )
    colnames(mc_inferences) <- nodes_to_plot$name
  } else {
    mc_inferences <- data.frame(
      blank = 0
    )
  }
  if (fcmconfr_object$params$additional_opts$perform_monte_carlo_inference_bootstrap_analysis) {
    mc_inference_CIs <- as.data.frame(fcmconfr_object$inferences$monte_carlo_fcms$bootstrap$CIs_and_quantiles_by_node)
    mc_inference_CIs <- data.frame(
      mc_inference_CIs[nodes_to_plot$index, ]
    )
    colnames(mc_inference_CIs)[colnames(mc_inference_CIs) == "node"] <- "name"
  } else {
    mc_inference_CIs <- data.frame(
      name = 0, # Have to label a column 'name' here since will not tidy::pivot_longer this dataframe
      lower_0.025 = NA,
      upper_0.975 = NA
    )
  }

  # browser()
  if (fcmconfr_object$fcm_class == "conventional") {
    # browser()
    fcm_class_subtitle <- "Conventional FCMs"
    input_inferences_longer <- tidyr::pivot_longer(input_inferences, cols = 2:ncol(input_inferences))
    aggregate_inferences_longer <- tidyr::pivot_longer(aggregate_inferences, cols = 1:ncol(aggregate_inferences))
    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences, cols = 1:ncol(mc_inferences))
    # mc_inference_CIs_longer <- tidyr::pivot_longer(mc_inference_CIs, cols = 1:ncol(mc_inference_CIs))

    # # Need to write a better filter for this
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

    aggregate_inferences_longer <- aggregate_inferences_longer[(aggregate_inferences_longer$name %in% nodes_to_plot), ]
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
  mc_inferences_longer$analysis_source <- "mc"
  mc_inference_CIs$analysis_source <- "mc_CIs"

  list(
    fcm_class_subtitle = fcm_class_subtitle,
    input_inferences = input_inferences_longer,
    aggregate_inferences = aggregate_inferences_longer,
    mc_inferences = mc_inferences_longer,
    mc_inference_CIs = mc_inference_CIs,
    # mc_CIs = mc_CIs,
    max_activation = max_y,
    min_activation = min_y
  )
}


#' Autoplot fcmconfr
#'
#' @description
#' Generate a generic plot visualizing \code{\link{fcmconfr}} results. Call the
#' function name directly (\code{\link{autoplot.fcmconfr}})) without parentheses
#' to see the exact code to generate the plots, then copy-and-paste and edit
#' as needed.
#'
#' @details
#' This function produces slightly different outputs for \code{\link{fcmconfr}}
#' outputs generated from conventional, ivfn, and tfn FCMs.
#'
#' @param object A direct output of the \code{\link{fcmconfr}} function
#' @param ... Additional inputs
#'
#' @importFrom ggplot2 autoplot ggplot aes
#' @importFrom rlang .data
#' @export
autoplot.fcmconfr <- function(object, ...) {
                              # exclude_input_inferences = FALSE,
                              # exclude_agg_inferences = FALSE,
                              # exclude_mc_inferences = FALSE,
                              # aggregate_fill = "#f7e3dd",
                              # aggregate_color = "#E1D1CB",
                              # aggregate_alpha = 0.7,
                              # mc_fill = "#f7e3dd",

  # .data <- NULL # Only here to satisfy R CMD Check

  additional_inputs <- list(...)[[1]]
  filter_limit <- additional_inputs$filter_limit
  coord_flip <- additional_inputs$coord_flip

  plot_data <- get_plot_data(object, filter_limit)
  concepts_to_plot <- unique(c(plot_data$input_inferences$name, plot_data$aggregate_inferences$name, plot_data$mc_inferences$name))
  concepts_to_plot <- concepts_to_plot[concepts_to_plot != "blank"]

  plot_data$max_activation <- plot_data$max_activation + 0.1*plot_data$max_activation
  plot_data$min_activation <- plot_data$min_activation - 0.1*plot_data$min_activation

  if (object$params$simulation_opts$squashing == "sigmoid" & all(object$params$simulation_opts$clamping == 0)) {
    zero_intercept <- 0.5
  } else {
    zero_intercept <- 0
  }

  # browser()

  if (object$fcm_class == "conventional") {
    ggplot_main <- ggplot() +
      ggplot2::geom_hline(yintercept = zero_intercept, linetype = "dotted") +
      ggplot2::geom_jitter(
        data = ggplot2::remove_missing(plot_data$mc_inferences),
        aes(x = .data$name, y = .data$value, color = .data$analysis_source),
        size = 0.7, alpha = 0.7, width = 0.2, shape = 16, na.rm = FALSE
      ) +
      ggplot2::geom_boxplot(
        data = ggplot2::remove_missing(plot_data$mc_inferences),
        aes(x = .data$name, y = .data$value),
        outlier.shape = NA, width = 0.6, na.rm = TRUE, linewidth = 0.1, fill = NA
      ) +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(x = .data$name, ymin = .data$X0.025_CI, y = .data$X0.025_CI, ymax = .data$X0.025_CI), # Lower CI in 8th column of df
        #aes(x = .data$name, ymin = .data$lower_0.025, y = .data$lower_0.025, ymax = .data$lower_0.025),
        linetype = "dotted", linewidth = 0.2, width = 0.8, na.rm = FALSE
      ) +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(x = .data$name, ymin = .data$X0.975_CI, y = .data$X0.975_CI, ymax = .data$X0.975_CI), # Upper CI in 8th column of df
        linetype = "dotted",, linewidth = 0.2, width = 0.8, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$input_inferences),
        # position = ggplot2::position_jitter(h = 0.025, w = 0),
        position = ggplot2::position_dodge2(width = 0.1),
        aes(x = .data$name, y = .data$value, color = .data$analysis_source),
        alpha = 1, shape = 16, size = 2, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$aggregate_inferences),
        aes(x = .data$name, y = .data$value),
        shape = 17, color = "red", size = 2,
      ) +
      ggplot2::scale_color_manual(values = c(input = "black", mc = "grey")) +
      ggplot2::scale_x_discrete(limits = concepts_to_plot) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(plot_data$min_activation, plot_data$max_activation)) +
      # ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, plot_data$max_activation)) +
      default_theme() +
      ggplot2::coord_flip()

  } else if (object$fcm_class == "ivfn") {
    # ggplot() +
    #   geom_col(data = plot_data$aggregate_inferences, aes(x = .data$name, y = .data$crisp))
    #
    # ggplot() +
    #   geom_col(data = mc_CIs, aes(x = node, y = expected_value, fill = analysis_source, color = monte_carlo_col_fill), width = 0.5, alpha = monte_carlo_col_alpha, linewidth = 0.3) +
    #   geom_segment(data = aggregate_inferences, aes(x = concept, xend = concept, y = lower, yend = upper, color = analysis_source), lineend = "round", linewidth = 2) +
    #   geom_errorbar(data = mc_CIs, aes(x = node, ymin = lower_0.025, ymax = upper_0.975, color = analysis_source), width = 0.5, linewidth = 0.5)
    #
    }




  # if (coord_flip) {
  #   fcmconr_plot <- ggplot_main + default_theme() + coord_flip()
  # } else {
  #   fcmconfr_plot <- ggplot_main + default_theme()
  # }
}


#' Plot fcmconfr
#'
#' @description
#' Print the output of the autoplot function (\code{\link{autoplot.fcmconfr}})
#'
#' @param x A direct output of the \code{\link{fcmconfr}} function
#' @param ... Additional inputs:
#'  - filter_limit Remove concepts whose inferences do not exceed this value
#'  - coord_flip Swap x- and y-axes (i.e. rotate plot)
#'
#' @importFrom graphics plot
#' @export
plot.fcmconfr <- function(x, ...) {
  additional_inputs = list(...)
  if (!("filter_limit" %in% names(additional_inputs))) {
    filter_limit = 1e-3
    additional_inputs$filter_limit <- filter_limit
  }
  if (!("coord_flip" %in% names(additional_inputs))) {
    coord_flip = TRUE
    additional_inputs$coord_flip <- coord_flip
  }
  stopifnot(is.numeric(filter_limit) & (filter_limit > 0 & filter_limit < 1))
  stopifnot(is.logical(coord_flip))

  suppressWarnings(print(autoplot(x, additional_inputs)))
}


#' Custom plot.fcmconfr Theme
#'
#' @description
#' Theme-ing for plot.fcmconfr to improve readability in
#' \code{\link{plot.fcmconfr}} function definition
#'
#' @param ... Additional Inputs
#'
#' @importFrom ggplot2 %+replace%
#' @export
theme_custom <- function(...) {
  ggplot2::theme_classic(...) %+replace%
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 20, r = 40, b = 20, l = 20),
      axis.title.x = ggplot2::element_blank(),  # element_text(margin = margin(t = 10)),
      axis.title.y = ggplot2::element_blank(),  # element_text(margin = margin(r = 10), angle = 90, vjust = 0.5),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.spacing = ggplot2::unit(0.001, 'cm')
    )
}

#' Default plot.fcmconfr Theme
#'
#' @description
#' A formal call to the custom_theme defined by \code{\link{theme_custom}}
#'
#' @export
default_theme <- function() {
  theme_custom()
}




# input_inferences <- fcmconfr_object$inferences$input_fcms$inferences[2:ncol(fcmconfr_object$inferences$input_fcms$inferences)]
# input_inferences_greater_than_filter_limit <- apply(input_inferences, 2, function(x) x > filter_limit)
# nodes_with_inferences_greater_than_filter_limit <- apply(input_inferences_greater_than_filter_limit, 2, function(column) any(column))
# nodes_of_interest_indexes <- which(nodes_with_inferences_greater_than_filter_limit)
# nodes_of_interest_indexes <- nodes_of_interest_indexes[which(names(nodes_of_interest_indexes) != clamped_nodes)]
# nodes_of_interest <- names(nodes_of_interest_indexes)

# mc_inference_CIs_longer$analysis_source <- "mc_CIs"
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

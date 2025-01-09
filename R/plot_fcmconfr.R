
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
    input_inferences_as_ivfns <- do.call(rbind, raw_input_inferences)
    rownames(input_inferences_as_ivfns) <- NULL
    input_inferences_as_ivfns <- input_inferences_as_ivfns[, !(colnames(input_inferences_as_ivfns) %in% c("adj_matrix"))]
    input_inferences <- list(
      lower_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_ivfns, c(1, 2), function(element) element[[1]]$lower))),
      upper_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_ivfns, c(1, 2), function(element) element[[1]]$upper)))
    )

    agg_inferences_df <- fcmconfr_object$inferences$aggregate_fcm$inferences_df
    lower_agg_inference_values <- agg_inferences_df$lower
    names(lower_agg_inference_values) <- agg_inferences_df$concept
    upper_agg_inference_values <- agg_inferences_df$upper
    names(upper_agg_inference_values) <- agg_inferences_df$concept
    aggregate_inferences <- list(
      lower_inference_values = lower_agg_inference_values,
      upper_inference_values = upper_agg_inference_values
    )

    fcmconfr_inferences = list(
      lower_input = input_inferences$lower_inference_values[, -1],
      upper_input = input_inferences$upper_inference_values[, -1],
      lower_agg = aggregate_inferences$lower_inference_values,
      upper_agg = aggregate_inferences$upper_inference_values,
      mc = fcmconfr_object$inferences$monte_carlo_fcms$all_inferences
    )
  } else if (identical(fcmconfr_object$fcm_class, "tfn")) {
    raw_input_inferences <- fcmconfr_object$inferences$input_fcms$inferences
    adj_matrix_labels <- names(raw_input_inferences)
    input_inferences_as_tfns <- do.call(rbind, raw_input_inferences)
    rownames(input_inferences_as_tfns) <- NULL
    input_inferences_as_ivfns <- input_inferences_as_tfns[, !(colnames(input_inferences_as_tfns) %in% c("adj_matrix"))]
    input_inferences <- list(
      lower_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_tfns, c(1, 2), function(element) element[[1]]$lower))),
      mode_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_tfns, c(1, 2), function(element) element[[1]]$mode))),
      upper_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_tfns, c(1, 2), function(element) element[[1]]$upper)))
    )

    agg_inferences_df <- fcmconfr_object$inferences$aggregate_fcm$inferences_df
    lower_agg_inference_values <- agg_inferences_df$lower
    names(lower_agg_inference_values) <- agg_inferences_df$concept
    mode_agg_inference_values <- agg_inferences_df$mode
    names(mode_agg_inference_values) <- agg_inferences_df$concept
    upper_agg_inference_values <- agg_inferences_df$upper
    names(upper_agg_inference_values) <- agg_inferences_df$concept
    aggregate_inferences <- list(
      lower_inference_values = lower_agg_inference_values,
      mode_inference_values = mode_agg_inference_values,
      upper_inference_values = upper_agg_inference_values
    )

    fcmconfr_inferences = list(
      lower_input = input_inferences$lower_inference_values[, -1],
      mode_input = input_inferences$mode_inference_values[, -1],
      upper_input = input_inferences$upper_inference_values[, -1],
      lower_agg = aggregate_inferences$lower_inference_values,
      mode_agg = aggregate_inferences$mode_inference_values,
      upper_agg = aggregate_inferences$upper_inference_values,
      mc = fcmconfr_object$inferences$monte_carlo_fcms$all_inferences
    )
  }

  non_null_inference_dfs <- !(unlist(lapply(fcmconfr_inferences, is.null)))
  fcmconfr_inferences_across_analyses <- data.frame(do.call(rbind, fcmconfr_inferences[non_null_inference_dfs]))

  # if (identical(fcmconfr_object$fcm_class, "conventional")) {
  abs_max_inference_by_node <- apply(fcmconfr_inferences_across_analyses, 2, function(col) max(abs(col)))
  # } else if (identical(fcmconfr_object$fcm_class, "ivfn")) {
  #   # upper_values_of_inferences <- apply(fcmconfr_inferences_across_analyses[, 2:ncol(fcmconfr_inferences_across_analyses)], c(1, 2), function(element) element[[1]]$upper)
  #   abs_max_inference_by_node <- apply(fcmconfr_inferences_across_analyses, 2, function(col) max(abs(col)))
  # }

  nodes_to_plot_indexes <-  which(abs_max_inference_by_node > filter_limit & !(fcm_nodes %in% clamped_nodes))
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
  nodes_to_plot <- get_concepts_to_plot(fcmconfr_object, filter_limit)

  if (length(nodes_to_plot$name) == 0) {
    stop("No inferences are greater than the filter limit, so no plot cannot be drawn.")
  }

  if (identical(fcmconfr_object$fcm_class, "conventional")) {
    input_inferences <- as.data.frame(fcmconfr_object$inferences$input_fcms$inferences)
    input_inferences <- fcmconfr_object$inferences$input_fcms$inferences[, c(1, nodes_to_plot$index + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
  } else if (identical(fcmconfr_object$fcm_class, "ivfn")) {
    raw_input_inferences <- fcmconfr_object$inferences$input_fcms$inferences
    adj_matrix_labels <- names(raw_input_inferences)
    input_inferences_as_ivfns <- do.call(rbind, raw_input_inferences)
    rownames(input_inferences_as_ivfns) <- NULL
    input_inferences_as_ivfns <- input_inferences_as_ivfns[, !(colnames(input_inferences_as_ivfns) %in% c("adj_matrix"))]
    input_inferences <- list(
      lower_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_ivfns, c(1, 2), function(element) element[[1]]$lower))),
      upper_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_ivfns, c(1, 2), function(element) element[[1]]$upper)))
    )
    input_inferences$lower_inference_values <- input_inferences$lower_inference_values[, c(1, nodes_to_plot$index + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
    input_inferences$upper_inference_values <- input_inferences$upper_inference_values[, c(1, nodes_to_plot$index + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
  } else if (identical(fcmconfr_object$fcm_class, "tfn")) {
    raw_input_inferences <- fcmconfr_object$inferences$input_fcms$inferences
    adj_matrix_labels <- names(raw_input_inferences)
    input_inferences_as_tfns <- do.call(rbind, raw_input_inferences)
    rownames(input_inferences_as_tfns) <- NULL
    input_inferences_as_tfns <- input_inferences_as_tfns[, !(colnames(input_inferences_as_tfns) %in% c("adj_matrix"))]
    input_inferences <- list(
      lower_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_tfns, c(1, 2), function(element) element[[1]]$lower))),
      mode_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_tfns, c(1, 2), function(element) element[[1]]$mode))),
      upper_inference_values = cbind(adj_matrix = adj_matrix_labels, data.frame(apply(input_inferences_as_tfns, c(1, 2), function(element) element[[1]]$upper)))
    )
    input_inferences$lower_inference_values <- input_inferences$lower_inference_values[, c(1, nodes_to_plot$index + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
    input_inferences$mode_inference_values <- input_inferences$mode_inference_values[, c(1, nodes_to_plot$index + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
    input_inferences$upper_inference_values <- input_inferences$upper_inference_values[, c(1, nodes_to_plot$index + 1)] # Add + 1 to match column indexes in input_fcms$inferences dataframe
  }

  if (fcmconfr_object$params$additional_opts$perform_aggregate_analysis) {
    aggregate_inferences <- as.data.frame(fcmconfr_object$inferences$aggregate_fcm$inferences)
    aggregate_inferences <- data.frame(
      aggregate_inferences[, nodes_to_plot$index]
    )
    names(aggregate_inferences) <- nodes_to_plot$name
  } else {
    aggregate_inferences <- data.frame(
      blank = NA
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
      blank = NA
    )
  }
  if (fcmconfr_object$params$additional_opts$perform_monte_carlo_inference_bootstrap_analysis) {
    mc_inference_CIs <- as.data.frame(fcmconfr_object$inferences$monte_carlo_fcms$bootstrap$CIs_and_quantiles_by_node)
    mc_inference_CIs <- data.frame(
      mc_inference_CIs[nodes_to_plot$index, ]
    )
    mc_inference_CIs <- mc_inference_CIs[, c(1, which(sapply(colnames(mc_inference_CIs), function(string) grepl("_CI", string))))]
    colnames(mc_inference_CIs) <- c("name", "lower_CI", "upper_CI")

  } else {
    mc_inference_CIs <- data.frame(
      name = 'blank',
      lower_CI = NA,
      upper_CI = NA
    )
  }

  if (fcmconfr_object$fcm_class == "conventional") {
    fcm_class_subtitle <- "Conventional FCMs"
    input_inferences_longer <- tidyr::pivot_longer(input_inferences, cols = 2:ncol(input_inferences))
    aggregate_inferences_longer <- tidyr::pivot_longer(aggregate_inferences, cols = 1:ncol(aggregate_inferences))
    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences, cols = 1:ncol(mc_inferences))
    # mc_inference_CIs_longer <- tidyr::pivot_longer(mc_inference_CIs, cols = 1:ncol(mc_inference_CIs))

    # Need to write a better filter for this
    if (any(abs(input_inferences_longer$value) > 1) | any(abs(aggregate_inferences_longer$value[!is.na(aggregate_inferences_longer$value)]) > 1) | any(abs(mc_inferences_longer$value[!is.na(mc_inferences_longer$value)]) > 1)) {
      warning("Some inferences have a magnitude greater than 1 which suggests that
              the simulations did not converge, and will likely output unclear and/or
              illogical results.. Either increase the max. number of iterations
              (max_iter) or decrease lambda for improved results.")
    }

    max_y <- max(max(input_inferences_longer$value), max(mc_inferences_longer$value), max(aggregate_inferences_longer$value))
    max_y <- (ceiling(max_y*1000))/1000
    min_y <- min(min(input_inferences_longer$value), min(mc_inferences_longer$value), min(aggregate_inferences_longer$value))
    min_y <- (floor(min_y*1000))/1000

    input_inferences_longer$analysis_source <- "Ind FCMs"
    aggregate_inferences_longer$analysis_source <- "Agg FCM"
    mc_inferences_longer$analysis_source <- "MC FCMs"
    mc_inference_CIs$analysis_source <- "CIs"

  } else if (fcmconfr_object$fcm_class == "ivfn") {
    fcm_class_subtitle <- "IVFN FCM"

    lower_input_inferences_longer <- tidyr::pivot_longer(input_inferences$lower_inference_values, cols = 2:ncol(input_inferences$lower_inference_values), values_to = "lower")
    upper_input_inferences_longer <- tidyr::pivot_longer(input_inferences$upper_inference_values, cols = 2:ncol(input_inferences$upper_inference_values), values_to = "upper")
    input_inferences_longer <- merge(lower_input_inferences_longer, upper_input_inferences_longer)
    input_inferences_longer$analysis_source <- "Ind FCMs"

    lower_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "ivfn"), element[[1]]$lower, NA), numeric(1))
    upper_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "ivfn"), element[[1]]$upper, NA), numeric(1))
    aggregate_inferences_longer <- data.frame(
      name = names(lower_aggregate_inferences),
      lower = lower_aggregate_inferences,
      upper = upper_aggregate_inferences
    )

    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences, cols = 1:ncol(mc_inferences))

    max_y <- max(max(input_inferences_longer$upper), max(mc_inferences_longer$value), max(aggregate_inferences_longer$upper))
    max_y <- (ceiling(max_y*1000))/1000
    min_y <- min(min(input_inferences_longer$lower), min(mc_inferences_longer$value), min(aggregate_inferences_longer$lower))
    min_y <- (floor(min_y*1000))/1000

    aggregate_inferences_longer$analysis_source <- "Agg FCM"
    mc_inferences_longer$analysis_source <- "MC FCMs"
    mc_inference_CIs$analysis_source <- "CIs"
  } else if (fcmconfr_object$fcm_class == "tfn") {
    fcm_class_subtitle <- "TFN FCM"

    lower_input_inferences_longer <- tidyr::pivot_longer(input_inferences$lower_inference_values, cols = 2:ncol(input_inferences$lower_inference_values), values_to = "lower")
    mode_input_inferences_longer <- tidyr::pivot_longer(input_inferences$mode_inference_values, cols = 2:ncol(input_inferences$mode_inference_values), values_to = "mode")
    upper_input_inferences_longer <- tidyr::pivot_longer(input_inferences$upper_inference_values, cols = 2:ncol(input_inferences$upper_inference_values), values_to = "upper")
    input_inferences_longer <- Reduce(function(x, y) merge(x, y, all=TRUE), list(lower_input_inferences_longer, mode_input_inferences_longer, upper_input_inferences_longer))
    #input_inferences_longer <- merge(lower_input_inferences_longer, mode_input_inferences_longer, upper_input_inferences_longer, all = TRUE)
    input_inferences_longer$analysis_source <- "Ind FCMs"

    lower_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "tfn"), element[[1]]$lower, NA), numeric(1))
    mode_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "tfn"), element[[1]]$mode, NA), numeric(1))
    upper_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "tfn"), element[[1]]$upper, NA), numeric(1))
    aggregate_inferences_longer <- data.frame(
      name = names(lower_aggregate_inferences),
      lower = lower_aggregate_inferences,
      mode = mode_aggregate_inferences,
      upper = upper_aggregate_inferences
    )

    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences, cols = 1:ncol(mc_inferences))

    max_y <- max(max(input_inferences_longer$upper), max(mc_inferences_longer$value), max(aggregate_inferences_longer$upper), na.rm = TRUE)
    max_y <- (ceiling(max_y*1000))/1000
    min_y <- min(min(input_inferences_longer$lower), min(mc_inferences_longer$value), min(aggregate_inferences_longer$lower), na.rm = TRUE)
    min_y <- (floor(min_y*1000))/1000

    aggregate_inferences_longer$analysis_source <- "Agg FCM"
    mc_inferences_longer$analysis_source <- "MC FCMs"
    mc_inference_CIs$analysis_source <- "CIs"
  }

  list(
    fcm_class_subtitle = fcm_class_subtitle,
    input_inferences = input_inferences_longer,
    aggregate_inferences = aggregate_inferences_longer,
    mc_inferences = mc_inferences_longer,
    mc_inference_CIs = mc_inference_CIs,
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

  inputs_only <- (identical(plot_data$aggregate_inferences$name, "blank") & identical(plot_data$mc_inferences$name, "blank") & identical(plot_data$mc_inference_CIs$name, "blank"))
  inputs_and_agg <- (!(identical(plot_data$aggregate_inferences$name, "blank")) & identical(plot_data$mc_inferences$name, "blank") & identical(plot_data$mc_inference_CIs$name, "blank"))
  inputs_agg_and_mc_no_bs <- (!(identical(plot_data$aggregate_inferences$name, "blank")) & !(identical(plot_data$mc_inferences$name, "blank")) & identical(plot_data$mc_inference_CIs$name, "blank"))
  inputs_agg_and_mc_w_bs <- (!(identical(plot_data$aggregate_inferences$name, "blank")) & !(identical(plot_data$mc_inferences$name, "blank")) & !(identical(plot_data$mc_inference_CIs$name, "blank")))

  if (object$fcm_class == "conventional") {
    ggplot_main <- ggplot() +
      # ggplot2::geom_hline(yintercept = zero_intercept, linetype = "dotted") +
      # MC CIs
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$lower_CI, x = .data$lower_CI, xmax = .data$lower_CI, linetype = .data$analysis_source),
        width = 0.8, color = "blue", linewidth = 0.2, na.rm = FALSE, key_glyph = ggplot2::draw_key_vline
      ) +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$upper_CI, x = .data$upper_CI, xmax = .data$upper_CI, linetype = .data$analysis_source),
        width = 0.8, color = "blue", linewidth = 0.2, na.rm = TRUE, key_glyph = ggplot2::draw_key_vline
      ) +
      # MC FCMs
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$mc_inferences),
        aes(y = .data$name, x = .data$value, color = .data$analysis_source),#, shape = .data$analysis_source),
        position = ggplot2::position_dodge2(width = 0.4), shape = 3, size = 1, alpha = 1, width = 0.2, na.rm = FALSE
      ) +
      # ggplot2::geom_boxplot(
      #   data = ggplot2::remove_missing(plot_data$mc_inferences),
      #   aes(y = .data$name, x = .data$value, linewidth = .data$analysis_source, color = .data$analysis_source),
      #   # outlier.color = "darkgrey", outlier.shape = 3, outlier.size = 1.75,
      #   width = 0.6, na.rm = TRUE, fill = NA
      # ) +
      # Ind FCMs
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$input_inferences),
        # position = ggplot2::position_jitter(h = 0.025, w = 0),
        position = ggplot2::position_dodge2(width = 0.1),
        aes(y = .data$name, x = .data$value, color = .data$analysis_source, shape = .data$analysis_source),
        size = 2, na.rm = TRUE
      ) +
      # Agg FCM
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$aggregate_inferences),
        aes(y = .data$name, x = .data$value, color = .data$analysis_source, shape = .data$analysis_source),
        size = 2,
      )
      # ggplot2::scale_y_discrete(limits = concepts_to_plot) +
      # ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(plot_data$min_activation, plot_data$max_activation))
      # ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, plot_data$max_activation)) +
  } else if (object$fcm_class == "ivfn") {
    ggplot_main <- ggplot2::ggplot() +
      # MC CIs
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$lower_CI, x = .data$lower_CI, xmax = .data$lower_CI, linetype = .data$analysis_source),
        width = 0.8, color = "blue", linewidth = 0.2, na.rm = FALSE, key_glyph = ggplot2::draw_key_vline
      ) +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$upper_CI, x = .data$upper_CI, xmax = .data$upper_CI, linetype = .data$analysis_source),
        width = 0.8, color = "blue", linewidth = 0.2, na.rm = TRUE, key_glyph = ggplot2::draw_key_vline
      )
    # MC FCMs
    if (inputs_agg_and_mc_no_bs | inputs_agg_and_mc_w_bs) {
      ggplot_main <- ggplot_main +
        ggplot2::geom_point(
          data = ggplot2::remove_missing(plot_data$mc_inferences),
          aes(y = .data$name, x = .data$value, color = .data$analysis_source),#, shape = .data$analysis_source),
          position = ggplot2::position_dodge2(width = 0.25), alpha = 0.8, shape = 3,
          na.rm = FALSE
        )
    }
    # Individual FCMs
    ggplot_main <- ggplot_main +
      ggplot2::geom_linerange(
      data = ggplot2::remove_missing(plot_data$input_inferences),
      aes(y = .data$name, xmin = .data$lower, x = .data$lower, xmax = .data$upper, color = .data$analysis_source),
      position = ggplot2::position_dodge2(width = 0.5), linewidth = 0.1
    )
    # Aggregate FCM
    if (!inputs_only) {
      ggplot_main <- ggplot_main +
        ggplot2::geom_linerange(
          data = ggplot2::remove_missing(plot_data$aggregate_inferences),
          aes(y = .data$name, xmin = .data$lower, x = .data$lower, xmax = .data$upper, color = .data$analysis_source),
          linewidth = 0.6
        )
    }
  } else if (object$fcm_class == "tfn") {
    ggplot_main <- ggplot2::ggplot() +
      # MC CIs
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$lower_CI, x = .data$lower_CI, xmax = .data$lower_CI, linetype = .data$analysis_source),
        width = 0.8, color = "blue", linewidth = 0.2, na.rm = FALSE, key_glyph = ggplot2::draw_key_vline
      ) +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$upper_CI, x = .data$upper_CI, xmax = .data$upper_CI, linetype = .data$analysis_source),
        width = 0.8, color = "blue", linewidth = 0.2, na.rm = TRUE, key_glyph = ggplot2::draw_key_vline
      ) +
      # MC FCMs
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$mc_inferences),
        aes(y = .data$name, x = .data$value, color = .data$analysis_source),#, shape = .data$analysis_source),
        position = ggplot2::position_dodge2(width = 0.25), alpha = 0.8, shape = 3, na.rm = FALSE
      ) +
      # Individual FCMs
      ggplot2::geom_pointrange(
        data = ggplot2::remove_missing(plot_data$input_inferences),
        aes(y = .data$name, xmin = .data$lower, x = .data$mode, xmax = .data$upper, color = .data$analysis_source),#, shape = .data$analysis_source),
        position = ggplot2::position_dodge2(width = 0.5), fatten = 0.6, linewidth = 0.1
      )
    # Aggregate FCM - Have to separate this because it's the only clean way to
    # make the legend's key look right
    if (!inputs_only) {
      ggplot_main <- ggplot_main +
        ggplot2::geom_pointrange(
          data = ggplot2::remove_missing(plot_data$aggregate_inferences),
          aes(y = .data$name, xmin = .data$lower, x = .data$mode, xmax = .data$upper, color = .data$analysis_source),#, shape = .data$analysis_source),
          fatten = 2, linewidth = 0.4
        )
    }
  }

  if  (inputs_only) {
    ggplot_main <- ggplot_main +
      ggplot2::scale_color_manual(values = c("Ind FCMs" = "black")) +#, guide = ggplot2::guide_legend(override.aes = list(shape = 16)))
      ggplot2::scale_shape_manual(values = c("Ind FCMs" = 16))
  } else if (inputs_and_agg) {
    ggplot_main <- ggplot_main +
      ggplot2::scale_color_manual(
        values = c("Ind FCMs" = "black", "Agg FCM" = "red"),
        breaks = c("Ind FCMs", "Agg FCM"),
        guide = ggplot2::guide_legend(override.aes = list(linewidth = c(0.1, 0.5)))
        ) +
      ggplot2::scale_shape_manual(values = c("Ind FCMs" = 16, "Agg FCM" = 17), breaks = c("Ind FCMs", "Agg FCM"))
  } else if (inputs_agg_and_mc_no_bs) {
    ggplot_main <- ggplot_main +
      ggplot2::scale_color_manual(
        values = c("Ind FCMs" = "black", "MC FCMs" = "blue", "Agg FCM" = "red"),
        breaks = c("Ind FCMs", "MC FCMs", "Agg FCM"),
        guide = ggplot2::guide_legend(override.aes = list(shape = c(16, 3, 17), linewidth = c(0.5, NA, 0.5)))
      ) +
      ggplot2::scale_shape_manual(values = c("Ind FCMs" = 16, "MC FCMs" = 3, "Agg FCM" = 17), breaks = c("Ind FCMs", "MC FCMs", "Agg FCM"))
  } else if (inputs_agg_and_mc_w_bs) {
    ggplot_main <- ggplot_main +
      # ggplot2::scale_color_manual(values = c("Ind FCMs" = "black", "Agg FCM" = "red", "MC FCMs" = "blue", "CIs" = "blue"), breaks = c("MC FCMs", "Ind FCMs", "Agg FCM", "CIs")) +
      ggplot2::scale_color_manual(
        values = c("Ind FCMs" = "black", "MC FCMs" = "blue", "Agg FCM" = "red"),
        breaks = c("Ind FCMs", "MC FCMs", "Agg FCM"),
        guide = ggplot2::guide_legend(override.aes = list(shape = c(16, 3, 17), linewidth = c(0.5, NA, 0.5)))
      ) +
      # ggplot2::guides(color = ggplot2::guide_legend(nrow = 3)) +
      # ggplot2::scale_shape_manual(values = c("Ind FCMs" = 16, "Agg FCM" = 17, "MC FCMs" = 3), breaks = c("MC FCMs", "Ind FCMs", "Agg FCM", "CIs")) +
      ggplot2::scale_linetype_manual(values = c("Ind FCMs" = "blank", "MC FCMs" = "blank", "Agg FCM" = "blank", "CIs" = "dotted"), breaks = c("MC FCMs", "Ind FCMs", "Agg FCM", "CIs")) #+
      # ggplot2::scale_linewidth_manual(values = c("Ind FCMs" = NA, "MC FCMs" = 0.2, "Agg FCM" = NA, "CIs" = NA), breaks = c("MC FCMs", "Ind FCMs", "Agg FCM", "CIs"))
  }

  if (coord_flip) {
    fcmconfr_plot <- ggplot_main + default_theme()
  } else {
    fcmconfr_plot <- ggplot_main + default_theme() + ggplot2::coord_flip()
  }

    # browser()
  fcmconfr_plot

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
      legend.justification = "center",
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

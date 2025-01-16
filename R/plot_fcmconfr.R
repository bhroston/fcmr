
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
#' @returns An array of concepts that should be included in the fcmconfr plot
#' output because they have simulation inferences greater than the filter_limit
#'
#' @export
#' @examples
#' NULL
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
#' @returns A list of fcmconfr output dataframes organized to streamline
#' functionality with ggplot
#'
#' @export
#' @examples
#' NULL
get_plot_data <- function(fcmconfr_object, filter_limit = 10e-3) {
  nodes_to_plot <- get_concepts_to_plot(fcmconfr_object, filter_limit)

  if (length(nodes_to_plot$name) == 0) {
    stop(cli::format_error(c(
      "x" = "Error: No inferences are greater than the {.var filter limit}, so no plot cannot be drawn.",
      "+++++> Reduce {.var filter limit}"
    )))
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
    mc_inference_values <- as.data.frame(fcmconfr_object$inferences$monte_carlo_fcms$all_inferences)
    mc_inference_values <- data.frame(
      mc_inference_values[, nodes_to_plot$index]
    )
    colnames(mc_inference_values) <- nodes_to_plot$name
    mean_mc_inferences <- data.frame(apply(fcmconfr_object$inferences$monte_carlo_fcms$all_inferences, 2, mean, simplify = FALSE))
    mean_mc_inferences <- data.frame(
      mean_mc_inferences[, nodes_to_plot$index]
    )
    colnames(mean_mc_inferences) <- nodes_to_plot$name
    mc_inferences <- list(
      inferences = mc_inference_values,
      averages = mean_mc_inferences
    )
  } else {
    mc_inferences <- list(
      inferences = data.frame(blank = NA),
      averages = data.frame(blank = NA)
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
    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences$inferences, cols = 1:ncol(mc_inferences$inferences))
    mc_avg_inferences_longer <- tidyr::pivot_longer(mc_inferences$averages, cols = 1:ncol(mc_inferences$averages))
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

    input_inferences_longer$analysis_source <- "Ind FCM Inferences"
    aggregate_inferences_longer$analysis_source <- "Agg FCM Inferences"
    mc_inferences_longer$analysis_source <- "MC FCM Inferences"
    mc_avg_inferences_longer$analysis_source <- "MC FCM Avg Inferences"
    mc_inference_CIs$analysis_source <- "CIs of MC FCM Avg Inferences"

  } else if (fcmconfr_object$fcm_class == "ivfn") {
    fcm_class_subtitle <- "IVFN FCM"

    lower_input_inferences_longer <- tidyr::pivot_longer(input_inferences$lower_inference_values, cols = 2:ncol(input_inferences$lower_inference_values), values_to = "lower")
    upper_input_inferences_longer <- tidyr::pivot_longer(input_inferences$upper_inference_values, cols = 2:ncol(input_inferences$upper_inference_values), values_to = "upper")
    input_inferences_longer <- merge(lower_input_inferences_longer, upper_input_inferences_longer)
    input_inferences_longer$analysis_source <- "Ind FCM Inferences"

    lower_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "ivfn"), element[[1]]$lower, NA), numeric(1))
    upper_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "ivfn"), element[[1]]$upper, NA), numeric(1))
    aggregate_inferences_longer <- data.frame(
      name = names(lower_aggregate_inferences),
      lower = lower_aggregate_inferences,
      upper = upper_aggregate_inferences
    )

    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences$inferences, cols = 1:ncol(mc_inferences$inferences))
    mc_avg_inferences_longer <- tidyr::pivot_longer(mc_inferences$averages, cols = 1:ncol(mc_inferences$averages))

    max_y <- max(max(input_inferences_longer$upper), max(mc_inferences_longer$value), max(aggregate_inferences_longer$upper))
    max_y <- (ceiling(max_y*1000))/1000
    min_y <- min(min(input_inferences_longer$lower), min(mc_inferences_longer$value), min(aggregate_inferences_longer$lower))
    min_y <- (floor(min_y*1000))/1000

    aggregate_inferences_longer$analysis_source <- "Agg FCM Inferences"
    mc_inferences_longer$analysis_source <- "MC FCM Inferences"
    mc_avg_inferences_longer$analysis_source <- "MC FCM Avg Inferences"
    mc_inference_CIs$analysis_source <- "CIs of MC FCM Avg Inferences"
  } else if (fcmconfr_object$fcm_class == "tfn") {
    fcm_class_subtitle <- "TFN FCM"

    lower_input_inferences_longer <- tidyr::pivot_longer(input_inferences$lower_inference_values, cols = 2:ncol(input_inferences$lower_inference_values), values_to = "lower")
    mode_input_inferences_longer <- tidyr::pivot_longer(input_inferences$mode_inference_values, cols = 2:ncol(input_inferences$mode_inference_values), values_to = "mode")
    upper_input_inferences_longer <- tidyr::pivot_longer(input_inferences$upper_inference_values, cols = 2:ncol(input_inferences$upper_inference_values), values_to = "upper")
    input_inferences_longer <- Reduce(function(x, y) merge(x, y, all=TRUE), list(lower_input_inferences_longer, mode_input_inferences_longer, upper_input_inferences_longer))
    #input_inferences_longer <- merge(lower_input_inferences_longer, mode_input_inferences_longer, upper_input_inferences_longer, all = TRUE)
    input_inferences_longer$analysis_source <- "Ind FCM Inferences"

    lower_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "tfn"), element[[1]]$lower, NA), numeric(1))
    mode_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "tfn"), element[[1]]$mode, NA), numeric(1))
    upper_aggregate_inferences <- vapply(aggregate_inferences, function(element) ifelse(identical(methods::is(element[[1]]), "tfn"), element[[1]]$upper, NA), numeric(1))
    aggregate_inferences_longer <- data.frame(
      name = names(lower_aggregate_inferences),
      lower = lower_aggregate_inferences,
      mode = mode_aggregate_inferences,
      upper = upper_aggregate_inferences
    )

    mc_inferences_longer <- tidyr::pivot_longer(mc_inferences$inferences, cols = 1:ncol(mc_inferences$inferences))
    mc_avg_inferences_longer <- tidyr::pivot_longer(mc_inferences$averages, cols = 1:ncol(mc_inferences$averages))

    max_y <- max(max(input_inferences_longer$upper), max(mc_inferences_longer$value), max(aggregate_inferences_longer$upper), na.rm = TRUE)
    max_y <- (ceiling(max_y*1000))/1000
    min_y <- min(min(input_inferences_longer$lower), min(mc_inferences_longer$value), min(aggregate_inferences_longer$lower), na.rm = TRUE)
    min_y <- (floor(min_y*1000))/1000

    aggregate_inferences_longer$analysis_source <- "Agg FCM Inferences"
    mc_inferences_longer$analysis_source <- "MC FCM Inferences"
    mc_avg_inferences_longer$analysis_source <- "MC FCM Avg Inferences"
    mc_inference_CIs$analysis_source <- "CIs of MC FCM Avg Inferences"
  }

  list(
    fcm_class_subtitle = fcm_class_subtitle,
    input_inferences = input_inferences_longer,
    aggregate_inferences = aggregate_inferences_longer,
    mc_inferences = mc_inferences_longer,
    mc_avg_inferences = mc_avg_inferences_longer,
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
#'
#' @returns An autoplot plot of an fcmconfr object's results
#'
#' @export
#' @examples
#' NULL
autoplot.fcmconfr <- function(object, ...) {
                              # additional_inputs = list(
                              #   filter_limit
                              #   coord_flip
                              #
                              # )

  # Parse additional_inputs ----
  additional_inputs <- list(...)[[1]]

  # Plot formatting parameters
  filter_limit <- additional_inputs$filter_limit
  coord_flip <- additional_inputs$coord_flip
  text_font_size <- additional_inputs$text_font_size
  # Plot aesthetic parameters
  mc_avg_and_CIs_color <- additional_inputs$mc_avg_and_CIs_color
  mc_inferences_color <- additional_inputs$mc_inferences_color
  mc_inferences_shape <- additional_inputs$mc_inferences_shape
  ind_inferences_color <- additional_inputs$ind_inferences_color
  ind_inferences_shape <- additional_inputs$ind_inferences_shape
  agg_inferences_color <- additional_inputs$agg_inferences_color
  agg_inferences_shape <- additional_inputs$agg_inferences_shape
  ind_ivfn_and_tfn_linewidth <- additional_inputs$ind_ivfn_and_tfn_linewidth
  agg_ivfn_and_tfn_linewidth <- additional_inputs$agg_ivfn_and_tfn_linewidth

  if (object$fcm_class == "ivfn") {
    ind_inferences_shape <- NA
    agg_inferences_shape <- NA
  }
  # ----

  # Get Plotting Data ----
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
  # ----

  ggplot_main <- ggplot() +
    ggplot2::geom_vline(xintercept = zero_intercept, linetype = "dotted", size = 0.5)

  # MC Avg Ingerences CIs ----
  if (inputs_agg_and_mc_w_bs) {
    ggplot_main <- ggplot_main +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$lower_CI, x = .data$lower_CI, xmax = .data$lower_CI, linewidth = .data$analysis_source),
        width = 0.7, color = mc_avg_and_CIs_color,
        na.rm = FALSE, key_glyph = ggplot2::draw_key_vline
      ) +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_inference_CIs),
        aes(y = .data$name, xmin = .data$upper_CI, x = .data$upper_CI, xmax = .data$upper_CI, linewidth = .data$analysis_source),
        width = 0.7, color = mc_avg_and_CIs_color,
        na.rm = TRUE, key_glyph = ggplot2::draw_key_vline
      )
  }
  # ----

  # MC FCM Inferences ----
  if (inputs_agg_and_mc_no_bs | inputs_agg_and_mc_w_bs) {
    ggplot_main <- ggplot_main +
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$mc_inferences),
        aes(y = .data$name, x = .data$value, color = .data$analysis_source, shape = .data$analysis_source),
        position = ggplot2::position_dodge2(width = 0.25), alpha = 0.6,
        # shape = 3,
        na.rm = FALSE
      ) +
      ggplot2::geom_crossbar(
        data = ggplot2::remove_missing(plot_data$mc_avg_inferences),
        aes(y = .data$name, xmin = .data$value, x = .data$value, xmax = .data$value),
        width = 0.9, linewidth = 0.1, color = mc_avg_and_CIs_color, na.rm = FALSE, key_glyph = ggplot2::draw_key_vline
      )
  }
  # ----

  # Individual FCM Inferences ----
  if (object$fcm_class == "conventional") {
    ggplot_main <- ggplot_main +
      ggplot2::geom_point(
        data = ggplot2::remove_missing(plot_data$input_inferences),
        position = ggplot2::position_dodge2(width = 0.1),
        aes(y = .data$name, x = .data$value, color = .data$analysis_source, shape = .data$analysis_source),
        size = 2, na.rm = TRUE
      )
  } else if (object$fcm_class == "ivfn") {
    ggplot_main <- ggplot_main +
      ggplot2::geom_linerange(
        data = ggplot2::remove_missing(plot_data$input_inferences),
        aes(y = .data$name, xmin = .data$lower, xmax = .data$upper, color = .data$analysis_source),
        position = ggplot2::position_dodge2(width = 0.5), linewidth = ind_ivfn_and_tfn_linewidth
      )
  } else if (object$fcm_class == "tfn") {
    ggplot_main <- ggplot_main +
      ggplot2::geom_pointrange(
        data = ggplot2::remove_missing(plot_data$input_inferences),
        aes(y = .data$name, xmin = .data$lower, x = .data$mode, xmax = .data$upper, color = .data$analysis_source, shape = .data$analysis_source),
        position = ggplot2::position_dodge2(width = 0.5), fatten = 0.6, linewidth = ind_ivfn_and_tfn_linewidth
      )
  }
  # ----

  # Aggregate FCM Inferences ----
  if (!inputs_only) {
    if (object$fcm_class == "conventional") {
      ggplot_main <- ggplot_main +
        ggplot2::geom_point(
          data = ggplot2::remove_missing(plot_data$aggregate_inferences),
          aes(y = .data$name, x = .data$value, color = .data$analysis_source, shape = .data$analysis_source),
          size = 2,
        )
    } else if (object$fcm_class == "ivfn") {
      ggplot_main <- ggplot_main +
        ggplot2::geom_linerange(
          data = ggplot2::remove_missing(plot_data$aggregate_inferences),
          aes(y = .data$name, xmin = .data$lower, xmax = .data$upper, color = .data$analysis_source),
          linewidth = agg_ivfn_and_tfn_linewidth
        )
    } else if (object$fcm_class == "tfn") {
      ggplot_main <- ggplot_main +
        ggplot2::geom_pointrange(
          data = ggplot2::remove_missing(plot_data$aggregate_inferences),
          aes(y = .data$name, xmin = .data$lower, x = .data$mode, xmax = .data$upper, color = .data$analysis_source, shape = .data$analysis_source),
          fatten = 2, linewidth = agg_ivfn_and_tfn_linewidth
        )
    }
  }
  # ----

  # Setup Legend Scales ----
  scale_color_manual_values_str <- paste0("c('Ind FCM Inferences' = ind_inferences_color")
  scale_shape_manual_values_str <- paste0("c('Ind FCM Inferences' = ind_inferences_shape")
  scale_shape_manual_override_str <- paste0("c(ind_inferences_shape")
  scale_linewidth_maual_values_str <- paste0("c('Ind FCM Inferences' = ind_ivfn_and_tfn_linewidth")
  scale_breaks_values_str <- paste0("c('Ind FCM Inferences'")
  if (!inputs_only) {
    scale_color_manual_values_str <- paste0(scale_color_manual_values_str, ", 'Agg FCM Inferences' = agg_inferences_color")
    scale_shape_manual_values_str <- paste0(scale_shape_manual_values_str, ", 'Agg FCM Inferences' = agg_inferences_shape")
    scale_shape_manual_override_str <- paste0(scale_shape_manual_override_str, ", agg_inferences_shape")
    scale_linewidth_maual_values_str <- paste0(scale_linewidth_maual_values_str, ", 'Agg FCM Inferences' = ind_ivfn_and_tfn_linewidth")
    scale_breaks_values_str <- paste0(scale_breaks_values_str, ", 'Agg FCM Inferences'")
  }
  if (inputs_agg_and_mc_no_bs | inputs_agg_and_mc_w_bs) {
    scale_color_manual_values_str <- paste0(scale_color_manual_values_str, ", 'MC FCM Inferences' = mc_inferences_color")
    scale_shape_manual_values_str <- paste0(scale_shape_manual_values_str, ", 'MC FCM Inferences' = mc_inferences_shape")
    scale_shape_manual_override_str <- paste0(scale_shape_manual_override_str, ", mc_inferences_shape")
    scale_linewidth_maual_values_str <- paste0(scale_linewidth_maual_values_str, ", 'MC FCM Inferences' = NA")
    scale_breaks_values_str <- paste0(scale_breaks_values_str, ", 'MC FCM Inferences'")
  }
  scale_color_manual_values_str <- paste0(scale_color_manual_values_str, ")")
  scale_shape_manual_values_str <- paste0(scale_shape_manual_values_str, ")")
  scale_shape_manual_override_str <- paste0(scale_shape_manual_override_str, ")")
  scale_linewidth_maual_values_str <- paste0(scale_linewidth_maual_values_str, ")")
  scale_breaks_values_str <- paste0(scale_breaks_values_str, ")")

  if (object$fcm_class == "conventional") {
    scales_str <- paste0(
      "ggplot_main +
        ggplot2::scale_color_manual(
          values = ", scale_color_manual_values_str, ",
          breaks = ", scale_breaks_values_str, ",
          guide = ggplot2::guide_legend(order = 1)
        ) +
        ggplot2::scale_shape_manual(
          values = ", scale_shape_manual_values_str, ",
          breaks = ", scale_breaks_values_str, ",
          guide = ggplot2::guide_legend(order = 1)
        ) +
        ggplot2::scale_linewidth_manual(
          values = c('CIs of MC FCM Avg Inferences' = 0.1),
          guide = ggplot2::guide_legend(order = 2)
        )"
    )
  } else if (object$fcm_class == "ivfn") {
    scales_str <- paste0(
      "ggplot_main +
        ggplot2::scale_color_manual(
          values = ", scale_color_manual_values_str, ",
          breaks = ", scale_breaks_values_str, ",
          guide = ggplot2::guide_legend(
            override.aes = list(
              shape = ", scale_shape_manual_override_str, ",
              linewidth = ", scale_linewidth_maual_values_str, "
          ), order = 1)
        ) +
        ggplot2::scale_shape_manual(
          values = ", scale_shape_manual_values_str, ",
          breaks = ", scale_breaks_values_str, ",
          guide = 'none'
        ) +
        ggplot2::scale_linewidth_manual(
          values = c('CIs of MC FCM Avg Inferences' = 0.1),
          guide = ggplot2::guide_legend(order = 2)
        )"
    )
  } else if (object$fcm_class == "tfn") {
    scales_str <- paste0(
      "ggplot_main +
        ggplot2::scale_color_manual(
          values = ", scale_color_manual_values_str, ",
          breaks = ", scale_breaks_values_str, ",
          guide = ggplot2::guide_legend(
            override.aes = list(
              shape = ", scale_shape_manual_override_str, ",
              linewidth = ", scale_linewidth_maual_values_str, "
          ), order = 1)
        ) +
        ggplot2::scale_shape_manual(
          values = ", scale_shape_manual_values_str, ",
          breaks = ", scale_breaks_values_str, ",
          #guide = ggplot2::guide_legend(order = 1)
          guide = 'none'
        ) +
        ggplot2::scale_linewidth_manual(
          values = c('CIs of MC FCM Avg Inferences' = 0.1),
          guide = ggplot2::guide_legend(order = 2)
        )"
    )
  }

  scales_expr <- parse(text = scales_str)
  ggplot_main <- eval(scales_expr)
  # ----

  if (!coord_flip) {
    fcmconfr_plot <- ggplot_main + fcmconfr_default_theme()
  } else {
    fcmconfr_plot <- ggplot_main + fcmconfr_default_theme() + ggplot2::coord_flip()
  }

  if (!is.na(text_font_size)) {
    fcmconfr_plot <- fcmconfr_plot +
      ggplot2::theme(text = ggplot2::element_text(size = text_font_size))
  }

  fcmconfr_plot
}


#' Plot fcmconfr
#'
#' @description
#' Print the output of the autoplot function (\code{\link{autoplot.fcmconfr}})
#'
#' @param x A direct output of the \code{\link{fcmconfr}} function
#' @param ... Additional inputs:
#'  - interactive Open plot in interactive shiny app
#'  # Plot Formatting Parameters
#'  - filter_limit Remove concepts whose inferences do not exceed this value
#'  - coord_flip Swap x- and y-axes (i.e. rotate plot)
#'  - text_font_size (Mainly for shiny use, i.e. leave blank) Set text font size
#'  # Plot Aesthetic Parameters (and defaults)
#'  - mc_avg_and_CIs_color = "blue"
#'  - mc_inferences_color = "blue"
#'  - mc_inferences_shape = 3
#'  - ind_inferences_color = "black"
#'  - ind_inferences_shape = 16
#'  - agg_inferences_color = "red"
#'  - agg_inferences_shape = 17
#'  - ind_ivfn_and_tfn_linewidth = 0.1
#'  - agg_ivfn_and_tfn_linewidth = 0.6
#'
#' @importFrom graphics plot
#'
#' @returns A plot of an fcmconfr object's results
#'
#' @export
#' @examples
#' man/examples/ex-plot_fcmconfr.R
plot.fcmconfr <- function(x, ...) {
  additional_inputs = list(...)

  # input validation template functions ----
  is_not_logical <- function(input_value) {
    !(is.logical(input_value))
  }
  is_not_numeric <- function(input_value) {
    !(is.numeric(input_value))
  }
  is_not_positive <- function(input_value) {
    !(input_value >= 0)
  }
  is_not_integer <- function(input_value) {
    !(input_value %% 1 == 0)
  }
  is_not_char <- function(input_string) {
    !("character" %in% methods::is(input_string))
  }
  is_not_color <- function(input_color) {
    "try-error" %in% methods::is(try(grDevices::col2rgb(input_color), silent = TRUE))
  }
  is_not_shape_value <- function(input_shape) {
    !(input_shape %in% 0:25)
  }
  is_not_shape_string <- function(input_shape) {
    "try-error" %in% methods::is(try(ggplot2::translate_shape_string(input_shape), silent = TRUE))
  }
  # ----

  # interactive ----
  if (!"interactive" %in% names(additional_inputs)) {
    interactive = FALSE
    additional_inputs$interactive <- interactive
  }
  if (is_not_logical(additional_inputs$interactive)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var interactive} must be logical (TRUE/FALSE)",
      "+++++> Input {.var interactive} was of type: {methods::is(additional_inputs$interactive)[1]}"
    )))
  }
  # ----

  # Plot Format Parameters
  # filter_limit ----
  if (!("filter_limit" %in% names(additional_inputs))) {
    filter_limit = 1e-3
    additional_inputs$filter_limit <- filter_limit
  }
  if (is_not_numeric(additional_inputs$filter_limit)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var filter_limit} must be a positive numeric value",
      "+++++> Input {.var filter_limit} was of type: {methods::is(additional_inputs$filter_limit)[1]}"
    )))
  }
  if (additional_inputs$filter_limit <= 0) {
    stop(cli::format_error(c(
      "x" = "Error: {.var filter_limit} must be a positive numeric value greater than 0",
      "+++++> Input {.var filter_limit} was: {additional_inputs$filter_limit}"
    )))
  }
  # ----

  # coord_flip ----
  if (!("coord_flip" %in% names(additional_inputs))) {
    coord_flip = FALSE
    additional_inputs$coord_flip <- coord_flip
  }
  if (is_not_logical(additional_inputs$coord_flip)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var coord_flip} must be logical (TRUE/FALSE)",
      "+++++> Input {.var coord_flip} was of type: {methods::is(additional_inputs$coord_flip)[1]}"
    )))
  }
  # ----

  # text_font_size ----
  if (!("text_font_size" %in% names(additional_inputs))) {
    text_font_size = NA
    additional_inputs$text_font_size <- text_font_size
  }
  if (!is.na(additional_inputs$text_font_size)) {
    if (is_not_numeric(additional_inputs$text_font_size)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var text_font_size} must be a positive numeric value",
        "+++++> Input {.var text_font_size} was of type: {methods::is(additional_inputs$text_font_size)[1]}"
      )))
    }
    if (additional_inputs$text_font_size <= 0) {
      stop(cli::format_error(c(
        "x" = "Error: {.var text_font_size} must be a positive numeric value greater than 0",
        "+++++> Input {.var text_font_size} was: {additional_inputs$text_font_size}"
      )))
    }
  }
  # ----


  # Plot Aesthetic Parameters
  # mc_avg_and_CIs_color ----
  if (!"mc_avg_and_CIs_color" %in% names(additional_inputs)) {
    mc_avg_and_CIs_color <- "blue"
    additional_inputs$mc_avg_and_CIs_color <- mc_avg_and_CIs_color
  }
  if (is_not_char(additional_inputs$mc_avg_and_CIs_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var mc_avg_and_CIs_color} must be a string (i.e. of type 'char')",
      "+++++> Input {.var mc_avg_and_CIs_color} was: {methods::is(additional_inputs$mc_avg_and_CIs_color)[1]}"
    )))
  }
  if (is_not_color(additional_inputs$mc_avg_and_CIs_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var mc_avg_and_CIs_color} must be a valid color",
      "+++++> Input {.var mc_avg_and_CIs_color} was '{additional_inputs$mc_avg_and_CIs_color}'"
    )))
  }
  # ----

  # mc_inferences_color ----
  if (!"mc_inferences_color" %in% names(additional_inputs)) {
    mc_inferences_color <- "blue"
    additional_inputs$mc_inferences_color <- mc_inferences_color
  }
  if (is_not_char(additional_inputs$mc_inferences_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var mc_inferences_color} must be a string (i.e. of type 'char')",
      "+++++> Input {.var mc_inferences_color} was: {methods::is(additional_inputs$mc_inferences_color)[1]}"
    )))
  }
  if (is_not_color(additional_inputs$mc_inferences_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var mc_inferences_color} must be a valid color",
      "+++++> Input {.var mc_inferences_color} was '{additional_inputs$mc_inferences_color}'"
    )))
  }
  # ----

  # mc_inferences_shape ----
  if (!"mc_inferences_shape" %in% names(additional_inputs)) {
    mc_inferences_shape <- 3
    additional_inputs$mc_inferences_shape <- mc_inferences_shape
  }
  if (is.numeric(additional_inputs$mc_inferences_shape)) {
    if (is_not_integer(additional_inputs$mc_inferences_shape) | is_not_shape_value(additional_inputs$mc_inferences_shape)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var mc_inferences_shape} must be a either an integer or a valid character string",
        "+++++> Integer inputs must be a valid pch value (i.e. 0:25)",
        "+++++> Input {.var mc_inferences_shape} was: {additional_inputs$mc_inferences_shape}",
        "+++++> Run ?points or ?gglpot2::translate_shape_string in the console for additional info"
      )))
    }
  } else {
    if (is_not_char(additional_inputs$mc_inferences_shape) | is_not_shape_string(additional_inputs$mc_inferences_shape)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var mc_inferences_shape} must be a either an integer or a valid character string",
        "+++++> Character string inputs must reference a valid pch value",
        "+++++> Input {.var mc_inferences_shape} was: '{additional_inputs$mc_inferences_shape}'",
        "+++++> Run ?points or ?gglpot2::translate_shape_string in the console for additional info"
      )))
    } else {
      additional_inputs$mc_inferences_shape <- ggplot2::translate_shape_string(additional_inputs$mc_inferences_shape)
    }
  }
  # ----

  # ind_inferences_color ----
  if (!"ind_inferences_color" %in% names(additional_inputs)) {
    ind_inferences_color = "black"
    additional_inputs$ind_inferences_color <- ind_inferences_color
  }
  if (is_not_char(additional_inputs$ind_inferences_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var ind_inferences_color} must be a string (i.e. of type 'char')",
      "+++++> Input {.var ind_inferences_color} was: {methods::is(additional_inputs$ind_inferences_color)[1]}"
    )))
  }
  if (is_not_color(additional_inputs$ind_inferences_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var ind_inferences_color} must be a valid color",
      "+++++> Input {.var ind_inferences_color} was '{additional_inputs$ind_inferences_color}'"
    )))
  }
  # ----

  # ind_inferences_shape ----
  if (!"ind_inferences_shape" %in% names(additional_inputs)) {
    ind_inferences_shape <- 16
    additional_inputs$ind_inferences_shape <- ind_inferences_shape
  }
  if (is.numeric(additional_inputs$ind_inferences_shape)) {
    if (is_not_integer(additional_inputs$ind_inferences_shape) | is_not_shape_value(additional_inputs$ind_inferences_shape)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var ind_inferences_shape} must be a either an integer or a valid character string",
        "+++++> Integer inputs must be a valid pch value (i.e. 0:25)",
        "+++++> Input {.var ind_inferences_shape} was: {additional_inputs$ind_inferences_shape}",
        "+++++> Run ?points or ?gglpot2::translate_shape_string in the console for additional info"
      )))
    }
  } else {
    if (is_not_char(additional_inputs$ind_inferences_shape) | is_not_shape_string(additional_inputs$ind_inferences_shape)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var ind_inferences_shape} must be a either an integer or a valid character string",
        "+++++> Character string inputs must reference a valid pch value",
        "+++++> Input {.var ind_inferences_shape} was: '{additional_inputs$ind_inferences_shape}'",
        "+++++> Run ?points or ?gglpot2::translate_shape_string in the console for additional info"
      )))
    } else {
      additional_inputs$ind_inferences_shape <- ggplot2::translate_shape_string(additional_inputs$ind_inferences_shape)
    }
  }
  # ----

  # agg_inferences_color ----
  if (!"agg_inferences_color" %in% names(additional_inputs)) {
    agg_inferences_color = "red"
    additional_inputs$agg_inferences_color <- agg_inferences_color
  }
  if (is_not_char(additional_inputs$agg_inferences_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var agg_inferences_color} must be a string (i.e. of type 'char')",
      "+++++> Input {.var agg_inferences_color} was: {methods::is(additional_inputs$agg_inferences_color)[1]}"
    )))
  }
  if (is_not_color(additional_inputs$agg_inferences_color)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var agg_inferences_color} must be a valid color",
      "+++++> Input {.var agg_inferences_color} was '{additional_inputs$agg_inferences_color}'"
    )))
  }
  # ----

  # agg_inferences_shape ----
  if (!"agg_inferences_shape" %in% names(additional_inputs)) {
    agg_inferences_shape = 17
    additional_inputs$agg_inferences_shape <- agg_inferences_shape
  }
  if (is.numeric(additional_inputs$agg_inferences_shape)) {
    if (is_not_integer(additional_inputs$agg_inferences_shape) | is_not_shape_value(additional_inputs$agg_inferences_shape)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var agg_inferences_shape} must be a either an integer or a valid character string",
        "+++++> Integer inputs must be a valid pch value (i.e. 0:25)",
        "+++++> Input {.var agg_inferences_shape} was: {additional_inputs$agg_inferences_shape}",
        "+++++> Run ?points or ?gglpot2::translate_shape_string in the console for additional info"
      )))
    }
  } else {
    if (is_not_char(additional_inputs$agg_inferences_shape) | is_not_shape_string(additional_inputs$agg_inferences_shape)) {
      stop(cli::format_error(c(
        "x" = "Error: {.var agg_inferences_shape} must be a either an integer or a valid character string",
        "+++++> Character string inputs must reference a valid pch value",
        "+++++> Input {.var agg_inferences_shape} was: '{additional_inputs$agg_inferences_shape}'",
        "+++++> Run ?points or ?gglpot2::translate_shape_string in the console for additional info"
      )))
    } else {
      additional_inputs$agg_inferences_shape <- ggplot2::translate_shape_string(additional_inputs$agg_inferences_shape)
    }
  }
  # ----

  # ind_ivfn_and_tfn_linewidth ----
  if (!"ind_ivfn_and_tfn_linewidth" %in% names(additional_inputs)) {
    ind_ivfn_and_tfn_linewidth = 0.1
    additional_inputs$ind_ivfn_and_tfn_linewidth <- ind_ivfn_and_tfn_linewidth
  }
  if (is_not_numeric(additional_inputs$ind_ivfn_and_tfn_linewidth)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var ind_ivfn_and_tfn_linewidth} must be a value greater than or equal to 0 (i.e. positive)",
      "+++++> Input {.var ind_ivfn_and_tfn_linewidth} was: {additional_inputs$ind_ivfn_and_tfn_linewidth}"
    )))
  }
  if (is_not_positive(additional_inputs$ind_ivfn_and_tfn_linewidth)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var ind_ivfn_and_tfn_linewidth} must be a value greater than or equal to 0 (i.e. positive)",
      "+++++> Input {.var ind_ivfn_and_tfn_linewidth} was: {additional_inputs$ind_ivfn_and_tfn_linewidth}"
    )))
  }
  # ----

  # agg_ivfn_and_tfn_linewidth ----
  if (!"agg_ivfn_and_tfn_linewidth" %in% names(additional_inputs)) {
    agg_ivfn_and_tfn_linewidth = 0.6
    additional_inputs$agg_ivfn_and_tfn_linewidth <- agg_ivfn_and_tfn_linewidth
  }
  if (is_not_numeric(additional_inputs$agg_ivfn_and_tfn_linewidth)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var agg_ivfn_and_tfn_linewidth} must be a value greater than or equal to 0 (i.e. positive)",
      "+++++> Input {.var agg_ivfn_and_tfn_linewidth} was: {additional_inputs$agg_ivfn_and_tfn_linewidth}"
    )))
  }
  if (is_not_positive(additional_inputs$agg_ivfn_and_tfn_linewidth)) {
    stop(cli::format_error(c(
      "x" = "Error: {.var agg_ivfn_and_tfn_linewidth} must be a value greater than or equal to 0 (i.e. positive)",
      "+++++> Input {.var agg_ivfn_and_tfn_linewidth} was: {additional_inputs$agg_ivfn_and_tfn_linewidth}"
    )))
  }
  # ----

  # Check for Invalid Parameters ----
  acceptable_inputs <- c(
    "interactive",
    # Plot Format Parameters
    "filter_limit", "coord_flip", "text_font_size",
    # Plot Aesthetic Parameters
    "mc_avg_and_CIs_color",
    "mc_inferences_color", "mc_inferences_shape",
    "ind_inferences_color", "ind_inferences_shape",
    "agg_inferences_color", "agg_inferences_shape",
    "ind_ivfn_and_tfn_linewidth", "agg_ivfn_and_tfn_linewidth"
  )
  if (!all(names(additional_inputs) %in% acceptable_inputs)) {
    unacceptable_inputs <- names(additional_inputs)[!(names(additional_inputs) %in% acceptable_inputs)]
    stop(cli::format_error(c(
      "x" = "Error: Invalid additional parameter given for plot.fcmconfr",
      "+++++> Invalid Additional Input(s): {unacceptable_inputs}",
      "",
      "+++++> Additional Input Options Include: {acceptable_inputs}"
    )))
  }
  # ----

  if (!additional_inputs$interactive) {
    suppressWarnings(print(autoplot(x, additional_inputs)))
  } else {
    suppressWarnings(interactive_plot_fcmconfr(x, additional_inputs))
  }
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
#'
#' @returns A custom ggplot2 theme for fcmconfr plot objects
#'
#' @export
#' @examples
#' NULL
fcmconfr_theme_custom <- function(...) {
  ggplot2::theme_classic(...) %+replace%
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 20, r = 40, b = 20, l = 20),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.justification = "center",
      legend.spacing = ggplot2::unit(0.001, 'cm')
    )
}


#' Default plot.fcmconfr Theme
#'
#' @description
#' A formal call to the custom_theme defined by \code{\link{fcmconfr_theme_custom}}
#'
#' @returns A default ggplot2 theme for fcmconfr plot objects
#'
#' @export
#' @examples
#' NULL
fcmconfr_default_theme <- function() {
  fcmconfr_theme_custom()
}


#' (Interactive) Plot fcmconfr
#'
#' @description
#' Load plot of fcmconfr output in an interactive shiny window
#'
#' @param x A direct output of the \code{\link{fcmconfr}} function
#' @param ... Additional inputs:
#'  - filter_limit Remove concepts whose inferences do not exceed this value
#'  - coord_flip Swap x- and y-axes (i.e. rotate plot)
#'
#' @importFrom graphics plot
#'
#' @returns A shiny window displaying a plot of an fcmconfr object's results
#'
#' @export
#' @examples
#' NULL
interactive_plot_fcmconfr <- function(x, ...) {
  # These bslib and shinyWidgets calls are only here to be acknowledged in
  # R CMD Check, they have no impact on the rest of the function and can be
  # ignored.
  bslib::versions()
  shinyWidgets::animations

  #assign("iplot_fcmconfr_obj", x, envir = .GlobalEnv)
  #assign("iplot_additional_inputs", as.list(...), envir = .GlobalEnv)
  shiny::runApp(appDir = system.file(file.path('shiny', 'interactive_fcmconfr_plot'), package = 'fcmconfr'))
  #rm(list = c("iplot_fcmconfr_obj", "iplot_additional_inputs"), envir = .GlobalEnv)
}

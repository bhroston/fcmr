#' shiny_server
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @param input the data streamed into the server from the ui
#' @param output the data streamed from to the ui from the server
#' @param session data surrounding the shiny instance itself
shiny_iplot_server <- function(input, output, session) {

  # iplot_fcmconfr_env <- shiny::reactive({
  #   # as.list(rlang::search_envs()[[which(names(rlang::search_envs()) == "package:fcmconfr")]])
  #   # as.list(rlang::search_envs()[[which(names(rlang::search_envs()) == "iplot_env")]])
  # })

  iplot_fcmconfr_obj <- shiny::reactive({
    fcmconfr_output
  })

  iplot_additional_inputs <- shiny::reactive({
    additional_inputs
  })

  analyses_performed <- shiny::reactive({
    analyses <- c("input")
    if (iplot_fcmconfr_obj()$params$additional_opts$run_agg_calcs) {
      analyses <- c(analyses, "aggregate")
    }
    if (iplot_fcmconfr_obj()$params$additional_opts$run_mc_calcs) {
      analyses <- c(analyses, "mc")
    }
    if (iplot_fcmconfr_obj()$params$additional_opts$run_ci_calcs) {
      analyses <- c(analyses, "bs")
    }
    analyses
  })

  inferences_to_plot_button_labels <- shiny::reactive({
    button_labels <- c("Individual FCMs")
    if ("aggregate" %in% analyses_performed()) {
      button_labels <- c(button_labels, "Aggregate FCM")
    }
    if ("mc" %in% analyses_performed()) {
      button_labels <- c(button_labels, "Monte Carlo (MC) FCMs")
    }
    if ("bs" %in% analyses_performed()) {
      button_labels <- c(button_labels, "CIs about Avg MC Inferences")
    }
  })

  output$inferences_to_plot <- shiny::renderUI({
    shiny::checkboxGroupInput(
      inputId = "inferences_to_plot",
      label = "Select Inferences to Plot:",
      choiceNames = inferences_to_plot_button_labels(),
      choiceValues = analyses_performed(),
      selected = analyses_performed()
    )
  })

  iplot_fcmconfr_aes_colors <- shiny::reactive({
    if (!("input" %in% input$inferences_to_plot)) {
      iplot_ind_inferences_alpha <- 0
    } else {
      iplot_ind_inferences_alpha <- iplot_additional_inputs()$ind_inferences_alpha
    }

    if (!("aggregate" %in% input$inferences_to_plot)) {
      iplot_agg_inferences_alpha <- 0
    } else {
      iplot_agg_inferences_alpha <- iplot_additional_inputs()$agg_inferences_alpha
    }

    if (!("mc" %in% input$inferences_to_plot)) {
      iplot_mc_inferences_alpha <- 0
    } else {
      iplot_mc_inferences_alpha <- iplot_additional_inputs()$mc_inferences_alpha
    }

    if (!("bs" %in% input$inferences_to_plot)) {
      iplot_mc_avg_and_CIs_color <- "transparent"
    } else {
      iplot_mc_avg_and_CIs_color <- iplot_additional_inputs()$mc_avg_and_CIs_color
    }

    list(
      ind_inferences_alpha = iplot_ind_inferences_alpha,
      agg_inferences_alpha = iplot_agg_inferences_alpha,
      mc_inferences_alpha = iplot_mc_inferences_alpha,
      mc_avg_and_CIs_color = iplot_mc_avg_and_CIs_color
    )
  })

  output$plot_font_size_numeric_input <- shiny::renderUI({
    shiny::numericInput(
      "plot_font_size",
      label = "Plot Text Font Size",
      value = iplot_additional_inputs()$text_font_size,
      min = 5,
      max = 100,
      step = 0.5
    )
  })

  iplot_fcmconfr_text_font_size <- shiny::reactive({
    input$plot_font_size
  })

  output$fcmconfr_plot_output <- shiny::renderPlot({
    if (!is.null(iplot_fcmconfr_obj())) {
      plot(iplot_fcmconfr_obj(),
           interactive = FALSE,
           # Plot Formatting Parameters
           filter_limit = iplot_additional_inputs()$filter_limit,
           coord_flip = iplot_additional_inputs()$coord_flip,
           text_font_size = iplot_fcmconfr_text_font_size(),
           # Plot Aesthetic Parameters
           mc_avg_and_CIs_color = iplot_fcmconfr_aes_colors()$mc_avg_and_CIs_color,
           mc_inferences_color = iplot_additional_inputs()$mc_inferences_color,
           mc_inferences_alpha = iplot_fcmconfr_aes_colors()$mc_inferences_alpha,
           mc_inferences_shape = iplot_additional_inputs()$mc_inferences_shape,
           ind_inferences_color = iplot_additional_inputs()$ind_inferences_color,
           ind_inferences_alpha = iplot_fcmconfr_aes_colors()$ind_inferences_alpha,
           ind_inferences_shape = iplot_additional_inputs()$ind_inferences_shape,
           agg_inferences_color = iplot_additional_inputs()$agg_inferences_color,
           agg_inferences_alpha = iplot_fcmconfr_aes_colors()$agg_inferences_alpha,
           agg_inferences_shape = iplot_additional_inputs()$agg_inferences_shape,
           ind_ivfn_and_tfn_linewidth = iplot_additional_inputs()$ind_ivfn_and_tfn_linewidth,
           agg_ivfn_and_tfn_linewidth = iplot_additional_inputs()$agg_ivfn_and_tfn_linewidth
      )
    }
  })
}

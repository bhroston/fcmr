
ui <- shiny::fluidPage(
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      width = sidebar_width,
      shiny::fluidRow(
        shiny::h4("Select Nodes to Display"),
        shiny::uiOutput("nodes_to_display")
      ),
      shiny::fluidRow(
        shiny::h4("Additional Options"),
        shiny::selectInput("node_shape", "Node shape:", choices = c("dot", "box", "text"), selected = "dot"),
        shiny::numericInput("node_size", "Node size:", value = 25, min = 1, step = 5)
      ),
      shiny::fluidRow(
        shiny::sliderInput("edge_roundness", "Edge roundness:", value = 0.5, step = 0.1, min = 0, max = 1),
        shiny::selectInput(
          "edge_smoothing", "Edge Smoothing Method:",
          choices = c('dynamic', 'continuous', 'discrete', 'diagonalCross', 'straightCross',
                      'horizontal', 'vertical', 'curvedCW', 'curvedCCW', 'cubicBezier'),
          selected = "continuous"
        )
      ),
      shiny::fluidRow(
        shiny::numericInput("font_size", "Font size:", value = 14, step = 1)
      )
    ),
    bslib::page_fillable(
      visNetwork::visNetworkOutput("fcm_display", width = "90vw", height = "90vw")
    )
  )
)

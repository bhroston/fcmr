#' shiny_ui
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
shiny_iplot_ui <- function() {
  bslib::page_sidebar(
    title = "FCMConfR Plot (Interactive)",
    sidebar = bslib::sidebar(
      width = "300px",
      shiny::uiOutput("inferences_to_plot"),
      shiny::uiOutput("plot_font_size_numeric_input")
    ),
    shiny::fluidPage(
      shiny::plotOutput("fcmconfr_plot_output", height = "600px")
    )
  )
}

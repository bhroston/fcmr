
server <- function(input, output) {

  output$fcm_display <- visNetwork::renderVisNetwork({
    fcm_as_network_obj
  })

  shiny::observe({
    shiny::invalidateLater(1000)
    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visGetPositions()
  })

  coords <- shiny::reactive({
    if (!is.null(input$fcm_display_positions)) {
      data.frame(do.call(rbind, input$fcm_display_positions))
    }
  })

  # shiny::observe({
  #   if (!is.null(input$fcm_display_positions)) {
  #     cat("\n\n\n\n\n")
  #     print(coords())
  #   }
  # })

  updated_fcm_visNetwork <- shiny::reactive({
    if (!is.null(input$fcm_display_positions)) {
      updated_fcm_as_network_obj <- fcm_as_network_obj
      updated_fcm_as_network_obj$x$nodes$x <- coords()$x
      updated_fcm_as_network_obj$x$nodes$y <- coords()$y
      updated_fcm_as_network_obj
    }
  })

  # shiny::observe({
  #   print(updated_fcm_visNetwork())
  # })

  shiny::onStop(
    function() {
      assign(
        x = "view_fcm_visNetwork",
        value = shiny::isolate(updated_fcm_visNetwork()),
        envir = sys.frame()
      )
      print("Note: visNetwork plot stored as fcm_view_visNetwork in Global Environment", quote = FALSE)
    }
  )
}

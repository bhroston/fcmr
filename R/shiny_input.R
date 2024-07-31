# setup_fcmconfr <- function() {
#   ui <- shiny::fluidPage(
#     shiny::selectInput("fcm_adj_matrices", "FCM Adj. Matrices", choices = names(as.list(.GlobalEnv)))
#   )
#
#   server <- function(input, output, session) {
#     fcm_adj_matrices <- reactive({input$fcm_adj_matrices})
#   }
#
#   shiny::shinyApp(ui, server)
# }

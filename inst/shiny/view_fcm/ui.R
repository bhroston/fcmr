
ui <- shiny::fluidPage(
  visNetwork::visNetworkOutput("fcm_display", width = "90vw", height = "90vw")
)

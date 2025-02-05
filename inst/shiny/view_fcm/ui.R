
ui <- shiny::fluidPage(
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      shiny::uiOutput("nodes_to_display")
    ),
    bslib::page_fillable(
      visNetwork::visNetworkOutput("fcm_display", width = "90vw", height = "90vw")
    )
  )
)

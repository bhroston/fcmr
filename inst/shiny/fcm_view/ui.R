
ui <- shiny::fluidPage(
  # bslib::card(
  #   shiny::p("Note: Store the call to fcm_view() in a variable to save changes to the visNetwork object."),
  #   shiny::HTML("<pre>fcm_view_output <- fcm_view(...)</pre>")
  # ),
  bslib::page_sidebar(
    # sidebar ----
    sidebar = bslib::sidebar(
      width = sidebar_width,
      shiny::fluidRow(
        shiny::column(
          width = 12, align = "center",
          shiny::actionButton(
            "save_visNetwork", "Close App", icon = shiny::icon("check"), width = "100%"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6, align = "center",
          shiny::uiOutput("show_node_labels")
          #shinyWidgets::prettyCheckbox("show_node_labels", label = "Node labels", value = TRUE, inline = TRUE, status = "primary")
        ),
        shiny::column(
          width = 6, align = "center",
          shiny::uiOutput("show_edge_labels")
          #shinyWidgets::prettyCheckbox("show_edge_labels", label = "Edge labels", value = TRUE, inline = TRUE, status = "primary")
        )
      ),
      shiny::fluidRow(
        shiny::h4("Select Nodes to Display"),
        shiny::uiOutput("nodes_to_display")
      ),
      shiny::fluidRow(
        shiny::h4("Additional Options"),
        shiny::uiOutput("node_shape"),
        shiny::uiOutput("node_size")
      ),
      shiny::fluidRow(
        shiny::uiOutput("font_size")
      ),
      shiny::fluidRow(
        shiny::uiOutput("arrow_size"),
        shiny::uiOutput("edge_roundness"),
        shiny::uiOutput("edge_smoothing")
      )
    ),
    # ----
    # visNetworkOutput ----
    bslib::page_fillable(
      visNetwork::visNetworkOutput("fcm_display", width = "90vw", height = "90vw")
    )
    # ----
  )
)

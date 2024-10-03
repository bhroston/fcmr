#' shiny_ui
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
shiny_ui <- function() {

  bslib::page_sidebar(
    title = "FCMconfR GUI",
    sidebar = bslib::sidebar(
      title = "Definitions", position = "right", open = FALSE
    ),
    bslib::navset_underline(
      id = "nav_panel",
      bslib::nav_panel(
        title = "Data", icon = shiny::icon("laptop-file"), # ----
        shiny::fluidRow(
          shiny::column(width = 12, div(style = "height:20px"))
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "center",
            shiny::selectInput("adj_matrices", "Adj. Matrix or List of Adj. Matrices", choices = c(names(as.list(.GlobalEnv)), ""), selected = "")
          ),
          shiny::column(
            width = 6,
            shiny::uiOutput("select_analyses_to_perform_ui")
          )
        ),
        shiny::uiOutput("rejected_adj_matrices_note"),
        bslib::navset_underline(
          bslib::nav_panel(
            title = "Initial State (Pulse) Vector", icon = shiny::icon("wave-square"),
            shiny::uiOutput("initial_state_vector_input_ui")
          ),
          bslib::nav_panel(
            title = "Clamping Vector", icon = shiny::icon("grip-lines"),
            shiny::uiOutput("clamping_vector_input_ui")
          )
        )
      ), # ----
      bslib::nav_panel(
        title = "Aggregation Options", icon = shiny::icon("user-group")
      ),
      bslib::nav_panel(
        title = "Monte Carlo Sampling Options", icon = shiny::icon("seedling")
      ),
      bslib::nav_panel(
        title = "Simulation Options", icon = shiny::icon("calculator")
      ),
      bslib::nav_panel(
        title = "Runtime Options", icon = shiny::icon("clock")
      ),
    )
  )
}

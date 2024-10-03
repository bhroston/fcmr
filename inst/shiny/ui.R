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
          shiny::selectInput("adj_matrices", "Adj. Matrix or List of Adj. Matrices", choices = c(names(as.list(.GlobalEnv)), ""), selected = "")
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
        title = "Agg. and Monte Carlo Options", icon = shiny::icon("layer-group"),
        bslib::card(
          id = "aggregation_options",
          shiny::fluidRow(
            shiny::column(
              width = 8, align = "left",
              shiny::HTML('<p><i class="fas fa-user-group" role="presentation" aria-label="user-group icon"></i>       Aggregation Options</p>')
            ),
            shiny::column(
              width = 4, align = "right",
              shiny::checkboxInput("perform_aggregation", "Aggregation Analaysis", value = TRUE),
            )
          ),
          shiny::uiOutput("aggregation_options_ui")
        ),
        bslib::card(
          id = "monte_carlo_options",
          shiny::fluidRow(
            shiny::column(
              width = 8, align = "left",
              shiny::HTML('<p><i class="fa-solid fa-seedling"></i>       Monte Carlo Options</p>')
            ),
            shiny::column(
              width = 4, align = "right",
              shiny::checkboxInput("perform_monte_carlo", "Monte Carlo Analysis", value = TRUE)
            )
          ),
          shiny::uiOutput("monte_carlo_options_ui")
        ),
        shiny::uiOutput("include_zero_edges_ui")
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

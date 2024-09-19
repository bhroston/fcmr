
#' shiny_ui
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
shiny_ui <- function() {
  bslib::page_fillable(
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Data", icon = shiny::icon("laptop-file"), #####
        shiny::fluidRow(
          shiny::column(width = 12, div(style = "height:20px"))
        ),
        shiny::selectInput("adj_matrix_list", "Adj. Matrix or List of Adj. Matrices", choices = c(names(as.list(.GlobalEnv)), ""), selected = ""),
        shiny::uiOutput("rejected_adj_matrix_list_note"),
        #shiny::conditionalPanel(
        #  condition = "!output.accepted_adj_matrix_list",
        #  shiny::uiOutput("rejected_adj_matrix_list_note")
        #),
        bslib::navset_underline(
          bslib::nav_panel(
            title = "Initial State (Pulse) Vector", icon = shiny::icon("wave-square"),
            shiny::fluidRow(
              shiny::column(
                width = 12, div(style = "height:15px"),
                shiny::p("\nThe Initial State (Pulse) Vector represents the starting 'activation' levels of each concept.")
              )
            ),
            shiny::conditionalPanel(
              condition = "output.accepted_adj_matrix_list",
              shiny::fluidRow(
                shiny::column(
                  width = 12, div(style = "height:20px"),
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6, align = "center",
                  shiny::uiOutput("initial_state_vector_numeric_inputs")
                ),
                shiny::column(
                  width = 6, align = "center",
                  shiny::tableOutput("initial_state_vector_table"),
                  shiny::actionButton("reset_initial_state_vectors", "Reset", icon = shiny::icon("rotate-right"))
                )
              )
            )
          ),
          bslib::nav_panel(
            title = "Clamping Vector", icon = shiny::icon("grip-lines"),
            shiny::fluidRow(
              shiny::column(
                width = 12, div(style = "height:15px"),
                shiny::p("\nThe Clamping Vector represents the steady 'activation' level or clamped 'activation' level for each concept throughout the simulation.")
              )
            ),
            shiny::conditionalPanel(
              condition = "output.accepted_adj_matrix_list",
              shiny::fluidRow(
                shiny::column(
                  width = 12, div(style = "height:20px"),
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6, align = "center",
                  shiny::uiOutput("clamping_vector_numeric_inputs")
                ),
                shiny::column(
                  width = 6, align = "center",
                  shiny::tableOutput("clamping_vector_table"),
                  shiny::actionButton("reset_initial_state_vectors", "Reset", icon = shiny::icon("rotate-right"))
                )
              )
            )
          )
        )
      ),#####
      bslib::nav_panel(
        title = "Simulation Options", icon = shiny::icon("calculator"), #####
        shiny::fluidRow(
          shiny::column(width = 12, div(style = "height:20px"))
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5, align = "right",
            shiny::h5("Activation Function", style = "padding: 35px;")
          ),
          shiny::column(
            width = 7, align = "left",
            shinyWidgets::radioGroupButtons("activation", "", choiceValues = c("kosko", "modified-kosko", "rescale"), choiceNames = c("Kosko", "Modified-Kosko", "Rescale"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5, align = "right",
            shiny::h5("Squashing Function", style = "padding: 35px;")
          ),
          shiny::column(
            width = 7, align = "left",
            shinyWidgets::radioGroupButtons("squashing", "", choices = c("sigmoid", "tanh"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5, align = "right",
            shiny::h5(paste0("Lambda (", "\U03BB", ")"), style = "padding: 28px;")
          ),
          shiny::column(
            width = 3, align = "left",
            shiny::numericInput("lambda", "", 1, min = 1, max = 10, step = 0.05)
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5, align = "right",
            shiny::h5(paste0("Max # of Iterations per Sim"), style = "padding: 28px;")
          ),
          shiny::column(
            width = 3, align = "left",
            shiny::numericInput("max_iter", "", 100, min = 1, step = 1)
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5, align = "right",
            shiny::h5(paste0("Min Acceptable Error per Step"), style = "padding: 28px;")
          ),
          shiny::column(
            width = 3, align = "left",
            shiny::numericInput("min_error", "", 1e-5, min = 0, max = 1)
          )
        )
      ), #####
      bslib::nav_panel(
        title = "Aggregation/Monte Carlo Options", icon = shiny::icon("layer-group"), #####
        shiny::fluidRow(
          shiny::column(width = 12, div(style = "height:20px"))
        ),
        p("User selects if they want to Aggregate via Traditional Aggregation or Estimate Sample Space w/ Monte Carlo"),
        bslib::navset_underline(
          bslib::nav_panel(
            title = "Monte Carlo Sampling Options", icon = shiny::icon("seedling"),
            shiny::column(
              width = 12,
              shiny::fluidRow(
                shiny::column(
                  width = 5, align = "right",
                  shiny::h5("# Sample Maps To Generate", style = "padding: 35px;")
                ),
                shiny::column(
                  width = 7, align = "left",
                  shiny::numericInput("monte_carlo_samples", "", value = 1000, min = 1, step = 500)
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 5, align = "right",
                  shiny::h5("Include 0's in MC Sampling", style = "padding: 35px;")
                ),
                shiny::column(
                  width = 7, align = "left",
                  shinyWidgets::radioGroupButtons("include_zeroes_in_mc_sampling", "", choices = c("Yes", "No"), selected = "No")
                )
              )
            )
          ),
          bslib::nav_panel(
            title = "Aggregation Options", icon = shiny::icon("user-group"),
            shiny::column(
              width = 12,
              shiny::fluidRow(
                shiny::column(
                  width = 5, align = "right",
                  shiny::h5("Aggregation Function", style = "padding: 35px;")
                ),
                shiny::column(
                  width = 7, align = "left",
                  shinyWidgets::radioGroupButtons("aggregation_fun", "", choices = c("Mean", "Median"), selected = "Mean"),
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 5, align = "right",
                  shiny::h5("Include 0's in Aggregation", style = "padding: 35px;")
                ),
                shiny::column(
                  width = 7, align = "left",
                  shinyWidgets::radioGroupButtons("include_zeroes_in_aggregation", "", choices = c("Yes", "No"), selected = "No")
                )
              )
            )
          )
        )
      ), #####
      bslib::nav_panel(
        title = "Bootstrap Options", icon = shiny::icon("chart-simple"), #####
        shiny::fluidRow(
          shiny::column(width = 12, div(style = "height:20px"))
        ),
        p("Bootstrap tab content"),
        shiny::fluidRow(
          shiny::column(
            width = 10, align = "center",
            shiny::h5("Estimate Confidence Interval Bounds about Monte Carlo Inferences?", style = "padding: 27px;")
          ),
          shiny::column(
            width = 2, align = "left",
            shinyWidgets::radioGroupButtons("bootstrap_inference_means_samples", "", choices = c("Yes", "No"), selected = "Yes", size = "sm")
          )
        ),
        shiny::conditionalPanel(
          condition = "input.bootstrap_inference_means_samples == 'Yes'",
          shiny::fluidRow(
            shiny::column(
              width = 5, align = "right",
              shiny::h5("Confidence Interval (CI)", style = "padding: 30px;")
            ),
            shiny::column(
              width = 7, align = "left",
              shiny::numericInput("bootstrap_CI", "", value = 0.95, min = 0, max = 1, step = 0.01)
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 5, align = "right",
              shiny::h5("# Draws per Bootstrap Rep.", style = "padding: 30px;")
            ),
            shiny::column(
              width = 7, align = "left",
              shiny::numericInput("bootstrap_draws_per_rep", "", value = 1000, min = 10, step = 100)
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 5, align = "right",
              shiny::h5("# Bootstrap Reps", style = "padding: 30px;")
            ),
            shiny::column(
              width = 7, align = "left",
              shiny::numericInput("bootstrap_reps", "", value = 1000, min = 10, step = 100)
            )
          )
        )
      ), #####
      bslib::nav_panel(
        title = "Runtime Options", icon = shiny::icon("clock"), #####
        shiny::fluidRow(
          shiny::column(width = 12, div(style = "height:20px"))
        ),
        p("Bootstrap tab content"),
        shiny::fluidRow(
          shiny::column(
            width = 5, align = "right",
            shiny::h5("Run using Parallel Processing", style = "padding: 35px;")
          ),
          shiny::column(
            width = 7, align = "left",
            shinyWidgets::radioGroupButtons("parallel", "", choices = c("Yes", "No"), selected = "No")
          )
        ),
        shiny::conditionalPanel(
          condition  = "input.parallel == 'Yes'",
          shiny::fluidRow(
            shiny::column(
              width = 5, align = "right",
              shiny::h5("# Cores to Use in Parallel", style = "padding: 35px;")
            ),
            shiny::column(
              width = 7, align = "left",
              shiny::numericInput("n_cores", "", value = parallel::detectCores(), min = 2, max = parallel::detectCores(), step = 1)
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 5, align = "right",
            shiny::h5("Show Progress at Runtime", style = "padding: 35px;")
          ),
          shiny::column(
            width = 7, align = "left",
            shinyWidgets::radioGroupButtons("show_progress", "", choices = c("Yes", "No"), selected = "Yes")
          )
        )
      ), #####
      footer = shiny::fluidRow(
        shiny::actionButton(
          "submit", "Submit", icon = shiny::icon("arrow-right-to-bracket")
        )
      )
    )
  )
}

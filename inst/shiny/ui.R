

shiny_ui <- function() {
  bslib::page_fillable(
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Data", icon = shiny::icon("laptop-file"),
        #####
        shiny::fluidRow(
          shiny::column(width = 12, div(style = "height:20px"))
        ),
        shiny::selectInput("adj_matrix_list", "Adj. Matrix or List of Adj. Matrices", choices = names(as.list(.GlobalEnv))),
        bslib::navset_underline(
          bslib::nav_panel(
            title = "Initial State (Pulse) Vector", icon = shiny::icon("wave-square"),
            shiny::fluidRow(
              shiny::column(
                width = 12, div(style = "height:15px"),
                shiny::p("\nThe Initial State (Pulse) Vector represents the starting 'activation' levels of each concept.")
              )
            ),
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
          ),
          bslib::nav_panel(
            title = "Clamping Vector", icon = shiny::icon("grip-lines"),
            shiny::fluidRow(
              shiny::column(
                width = 12, div(style = "height:15px"),
                shiny::p("\nThe Clamping Vector represents the steady 'activation' level or clamped 'activation' level for each concept throughout the simulation.")
              )
            ),
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
      ),#####
      bslib::nav_panel(
        title = "Simulation Options", icon = shiny::icon("calculator"),
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
            shinyWidgets::radioGroupButtons("activation", "", choices = c("Kosko", "Modified-Kosko", "Rescale"))
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
      ),
      bslib::nav_panel(title = "Aggregation/Uncertainty Options", icon = shiny::icon("layer-group"), p("Second tab content.")),
      bslib::nav_panel(title = "Bootstrap Options", icon = shiny::icon("seedling"), p("Third tab content")),
      bslib::nav_panel(title = "Runtime Options", icon = shiny::icon("clock"), p("Third tab content"))
    ),
    shiny::actionButton("submit", "Submit", icon = shiny::icon("arrow-right-to-bracket"))

    #bslib::nav_panel(
      #         title = "Inference Options",
      #         shiny::fluidPage(
      #           shiny::fluidRow(
      #             shiny::column(
      #               width = 4,
      #               shiny::selectInput("activation", shiny::HTML("Activation Function<br>(activation)"), choices = c("kosko", "modified-kosko", "rescale"), selected = "kosko"),
      #             ),
      #             shiny::column(
      #               width = 4,
      #               shiny::selectInput("squashing", shiny::HTML("Squashing Function<br>(squashing)"), choices = c("sigmoid", "tanh"), selected = "sigmoid")
      #             ),
      #             shiny::column(
      #               width = 4,
      #               shiny::numericInput("lambda", shiny::HTML("<br>Lambda"), value = 1, min = 0.001, step = 0.1)
      #             )
      #           ),
      #           shiny::fluidRow(
      #             shiny::column(
      #               width = 6,
      #               shiny::numericInput("max_iter", shiny::HTML("Max. # Iterations to Simulate FCM before Force-Stopping<br>(max_iter)"), value = 100, min = 10, step = 10)
      #             ),
      #             shiny::column(
      #               width = 6,
      #               shiny::numericInput("min_error", shiny::HTML("Min. Change in Values b/w Iterations to Stop Simulation Run<br>(min_error)"), value = 10^-5, min = 10^-10, max = 10^-3)
      #             )
      #           )
  )
}

# bslib::navset_pill_list(
#   bslib::nav_panel(
#     title = "Inference Options",
#     shiny::fluidPage()
#   ),
#   bslib::nav_panel(
#     title = "Monte Carlo Agg. Options",
#     shiny::fluidPage()
#   ),
#   bslib::nav_panel(
#     title = "Bootstrap Options",
#     shiny::fluidPage()
#   ),
#   bslib::nav_panel(
#     title = "Runtime Options",
#     shiny::fluidPage()
#   )
# )



# shiny_ui <- function() {
#   bslib::page_fluid(
#     title = "fcmconfr Input Manager",
#     theme = bslib::bs_theme(version = 5),
#     bslib::navset_pill_list(
#       bslib::nav_panel(
#         title = "Inference Options",
#         shiny::fluidPage(
#           shiny::fluidRow(
#             shiny::column(
#               width = 6,
#               shiny::selectInput("adj_matrices", shiny::HTML("Adjacency Matrices<br>(adj_matrices)"), choices = names(as.list(.GlobalEnv)))
#             ),
#             shiny::column(
#               width = 6,
#               shiny::selectInput("aggregation_fun", shiny::HTML("Aggregation function<br>(aggregation_fun)"), choices = c("mean", "median"), selected = "mean")
#             )
#           ),
#           shiny::fluidRow(
#             shiny::column(
#               width = 6,
#               shiny::textInput("initial_state_vector", shiny::HTML("Initial State (Pulse) Vector<br>(initial_state_vector)"), value = "c(1, 1, ..., 1)"),
#             ),
#             shiny::column(
#               width = 6,
#               shiny::textInput("clamping_vector", shiny::HTML("Clamping Vector<br>(clamping_vector)"), value = "c(1, 0, ..., 0)")
#             )
#           ),
#           shiny::fluidRow(
#             shiny::column(
#               width = 4,
#               shiny::selectInput("activation", shiny::HTML("Activation Function<br>(activation)"), choices = c("kosko", "modified-kosko", "rescale"), selected = "kosko"),
#             ),
#             shiny::column(
#               width = 4,
#               shiny::selectInput("squashing", shiny::HTML("Squashing Function<br>(squashing)"), choices = c("sigmoid", "tanh"), selected = "sigmoid")
#             ),
#             shiny::column(
#               width = 4,
#               shiny::numericInput("lambda", shiny::HTML("<br>Lambda"), value = 1, min = 0.001, step = 0.1)
#             )
#           ),
#           shiny::fluidRow(
#             shiny::column(
#               width = 6,
#               shiny::numericInput("max_iter", shiny::HTML("Max. # Iterations to Simulate FCM before Force-Stopping<br>(max_iter)"), value = 100, min = 10, step = 10)
#             ),
#             shiny::column(
#               width = 6,
#               shiny::numericInput("min_error", shiny::HTML("Min. Change in Values b/w Iterations to Stop Simulation Run<br>(min_error)"), value = 10^-5, min = 10^-10, max = 10^-3)
#             )
#           ),
#           shiny::actionButton("submit", "Submit")
#         )
#       ),
#       bslib::nav_panel(
#         title = "Monte Carlo Agg. Options",
#         shiny::fluidPage(
#           shiny::fluidRow(
#             shiny::column(
#               width = 5,
#               shiny::numericInput("samples", shiny::HTML("# Monte Carlo Models to Build<br>(samples)"), value = 1000, min = 1, step = 1)
#             ),
#             shiny::column(
#               width = 7,
#               shiny::checkboxInput("include_zeroes_in_aggregation", shiny::HTML("Include zero-value edges in aggregation<br>(include_zeroes_in_aggregations)"), value = FALSE)
#             )
#           ),
#           shiny::fluidRow(
#             shiny::checkboxInput("include_simulations_in_output", shiny::HTML("Output MC Simulations<br>(include_simulations_in_output)"), value = TRUE)
#           ),
#           shiny::fluidRow(
#             shiny::uiOutput("sampling")
#           )
#         )
#       ),
#       bslib::nav_panel(
#         title = "Bootstrap Options",
#         shiny::fluidPage(
#           shiny::fluidRow(
#             shiny::checkboxInput("bootstrap_inference_means", shiny::HTML("Estimate Confidence Interval about Mean Inferences<br>(bootstrap_inference_means)"), value = TRUE)
#           ),
#           shiny::conditionalPanel(
#             condition = "input.bootstrap_inference_means == true",
#             shiny::fluidRow(
#               shiny::numericInput("bootstrap_CI", shiny::HTML("Confidence Interval %<br>(bootstrap_CI)"), value = 0.95, min = 0, max = 1, step = 0.01)
#             ),
#             shiny::fluidRow(
#               shiny::numericInput("bootstrap_reps", shiny::HTML("# Bootstrap Repetitions<br>(bootstrap_reps)"), value = 1000, min = 10, step = 100)
#             ),
#             shiny::fluidRow(
#               shiny::numericInput("bootstrap_draws_per_rep", shiny::HTML("# Samples to Draw per Bootstrap<br>(boostrap_draws_per_rep)"), value = 1000, min = 10, step = 100)
#             )
#           )
#         )
#       ),
#       bslib::nav_panel(
#         title = "Runtime Options",
#         shiny::fluidPage(
#           shiny::checkboxInput("parallel", "Parallel Processing", value = TRUE),
#           shiny::conditionalPanel(
#             condition  = "input.parallel == true",
#             shiny::numericInput("n_cores", "# Cores to use for Parallel Processing", value = parallel::detectCores(), min = 2, max = parallel::detectCores(), step = 1)
#           ),
#           shiny::checkboxInput("show_progress", "Show Progress at Runtime", value = TRUE)
#         )
#       )
#     )
#   )
# }



#' setup_fcmconfr
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @details
#' [ADD DETAILS HERE!!!]
#'
#' @export
setup_fcmconfr <- function() {
  fields <- c(
    "adj_matrices",
    "aggregation_fun",
    "samples",
    "include_zeroes_in_aggregation",
    "initial_state_vector",
    "clamping_vector",
    "activation",
    "squashing",
    "lambda",
    "max_iter",
    "min_error",
    "bootstrap_inference_means",
    "bootstrap_CI",
    "bootstrap_reps",
    "bootstrap_draws_per_rep",
    "parallel",
    "n_cores",
    "show_progress",
    "include_zeroes_in_aggregation",
    "include_simulations_in_output",
    "sampling"
  )

  app <- shiny::shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      bslib::navset_pill_list(
        bslib::nav_panel(
          title = "Inference Options",
          shiny::fluidPage(
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectInput("adj_matrices", shiny::HTML("Adjacency Matrices<br>(adj_matrices)"), choices = names(as.list(.GlobalEnv)))
              ),
              shiny::column(
                width = 6,
                shiny::selectInput("aggregation_fun", shiny::HTML("Aggregation function<br>(aggregation_fun)"), choices = c("mean", "median"), selected = "mean")
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textInput("initial_state_vector", shiny::HTML("Initial State (Pulse) Vector<br>(initial_state_vector)"), value = "c(1, 1, ..., 1)"),
              ),
              shiny::column(
                width = 6,
                shiny::textInput("clamping_vector", shiny::HTML("Clamping Vector<br>(clamping_vector)"), value = "c(1, 0, ..., 0)")
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput("activation", shiny::HTML("Activation Function<br>(activation)"), choices = c("kosko", "modified-kosko", "rescale"), selected = "kosko"),
              ),
              shiny::column(
                width = 4,
                shiny::selectInput("squashing", shiny::HTML("Squashing Function<br>(squashing)"), choices = c("sigmoid", "tanh"), selected = "sigmoid")
              ),
              shiny::column(
                width = 4,
                shiny::numericInput("lambda", shiny::HTML("<br>Lambda"), value = 1, min = 0.001, step = 0.1)
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::numericInput("max_iter", shiny::HTML("Max. # Iterations to Simulate FCM before Force-Stopping<br>(max_iter)"), value = 100, min = 10, step = 10)
              ),
              shiny::column(
                width = 6,
                shiny::numericInput("min_error", shiny::HTML("Min. Change in Values b/w Iterations to Stop Simulation Run<br>(min_error)"), value = 10^-5, min = 10^-10, max = 10^-3)
              )
            ),
            shiny::actionButton("submit", "Submit")
          )
        ),
        bslib::nav_panel(
          title = "Monte Carlo Agg. Options",
          shiny::fluidPage(
            shiny::fluidRow(
              shiny::column(
                width = 5,
                shiny::numericInput("samples", shiny::HTML("# Monte Carlo Models to Build<br>(samples)"), value = 1000, min = 1, step = 1)
              ),
              shiny::column(
                width = 7,
                shiny::checkboxInput("include_zeroes_in_aggregation", shiny::HTML("Include zero-value edges in aggregation<br>(include_zeroes_in_aggregations)"), value = FALSE)
              )
            ),
            shiny::fluidRow(
              shiny::checkboxInput("include_simulations_in_output", shiny::HTML("Output MC Simulations<br>(include_simulations_in_output)"), value = TRUE)
            ),
            shiny::fluidRow(
              shiny::uiOutput("sampling")
            )
          )
        ),
        bslib::nav_panel(
          title = "Bootstrap Options",
          shiny::fluidPage(
            shiny::fluidRow(
              shiny::checkboxInput("bootstrap_inference_means", shiny::HTML("Estimate Confidence Interval about Mean Inferences<br>(bootstrap_inference_means)"), value = TRUE)
            ),
            shiny::conditionalPanel(
              condition = "input.bootstrap_inference_means == true",
              shiny::fluidRow(
                shiny::numericInput("bootstrap_CI", shiny::HTML("Confidence Interval %<br>(bootstrap_CI)"), value = 0.95, min = 0, max = 1, step = 0.01)
              ),
              shiny::fluidRow(
                shiny::numericInput("bootstrap_reps", shiny::HTML("# Bootstrap Repetitions<br>(bootstrap_reps)"), value = 1000, min = 10, step = 100)
              ),
              shiny::fluidRow(
                shiny::numericInput("bootstrap_draws_per_rep", shiny::HTML("# Samples to Draw per Bootstrap<br>(boostrap_draws_per_rep)"), value = 1000, min = 10, step = 100)
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Runtime Options",
          shiny::fluidPage(
            shiny::checkboxInput("parallel", "Parallel Processing", value = TRUE),
            shiny::conditionalPanel(
              condition  = "input.parallel == true",
              shiny::numericInput("n_cores", "# Cores to use for Parallel Processing", value = parallel::detectCores(), min = 2, max = parallel::detectCores(), step = 1)
            ),
            shiny::checkboxInput("show_progress", "Show Progress at Runtime", value = TRUE)
          )
        )
      )
    ),
    server = function(input, output, session) {

      adj_matrices_obj <- shiny::reactive({
        adj_matrices_obj_name <- input$adj_matrices
        as.list(.GlobalEnv)[names(as.list(.GlobalEnv)) == adj_matrices_obj_name]
      })

      # observe({
      #  print(adj_matrices_obj())
      #  print(length(adj_matrices_obj()))
      #})

      adj_matrices_obj_is_a_list_of_adj_matrices <- shiny::reactive({
        adj_matrices_obj_is_not_an_individual_matrix <- is.null(dim(adj_matrices_obj()))
        adj_matrices_obj_is_a_list <- identical(typeof(adj_matrices_obj()), "list")
        adj_matrices_obj_has_length_greater_than_one <- length(adj_matrices_obj()[[1]]) > 1
        adj_matrices_obj_is_a_list_of_square_matrices <- all(unlist(lapply(lapply(adj_matrices_obj()[[1]], dim), function(x) length(unique(x)) == 1)))

       # print(paste0("Adj Matrices is not a singular matrix: ", adj_matrices_obj_is_not_an_individual_matrix))
        #print(paste0("Adj Matrices is list: ", adj_matrices_obj_is_a_list))
        #print(paste0("Adj Matrices is list > 1: ", adj_matrices_obj_has_length_greater_than_one))
        #print(paste0("Adj Matrices is list of square mats: ", adj_matrices_obj_is_a_list_of_square_matrices))

        (adj_matrices_obj_is_not_an_individual_matrix &
            adj_matrices_obj_is_a_list &
            adj_matrices_obj_has_length_greater_than_one &
            adj_matrices_obj_is_a_list_of_square_matrices)
      })

      input_adj_matrices_fcm_class <- shiny::reactive({
        if (adj_matrices_obj_is_a_list_of_adj_matrices()) {
          get_fcm_class_from_adj_matrices(adj_matrices_obj()[[1]])
        } else {
          FALSE
        }
      })

      nodes <- reactive({
        if (adj_matrices_obj_is_a_list_of_adj_matrices()) {
          colnames(adj_matrices_obj()[[1]][[1]])
        }
      })

      #observe({
      #  print(nodes())
      #})

      observe({
        if (adj_matrices_obj_is_a_list_of_adj_matrices()) {
          shiny::updateTextInput(session, "initial_state_vector",  value = paste0(list(rep(1, length(nodes())))))
          shiny::updateTextInput(session, "clamping_vector",  value = paste0(list(rep(0, length(nodes())))))
        }
      })

      output$sampling <- shiny::renderUI({
        if (adj_matrices_obj_is_a_list_of_adj_matrices() & input_adj_matrices_fcm_class() == "fcm") {
          shiny::selectInput("sampling", "If FCM only, select mc sampling method\n(sampling)", choices = c("", "nonparametric", "uniform", "triangular"))
        } else {
          NULL
        }}
      )

      form_data <- shiny::reactive({
        data <- sapply(fields, function(x) input[[x]])
        data
      })

      shiny::observeEvent(input$submit, {
        assign(
          x = "session_variables",
          value = as.list(form_data()),
          envir = .GlobalEnv
        )

        shiny::stopApp()
      })
    }
  )

  shiny::runApp(app)

  print(session_variables$adj_matrices)

  selected_adj_matrix_list_var_name <- session_variables$adj_matrices
  session_variables$adj_matrices <- as.list(.GlobalEnv)[names(as.list(.GlobalEnv)) == selected_adj_matrix_list_var_name][[1]]
  session_variables$initial_state_vector <- eval(parse(text = session_variables$initial_state_vector))
  session_variables$clamping_vector <- eval(parse(text = session_variables$clamping_vector))
  session_variables

  fcmconfr_function_call <- paste0(
    "fcmconfr(",
    "adj_matrices = ", gsub("list\\(A", "data.frame\\(A", paste0(list(session_variables$adj_matrices))),
    ", samples = ", session_variables$samples,
    ", include_zeroes_in_aggregation = ", session_variables$include_zeroes_in_aggregation,
    ", aggregation_fun = ", paste0("'", session_variables$aggregation_fun, "'"),
    ", initial_state_vector = ", list(session_variables$initial_state_vector),
    ", clamping_vector = ", list(session_variables$clamping_vector),
    ", activation = ", paste0("'", session_variables$activation, "'"),
    ", squashing = ", paste0("'", session_variables$squashing, "'"),
    ", lambda = ", session_variables$lambda,
    ", max_iter = ", session_variables$max_iter,
    ", min_error = ", session_variables$min_error,
    ", bootstrap_inference_means = ", session_variables$bootstrap_inference_means,
    ", bootstrap_CI = ", session_variables$bootstrap_CI,
    ", bootstrap_reps = ", session_variables$bootstrap_reps,
    ", bootstrap_draws_per_rep = ", session_variables$bootstrap_draws_per_rep,
    ", show_progress = ", session_variables$show_progress,
    ", parallel = ", session_variables$parallel,
    ", n_cores = ", session_variables$n_cores,
    ", include_simulations_in_output = ", session_variables$include_simulations_in_output,
    ", sampling = ", paste0("'", session_variables$sampling, "'"),
    ")"
  )

    eval(parse(text = fcmconfr_function_call))
}


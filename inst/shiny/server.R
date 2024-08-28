
#' shiny_server
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @param input the data streamed into the server from the ui
#' @param output the data streamed from to the ui from the server
#' @param session data surrounding the shiny instance itself
shiny_server <- function(input, output, session) {
  adj_matrix <- shiny::reactive({as.list(.GlobalEnv)[names(.GlobalEnv) == input$adj_matrix_list][[1]]})
  concepts <- shiny::reactive({colnames(adj_matrix())})

  # Data Nav Panel ####
  output$initial_state_vector_numeric_inputs <- shiny::renderUI({
    lapply(concepts(), function(i) {
      shiny::numericInput(paste0("initial_state_", i), label = i, value = 1, min = -1, max = 1, step = 0.05)
    })
  })

  initial_state_vector <- reactive({
    initial_state_vector_input_vars <- paste0("initial_state_", concepts())
    unlist(lapply(initial_state_vector_input_vars, function(i) input[[i]][[1]]))
  })

  output$initial_state_vector_table <- shiny::renderTable({
    data.frame(
      cbind(
        "Concept" = concepts(),
        "Value" = initial_state_vector()
      )
    )
  }, align = "c", spacing = "xs")

  shiny::observeEvent(input$reset_initial_state_vectors, {
    lapply(paste0("initial_state_", concepts()), function(i) shiny::updateNumericInput(session, i, value = 1))
  })

  output$clamping_vector_numeric_inputs <- shiny::renderUI({
    lapply(concepts(), function(i) {
      shiny::numericInput(paste0("clamping_", i), label = i, value = 0, min = -1, max = 1, step = 0.05)
    })
  })

  clamping_vector <- reactive({
    clamping_vector_input_vars <- paste0("clamping_", concepts())
    unlist(lapply(clamping_vector_input_vars, function(i) input[[i]][[1]]))
  })

  output$clamping_vector_table <- shiny::renderTable({
    clamping_vector_table_df <- data.frame(
      cbind(
        "Concept" = concepts(),
        "Value" = clamping_vector()
      )
    )
  }, align = "c", spacing = "xs")

  shiny::observeEvent(input$reset_clamping_vectors, {
    lapply(paste0("clamping_", concepts()), function(i) shiny::updateNumericInput(session, i, value = 0))
  })
  #####

  form_data <- shiny::reactive({
    inputs <- shiny::reactiveValuesToList(input)
    inputs$initial_state_vector <- initial_state_vector()
    inputs$clamping_vector <- clamping_vector()
    inputs$adj_matrix_list <- adj_matrix()
    inputs
  })

  shiny::observeEvent(input$submit, {
    assign(
      x = "session_variables",
      value = form_data(),
      envir = .GlobalEnv
    )
    shiny::stopApp()
  })

  shiny::onStop(
    function() {
      assign(
        x = "session_variables",
        value = shiny::isolate(form_data()),
        envir = .GlobalEnv
      )
    }
  )
}


#'
#' #' shiny_server
#' #'
#' #' @description
#' #' [ADD DETAILS HERE!!!]
#' #'
#' #' @param input the data streamed into the server from the ui
#' #' @param output the data streamed from to the ui from the server
#' #' @param session data surrounding the shiny instance itself
#' shiny_server <- function(input, output, session) {
#'   adj_matrices_obj <- shiny::reactive({
#'     adj_matrices_obj_name <- input$adj_matrices
#'     as.list(.GlobalEnv)[names(as.list(.GlobalEnv)) == adj_matrices_obj_name]
#'   })
#'
#'   # observe({
#'   #  print(adj_matrices_obj())
#'   #  print(length(adj_matrices_obj()))
#'   #})
#'
#'   adj_matrices_obj_is_a_list_of_adj_matrices <- shiny::reactive({
#'     adj_matrices_obj_is_not_an_individual_matrix <- is.null(dim(adj_matrices_obj()))
#'     adj_matrices_obj_is_a_list <- identical(typeof(adj_matrices_obj()), "list")
#'     adj_matrices_obj_has_length_greater_than_one <- length(adj_matrices_obj()[[1]]) > 1
#'     adj_matrices_obj_is_a_list_of_square_matrices <- all(unlist(lapply(lapply(adj_matrices_obj()[[1]], dim), function(x) length(unique(x)) == 1)))
#'
#'     # print(paste0("Adj Matrices is not a singular matrix: ", adj_matrices_obj_is_not_an_individual_matrix))
#'     #print(paste0("Adj Matrices is list: ", adj_matrices_obj_is_a_list))
#'     #print(paste0("Adj Matrices is list > 1: ", adj_matrices_obj_has_length_greater_than_one))
#'     #print(paste0("Adj Matrices is list of square mats: ", adj_matrices_obj_is_a_list_of_square_matrices))
#'
#'     (adj_matrices_obj_is_not_an_individual_matrix &
#'         adj_matrices_obj_is_a_list &
#'         adj_matrices_obj_has_length_greater_than_one &
#'         adj_matrices_obj_is_a_list_of_square_matrices)
#'   })
#'
#'   input_adj_matrices_fcm_class <- shiny::reactive({
#'     if (adj_matrices_obj_is_a_list_of_adj_matrices()) {
#'       get_fcm_class_from_adj_matrices(adj_matrices_obj()[[1]])
#'     } else {
#'       FALSE
#'     }
#'   })
#'
#'   nodes <- reactive({
#'     if (adj_matrices_obj_is_a_list_of_adj_matrices()) {
#'       colnames(adj_matrices_obj()[[1]][[1]])
#'     }
#'   })
#'
#'   #observe({
#'   #  print(nodes())
#'   #})
#'
#'   observe({
#'     if (adj_matrices_obj_is_a_list_of_adj_matrices()) {
#'       shiny::updateTextInput(session, "initial_state_vector",  value = paste0(list(rep(1, length(nodes())))))
#'       shiny::updateTextInput(session, "clamping_vector",  value = paste0(list(rep(0, length(nodes())))))
#'     }
#'   })
#'
#'   output$sampling <- shiny::renderUI({
#'     if (adj_matrices_obj_is_a_list_of_adj_matrices() & input_adj_matrices_fcm_class() == "fcm") {
#'       shiny::selectInput("sampling", "If FCM only, select mc sampling method\n(sampling)", choices = c("", "nonparametric", "uniform", "triangular"))
#'     } else {
#'       NULL
#'     }}
#'   )
#'
#'   form_data <- shiny::reactive({
#'     data <- sapply(fields, function(x) input[[x]])
#'     data
#'   })
#'
#'   shiny::observeEvent(input$submit, {
#'     assign(
#'       x = "session_variables",
#'       value = as.list(form_data()),
#'       envir = .GlobalEnv
#'     )
#'
#'     shiny::stopApp()
#'   })
#' #' }
#'
#'
#'
#' output$aggregation_or_mc_selection_ui <- shiny::renderUI({
#'   if (aggregation_or_mc_selection() == "aggregate") {
#'     shiny::fluidPage(
#'       # Aggregation UI Elements
#'       shinydashboard::box(
#'         id = "aggregate_options_ui", title = "", width = 12,
#'         shiny::fluidRow(
#'           shiny::column(
#'             width = 5, align = "right",
#'             shiny::h5("Aggregation Function", style = "padding: 35px;")
#'           ),
#'           shiny::column(
#'             width = 7, align = "left",
#'             shinyWidgets::radioGroupButtons("aggregation_fun", "", choices = c("Mean", "Median"), selected = "Mean")
#'           )
#'         ),
#'         shiny::fluidRow(
#'           shiny::column(
#'             width = 5, align = "right",
#'             shiny::h5("Include 0's in Calculations", style = "padding: 35px;")
#'           ),
#'           shiny::column(
#'             width = 7, align = "left",
#'             shinyWidgets::radioGroupButtons("include_zeroes_in_aggregation", "", choices = c("Yes", "No"), selected = "No")
#'           )
#'         )
#'       ),
#'       shinyjs::hidden(
#'         shinydashboard::box(
#'           id = "monte_carlo_options_ui", title = "", width = 12,
#'           shiny::fluidRow(
#'             shiny::column(
#'               width = 5, align = "right",
#'               shiny::h5("# Sample Maps To Generate", style = "padding: 35px;")
#'             ),
#'             shiny::column(
#'               width = 7, align = "left",
#'               shiny::numericInput("samples", "", value = 1000, min = 0, step = 100)
#'             )
#'           )
#'         )
#'       )
#'     )
#'   } else if (aggregation_or_mc_selection() == "mc") {
#'     shiny::fluidPage(
#'       shinyjs::hidden(
#'         # Aggregation UI Elements
#'         shinydashboard::box(
#'           id = "aggregate_options_ui", title = "", width = 12,
#'           shiny::fluidRow(
#'             shiny::column(
#'               width = 5, align = "right",
#'               shiny::h5("Aggregation Function", style = "padding: 35px;")
#'             ),
#'             shiny::column(
#'               width = 7, align = "left",
#'               shinyWidgets::radioGroupButtons("aggregation_fun", "", choices = c("Mean", "Median"), selected = "Mean")
#'             )
#'           ),
#'           shiny::fluidRow(
#'             shiny::column(
#'               width = 5, align = "right",
#'               shiny::h5("Include 0's in Calculations", style = "padding: 35px;")
#'             ),
#'             shiny::column(
#'               width = 7, align = "left",
#'               shinyWidgets::radioGroupButtons("include_zeroes_in_aggregation", "", choices = c("Yes", "No"), selected = "No")
#'             )
#'           )
#'         )
#'       ),
#'       shinydashboard::box(
#'         id = "monte_carlo_options_ui", title = "", width = 12,
#'         shiny::fluidRow(
#'           shiny::column(
#'             width = 5, align = "right",
#'             shiny::h5("# Sample Maps To Generate", style = "padding: 35px;")
#'           ),
#'           shiny::column(
#'             width = 7, align = "left",
#'             shiny::numericInput("samples", "", value = 1000, min = 0, step = 100)
#'           )
#'         )
#'       )
#'     )
#'   }
#' })
#'
#' observe({
#'   print(input[["samples"]])
#' })
#'
#'
#' # observe({
#' #   if (aggregation_or_mc_selection() == "mc") {
#' #     if (!is.null(input[["aggregation_fun"]])) rm(input[["aggregation_fun"]])
#' #     if (!is.null(input[["include_zeroes_in_aggregation"]])) rm(input[["include_zeroes_in_aggregation"]])
#' #   } else if (aggregation_or_mc_selection() == "aggregate") {
#' #     if (!is.null(input[["samples"]])) rm(input[["samples"]])
#' #   }
#' # })
#'
#' # observe({
#' #   if (aggregation_or_mc_selection() == "mc") {
#' #     shinyjs::hide("aggregate_options_ui")
#' #     shinyjs::show("monte_carlo_options_ui")
#' #   } else if (aggregation_or_mc_selection() == "aggregate") {
#' #     shinyjs::hide("monte_carlo_options_ui")
#' #     shinyjs::show("aggregate_options_ui")
#' #   }
#' # })
#'
#' observe({
#'   print(input[["aggregation_or_mc_selection"]])
#' })




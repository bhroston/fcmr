
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




   #output$test_sliders <- shiny::renderUI({
   # concepts <- colnames(input$adj_matrix_list)
    # lapply(1:length(concepts), function(i) {
    #
    #})
    #lapply(1:length(concepts), function(i) {
    #  shiny::sliderInput(inputID = paste0("concept_", i), label = concepts[i], min = -1, max = 1, value = 0, step = 0.05)
    #})
  #})

  #output$test_sliders <- shiny::renderUI({
  #  concepts <- colnames(input$adj_matrix_list)

  #  lapply(concepts, function(i) {
  #    numericInput(paste0("n_input_", i), label = paste0("n_input", i), value = 0)
  #  })
  #})
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
#' }

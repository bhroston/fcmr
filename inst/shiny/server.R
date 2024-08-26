
#' shiny_server
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @param input the data streamed into the server from the ui
#' @param output the data streamed from to the ui from the server
#' @param session data surrounding the shiny instance itself
shiny_server <- function(input, output, session) {
  adj_matrix <- reactive({as.list(.GlobalEnv)[names(.GlobalEnv) == input$adj_matrix_list][[1]]})
  concepts <- reactive({colnames(adj_matrix())})

  output$initial_state_vector_numeric_inputs <- shiny::renderUI({
    lapply(concepts(), function(i) {
      shiny::sliderInput(i, label = i, value = 0, min = -1, max = 1, step = 0.05)
    })
  })

  output$initial_state_vector_table <- shiny::renderTable({
    initial_state_vector_table_df <- data.frame(
      cbind(
        "Concept" = concepts(),
        "Value" = unlist(lapply(concepts(), function(i) input[[i]]))
      )
    )
  }, align = "c", spacing = "xs")



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

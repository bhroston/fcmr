
#' shiny_server
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @param input the data streamed into the server from the ui
#' @param output the data streamed from to the ui from the server
#' @param session data surrounding the shiny instance itself
shiny_server <- function(input, output, session) {
  #adj_matrix <- shiny::reactive({as.list(.GlobalEnv)[names(.GlobalEnv) == input$adj_matrix_list][[1]]})
  #concepts <- shiny::reactive({colnames(adj_matrix())})


  # Data Nav Panel ####
  adj_matrix_list_selected <- reactive({
    input$adj_matrix_list != ""
  })
  adj_matrix_list_checks <- shiny::eventReactive(input$adj_matrix_list, {
    note <- NULL
    if (!adj_matrix_list_selected()) {
      adj_matrix_list_input <- list(data.frame())
      checked_adj_matrix_list <- list(data.frame())
      note <- "nothing_selected"
    } else {
      adj_matrix_list_input <- as.list(.GlobalEnv)[names(as.list(.GlobalEnv)) == input$adj_matrix_list][[1]]
    }

    adj_matrix_list_input_type <- get_adj_matrix_list_input_type(adj_matrix_list_input)
    if (adj_matrix_list_input_type$input_type == "unavailable" & adj_matrix_list_input_type$list_objects == "unavailable") {
      checked_adj_matrix_list <- list(data.frame())
      note <- "non_list_or_matrix_input"
    } else if (adj_matrix_list_input_type$input_type %in% c("data.frame", "matrix", "sparseMatrix", "data.table", "tibble")) {
      checked_adj_matrix_list <- list(adj_matrix_list_input)
    } else if (adj_matrix_list_input_type$input_type == "list" & adj_matrix_list_input_type$list_objects %in% c("data.frame", "matrix", "sparseMatrix", "data.table", "tibble")) {
      checked_adj_matrix_list <- adj_matrix_list_input
    } else if (adj_matrix_list_input_type$input_type == "list" & adj_matrix_list_input_type$list_objects == "unavailable") {
      checked_adj_matrix_list <- list(data.frame())
      note <- "list_of_nonmatrices_input"
    } else {
      checked_adj_matrix_list <- list(data.frame())
      note <- "incompatible_input"
    }

    squared_dims_of_adj_matrix_list <- unlist(unique(lapply(checked_adj_matrix_list, function(x) (unique(dim(x))))))
    concept_names_in_adj_matrix_list <- unique(lapply(checked_adj_matrix_list, function(x) colnames(x)))
    if (length(squared_dims_of_adj_matrix_list) > 1) {
      checked_adj_matrix_list <- list(data.frame())
      note <- "adj_matrices_of_different_dimensions_or_not_square"
    }
    if (length(concept_names_in_adj_matrix_list) > 1) {
      checked_adj_matrix_list <- list(data.frame())
      note <- "adj_matrices_have_different_named_concepts"
    }

    list(
      adj_matrix_list = checked_adj_matrix_list,
      note = note
    )
  })
  accepted_adj_matrix_list <- shiny::reactive({
    if (!identical(adj_matrix_list_checks()$adj_matrix, list(data.frame())) & !is.null(adj_matrix_list_checks()$note)) {
      FALSE
    } else {
      TRUE
    }
  })
  output$accepted_adj_matrix_list <- reactive({accepted_adj_matrix_list()})
  shiny::outputOptions(output, "accepted_adj_matrix_list", suspendWhenHidden = FALSE)
  output$rejected_adj_matrix_list_note <- shiny::renderUI({
    if (!exists("adj_matrix_list_checks")) {
      NULL
    } else if (accepted_adj_matrix_list()) {
      NULL
    } else {
      note <- adj_matrix_list_checks()$note
      shiny::fluidRow(
        if (note == "nothing_selected") {
          NULL
        } else if (note == "non_list_or_matrix_input") {
          shiny::column(
            width = 12, align = "center",
            shiny::h5("Selection is NOT a matrix or list of matrices")
          )
        } else if (note == "list_of_nonmatrices_input") {
          shiny::column(
            width = 12, align = "center",
            shiny::h5("Selection is NOT a list of matrices"),
            shiny::p("All matrices in list must be of the same object type."),
            shiny::p("Acceptable object types include: data.frame, matrix, sparseMatrix, data.table, or tibble")
          )
        } else if (note == "adj_matrices_of_different_dimensions_or_not_square") {
          shiny::column(
            width = 12, align = "center",
            shiny::h5("Selection is must be either a square matrix or a list of square matrices (all of the same size)")
          )
        } else if (note == "adj_matrices_have_different_named_concepts") {
          shiny::column(
            width = 12, align = "center",
            shiny::h5("Selection is a list of matrices, but all matrices must have the same concepts (i.e. column names)")
          )
        }
        else if (note == "incompatible_input") {
          shiny::column(
            width = 12, align = "center",
            shiny::h5("Selection is unable to be read.")
          )
        }
      )
    }
  })

  adj_matrix_list <- shiny::reactive({
    if (accepted_adj_matrix_list()) {
      adj_matrix_list_checks()$adj_matrix
    } else {
      list(data.frame())
    }
  })
  concepts <- reactive({
    if (accepted_adj_matrix_list()) {
      concepts <- unique(lapply(adj_matrix_list(), colnames))[[1]]
    } else {
      concepts <- NULL
    }
  })

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
    #inputs$adj_matrix_list <- adj_matrix_list()
    inputs$concepts = concepts()
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


#' shiny_server
#'
#' @description
#' [ADD DETAILS HERE!!!]
#'
#' @param input the data streamed into the server from the ui
#' @param output the data streamed from to the ui from the server
#' @param session data surrounding the shiny instance itself
shiny_server <- function(input, output, session) {

  # Data Nav Panel
  # Data Loading and Checks ====
  adj_matrices_selected <- reactive({
    input$adj_matrices != ""
  })

  adj_matrices_checks <- shiny::eventReactive(input$adj_matrices, {
    if (!adj_matrices_selected()) {
      return(
        list(
          adj_matrices = list(data.frame()),
          note = "nothing_selected"
        )
      )
    } else {
      note <- NULL
      adj_matrices_input <- as.list(.GlobalEnv)[names(as.list(.GlobalEnv)) == input$adj_matrices][[1]]

      adj_matrices_input_type <- get_adj_matrices_input_type(adj_matrices_input)
      adj_matrices_input_is_list <- adj_matrices_input_type$adj_matrices_input_is_list
      if (!adj_matrices_input_is_list) {
        adj_matrices_input <- list(adj_matrices_input)
      }
      fcm_class <- adj_matrices_input_type$object_types_in_list[1]

      concepts_in_adj_matrices <- unique(lapply(adj_matrices_input, colnames))
      dims_of_adj_matrices <- unique(unlist(lapply(adj_matrices_input, function(x) unique(dim(x)))))

      if (fcm_class == "unavailable") {
        note <- "objects_in_matrix_not_available_fcm_class"
      }
      if (length(dims_of_adj_matrices) > 1) {
        note <- "adj_matrices_of_different_dimensions_or_not_square"
      }
      if (length(concepts_in_adj_matrices) > 1) {
        note <- "different_concept_sets_across_matrices"
      }

      if (is.null(note)) {
        checked_adj_matrices <- adj_matrices_input
      } else {
        checked_adj_matrices <- list(data.frame())
      }

      # browser()
      return(
        list(
          adj_matrices = checked_adj_matrices,
          note = note
        )
      )
    }
  })

  accepted_adj_matrices_input <- shiny::reactive({
    # (identical(checked_adj_matrices, list(data.frame())))
    # !is.null(note)
    # identical(note, "nothing_selected")
    if (identical(adj_matrices_checks()$adj_matrices, list(data.frame()))) {
      FALSE
    } else if (!is.null(adj_matrices_checks()$note)) {
      FALSE
    } else if (identical(adj_matrices_checks()$note, "nothing_selected")) {
      FALSE
    } else {
      TRUE
    }
  })

  adj_matrices <- shiny::reactive({
    if (accepted_adj_matrices_input()) {
      adj_matrices_checks()$adj_matrices
    } else {
      list(data.frame())
    }
  })

  concepts <- shiny::reactive({
    if (accepted_adj_matrices_input()) {
      unique(lapply(adj_matrices(), colnames))[[1]]
    } else {
      NULL
    }
  })

  fcm_class <- shiny::reactive({
    if (accepted_adj_matrices_input()) {
      get_adj_matrices_input_type(adj_matrices())$object_types_in_list[1]
    } else {
      "unavailable"
    }
  })

  output$rejected_adj_matrices_note <- shiny::renderUI({
    if (!adj_matrices_selected()) {
      shiny::column(
        width = 12, align = "left", shiny::h5("Please select a list of Adj. Matrices or an Individual Adj. Matrix")
      )
    } else if (accepted_adj_matrices_input()) {
      NULL
    } else {
      note <- adj_matrices_checks()$note
      # browser()
      if (note == "objects_in_matrix_not_available_fcm_class") {
        shiny::column(
          width = 12, align = "left", shiny::h5("Selection must be an Adj. Matrix with edge values being Numerics, IVFNs, or TFNs"),
          shiny::p("All matrices must represent edges with the same class (all edges are either Numerics or IVFNs or TFNs)")
        )
      } else if (note == "adj_matrices_of_different_dimensions_or_not_square") {
        shiny::column(
          width = 12, align = "left", shiny::h5("Selected Adj. Matrix/Matrices must be square (and all must have the same dimensions if multiple matrices)")
        )
      } else if (note == "different_concept_sets_across_matrices") {
        shiny::column(
          width = 12, align = "left", shiny::h5("Selected Adj. Matrices have different concept names (column names)"),
          shiny::p("Call standardize_adj_matrices() to standardize the size of the list of adj. matrices")
        )
      }
    }
  })
  # ====

  # Initial State Vector ====
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

  output$initial_state_vector_input_ui <- shiny::renderUI({
    if (!accepted_adj_matrices_input()) {
      NULL
    } else {
      shiny::fluidRow(
        shiny::column(
          width = 12, div(style = "height:20px")
        )
      )
      shiny::fluidRow(
        shiny::column(
          width = 6, align = "center",
          bslib::card(
            max_height = "450px", full_screen = TRUE,
            shiny::uiOutput("initial_state_vector_numeric_inputs")
          )
        ),
        shiny::column(
          width = 6, align = "center",
          bslib::card(
            max_height = "450px", full_screen = TRUE,
            shiny::tableOutput("initial_state_vector_table")
          ),
          bslib::card(
            shiny::actionButton("reset_initial_state_vector", "Reset", icon = shiny::icon("rotate-right"))
          )
        )
      )
    }
  })

  shiny::observeEvent(input$reset_initial_state_vector, {
    lapply(paste0("initial_state_", concepts()), function(i) shiny::updateNumericInput(session, i, value = 1))
  })
  # ====

  # Clamping Vector ====
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

  output$clamping_vector_input_ui <- shiny::renderUI({
    if (!accepted_adj_matrices_input()) {
      NULL
    } else {
      shiny::fluidRow(
        shiny::column(
          width = 12, div(style = "height:20px")
        )
      )
      shiny::fluidRow(
        shiny::column(
          width = 6, align = "center",
          bslib::card(
            max_height = "450px", full_screen = TRUE,
            shiny::uiOutput("clamping_vector_numeric_inputs")
          )
        ),
        shiny::column(
          width = 6, align = "center",
          bslib::card(
            max_height = "450px", full_screen = TRUE,
            shiny::tableOutput("clamping_vector_table")
          ),
          bslib::card(
            shiny::actionButton("reset_clamping_vector", "Reset", icon = shiny::icon("rotate-right"))
          )
        )
      )
    }
  })

  shiny::observeEvent(input$reset_clamping_vector, {
    lapply(paste0("clamping_", concepts()), function(i) shiny::updateNumericInput(session, i, value = 0))
  })
  # ====

  # Aggregation and Monte Carlo Panel
  # Aggregation Card ----
  can_perform_aggregation_analysis <- shiny::reactive({
    if (!accepted_adj_matrices_input()) {
      FALSE
    } else if (accepted_adj_matrices_input() & length(adj_matrices()) == 1 & !identical(fcm_class(), "conventional")) {
      FALSE
    } else if (accepted_adj_matrices_input() & length(adj_matrices()) == 1 & identical(fcm_class(), "conventional")) {
      FALSE
    } else {
      TRUE
    }
  })

  perform_aggregation_analysis <- shiny::reactive({
    if (is.null(input$perform_aggregation)) {
      FALSE
    } else {
      input$perform_aggregation
    }
  })

  output$aggregation_options_ui <- shiny::renderUI({
    if ((can_perform_aggregation_analysis() & perform_aggregation_analysis()) | !adj_matrices_selected()) {
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "right",
              shiny::h5("Aggregation Function", style = "padding: 35px;")
            ),
            shiny::column(
              width = 6, align = "left",
              #shinyWidgets::radioGroupButtons("aggregation_fun", "", choices = c("Mean", "Median"), selected = "Mean"),
              shiny::radioButtons("aggregation_fun", "", choiceNames = c("Mean", "Median"), choiceValues = c("mean", "median"))
            )
          )
        )
      )
    } else if (!can_perform_aggregation_analysis()) {
      shiny::fluidRow(
        shiny::p("Aggregation analysis unavailable")
      )
    } else {
      NULL
    }
  })

  shiny::observe({
    if (!can_perform_aggregation_analysis()) {
      shiny::updateCheckboxInput(session, "perform_aggregation", value = FALSE)
    }
  })

  shiny::observeEvent(input$perform_aggregation, {
    if (!can_perform_aggregation_analysis()) {
      shiny::updateCheckboxInput(session, "perform_aggregation", value = FALSE)
    }
  })

  shiny::observeEvent(can_perform_aggregation_analysis(), {
    if (can_perform_aggregation_analysis()) {
      shiny::updateCheckboxInput(session, "perform_aggregation", value = TRUE)
    }
  })
  # ----

  # Monte Carlo Card ----
  can_perform_monte_carlo_analysis <- shiny::reactive({
    if (!accepted_adj_matrices_input()) {
      FALSE
    } else if (accepted_adj_matrices_input() & length(adj_matrices()) == 1 & !identical(fcm_class(), "conventional")) {
      TRUE
    } else if (accepted_adj_matrices_input() & length(adj_matrices()) == 1 & identical(fcm_class(), "conventional")) {
      FALSE
    } else {
      TRUE
    }
  })

  perform_monte_carlo_analysis <- shiny::reactive({
    if (is.null(input$perform_monte_carlo)) {
      FALSE
    } else {
      input$perform_monte_carlo
    }
  })

  output$monte_carlo_options_ui <- shiny::renderUI({
    if ((can_perform_monte_carlo_analysis() & perform_monte_carlo_analysis()) | !adj_matrices_selected()) {
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "right",
            shiny::h5("# Sample Maps To Generate", style = "padding: 35px;")
          ),
          shiny::column(
            width = 6, align = "left",
            shiny::numericInput("monte_carlo_samples", "", value = 1000, min = 1, step = 500)
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "right",
            shiny::h5("Include MC Sims in Output?")
          ),
          shiny::column(
            width = 6, align = "left",
            shiny::checkboxInput("include_monte_carlo_FCM_simulations_in_output", "", value = TRUE)
          )
        )
      )
    } else if (!can_perform_monte_carlo_analysis()) {
      shiny::fluidRow(
        shiny::p("Monte Carlo analysis unavailable")
      )
    } else {
      NULL
    }
  })

  shiny::observe({
    if (!can_perform_monte_carlo_analysis()) {
      shiny::updateCheckboxInput(session, "perform_monte_carlo", value = FALSE)
    }
  })

  shiny::observeEvent(input$perform_monte_carlo, {
    if (!can_perform_monte_carlo_analysis()) {
      shiny::updateCheckboxInput(session, "perform_monte_carlo", value = FALSE)
    }
  })

  shiny::observeEvent(can_perform_monte_carlo_analysis(), {
    if (can_perform_monte_carlo_analysis()) {
      shiny::updateCheckboxInput(session, "perform_monte_carlo", value = TRUE)
    }
  })


  # Inference Bootstrap Card ----
  can_perform_inference_bootstrap_analysis <- shiny::reactive({
    if (perform_monte_carlo_analysis()) {
      TRUE
    } else {
      FALSE
    }
  })

  perform_inference_bootstrap_analysis <- shiny::reactive({
    if (is.null(input$perform_inference_bootstrap)) {
      FALSE
    } else {
      input$perform_inference_bootstrap
    }
  })

  output$monte_carlo_inference_bootstrap_options_ui <- shiny::renderUI({
    if ((can_perform_inference_bootstrap_analysis() & perform_inference_bootstrap_analysis()) | !adj_matrices_selected()) {
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "right",
            shiny::h5("Confidence Interval about the Means of Inferences", style = "padding: 35px;")
          ),
          shiny::column(
            width = 6, align = "left",
            shiny::numericInput("mc_inference_estimation_CI", "", value = 0.95, min = 1e-10, max = 1)
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "right",
            shiny::h5("# Bootstraps (Repetitions)", style = "padding: 35px;")
          ),
          shiny::column(
            width = 6, align = "left",
            shiny::numericInput("mc_inference_bootstrap_reps", "", value = 1000, min = 100, step = 100)
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "right",
            shiny::h5("# Draws per Bootstrap (Repetition)", style = "padding: 35px;")
          ),
          shiny::column(
            width = 6, align = "left",
            shiny::numericInput("mc_inference_bootstrap_draws_per_rep", "", value = 1000, min = 100, step = 100)
          )
        )
      )
    } else if (!can_perform_monte_carlo_analysis()) {
      shiny::fluidRow(
        shiny::p("Monte Carlo Inference Bootstrap analysis unavailable")
      )
    } else {
      NULL
    }
  })

  shiny::observe({
    if (!can_perform_inference_bootstrap_analysis()) {
      shiny::updateCheckboxInput(session, "perform_inference_bootstrap", value = FALSE)
    }
  })

  shiny::observeEvent(input$perform_inference_bootstrap, {
    if (!can_perform_inference_bootstrap_analysis()) {
      shiny::updateCheckboxInput(session, "perform_inference_bootstrap", value = FALSE)
    }
  })

  shiny::observeEvent(can_perform_inference_bootstrap_analysis(), {
    if (can_perform_inference_bootstrap_analysis()) {
      shiny::updateCheckboxInput(session, "perform_inference_bootstrap", value = TRUE)
    }
  })

  # ----

  # Include 0's Option
  output$include_zero_edges_ui <- shiny::renderUI({
    if ((perform_monte_carlo_analysis() | perform_aggregation_analysis()) | (!adj_matrices_selected())) {
      bslib::card(
        shiny::fluidRow(
          shiny::column(
            width = 2, align = "center",
            shiny::checkboxInput("include_zero_weighted_edges_in_aggregation_and_mc_sampling", "", value = TRUE)
          ),
          shiny::column(
            width = 10, align = "left",
            shiny::p("Include 0-weighted Edges in Aggregation and Monte Carlo Sampling?")
          )
        )
      )
    }
  })
  # ----

  # Fuzzy Set Samples
  output$fuzzy_set_samples_ui <- shiny::renderUI({
    if (fcm_class() %in% c("ivfn", "tfn") | !adj_matrices_selected()) {
      shiny::fluidRow(
        shiny::column(
          width = 5, align = "right",
          shiny::h5(paste0("Fuzzy Set Samples"), style = "padding: 28px;")
        ),
        shiny::column(
          width = 3, align = "left",
          shiny::numericInput("fuzzy_set_samples", "", 1, min = 10, step = 10)
        )
      )
    } else {
      NULL
    }
  })

  # Num Cores in Parallel
  output$num_cores_in_paralell <- shiny::renderUI({
    if (input$parallel) {
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
    } else {
      NULL
    }
  })


  # Form Data
  form_data <- shiny::reactive({
    inputs <- shiny::reactiveValuesToList(input)
    inputs$initial_state_vector <- initial_state_vector()
    inputs$clamping_vector <- clamping_vector()
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

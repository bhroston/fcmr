
server <- function(input, output) {

  input_nodes <- reactive({
    fcm_as_visNetwork_obj$x$nodes
  })

  input_edges <- reactive({
    fcm_as_visNetwork_obj$x$edges
  })

  output$fcm_display <- visNetwork::renderVisNetwork({
    fcm_as_visNetwork_obj %>%
      visNetwork::visOptions(nodesIdSelection = TRUE) %>%
      visNetwork::visInteraction(zoomSpeed = 0.5) %>%
      visNetwork::visEdges(smooth = list(enabled = TRUE, type = "continuous", roundness = 0.4), physics = FALSE)
  })

  shiny::observe({
    shiny::invalidateLater(1000)
    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visGetNodes()
  })

  shiny::observe({
    shiny::invalidateLater(1000)
    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visGetPositions()
  })

  shiny::observe({
    shiny::invalidateLater(1000)
    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visGetEdges()
  })

  coords <- shiny::reactive({
    if (!is.null(input$nodes_to_display)) {
      data.frame(do.call(rbind, input$fcm_display_positions))
    }
  })

  nodes <- shiny::reactive({
    if (!is.null(input$fcm_display_nodes)) {
      nodes_df <- data.frame(do.call(rbind, input$fcm_display_nodes))
      nodes_df <- as.data.frame(apply(nodes_df, c(1, 2), function(element) element[[1]], simplify = TRUE))
      nodes_df$x <- as.numeric(nodes_df$x)
      nodes_df$y <- as.numeric(nodes_df$y)
      nodes_df
    }
  })

  edges <- shiny::reactive({
    if (!is.null(input$fcm_display_edges)) {
      edges_df <- data.frame(do.call(rbind, input$fcm_display_edges))
      edges_df <- as.data.frame(apply(edges_df, c(1, 2), function(element) element[[1]], simplify = TRUE))
      edges_df$weight <- as.numeric(edges_df$weight)
      edges_df
    }
  })

  output$nodes_to_display <- shiny::renderUI({
    shiny::checkboxGroupInput(
      inputId = "nodes_to_display",
      label = "",
      choices = input_nodes()$id,
      selected = input_nodes()$id
    )
  })

  shiny::observe({
    if (!is.null(input$nodes_to_display) & (!is.null(nodes())) & (!is.null(edges()))) {
      nodes_to_display <- nodes()$id[nodes()$id %in% input$nodes_to_display]
      nodes_to_hide <- nodes()$id[!(nodes()$id %in% input$nodes_to_display)]

      nodes_to_display_indices <- which(nodes()$id %in% input$nodes_to_display)
      node_color <- vector(mode = "numeric", length = nrow(nodes()))
      for (i in seq_along(node_color)) {
        if (i %in% nodes_to_display_indices) {
          node_color[i] <- input_nodes()$color[i]
        } else {
          node_color[i] <- "transparent"
        }
      }
      updated_nodes_df <- nodes()
      updated_nodes_df$color <- node_color
      updated_nodes_df$label <- ifelse(updated_nodes_df$color == "transparent", " ", input_nodes()$label)

      edge_color <- ifelse((edges()$from %in% nodes_to_hide | edges()$to %in% nodes_to_hide), "transparent", input_edges()$color)
      updated_edges_df <- edges()
      updated_edges_df$color <- edge_color
      updated_edges_df$hidden <- ifelse(edge_color == "transparent", TRUE, FALSE)

      visNetwork::visUpdateEdges(visNetwork::visNetworkProxy("fcm_display"), edges = updated_edges_df)
      visNetwork::visUpdateNodes(visNetwork::visNetworkProxy("fcm_display"), nodes = updated_nodes_df)
    }
  })

  shiny::observeEvent(input$font_size, {
    updated_nodes_df <- nodes()
    updated_nodes_df$font <- list(size = input$font_size)
    visNetwork::visUpdateNodes(visNetwork::visNetworkProxy("fcm_display"), nodes = updated_nodes_df)

    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visEdges(font = list(size = input$font_size))
  })

  shiny::observeEvent(input$node_shape, {
    updated_nodes_df <- nodes()
    updated_nodes_df$shape <- input$node_shape

    visNetwork::visUpdateNodes(visNetwork::visNetworkProxy("fcm_display"), nodes = updated_nodes_df)
  })

  shiny::observeEvent(input$node_size, {
    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visNodes(size = input$node_size)
  })

  shiny::observeEvent(input$edge_roundness, {
    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visEdges(smooth = list(enabled = TRUE, roundness = input$edge_roundness), physics = FALSE)
  })

  shiny::observeEvent(input$edge_smoothing, {
    visNetwork::visNetworkProxy("fcm_display") %>%
      visNetwork::visEdges(smooth = list(enabled = TRUE, type = input$edge_smoothing), physics = FALSE)
  })

  updated_fcm_visNetwork <- shiny::reactive({
    if (!is.null(input$fcm_display_positions)) {
      updated_fcm_as_visNetwork_obj <- fcm_as_visNetwork_obj
      updated_fcm_as_visNetwork_obj$x$nodes <- nodes()
      updated_fcm_as_visNetwork_obj$x$edges <- edges()
      updated_fcm_as_visNetwork_obj

      #updated_fcm_visNetwork_proxy <- fcm_as_visNetwork_obj
      #updated_fcm_visNetwork_proxy$x$nodes <- nodes()
      #updated_fcm_visNetwork_proxy$x$edges <- edges()
      #updated_fcm_visNetwork_proxy
    }
  })

  updated_nodes <- shiny::reactive({
    if (!is.null(output$fcm_display)) {
      updated_nodes <- nodes()
    }
  })

  updated_edges <- shiny::reactive({
    if (!is.null(output$fcm_display)) {
      updated_nodes <- edges()
    }
  })

  shiny::onStop(
    function() {
      assign(x = "nodes", value = shiny::isolate(nodes()), envir = shiny_env)
      assign(x = "edges", value = shiny::isolate(edges()), envir = shiny_env)
      assign(x = "coords", value = shiny::isolate(coords()), envir = shiny_env)
      assign(x = "nodes_to_display", value = shiny::isolate(input$nodes_to_display), envir = shiny_env)
      assign(x = "nodes_shape", value = shiny::isolate(input$node_shape), envir = shiny_env)
      assign(x = "nodes_size", value = shiny::isolate(input$node_size), envir = shiny_env)
      assign(x = "edge_roundness", value = shiny::isolate(input$edge_roundness), envir = shiny_env)
      assign(x = "edge_smoothing", value = shiny::isolate(input$edge_smoothing), envir = shiny_env)
      assign(x = "font_size", value = shiny::isolate(input$font_size), envir = shiny_env)
      # print("Note: visNetwork plot stored as fcm_view_visNetwork in Global Environment", quote = FALSE)
    }
  )
}

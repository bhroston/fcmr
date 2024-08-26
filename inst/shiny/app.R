#' My Shiny App
#' @export
my_shiny_app <- function() {
  shiny::runApp(appDir = system.file('shiny', package = 'fcmconfr'))
}

# shinyuieditor::launch_editor(app_loc = system.file('shiny/app.R', package = 'fcmconfr'))

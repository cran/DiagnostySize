#' Run the app
#'
#' This function starts Shiny application.
#'
#' @importFrom htmltools tagList
#' @importFrom htmltools HTML
#' @importFrom shinyBS bsButton 
#' @importFrom shinyBS bsTooltip
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @import markdown
#' @export
#' @examples
#' if(interactive()){run_app()}
#' @return No return value, starts the shiny app.
run_app <- function() {
    shiny::addResourcePath("img", system.file("WWW", package = "DiagnostySize"))
    shiny::shinyApp(ui=ui,
             server=server)

}

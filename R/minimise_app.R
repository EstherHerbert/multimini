#' App interface for the multimini package
#'
#' @param data patient data
#' @param ... options passed to `shiny::shinyApp()`
#'
#' @export
minimise_app <- function() {

  app_dir <- system.file("shiny", "multimini", package = "multimini")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing multimini.")
  }
  shiny::runApp(app_dir, launch.browser = TRUE, display.mode = "normal")

}

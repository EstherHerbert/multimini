#' App interface for the multimini package
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

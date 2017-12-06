#' @export
MC_gui = function(){
  appDir = system.file("Shiny", package = "group5project")
  shiny::runApp(appDir, display.mode = "normal")
}

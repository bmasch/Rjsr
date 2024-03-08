#' @export
runShiny <- function() {
  appDir <- system.file("shiny-examples", "ViewRjsrTables", package = "Rjsr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Rjsr`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
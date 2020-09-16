#' Global User Interface
#'
#' @return An html user interface.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'  gui()
#' }
#' 
gui <- function() {
  appDir <- system.file("gui", package = "ecoevosimulator")
  if (appDir == "") {
    stop("Could not find gui Try re-installing `ecoevosimulator`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
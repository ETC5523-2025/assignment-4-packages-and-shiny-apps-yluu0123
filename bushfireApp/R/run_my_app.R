#' Launch the Shiny app
#'
#' Opens the interactive temperature anomaly explorer.
#' The app lets you:
#' \itemize{
#'   \item Filter by country (e.g. World, Australia)
#'   \item Choose a year range
#'   \item View a time series plot of temperature anomaly
#'   \item Inspect the underlying data table
#' }
#'
#' @return No return value. This function is called for its side effect
#'   of launching a Shiny app.
#'
#' @export
#' @importFrom shiny runApp
run_my_app <- function() {
  appDir <- system.file("shiny", package = "bushfireApp")
  if (appDir == "") {
    stop("Could not find Shiny app directory inside the package.")
  }
  shiny::runApp(appDir)
}

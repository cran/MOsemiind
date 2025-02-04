#' Launch Shiny App for 'MOsemiind'
#'
#' This function launches the interactive Shiny application for exploring
#' copula-based dependence structures in Marshall-Olkin shock models.
#'
#' @return No return value, called for side effects.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   launch_MOsemiind_app()
#' }
launch_MOsemiind_app <- function() {
  app_dir <- system.file("app", package = "MOsemiind")  # Ensure correct path
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Could not find Shiny app directory. Try reinstalling the package.", call. = FALSE)
  }

  # Run the Shiny app
  shiny::runApp(app_dir, launch.browser = TRUE)
}

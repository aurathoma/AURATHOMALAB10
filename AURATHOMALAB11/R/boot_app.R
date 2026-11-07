#' Launch the Bootstrapping Shiny Application
#' @export
run_boot_app <- function() {
  app_dir <- system.file("app", package = "AURATHOMALAB11")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `AURATHOMALAB11`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}


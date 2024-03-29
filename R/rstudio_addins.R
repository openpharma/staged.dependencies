#' @include rstudio_jobs.R
NULL

# Helper functions for addins since the addin functions are invoked without arguments
install_deps_and_project_addin <- function(...) {
  install_deps_job(project = rstudioapi::getActiveProject(), install_project = TRUE)
}

install_deps_app_addin <- function(...) {
  install_deps_app(verbose = 1)
}

check_downstream_addin <- function(...) {
  check_downstream_job(project = rstudioapi::getActiveProject(), check_args = Sys.getenv("RCMDCHECK_ARGS"))
}

test_downstream_addin <- function(...) {
  check_downstream_job(project = rstudioapi::getActiveProject(), only_tests = TRUE)
}

install_deps_addin <- function(...) {
  install_deps_job(project = rstudioapi::getActiveProject(), install_project = FALSE)
}

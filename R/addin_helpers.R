# Helper functions for addins since the addin functions are invoked without arguments

install_deps_app_addin <- function() {
  install_deps_app(verbose = 1)
}

check_downstream_addin <- function() {
  check_downstream(verbose = 1, check_args = Sys.getenv("RCMDCHECK_ARGS"))
}

test_downstream_addin <- function() {
  check_downstream(verbose = 1, only_tests = TRUE)
}

install_deps_addin <- function() {
  install_deps(verbose = 1)
}

run_job <- function(text, tempfile_prefix = "file", jobname_prefix = "Job", ...) {
  tmp_file <- tempfile(tempfile_prefix)
  message("Executing script in file ", tmp_file, "with content: \n", text)
  cat(text, file = tmp_file)
  rstudioapi::jobRunScript(tmp_file, name = paste0(jobname_prefix, " @ ", Sys.time()), ...)
}

#' Check downstream job
#'
#' @inheritParams check_downstream
#' @seealso check_downstream
#' @export
#'
check_downstream_job <- function(project = ".", check_args = Sys.getenv("RCMDCHECK_ARGS")) {
  path <- normalizePath(project)
  script = glue::glue('staged.dependencies::check_downstream(project = "{path}", verbose = 1, check_args = "{check_args}")')
  run_job(script, "check_downstream", "Check downstream")
}
# check_downstream_job()

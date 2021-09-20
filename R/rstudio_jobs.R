# run the script provided as character `text` as an rstudiojob
run_job <- function(text, tempfile_prefix = "file", jobname_prefix = "Job", ...) {
  require_pkgs("rstudioapi")
  tmp_file <- tempfile(tempfile_prefix)
  message("Executing script in file ", tmp_file, " with content:\n", text)
  cat(text, file = tmp_file)
  rstudioapi::jobRunScript(tmp_file, name = paste0(jobname_prefix, " @ ", Sys.time()), ...)
}

#' Install dependencies job
#'
#' @inheritParams dependency_table
#' @inheritDotParams install_deps
#' @param create_args \code{named list} - additional arguments passed to `dependency_table` function
#' @seealso install_deps
#' @export
#'
#' @examples
#' \dontrun{
#' install_deps_job()
#' install_deps_job(create_args = list(ref = "6_makegraph@main"))
#'
#' # install all dependencies
#' install_deps_job(create_args = list(direction = c("upstream", "downstream")))
#' install_deps_job(dry_install = TRUE)
#' }
install_deps_job <- function(project = ".", verbose = 1, create_args = list(), ...) {
  project <- normalize_path(project)
  create_args <- c(list(project = project, verbose = verbose), create_args)
  create_args_str <- paste(deparse(create_args), collapse = "\n")
  install_args <- c(list(dep_structure = substitute(x), verbose = verbose), list(...))
  install_args_str <- paste(deparse(install_args), collapse = "\n")
  script <- glue::glue('x <- do.call(staged.dependencies::dependency_table, {create_args_str})
                       print(x)
                       do.call(staged.dependencies::install_deps, {install_args_str})')
  run_job(script, "install_deps", paste0("Install deps of ", basename(project)))
}

#' Check & install downstream job
#'
#' @inheritParams dependency_table
#' @inheritDotParams check_downstream
#' @param create_args \code{named list} - additional arguments passed to `dependency_table` function
#' @seealso check_downstream
#' @export
#'
#' @examples
#' \dontrun{
#' check_downstream_job(check_args = Sys.getenv("RCMDCHECK_ARGS"))
#' check_downstream_job(check_args = Sys.getenv("RCMDCHECK_ARGS"),
#'                      list(create_arg = list(ref = "6_makegraph@main")))
#' check_downstream_job(only_tests = TRUE)
#' }
check_downstream_job <- function(project = ".", verbose = 1, create_args = list(), ...) {
  project <- normalize_path(project)
  create_args <- c(list(project = project, verbose = verbose), create_args)
  create_args_str <- paste(deparse(create_args), collapse = "\n")
  check_downstream_args <- c(list(dep_structure = substitute(x), verbose = verbose), list(...))
  check_downstream_args_str <- paste(deparse(check_downstream_args), collapse = "\n")
  script <- glue::glue('x <- do.call(staged.dependencies::dependency_table, {create_args_str})
                       print(x)
                       do.call(staged.dependencies::check_downstream, {check_downstream_args_str})')
  run_job(script, "check_downstream", paste0("Check downstream of ", basename(project)))
}

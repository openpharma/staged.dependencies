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
#' @inheritParams check_downstream
#' @inheritDotParams check_downstream
#' @seealso check_downstream
#' @export
#'
#' @examples
#' \dontrun{
#' install_deps_job()
#' install_deps_job(feature = "6_makegraph@main@master")
#'
#' # install all dependencies
#' install_deps_job(direction = c("upstream", "downstream"))
#' install_deps_job(dry_install = TRUE)
#' }
install_deps_job <- function(project = ".", verbose = 1, ...) {
  project <- normalizePath(project)
  args <- c(list(project = project, verbose = verbose), list(...))
  script <- glue::glue('do.call(staged.dependencies::install_deps, {list(dput(args))})')
  run_job(script, "install_deps_job", paste0("Install deps of ", basename(project)))
}

#' Check & install downstream job
#'
#' @inheritParams check_downstream
#' @inheritDotParams check_downstream
#' @seealso check_downstream
#' @export
#'
#' @examples
#' \dontrun{
#' check_downstream_job(check_args = Sys.getenv("RCMDCHECK_ARGS"))
#' check_downstream_job(check_args = Sys.getenv("RCMDCHECK_ARGS"), feature = "6_makegraph@main@master")
#' check_downstream_job(only_tests = TRUE)
#' }
check_downstream_job <- function(project = ".", verbose = 1, ...) {
  project <- normalizePath(project)
  args <- c(list(project = project, verbose = verbose), list(...))
  script <- glue::glue('do.call(staged.dependencies::check_downstream, {list(dput(args))})')
  run_job(script, "check_downstream", paste0("Check downstream of ", basename(project)))
}

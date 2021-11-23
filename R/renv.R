get_renv_lock_from_repo_dir <- function(repo_dir, renv_profile = NULL) {
  if (!is.null(renv_profile)) {
    renv_file <- fs::path_join(c(repo_dir, "renv", "profiles", renv_profile, "renv.lock"))
  } else {
    renv_file <- fs::path_join(c(repo_dir, "renv.lock"))
  }


  if (!fs::file_exists(renv_file)) {
    return(NULL)
  }

  renv_lock <- tryCatch(
    jsonlite::read_json(renv_file),
    error = function(cond) {
      warning(paste("Unable to open renv.lock file", renv_file, "so it will be ignored"))
      NULL
    })
  return(renv_lock)
}

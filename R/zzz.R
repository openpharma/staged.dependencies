STAGEDDEPS_FILENAME <- "staged_dependencies.yaml"
CONFIG_FILENAME <- "config.yaml" # staged.dependencies._storage_dir

# Use this function when you want to change the storage directory while the package is already loaded
# It does not delete the old storage directory
setup_storage_dir <- function(storage_dir) {
  set_storage_dir(storage_dir)
  # only copy config if storage dir does not exist
  if (!dir.exists(storage_dir)) {
    dir.create(storage_dir)
    copy_config_to_storage_dir()
  }
  if (!dir.exists(get_packages_cache_dir())) {
    dir.create(get_packages_cache_dir())
  }

  storage_dir
}


get_storage_dir <- function() {
  options()$staged.dependencies._storage_dir
}

set_storage_dir <- function(storage_dir) {
  options("staged.dependencies._storage_dir" = storage_dir)
}

.onLoad <- function(libname, pkgname) {

  storage_dir <- getOption(
    "staged.dependencies._storage_dir",
    path.expand("~/.staged_dependencies")
  )
  op <- options()
  op.package <- list(
    # mapping from hosts to
    staged.dependencies.token_mapping = c(
      "https://github.com" = "GITHUB_PAT",
      "https://gitlab.com" = "GITLAB_PAT"
    )
  )
  toset <- !(names(op.package) %in% names(op))
  if (any(toset)) options(op.package[toset])
  setup_storage_dir(storage_dir)
}

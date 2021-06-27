STORAGE_DIR <- path.expand("~/.staged.dependencies")
STAGEDDEPS_FILENAME <- "staged_dependencies.yaml"
CONFIG_FILENAME <- "config.yaml" # in STORAGE_DIR

.onLoad <- function(libname, pkgname) {
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

  # only copy config if STORAGE_DIR does not exist
  if (!dir.exists(STORAGE_DIR)) {
    dir.create(STORAGE_DIR)
    copy_config_to_storage_dir()
  }
  if (!dir.exists(get_packages_cache_dir())) {
    dir.create(get_packages_cache_dir())
  }
}

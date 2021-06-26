CACHE_DIR <- path.expand("~/.staged.dependencies")
STAGEDDEPS_FILENAME <- "staged_dependencies.yaml"

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

  if (!dir.exists(CACHE_DIR)) {
    dir.create(CACHE_DIR)
  }
}

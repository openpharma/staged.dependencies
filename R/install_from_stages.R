

#' Restore Staged Dependencies
#'
#' @importFrom yaml read_yaml
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' restore()
#' }
restore <- function(project = ".", upgrade = c("default", "ask", "always", "never")) {

  upgrade <- match.arg(upgrade)

  fpath <- normalizePath(
    file.path(project, "staged_dependencies.yaml"),
    winslash = "/", mustWork = FALSE
  )

  if (!file.exists(fpath)) {
    message("file staged_dependencies.yaml does not exits in project folder")
  } else {

    # TODO: is there a better way?
    local_branch <- system2("git", args = c("--git-dir", file.path(fpath, ".git"), "branch"), stdout = TRUE)

    if (identical(local_branch,128)) ## TODO: is there a better way?
      stop("no git repo")

    local_branch <- gsub("^\\*[[:space:]]", "", local_branch)

    x <- read_yaml(fpath)$repo

    staging_rule <- x$staging_rule

    lapply(x$repos, install_staged_dependency,
           upgrade = upgrade, local_branch = local_branch, staging_rule = staging_rule)
  }
}


install_staged_dependency <- function(x, upgrade, local_branch, staging_rule) {

  if (all(c("repo", "host", "type") %in% names(x)))
    stop("incomplete information for:", paste(paste(names(x), x, sep = ": "), collapse = ", "))

  GITHUB_PAT <- Sys.getenv("GITHUB_PAT")
  GITLAB_PAT <- Sys.getenv("GITLAB_PAT")

  remote_branches <- switch(x$type,
                            "github" = gh::gh(paste0("GET /repos/", x$repo, "/branches")), # todo GITHUB_PAT
                            "gitlab" = c(), # TODO: gitlab rest api call with GITLAB_PAT
                            stop(paste("remote types supported are github and gitlab,", x$ref, "has remote type:", x$type)),
  )

  # branch to install remote repo from
  remote_ref <- determine_remote_ref(local_branch, remote_branches, staging_rule)

  # TODO check if this works
  switch(x$type,
         "github" = do.call(remotes::install_github, c(x[-("type")], list(ref = branch, auth_token = GITHUB_PAT))),
         "gitlab" = do.call(remotes::install_gitlab, c(x[-("type")], list(ref = branch, auth_token = GITLAB_PAT)))
  )
}


determine_remote_ref <- function(local_branch, remote_branches, staging_rule) {

  # TODO: first implement the feature/* rule
  "master"
}


# TODO: create rstudio adin restore that calls restore(".")



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
restore <- function(local_branch = NULL, project = ".", upgrade = c("default", "ask", "always", "never")) {

  upgrade <- match.arg(upgrade)

  fpath <- normalizePath(
    file.path(project, "staged_dependencies.yaml"),
    winslash = "/", mustWork = FALSE
  )

  if (!file.exists(fpath)) {
    message("file staged_dependencies.yaml does not exits in project folder")
  } else {

    if (is.null(local_branch)) {
      # TODO: is there a better way?
      local_branch <- system2("git", args = c("--git-dir", file.path(project, ".git"), "branch"), stdout = TRUE)
      if (identical(local_branch,128)) ## TODO: is there a better way?
        stop("no git repo")
      local_branch <- gsub("^\\*[[:space:]]", "", local_branch)
    }

    x <- read_yaml(fpath)$repo

    lapply(x$repos, install_staged_dependency,
           upgrade = upgrade, local_branch = local_branch, staging_rule = x$staging_rule)

  }
}

empty_to_null <- function(x) {
  if (identical(x, ""))
    NULL
  else
    x
}

install_staged_dependency <- function(x, upgrade, local_branch, staging_rule) {

  if (all(c("repo", "host", "type") %in% names(x)))
    stop("incomplete information for:", paste(paste(names(x), x, sep = ": "), collapse = ", "))

  GITHUB_PAT <- Sys.getenv("GITHUB_PAT")
  GITLAB_PAT <- Sys.getenv("GITLAB_PAT")

  remote_branches <- switch(
    EXPR = x$type,
    "github" = {
      obj <- gh::gh(
        endpoint = paste0("GET /repos/", x$repo, "/branches"),
        .token = empty_to_null(GITHUB_PAT),
        .api_url = x$host,
        .send_headers = c("Accept" = "application/vnd.github.inertia-preview+json")
      )
      vapply(obj, `[[`, character(1), "name")
    },
    "gitlab" = c(), # TODO: gitlab rest api call with GITLAB_PAT
    stop(paste("remote types supported are github and gitlab,", x$ref, "has remote type:", x$type))
  )

  # branch to install remote repo from
  remote_ref <- determine_remote_ref(local_branch, remote_branches, staging_rule)

  # TODO check if this works
  switch(x$type,
         "github" = do.call(remotes::install_github, c(x[-("type")], list(ref = branch, auth_token = GITHUB_PAT))),
         "gitlab" = do.call(remotes::install_gitlab, c(x[-("type")], list(ref = branch, auth_token = GITLAB_PAT)))
  )
}


#' Implement Staging Rules
#'
#' @param local_brach name of branch of package to install
#' @param remote_branches names of
#'
#'
#' @examples
#'
#' determine_remote_ref("01_abc", c("main", "01_abc"))
#' determine_remote_ref("01_abc", c("main", "01_abc"), feature_keyword = "") # always match branches
#'
#' determine_remote_ref("pre-release/01_abc", c("main", "01_abc", "pre-release"))
#'
#' determine_remote_ref("feature:01_abc", c("main", "feature:01_abc", "devel"))
#'
#' determine_remote_ref("feature:01_abc", c("main", "feature:01_abc", "devel"))
#'
#'
determine_remote_ref <- function(local_branch, remote_branches, overall_fallback = "main", feature_keyword = "^feature:") {

  els <- unlist(strsplit(local_branch, "/", fixed = TRUE))

  if (grepl(feature_keyword, tail(els, 1)) && local_branch %in% remote_branches) {
    local_branch
  } else {
    fallback <- c(head(els, -1), overall_fallback)

    final_fallback <- fallback[fallback %in% remote_branches]

    if (length(final_fallback) == 0)
      stop("none of the fallback branches exist in the remote repository:", paste(fallback, collapse = ", "))
    else
      final_fallback[1]
  }
}

# TODO
check_downstream <- function(includ_current_package = TRUE) {

  # download downstream packages from particular branch and run R CMD build & R CMD check

}


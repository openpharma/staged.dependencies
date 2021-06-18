

#' Restore Staged Dependencies
#'
#' This reads the upstream dependencies for the project and installs the right branches based
#' on the target. This does not include the current package (specified by `project`).
#'
#' @md
#' @param project directory of project (for which to restore the dependencies according to target)
#' @inheritParams install_staged_dependency
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' restore()
#' }
#'
#' @importFrom yaml read_yaml
restore <- function(target = NULL, project = ".", include_current_package = TRUE, ...) {
  fpath <- normalizePath(
    file.path(project, "staged_dependencies.yaml"),
    winslash = "/", mustWork = FALSE # output error, see below
  )
  if (!file.exists(fpath)) {
    stop("file staged_dependencies.yaml does not exits in project folder")
  }

  if (is.null(target)) {
    # TODO: is there a better way? use git2r::branches
    # get current branch as target
    target <- get_current_branch(project)
  }

  repos_to_install <- read_yaml(fpath)$upstream_repos
  if (include_current_package) {
    repos_to_install <- c(repos_to_install, list())
  }
  lapply(repos_to_install, install_staged_dependency, target = target, ...)
}

#' Download downstream packages from particular branch and run `R CMD build & R CMD check`
#' For each downstream package, we also install all of its dependencies.
#'
#' @inheritParams restore
#' @param include_current_package whether to also install the current package
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_downstream()
#' }
check_downstream <- function(target = NULL, project = ".", include_current_package = TRUE, ...) {
  fpath <- normalizePath(
    file.path(project, "staged_dependencies.yaml"),
    winslash = "/", mustWork = FALSE # output error, see below
  )
  if (!file.exists(fpath)) {
    stop("file staged_dependencies.yaml does not exits in project folder")
  }

  if (is.null(target)) {
    # TODO: is there a better way? use git2r::branches
    # get current branch
    target <- get_current_branch(project)
  }

  x <- c(read_yaml(fpath)$upstream_repos, read_yaml(fpath)$downstream_repos)
  lapply(x, install_staged_dependency, target = target, ...)

  # todo
  # install this package as well
  # R CMD check "/Library/Frameworks/R.framework/Versions/3.6/Resources/library/scda.2021"
}

get_current_branch <- function(project) {
  target <- system2("git", args = c("--git-dir", file.path(project, ".git"), "rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
  if (identical(attr(target, "status"), 128L)) {
    stop("no git repo")
  }
  target
}

empty_to_null <- function(x) {
  if (identical(x, "")) {
    NULL
  } else {
    x
  }
}

# Returns the auth token from environment variable
get_auth_token <- function(host) {
  switch(
    host,
    "https://api.github.com" = Sys.getenv("PUBLIC_GITHUB_PAT"),
    "https://github.roche.com/api/v3" = Sys.getenv("ROCHE_GITHUB_PAT"), # todo
    "https://code.roche.com" = Sys.getenv("ROCHE_GITLAB_PAT") # todo: add to config file
  )
}

# get branches for package specified by x with the remote API
get_remote_branches <- function(x) {
  auth_token <- get_auth_token(x$host)
  available_branches <- switch(
    EXPR = x$type,
    "github" = {
      obj <- gh::gh(
        endpoint = paste0("GET /repos/", x$repo, "/branches"),
        .token = empty_to_null(auth_token),
        .api_url = x$host,
        .send_headers = c("Accept" = "application/vnd.github.inertia-preview+json")
      )
      vapply(obj, `[[`, character(1), "name")
    },
    "gitlab" = {
      # not working for GitLab, there is also the GitlabR package, but avoid this dependency
      # obj <- gh::gh(
      #   endpoint = paste0("GET /projects/", URLencode(x$repo, reserved = TRUE), "/repository/branches"),
      #   .token = empty_to_null(auth_token),
      #   .api_url = x$host
      # )
      obj <- content(GET(paste0(x$host, "/api/v4/projects/", URLencode(x$repo, reserved = TRUE),
                                "/repository/branches"), query=list(private_token=empty_to_null(auth_token))))

      vapply(obj, `[[`, character(1), "name")
    },
    stop(paste("remote types supported are github and gitlab,", x$ref, "has remote type:", x$type))
  )
}

#' Install a package from a git repo with branch corresponding to target
#'
#' @param x info about how to fetch the package from git (GitHub or GitLab)
#' @inheritParams determine_branch
#' @param ... additional args to pass to installation functions
#'
#' @examples
#' \dontrun{
#' install_staged_dependency(list(repo = "openpharma/staged.dependencies", host = "https://api.github.com",
#' type = "github"), "master", upgrade = FALSE)
#' install_staged_dependency(list(repo = "NEST/utils.nest", host = "https://github.roche.com/api/v3",
#'                                type = "github"), "master", upgrade = FALSE)
#' install_staged_dependency(list(repo = "nest/scda.2021", host = "https://code.roche.com", type = "gitlab"), "master", upgrade = FALSE)
#'
#' # deps_file <- yaml::read_yaml("staged_dependencies.yaml")
#' # install_staged_dependency(deps_file[["upstream_repos"]][[3]], "master", upgrade = FALSE)
#' }
#'
#' @importFrom httr content GET
install_staged_dependency <- function(x, target, ...) {

  if (!all(c("repo", "host", "type") %in% names(x))) {
    stop("incomplete information for:", paste(paste(names(x), x, sep = ": "), collapse = ", "))
  }

  # branch to install remote repo from
  branch <- determine_branch(target, available_branches = get_remote_branches(x))
  auth_token <- get_auth_token(x$host)

  switch(x$type,
         "github" = do.call(remotes::install_github, c(x[names(x) != "type"], list(ref = branch, auth_token = auth_token), list(...))),
         "gitlab" = do.call(remotes::install_gitlab, c(x[names(x) != "type"], list(ref = branch, auth_token = auth_token), list(...)))
  )
}

# install_staged_dependency(list(repo = "nest/scda.2021", host = "https://code.roche.com", type = "gitlab"), "master", upgrade = FALSE, force=TRUE, INSTALL_opts = "--with-keep.source")
# remotes:::remote_download
#
# # git clone https://oauth2:, token, host_without_https
# install_staged_dependency(x, target, ...)
# branch <- determine_branch(target, available_branches = get_remote_branches(x))
# path <- remotes:::remote_download(remotes:::github_remote(x$repo, ref = branch, auth_token = auth_token, host = x$host))
# R CMD check path
# R CMD install path

# todo: incorporate workflow in gitlab

#' Implement Staging Rules
#'
#' Given a feature branch (target), return the branch to build the target, given the available branches.
#' A target consists of branches separated by slashes of the form `name1/name2/.../nameN`.
#' Among the available branches, it searches in the order
#' `name1/name2/.../nameN`, `name2/name3/.../nameN`, `name3/name4/.../nameN`, ..., `nameN`.
#'
#' # todo: document
#'
#' @param target feature branch we want to build, includes fallbacks
#' @param available_branches branches that are available
#'
#' @examples
#' determine_branch("feature1", c("main", "feature1")) == "feature1"
#' determine_branch("feature1/devel", c("main", "devel", "feature1")) == "devel"
#' determine_branch("fix1/feature1/devel", c("main", "devel", "feature1", "feature1/devel", "fix1/feature1/devel", "fix1")) == "fix1/feature1/devel"
#' determine_branch("fix1/feature1/devel", c("main", "devel", "feature1", "feature1/devel", "fix1")) == "feature1/devel"
#' determine_branch("fix1/feature1/devel", c("main", "devel", "feature1", "fix1")) == "devel"
#' # determine_branch("feature1/release", c("main", "devel"))  # error because neither `feature1/release` nor `release` branch exists
#'
determine_branch <- function(target, available_branches) {
  els <- unlist(strsplit(target, "/", fixed = TRUE))
  branches_to_check <- rev(Reduce(function(x, y) paste0(y, "/", x), rev(els), accumulate = TRUE))

  for (branch in branches_to_check) {
    if (branch %in% available_branches) {
      return(branch)
    }
  }

  stop("Available branches ", toString(available_branches), " must include at least one of ", toString(branches_to_check))
}


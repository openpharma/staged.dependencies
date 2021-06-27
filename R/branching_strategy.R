#' Determine the branch to install based on feature (staging rules)
#'
#' Return the branch to build the feature, given the available branches.
#' A feature consists of branches separated by slashes of the form `name1@name2@...@nameN`.
#' Among the available branches, it searches in the order
#' `name1@name2@...@nameN`, `name2@name3@...@nameN`, `name3@name4@...@nameN`, ..., `nameN`.
#'
#' Use case: See the readme.
#'
#' @md
#' @param feature feature we want to build, includes fallbacks
#' @param available_branches branches to search in
#' @param branch_sep separator between branches in `feature`, `/` does not
#'   work well with `git` because it clashes with the filesystem paths
#'
#' @return branch to choose to match feature, error if no suitable branch was provided
#' @export
#'
#' @examples
#'
#' determine_branch("feature1", c("main", "feature1")) == "feature1"
#' determine_branch("feature1@devel", c("main", "devel", "feature1")) == "devel"
#' determine_branch("fix1@feature1@devel",
#' c("main", "devel", "feature1", "feature1@devel", "fix1@feature1@devel", "fix1")
#' ) == "fix1@feature1@devel"
#' determine_branch("fix1@feature1@devel",
#' c("main", "devel", "feature1", "feature1@devel", "fix1")
#' ) == "feature1@devel"
#' determine_branch("fix1@feature1@devel",
#' c("main", "devel", "feature1", "fix1")) == "devel"
#'
#' # error because neither `feature1@release` nor `release` branch exists
#' # determine_branch("feature1@release", c("main", "devel"))
determine_branch <- function(feature, available_branches, branch_sep = "@") {
  stopifnot(
    is_non_empty_char(feature),
    is.character(available_branches),
    is_non_empty_char(branch_sep)
  )

  els <- unlist(strsplit(feature, branch_sep, fixed = TRUE))
  branches_to_check <- rev(Reduce(function(x, y) paste0(y, branch_sep, x), rev(els), accumulate = TRUE))

  for (branch in branches_to_check) {
    if (branch %in% available_branches) {
      return(branch)
    }
  }

  stop("Available branches '", toString(available_branches), "' must include at least one of '",
       toString(branches_to_check), "'")
}

# infer the feature if it is null from the project branch
# if feature is provided, check it is consistent with the checked out project branch
infer_feature_from_branch <- function(feature = NULL, project = ".") {
  stopifnot(
    is.null(feature) || is_non_empty_char(feature)
  )
  check_dir_exists(project)
  project_branch <- get_current_branch(project)
  if (is.null(feature)) {
    feature <- project_branch
  }

  expected_project_branch <- determine_branch(
    feature, available_branches = setdiff(gsub("origin/", "", names(git2r::branches(project)), fixed = TRUE), "HEAD")
  )
  if (project_branch != expected_project_branch) {
    warning("feature ", feature, " would match ", expected_project_branch, " in project ", project,
            ", but currently checked out branch is ", project_branch)
  }

  feature
}

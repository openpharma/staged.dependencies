#' Determine the branch/tag to install based on feature (staging rules)
#'
#' Return the git ref (tag or branch) of the repo to install given the available branches and tags.
#'
#'
#' A ref is either a tag or branches separated by slashes of the form `name1@name2@...@nameN`.
#' Where separator is specified by `branch_sep` argument
#'
#' This function checks for an exact match for the tag if this is not found then
#' among the available branches, it searches in the order
#' `name1@name2@...@nameN`, `name2@name3@...@nameN`, `name3@name4@...@nameN`, ..., `nameN`
#'
#' @md
#' @param ref ref we want to build
#' @param available_refs data.frame with columns `ref` the names of the available refs
#'   and `type` (`branch` or `tag`)
#' @param fallback_branch the default branch to try to use if no other matches found
#' @param branch_sep separator between branches in `feature`, `/` does not
#'   work well with `git` because it clashes with the filesystem paths
#'
#' @return branch/tag to choose to match feature, error if no suitable branch was provided
#'   with the type attribute "tag" or "branch"
#'
#' @export
#'
#' @examples
#' determine_ref("feature1",
#'   data.frame(ref = c("main", "feature1"), type = "branch")
#' ) == structure("feature1", type = "branch")
#'
#' determine_ref("feature1@devel",
#'   data.frame(ref = c("main", "devel", "feature1"), type = "branch")
#' ) == structure("devel", type = "branch")
#'
#' determine_ref(
#'   ref = "fix1@feature1@devel",
#'   available_refs = data.frame(
#'     ref = c("main", "devel", "feature1", "feature1@devel",
#'     "fix1@feature1@devel", "fix1"),
#'     type = "branch")
#' ) == structure("fix1@feature1@devel", type = "branch")
#'
#' determine_ref(
#'   "fix1@feature1@devel",
#'   data.frame(ref = c("main", "devel", "feature1", "feature1@devel", "fix1"),
#'              type = "branch")
#' ) == structure("feature1@devel", type = "branch")
#'
#' determine_ref(
#'   "fix1@feature1@devel",
#'   data.frame(ref = c("main", "devel", "feature1", "fix1"), type = "branch")
#' ) == structure("devel", type = "branch")
#'
#' determine_ref("feature1@release", data.frame(ref = c("main", "devel"), type = "branch"))
#'
#' # error because neither `feature1@release` nor `release` branch exists
#' # determine_ref("feature1@release", data.frame(ref = c("master", "devel"), type = "branch"))
#'
#' # tag examples
#' determine_ref("v0.1",
#'   data.frame(ref = c("main", "devel", "feature1", "v0.1"), type = c(rep("branch", 3), "tag"))
#' ) == structure("v0.1", type = "tag")
#'
#' determine_ref("v0.2",
#'   data.frame(ref = c("main", "devel", "feature1", "v0.1"), type = c(rep("branch", 3), "tag"))
#' ) == structure("main", type = "branch")
#'
#'
determine_ref <- function(ref, available_refs, fallback_branch = "main", branch_sep = "@") {
  stopifnot(
    is_non_empty_char(ref),
    is.data.frame(available_refs),
    colnames(available_refs) == c("ref", "type"),
    is_non_empty_char(branch_sep)
  )

  # check for tag
  if (ref %in% available_refs$ref[available_refs$type == "tag"]){
    attr(ref, "type") <- "tag"
    return(ref)
  }

  # if tag not found now look at branch strategy
  available_branches <- available_refs$ref[available_refs$type == "branch"]

  els <- unlist(strsplit(ref, branch_sep, fixed = TRUE))
  branches_to_check <- union(
    rev(Reduce(function(x, y) paste0(y, branch_sep, x), rev(els), accumulate = TRUE)),
    fallback_branch
  )

  for (branch in branches_to_check) {
    if (branch %in% available_branches) {
      attr(branch, "type") <- "branch"
      return(branch)
    }
  }

  stop("Available refs '", toString(available_refs$ref), "' must include at least one of '",
       toString(branches_to_check), "'")
}

# infer the ref if it is null
infer_ref_from_branch <- function(project = ".") {
  check_dir_exists(project)
  return(get_current_branch(project))
}


check_ref_consistency <- function(ref, project = ".", fallback_branch = "main") {
  check_dir_exists(project)
  current_branch <- get_current_branch(project)
  # if in detached HEAD then we ignore the ref_consistency check (i.e. so gitlab
  # automation does not throw a warning)
  if (!is.null(current_branch)) {
    expected_current_branch <- determine_ref(ref,
      available_refs = available_references(project),
      fallback_branch = fallback_branch
    )
    if (current_branch != expected_current_branch) {
      warning("Branch ", ref, " would match ", expected_current_branch, " in project ", project,
              ", but currently checked out branch is ", current_branch)
    }
  }
}


# return a dataframe with columns ref (git tag or branch name), type ("branch" or "tag")
available_references <- function(repo = ".", branch_flag = "remote") {
  branches <- names(git2r::branches(repo = repo, flags = branch_flag))
  branches <- setdiff(gsub(paste0(git2r::remotes(repo)[1], "/"), "", branches, fixed = TRUE), "HEAD")
  refs <- data.frame(ref = branches, type = "branch")
  tags <- names(git2r::tags(repo))
  if (length(tags) > 0) {
    refs <- rbind(refs, data.frame(ref = tags, type = "tag"))
  }
  refs
}

# todo: into package.R
CACHE_DIR <- "~/.staged.dependencies"
if (!dir.exists(CACHE_DIR)) {
  dir.create(CACHE_DIR)
}

# todo: function to clean cache

get_repo_cache_dir <- function(repo, host) {
  file.path(CACHE_DIR, paste0(gsub("/", "_", repo, fixed = TRUE), "_", digest::digest(paste0(repo, "/", host))))
}

# system2_succeed <- function(...) {
#   res <- system2(..., stdout = TRUE)
#   if (!is.null(attr(res, "status"))) {
#     # stop("Error running command '", toString(list(...)), "': ", toString(attr(res, "errmsg")))
#     stop("Error running command")
#   }
#   res
# }
# system2_succeed("git", "hh")
# system2_succeed("echo", "hh"
# system2_succeed("git", args = c("-C", repo_dir, "pull")))
# system2_succeed("git", args = c("clone", file.path(host, repo), repo_dir))
# system2_succeed("git", args = c("-C", repo_dir, "branch", "-r", "-l"))
# system2_succeed("git", args = c("-C", repo_dir, "checkout", ))
#todo: remove

# todo: add auth tokens?

# checks out the correct branch (corresponding to target) in the repo, clones the repo if necessary
checkout_repo <- function(repo, host, target) {
  repo_dir <- get_repo_cache_dir(repo, host)
  if (!dir.exists(repo_dir)) {
    git_repo <- git2r::clone(url = file.path(host, repo), local_path = repo_dir)

  } else {
    git_repo <- git2r::repository(repo_dir)
  }
  git2r::pull(git_repo)
  # this directory should only contain remote branches (+ 1 local master branch)
  available_branches <- names(git2r::branches(git_repo, flags = "remote"))
  available_branches <- setdiff(gsub("origin/", "", available_branches, fixed = TRUE), "HEAD")
  branch <- determine_branch(target, available_branches)
  git2r::checkout(git_repo, branch = branch, force = TRUE) # force = TRUE to discard any changes (which should not happen)
  repo_dir
}

# get upstream repos and downstream repos according to yaml file in repo directory
get_deps_info <- function(repo_dir) {
  stopifnot(dir.exists(repo_dir))
  yaml_file <- file.path(repo_dir, "staged_dependencies.yaml")
  if (file.exists(yaml_file)) {
    yaml::read_yaml(yaml_file)
  } else {
    list(upstream_repos = list(), downstream_repos = list())
  }
}

# checks out all repos to match target, starting from repos_to_process and including all upstream repos recursively
# returns the order in which the repos must be installed
checkout_repos <- function(repos_to_process, target) {
  upstream_deps_graph <- list()
  while (length(repos_to_process) > 0) {
    repo_and_host <- repos_to_process[[1]]
    repos_to_process <- setdiff(repos_to_process, repo_and_host)

    stopifnot(!is.null(repo_and_host$repo))
    stopifnot(!is.null(repo_and_host$host))

    repo_dir <- checkout_repo(repo_and_host$repo, repo_and_host$host, target)
    upstream_deps <- get_deps_info(repo_dir)$upstream_repos
    processed_repos <- names(upstream_deps_graph)
    repos_to_process <- union(repos_to_process, setdiff(upstream_deps, processed_repos))
    upstream_deps_graph[[repo_and_host]] <- upstream_deps
  }

  install_order <- topological_sort(upstream_deps_graph)
  install_order
}

# gets the currently checked out branch
get_current_branch <- function(repo_dir) {
  git2r::repository_head(git2r::repository(repo_dir))$name
}

#' Install upstream dependencies of project corresponding to target
#'
#' This reads the upstream dependencies for the project and installs the right branches based
#' on the target.
#'
#' @md
#' @param project directory of project (for which to restore the dependencies according to target)
#'   must be a git repository; currently checked out branch must be a local branch (not a remote branch)
#' @param install_current_package whether to also install the current package
#' @inheritParams install_staged_dependency
#'
#' @return installed packages in installation order
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' install_upstream_deps()
#' }
#'
#' # todo @importFrom yaml read_yaml
install_upstream_deps <- function(target = NULL, project = ".", install_current_package = TRUE) {
  fpath <- normalizePath(
    file.path(project, "staged_dependencies.yaml"), #todo: yaml name into variable
    winslash = "/", mustWork = FALSE # output error, see below
  )
  if (!file.exists(fpath)) {
    warning("file staged_dependencies.yaml does not exist in project folder: not restoring anything")
  }

  project_branch <- get_current_branch(project)
  # todo: add
  # stopifnot(project_branch == determine_branch(
  #   target, available_branches = setdiff(gsub("origin/", "", names(git2r::branches(project)), fixed = TRUE), "HEAD")
  # ))

  if (is.null(target)) {
    target <- project_branch
  }

  repos_to_process <- get_deps_info(project)$upstream_repos
  install_order <- checkout_repos(repos_to_process)

  for (repo_and_host in install_order) {
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host)
    install_repo_add_sha(repo_dir)
  }

  installed_pkgs <- install_order
  if (install_current_package) {
    install_repo_add_sha(project)
    installed_pkgs <- c(installed_pkgs, project)
  }

  installed_pkgs
}

install_repo_add_sha <- function(repo_dir) {
  read_dcf <- function(path) {
    fields <- colnames(read.dcf(path))
    as.list(read.dcf(path, keep.white = fields)[1, ])
  }

  write_dcf <- function(path, desc) {
    write.dcf(
      rbind(unlist(desc)),
      file = path,
      keep.white = names(desc),
      indent = 0
    )
  }

  # returns the installed sha of a git package and NULL if package is not installed or sha was not saved in DESCRIPTION file
  get_local_sha <- function(pkg_name) {
    # see remotes:::package2remote
    pkg_desc <- tryCatch(utils::packageDescription(pkg_name),
                         error = function(e) NA, warning = function(e) NA)
    if (identical(pkg_desc, NA)) {
      return(NULL)
    }
    pkg_desc$RemoteSha
  }

  commit_sha <- git2r::sha(git2r::repository_head(git2r::repository(repo_dir)))

  # see remotes:::add_metadata
  source_desc <- file.path(repo_dir, "DESCRIPTION")
  desc <- read_dcf(source_desc)
  desc <- utils::modifyList(desc, list(RemoteSha = commit_sha))
  write_dcf(source_desc, desc)

  # only install if SHA differs
  if (identical(commit_sha, get_local_sha(desc$Package))) {
    cat("Skipping installation of", repo_dir, "since same commit sha already installed")
    return(invisible(NULL))
  }

  install.packages(repo_dir, repos = NULL, type = "source")
}

# topologically sorts nodes so that parents are listed before all their children
# child_to_parents: mapping from child to its parents (upstream dependencies)
topological_sort <- function(child_to_parents) {
  # depth-first search from children to parents, then output nodes in the order of finishing times
  ordering <- c()

  treat_node <- function(node) {
    # cat("Treating node '", node, "'")
    # Sys.sleep(1)
    for (parent in setdiff(child_to_parents[[node]], ordering)) {
      treat_node(parent)
    }
    ordering <<- c(ordering, node)
  }

  nodes_to_process <- names(child_to_parents)
  while (length(nodes_to_process) > 0) {
    treat_node(nodes_to_process[[1]])
    nodes_to_process <- setdiff(nodes_to_process, ordering)
  }

  ordering
}
# all(topological_sort(list(n1 = c(), n2 = c("n1"), n3 = c("n2"), n4 = c("n2", "n3"))) == c("n1", "n2", "n3", "n4"))
# is.null(topological_sort(list()))

child_to_parents <- list(n1 = c(), n2 = c("n1"), n3 = c("n2"), n4 = c("n2", "n3"))
target <- "feature1/devel"
repo <- "Roche/rtables"
host <- "https://github.com"



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


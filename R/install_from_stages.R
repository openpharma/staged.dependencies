# todo: into package.R
CACHE_DIR <- path.expand("~/.staged.dependencies")
# unlink(CACHE_DIR, recursive = TRUE)
if (!dir.exists(CACHE_DIR)) {
  dir.create(CACHE_DIR)
}
STAGEDDEPS_FILENAME <- "staged_dependencies.yaml"

# todo: function to clean cache
# todo: add auth tokens?
# todo: check_downstream
# todo: allow local source (rather than remote git)
# todo: rstudio addin
# use gert instead of git2r to handle credentials smoothly
# todo: check verbose arg
# todo: clean up functions

cat_nl <- function(...) cat(paste0(paste(...), "\n"))

get_repo_cache_dir <- function(repo, host) {
  file.path(CACHE_DIR, paste0(gsub("/", "_", repo, fixed = TRUE), "_", digest::digest(paste0(repo, "/", host))))
}

get_repo_url <- function(repo, host) {
  # todo
  file.path(host, repo)
  # if (host == "https://code.roche.com") {
  #   paste0("git@ssh.code.roche.com:", repo, ".git")
  # } else {
  #   stop("host ", host, " not yet supported")
  # }
}

# todo: via environment vars
# Returns the auth token from environment variable
get_authtoken_envvar <- function(host) {
  switch(
    host,
    "https://api.github.com" = "PUBLIC_GITHUB_PAT",
    "https://github.roche.com/api/v3" = "ROCHE_GITHUB_PAT", # todo
    "https://code.roche.com" = "ROCHE_GITLAB_PAT" # todo: add to config file
  )
}

# checks out the correct branch (corresponding to target) in the repo, clones the repo if necessary
checkout_repo <- function(repo, host, target, verbose = 0) {
  repo_dir <- get_repo_cache_dir(repo, host)
  creds <- git2r::cred_token(token = get_authtoken_envvar(host))
  if (!dir.exists(repo_dir)) {
    git_repo <- git2r::clone(
      url = get_repo_url(repo, host), local_path = repo_dir, credentials = creds, progress = verbose >= 2
    )
    # ssh not enabled, see https://github.com/ropensci/git2r/issues/415
    # gert::git_clone(get_repo_url(repo, host), path = repo_dir, verbose = verbose)
    # git_repo <- git2r::repository(repo_dir)
  } else {
    git_repo <- git2r::repository(repo_dir)
    # gert::git_pull(repo = repo_dir)
    git2r::pull(git_repo, credentials = creds)
  }
  # this directory should only contain remote branches (+ 1 local master branch)
  available_branches <- names(git2r::branches(git_repo, flags = "remote"))
  available_branches <- setdiff(gsub("origin/", "", available_branches, fixed = TRUE), "HEAD")
  branch <- determine_branch(target, available_branches)
  git2r::checkout(git_repo, branch = branch, force = TRUE) # force = TRUE to discard any changes (which should not happen)
  if (verbose >= 1) {
    cat_nl("Checked out branch", branch, "from repo in directory", repo_dir)
  }
  repo_dir
}

# get upstream repos and downstream repos according to yaml file in repo directory
# if yaml file does not exist, returns empty lists
get_deps_info <- function(repo_dir) {
  stopifnot(dir.exists(repo_dir))
  yaml_file <- file.path(repo_dir, STAGEDDEPS_FILENAME)
  if (file.exists(yaml_file)) {
    content <- yaml::read_yaml(yaml_file)
    required_fields <- c("upstream_repos", "downstream_repos")
    if (!all(required_fields %in% names(content))) {
      stop("File ", yaml_file, " must contain fields ", toString(required_fields))
    }
    content
  } else {
    list(upstream_repos = list(), downstream_repos = list())
  }
}

# we need these functions because R does not support tuple indices,
# e.g. lst[[c(host=.., repo=..)]] is not possible
# todo: use R package collections?
hash_repo_and_host <- function(repo_and_host) {
  paste0(repo_and_host$repo, " @ ", repo_and_host$host)
}
unhash_repo_and_host <- function(hashed_repo_and_host) {
  repo_and_host <- strsplit(hashed_repo_and_host, " @ ", fixed = TRUE)[[1]]
  list(repo = repo_and_host[[1]], host = repo_and_host[[2]])
}

# checks out all repos to match branch determined by target,
# starting from repos_to_process and including all upstream repos recursively
# returns the order in which the repos must be installed
rec_checkout_repos <- function(repos_to_process, target, verbose = 0) {
  hashed_repos_to_process <- lapply(repos_to_process, hash_repo_and_host)
  rm(repos_to_process)

  upstream_deps_graph <- list()
  while (length(hashed_repos_to_process) > 0) {
    hashed_repo_and_host <- hashed_repos_to_process[[1]]
    hashed_repos_to_process <- hashed_repos_to_process[-1]

    repo_and_host <- unhash_repo_and_host(hashed_repo_and_host)
    stopifnot(!is.null(repo_and_host$repo))
    stopifnot(!is.null(repo_and_host$host))

    repo_dir <- checkout_repo(repo_and_host$repo, repo_and_host$host, target, verbose = verbose)

    hashed_upstream_deps <- lapply(get_deps_info(repo_dir)$upstream_repos, hash_repo_and_host)
    hashed_processed_repos <- names(upstream_deps_graph)
    hashed_repos_to_process <- union(hashed_repos_to_process, setdiff(hashed_upstream_deps, hashed_processed_repos))
    upstream_deps_graph[[hashed_repo_and_host]] <- hashed_upstream_deps
  }

  install_order <- topological_sort(upstream_deps_graph)
  install_order <- lapply(install_order, unhash_repo_and_host)
  install_order
}

# gets the currently checked out branch
get_current_branch <- function(repo_dir) {
  git2r::repository_head(git2r::repository(repo_dir))$name
}

warn_if_stageddeps_inexistent <- function(project) {
  fpath <- normalizePath(
    file.path(project, STAGEDDEPS_FILENAME), #todo: yaml name into variable
    winslash = "/", mustWork = FALSE # output error, see below
  )
  if (!file.exists(fpath)) {
    warning("file staged_dependencies.yaml does not exist in project folder: not restoring anything")
  }
}

# todo: Install downstream dependencies into temporary path

#' Check downstream dependencies
#'
#' It installs the downstream dependencies and their upstream dependencies,
#' and then runs `rcmdcheck` (`R CMD check`) on the downstream dependencies.
#'
#' @md
#' @param dry_install_and_check whether to install upstream dependencies and run the checks;
#'   useful to see a dry-run (it however updates the cached repos!)
#' @inheritParams install_upstream_deps
#' @export
#'
#'
check_downstream <- function(target = NULL, project = ".", downstream_repos = NULL, dry_install_and_check = FALSE, verbose = 0) {
  warn_if_stageddeps_inexistent(project)

  project_branch <- get_current_branch(project)
  if (is.null(target)) {
    target <- project_branch
  }

  if (is.null(downstream_repos)) {
    downstream_repos <- get_deps_info(project)$downstream_repos
  }

  install_order <- rec_checkout_repos(downstream_repos, target, verbose = verbose)
  for (repo_and_host in install_order) {
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host)
    if (hash_repo_and_host(repo_and_host) %in% lapply(downstream_repos, hash_repo_and_host)) {
      if (!dry_install_and_check) {
        rcmdcheck::rcmdcheck(repo_dir, error_on = "warning")
      } else if (verbose >= 1) {
        cat_nl("Skipping check of", repo_dir)
      }
    }
    if (!dry_install_and_check) {
      install_repo_add_sha(repo_dir)
    } else if (verbose >= 1) {
      cat_nl("Skipping installation of", repo_dir)
    }
  }
}

#' Install upstream dependencies of project corresponding to target
#'
#' This reads the upstream dependencies for the project and installs the right branches based
#' on the target.
#'
#' @md
#' @param project directory of project (for which to restore the dependencies according to target)
#'   must be a git repository; currently checked out branch must be a local branch (not a remote branch)
#' @param install_project whether to also install the current package (project)
#' @param verbose verbosity level (0: None, 1: more, 2: includes git checkout)
#' @param dry_install whether to install or just print (useful for dry-runs, but still
#'   checks out the git repos)
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
install_upstream_deps <- function(target = NULL, project = ".",
                                  install_project = TRUE, dry_install = FALSE, verbose = FALSE) {
  warn_if_stageddeps_inexistent(project)

  project_branch <- get_current_branch(project)
  if (is.null(target)) {
    target <- project_branch
  }

  expected_project_branch <- determine_branch(
    target, available_branches = setdiff(gsub("origin/", "", names(git2r::branches(project)), fixed = TRUE), "HEAD")
  )
  if (project_branch != expected_project_branch) {
    warning("target ", target, " would match ", expected_project_branch,
            ", but currently checked out branch is ", project_branch)
  }

  repos_to_process <- get_deps_info(project)$upstream_repos
  install_order <- rec_checkout_repos(repos_to_process, target, verbose = verbose)

  if (verbose) {
    cat_nl("Installing upstream packages: ", toString(install_order))
  }
  for (repo_and_host in install_order) {
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host)
    if (!dry_install) {
      install_repo_add_sha(repo_dir)
    }
  }

  installed_pkgs <- install_order
  if (install_project) {
    if (verbose) {
      cat_nl("Installing current package from directory ", project)
    }
    if (!dry_install) {
      install_repo_add_sha(project)
    }
    installed_pkgs <- c(installed_pkgs, project)
  }

  installed_pkgs
}

#' Install git repository
#'
#' It adds the git SHA to the DESCRIPTION file, so that the package
#' does not need to be installed again when the same commit is already
#' installed.
#'
#' @param repo_dir directory of repo
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
    cat_nl("Skipping installation of", repo_dir, "since same commit sha already installed")
    return(invisible(NULL))
  }

  install.packages(repo_dir, repos = NULL, type = "source")
}

#' Topologically sorts nodes so that parents are listed before all their children
#'
#' @param child_to_parents  mapping from child to its parents (upstream dependencies)
#'
#' @return vector listing parents before children
#'
#' @examples
#' all(topological_sort(list(n1 = c(), n2 = c("n1"), n3 = c("n2"), n4 = c("n2", "n3"))) == c("n1", "n2", "n3", "n4"))
#' is.null(topological_sort(list()))
#'
topological_sort <- function(child_to_parents) {
  # depth-first search from children to parents, then output nodes in the order of finishing times
  ordering <- c()

  treat_node <- function(node) {
    # cat_nl("Treating node '", node, "'")
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


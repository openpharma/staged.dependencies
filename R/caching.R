# directory where to clone packages to
get_packages_cache_dir <- function() {
  file.path(STORAGE_DIR, "packages_cache")
}

#' Clear the repository cache
#'
#' Use this function to clear the package cache and reclone some
#' or all repositories (depending on `pattern`) if
#' the `git` operations fail.
#'
#' @md
#' @param pattern files to remove, see `unlink` (wildcards `*` and `?` allowed)
#' @export
clear_cache <- function(pattern = "*") {
  unlink(file.path(get_packages_cache_dir(), pattern), recursive = TRUE)
}

# copies example config file to package settings directory
# fails if copy did not work
copy_config_to_storage_dir <- function() {
  stopifnot(file.copy(
    system.file("config.yaml", package = "staged.dependencies", mustWork = TRUE),
    STORAGE_DIR
  ))
}

# directory where repo is cached locally
get_repo_cache_dir <- function(repo, host, local = FALSE) {
  stopifnot(
    is_non_empty_char(repo),
    is_non_empty_char(host),
    is.logical(local)
  )
  # the host can be rather long, so we hash it
  # a repo is uniquely identified by the pair (repo, host)
  # hash both (repo, host) since otherwise the repos
  # `owner/repo` and `owner_repo` (without owner -> unlikely) may collide
  prefix <- if (local) "local_" else ""
  file.path(
    get_packages_cache_dir(),
    paste0(
      prefix, gsub("/", "_", repo, fixed = TRUE),
      "_", digest::digest(paste0(repo, "/", host))
    )
  )
}

# get currently active branch of repo in cache
get_active_branch_in_cache <- function(repo, host, local = FALSE) {
  get_current_branch(get_repo_cache_dir(repo, host, local))
}

# copies a local directory to the cache dir and commits the current state in
# that cache dir, so the SHA can be added to the DESCRIPTION file
# note: files in .gitignore are also available to the package locally
copy_local_repo_to_cachedir <- function(local_dir, repo, host, verbose = 0) {
  check_dir_exists(local_dir, prefix = "Local directory: ")
  check_verbose_arg(verbose)

  repo_dir <- get_repo_cache_dir(repo, host, local = TRUE)
  if (dir.exists(repo_dir)) {
    unlink(repo_dir, recursive = TRUE)
  }

  if (verbose >= 1) {
    message(paste("Copying local dir", local_dir, "to cache dir", repo_dir))
  }
  # file.copy copies a directory inside an existing directory
  fs::dir_copy(local_dir, repo_dir)

  if ((length(git2r::status(repo_dir)$staged) > 0) ||
      (length(git2r::status(repo_dir)$unstaged) > 0) ||
      (length(git2r::status(repo_dir)$untracked) > 0)) {
    # add all files, including untracked (all argument of git2r::commit does not do this)
    git2r::add(repo_dir, ".")
    git2r::commit(
      repo_dir,
      paste0("committing everything for installation, copied from ", local_dir)
    )
  }

  repo_dir
}

# get upstream repos and downstream repos according to yaml file in repo directory
# if yaml file does not exist, returns empty lists
get_deps_info <- function(repo_dir) {
  check_dir_exists(repo_dir, "deps_info: ")

  yaml_file <- file.path(repo_dir, STAGEDDEPS_FILENAME)
  if (file.exists(yaml_file)) {
    content <- yaml::read_yaml(yaml_file)
    required_fields <- c("upstream_repos", "downstream_repos", "current_repo")
    if (!all(required_fields %in% names(content))) {
      stop("File ", yaml_file, " invalid, it must contain fields ", toString(required_fields))
    }
    content
  } else {
    list(upstream_repos = list(), downstream_repos = list(),
         # function() so it does not error immediately
         current_repo = function() stop("Directory ", repo_dir, " has no ", STAGEDDEPS_FILENAME))
  }
}

error_if_stageddeps_inexistent <- function(project) {
  fpath <- normalizePath(
    file.path(project, STAGEDDEPS_FILENAME),
    winslash = "/", mustWork = FALSE # output error, see below
  )
  if (!file.exists(fpath)) {
    stop("file staged_dependencies.yaml does not exist in project folder: not restoring anything")
  }
}

# local_repos: data.frame that maps repo and host to local directory
# returns named character vector mapping hashed repo and host to the directory
get_hashed_repo_to_dir_mapping <- function(local_repos) {
  stopifnot(
    is.data.frame(local_repos) || is.null(local_repos),
    setequal(colnames(local_repos), c("repo", "host", "directory"))
  )
  if (is.null(local_repos) || (nrow(local_repos) == 0)) {
    list()
  } else {
    stats::setNames(local_repos$directory, hash_repo_and_host(local_repos))
  }
}

#' Recursively check out all repos to match branch determined by feature
#' starting from repos_to_process.
#'
#' @md
#' @param repos_to_process `list` of `list(repo, host)`
#' @param feature (`character`) feature to build
#' @param direction (`character`) either or both of "upstream" and "downstream"
#'   to recursively checkout upstream and/or downstream dependencies
#' @param local_repos (`data.frame`) repositories that should be taken from
#'   local rather than cloned; columns are `repo, host, directory`
#' @param verbose (`numeric`) verbosity level, incremental;
#'   (0: None, 1: packages that get installed + high-level git operations,
#'   2: includes git checkout infos)
#'
#' @return the upstream and/or downstream dependency graphs
rec_checkout_repos <- function(repos_to_process, feature, direction = c("upstream"),
                               local_repos = get_local_pkgs_from_config(),
                               verbose = 0) {
  stopifnot(
    is.list(repos_to_process),
    all(direction %in% c("upstream", "downstream")), length(direction) >= 1
  )
  check_verbose_arg(verbose)

  local_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
  rm(local_repos)

  hashed_repos_to_process <- vapply(repos_to_process, hash_repo_and_host, character(1))
  rm(repos_to_process)

  # only one of them may be filled depending on the direction
  upstream_deps_graph <- c()
  downstream_deps_graph <- c()

  while (length(hashed_repos_to_process) > 0) {
    hashed_repo_and_host <- hashed_repos_to_process[[1]]
    hashed_repos_to_process <- hashed_repos_to_process[-1]

    repo_and_host <- unhash_repo_and_host(hashed_repo_and_host)
    stopifnot(!is.null(repo_and_host$repo))
    stopifnot(!is.null(repo_and_host$host))

    if (hashed_repo_and_host %in% names(local_repo_to_dir)) {
      repo_dir <- copy_local_repo_to_cachedir(
        local_repo_to_dir[[hashed_repo_and_host]], repo_and_host$repo, repo_and_host$host,
        verbose = verbose
      )
    } else {
      repo_dir <- checkout_repo(
        get_repo_cache_dir(repo_and_host$repo, repo_and_host$host),
        get_repo_url(repo_and_host$repo, repo_and_host$host),
        token_envvar = get_authtoken_envvar(repo_and_host$host),
        select_branch_rule = function(available_branches) {
          determine_branch(feature, available_branches)
        },
        verbose = verbose
      )
    }

    hashed_new_repos <- c()
    if ("upstream" %in% direction) {
      # Attention: use lapply because with vapply, vector may be NULL, otherwise assignment to
      # upstream_deps_graph removes the element
      hashed_upstream_repos <- lapply(get_deps_info(repo_dir)$upstream_repos, hash_repo_and_host)
      upstream_deps_graph[[hashed_repo_and_host]] <- hashed_upstream_repos
      hashed_new_repos <- c(hashed_new_repos, hashed_upstream_repos)
    }
    if ("downstream" %in% direction) {
      hashed_downstream_repos <- lapply(get_deps_info(repo_dir)$downstream_repos, hash_repo_and_host)
      downstream_deps_graph[[hashed_repo_and_host]] <- hashed_downstream_repos
      hashed_new_repos <- c(hashed_new_repos, hashed_downstream_repos)
    }
    hashed_processed_repos <- union(names(upstream_deps_graph), names(downstream_deps_graph))
    hashed_repos_to_process <- union(
      hashed_repos_to_process, setdiff(hashed_new_repos, hashed_processed_repos)
    )
  }

  res <- list()
  if ("upstream" %in% direction) {
    res[["upstream_deps"]] <- upstream_deps_graph
  }
  if ("downstream" %in% direction) {
    res[["downstream_deps"]] <- downstream_deps_graph
  }

  res
}

# directory where to clone packages to
get_packages_cache_dir <- function() {
  file.path(get_storage_dir(), "packages_cache")
}

#' Clear the repository cache
#'
#' Use this function to clear the package cache of some
#' or all repositories (depending on `pattern`) if
#' the `git` operations fail.
#'
#' @md
#' @param pattern files to remove, see `unlink` (wildcards `*` and `?` allowed)
#' @export
#'
#' @examples
#' \dontrun{
#' clear_cache()
#' clear_cache("*elecinfra*")
#' }
clear_cache <- function(pattern = "*") {
  dirs_to_remove <- Sys.glob(file.path(get_packages_cache_dir(), pattern))
  if (length(dirs_to_remove) > 0) {
    fs::dir_delete(dirs_to_remove)
  }
  if (!identical(pattern, "*")) {
    direcs <- dir(get_packages_cache_dir())
    if (length(direcs) == 0) {
      message("Cache empty")
    } else {
      message("Directories remaining in cache:\n", paste(direcs, collapse = "\n"))
    }
  }
}

# copies example config file to package settings directory
# fails if copy did not work
copy_config_to_storage_dir <- function() {
  stopifnot(file.copy(
    system.file("config.yaml", package = "staged.dependencies", mustWork = TRUE),
    get_storage_dir()
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
copy_local_repo_to_cachedir <- function(local_dir, repo, host, select_branch_rule, verbose = 0) {
  check_dir_exists(local_dir, prefix = "Local directory: ")
  check_verbose_arg(verbose)

  repo_dir <- get_repo_cache_dir(repo, host, local = TRUE)
  if (dir.exists(repo_dir)) {
    fs::dir_delete(repo_dir)
  }

  if (verbose >= 1) {
    message(paste("Copying local dir", local_dir, "to cache dir", repo_dir))
  }
  # file.copy copies a directory inside an existing directory
  fs::dir_copy(local_dir, repo_dir)

  # check that locally checked out branch is consistent with branch rule
  available_branches <- names(git2r::branches(repo_dir, flags = "local"))
  branch <- select_branch_rule(available_branches)
  stopifnot(branch %in% available_branches)
  if (branch != get_current_branch(repo_dir)) {
    stop("You must check out branch ", branch, " for repository in directory ", repo_dir,
         ", currently ", get_current_branch(repo_dir), " is checked out.")
  }

  if ((length(git2r::status(repo_dir)$staged) > 0) ||
      (length(git2r::status(repo_dir)$unstaged) > 0) ||
      (length(git2r::status(repo_dir)$untracked) > 0)) {
    # add all files, including untracked (all argument of git2r::commit does not do this)
    if (verbose >= 2) {
      message("Adding all of the following files: \n",
              paste(utils::capture.output(git2r::status(repo_dir)), collapse = "\n"))
    }
    git2r::add(repo_dir, ".")
    git2r::commit(
      repo_dir,
      paste0("committing everything for installation, copied from ", local_dir)
    )
  }

  branch <-  git2r::repository_head(repo_dir)$name
  return(list(dir = repo_dir, branch = paste0("local (", branch, ")")))
}

# local_repos: data.frame that maps repo and host to local directory
# returns named character vector mapping hashed repo and host to the directory
get_hashed_repo_to_dir_mapping <- function(local_repos) {
  if (is.null(local_repos) || (nrow(local_repos) == 0)) {
    list()
  } else {
    stopifnot(
      is.data.frame(local_repos),
      setequal(colnames(local_repos), c("repo", "host", "directory"))
    )
    stats::setNames(local_repos$directory, hash_repo_and_host(local_repos))
  }
}

#' Recursively check out all repos to match branch determined by feature
#' starting from repos_to_process.
#'
#' This uses the `staged_dependencies.yaml` to discover the upstream
#' and downstream packages.
#' Another function allows to check that only (and all) direct upstream
#' and downstream packages are listed there.
#' The packages listed there are internal packages. All other dependencies
#' listed in the `DESCRIPTION` file are external dependencies.
#'
#' @md
#' @param repos_to_process `list` of `list(repo, host)` repos to start from
#' @param feature (`character`) feature to build
#' @param direction (`character`) direction in which to discover packages
#'   either or both of "upstream" and "downstream"
#'   to recursively checkout upstream and/or downstream dependencies
#' @param local_repos (`data.frame`) repositories that should be taken from
#'   local rather than cloned; columns are `repo, host, directory`
#' @param verbose (`numeric`) verbosity level, incremental;
#'   (0: None, 1: packages that get installed + high-level git operations,
#'   2: includes git checkout infos)
#'
#' @return A data frame, one row per checked out repository with columns
#' repo, host and cache_dir
rec_checkout_internal_deps <- function(repos_to_process, feature,
                                      direction = c("upstream"),
                                      local_repos = get_local_pkgs_from_config(),
                                      verbose = 0) {
  stopifnot(
    is.list(repos_to_process)
  )
  check_direction_arg(direction)
  check_verbose_arg(verbose)

  local_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
  rm(local_repos)

  hashed_repos_to_process <- vapply(repos_to_process, hash_repo_and_host, character(1))
  rm(repos_to_process)

  hashed_processed_repos <- list()
  hashed_repos_branches <- list()

  while (length(hashed_repos_to_process) > 0) {
    hashed_repo_and_host <- hashed_repos_to_process[[1]]
    hashed_repos_to_process <- hashed_repos_to_process[-1]

    repo_and_host <- unhash_repo_and_host(hashed_repo_and_host)
    stopifnot(!is.null(repo_and_host$repo))
    stopifnot(!is.null(repo_and_host$host))

    if (hashed_repo_and_host %in% names(local_repo_to_dir)) {
      repo_info <- copy_local_repo_to_cachedir(
        local_repo_to_dir[[hashed_repo_and_host]], repo_and_host$repo, repo_and_host$host,
        select_branch_rule = function(available_branches) {
          determine_branch(feature, available_branches)
        },
        verbose = verbose
      )
    } else {
      repo_info <- checkout_repo(
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
      hashed_upstream_repos <- lapply(get_yaml_deps_info(repo_info$dir)$upstream_repos, hash_repo_and_host)
      hashed_new_repos <- c(hashed_new_repos, hashed_upstream_repos)
    }
    if ("downstream" %in% direction) {
      hashed_downstream_repos <- lapply(get_yaml_deps_info(repo_info$dir)$downstream_repos, hash_repo_and_host)
      hashed_new_repos <- c(hashed_new_repos, hashed_downstream_repos)
    }
    hashed_processed_repos[[hashed_repo_and_host]] <- repo_info$dir
    hashed_repos_branches[[hashed_repo_and_host]] <- repo_info$branch
    hashed_repos_to_process <- union(
      hashed_repos_to_process, setdiff(hashed_new_repos, names(hashed_processed_repos))
    )
  }

  df <- data.frame(unhash_repo_and_host(names(hashed_processed_repos)), stringsAsFactors = FALSE)
  df$cache_dir <- unlist(unname(hashed_processed_repos))
  df$branch <- unlist(unname(hashed_repos_branches))
  return(df)
}

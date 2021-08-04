# returns the environment variable that stores the auth token
get_authtoken_envvar <- function(host) {
  stopifnot(is_non_empty_char(host))
  token_mapping <- getOption("staged.dependencies.token_mapping")
  if (!host %in% names(token_mapping)) {
    stop("unknown host ", host, ", please set the package option staged.dependencies.token_mapping")
  }
  token_mapping[[host]]
}

# url for `git clone`
get_repo_url <- function(repo, host) {
  stopifnot(
    is_non_empty_char(repo),
    is_non_empty_char(host)
  )
  file.path(host, paste0(repo, ".git"))
}

# gets the currently checked out branch
get_current_branch <- function(git_repo) {
  git2r::repository_head(git_repo)$name
}

# checks that all branches start with origin/
check_only_remote_branches <- function(git_repo) {
  all_branches <- names(git2r::branches(git_repo))
  stopifnot(all(vapply(all_branches, function(x) startsWith(x, "origin/"), logical(1))))
}

# clones the repo and only keeps remote branches
# if repo is already there, fetches and prunes (removes) remote branches that are
# no longer there
# select_branch_rule is a function that is given the available branches
# and selects one of them
# verbose level: 0: none, 1: print high-level git operations, 2: print git clone detailed messages etc.
#get_repo_cache_dir(repo = "maximilian_oliver.mordig/testPruneFetch", host = "https://code.roche.com"); verbose <- 2; select_branch_rule <- function(x) "master"
checkout_repo <- function(repo_dir, repo_url, select_branch_rule, token_envvar, verbose = 0) {
  stopifnot(
    is.function(select_branch_rule)
  )
  check_verbose_arg(verbose)

  creds <- git2r::cred_token(token = token_envvar)
  if (!dir.exists(repo_dir)) {
    stopifnot(is_non_empty_char(repo_url))
    if (verbose >= 1) {
      message(paste("clone", repo_url, "to directory", repo_dir))
    }

    # catch some common errors
    # only do this when cloning because the API calls introduce quite some time overhead
    if (grepl("https://github.com", repo_url, fixed = TRUE)) {
      if (!identical(httr::status_code(httr::HEAD("https://github.com")), 200L)) {
        stop("Host https://github.com not reachable")
      }
      repo <- paste(tail(strsplit(repo_url, "/", fixed = TRUE)[[1]], 2), collapse = "/")
      repo <- substr(repo, start = 0, stop = nchar(repo) - nchar(".git"))
      tryCatch(
        # will error if URL not reachable
        gh::gh(paste0("/repos/", repo), token = Sys.getenv(token_envvar)),
        error = function(e) {
          stop(
            paste0("Could not access repo '", repo,
                   "'. Check that repo and token in envvar '", token_envvar,
                   "' are correct.\n"),
            e
          )
        }
      )
    }

    git_repo <- git2r::clone(
      url = repo_url, local_path = repo_dir, credentials = creds, progress = verbose >= 2
    )

    # git automatically created local tracking branch (for master or main), checkout
    # corresponding remote branch and delete local branch, so we only have remote
    # branches
    local_branch <- git2r::repository_head(git_repo)
    remote_branch <- git2r::branch_get_upstream(local_branch)
    git2r::checkout(git_repo, branch = remote_branch$name)
    git2r::branch_delete(local_branch)
    rm(local_branch, remote_branch)

    # todo: on.exit if unsuccessful
  } else {
    if (verbose >= 1) {
      message(paste("fetch", git2r::remote_url(repo_dir), "in directory", repo_dir))
    }

    git_repo <- git2r::repository(repo_dir)
    check_only_remote_branches(git_repo)
    # prune (remove) remote branches that were deleted from remote
    git2r::config(git_repo, remote.origin.prune = "true")
    git2r::fetch(git_repo, name = "origin", credentials = creds, verbose = verbose >= 2)
  }

  check_only_remote_branches(git_repo)
  available_branches <- names(git2r::branches(git_repo))
  available_branches <- setdiff(gsub("origin/", "", available_branches, fixed = TRUE), "HEAD")
  branch <- select_branch_rule(available_branches)
  stopifnot(branch %in% available_branches)
  branch <- paste0("origin/", branch)


  # force = TRUE to discard any changes (which should not happen)
  if (startsWith(git_repo$path, get_packages_cache_dir())) {
    if (verbose >= 1) {
      message("   - in cache: reset --hard HEAD")
    }
    git2r::reset(git_repo, reset_type = "hard", path = "HEAD")
  }
  if (verbose >= 1) {
    message(paste("   - checkout branch", branch, "in directory", repo_dir))
  }
  git2r::checkout(git_repo, branch = branch, force = TRUE)

  repo_dir
}

#' Install a git repository
#'
#' It adds the git SHA to the DESCRIPTION file, so that the package
#' does not need to be installed again when the same commit is already
#' installed.
#'
#' @param repo_dir directory of repo
install_repo_add_sha <- function(repo_dir) {
  check_dir_exists(repo_dir)

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

  # returns the installed sha of a git package and NULL if package is not installed
  # or sha was not saved in DESCRIPTION file
  get_local_sha <- function(pkg_name) {
    # see remotes:::package2remote
    pkg_desc <- tryCatch(utils::packageDescription(pkg_name),
                         error = function(e) NA, warning = function(e) NA)
    if (identical(pkg_desc, NA)) {
      return(NULL)
    }
    pkg_desc$RemoteSha
  }

  stopifnot(git2r::in_repository(repo_dir)) # should be a git repository
  commit_sha <- git2r::sha(git2r::repository_head(repo_dir))
  git_status <- git2r::status(repo_dir)
  if ((length(git_status$staged) > 0) || (length(git_status$unstaged) > 0) ||
      (length(git_status$untracked) > 0)) {
    # check that there are no changes (so that sha is correct), there should be no
    # untracked files (because the user might work on a local repo on a new file that is
    # still untracked)
    # a file is untracked if it was not part of the previous commit and was not staged yet
    # when a file that was in the previous commit and is modified, it is unstaged
    # until it is staged (with git add)
    stop("The git directory ", repo_dir, " contains changes.")
  }

  # see remotes:::add_metadata
  source_desc <- file.path(repo_dir, "DESCRIPTION")
  desc <- read_dcf(source_desc)
  # see https://github.com/r-lib/remotes/blob/055754a709314f325b254a6182820e1e6d9bea32/R/install-git.R#L128
  # we use generic git2r remote type rather than github or gitlab because we cannot
  # deduce this directly from the URL
  metadata <- list(
    RemoteType = "git2r",
    RemoteUrl = git2r::remote_url(repo_dir),
    #RemoteSubdir = NULL,
    #RemoteRef = x$ref,
    RemoteSha = commit_sha
  )
  desc <- utils::modifyList(desc, metadata)
  write_dcf(source_desc, desc)

  # only install if SHA differs
  if (identical(commit_sha, get_local_sha(desc$Package))) {
    message("Skipping installation of", repo_dir, "since same commit sha already installed")
    return(invisible(NULL))
  }

  utils::install.packages(repo_dir, repos = NULL, type = "source")

  # we do not clean up the DESCRIPTION file

  invisible(NULL)
}

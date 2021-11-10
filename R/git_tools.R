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

# checks that all branches start with "<<remote_name>>/" (or staged_dep_tag_ which is the name of a branch where
# staged_dep has previously checked out a version where ref = <<tag_name>>)
check_only_remote_branches <- function(git_repo, remote_name) {
  all_branches <- names(git2r::branches(git_repo))
  stopifnot(all(vapply(all_branches, function(x) startsWith(x, paste0(remote_name, "/")) || startsWith(x, "staged_dep_tag_"), logical(1))))
}

# clones the repo and only keeps remote branches
# if repo is already there, fetches and prunes (removes) remote branches that are
# no longer there
# select_ref_rule is a function that is given the available refs
# and selects one of them
# if must_work is TRUE then error is thrown if repo is not accessible, if FALSE then wantning is thrown
# verbose level: 0: none, 1: print high-level git operations, 2: print git clone detailed messages etc.
# returns: list of repo_dir and checked out branch/ref (according to branch rule)
checkout_repo <- function(repo_dir, repo_url, select_ref_rule, token_envvar = NULL, must_work = FALSE, verbose = 0) {
  stopifnot(
    is.function(select_ref_rule),
    endsWith(repo_url, ".git")
  )
  check_verbose_arg(verbose)

  creds <- if (is.null(token_envvar)) {
    NULL
  } else {
    git2r::cred_token(token = token_envvar)
  }
  if (!dir.exists(repo_dir)) {
    stopifnot(is_non_empty_char(repo_url))
    if (verbose >= 1) {
      message(paste("clone", repo_url, "to directory", repo_dir))
    }

    cloned_repo <- tryCatch({
      git_repo <- git2r::clone(
        url = repo_url, local_path = repo_dir,
        credentials = creds, progress = verbose >= 2
      )
    }, error = function(e) {
      # catch some common errors
      # only do this when cloning because the API calls introduce quite some time overhead
      host <- paste(utils::head(strsplit(repo_url, "/", fixed = TRUE)[[1]], -2), collapse = "/")
      repo <- paste(utils::tail(strsplit(repo_url, "/", fixed = TRUE)[[1]], 2), collapse = "/")
      repo <- substr(repo, start = 0, stop = nchar(repo) - nchar(".git"))

      if (!identical(httr::status_code(httr::HEAD(host)), 200L)) {
        stop("Host ", host, " not reachable")
      }

      notification_function <- if (must_work) stop else warning

      resp <- get_repo_access(repo, host, token_envvar)
      if (!is.null(resp) && httr::status_code(resp) > 200) {
        notification_function(
          paste0("You cannot access ", repo, " at host ", host,
            ". If you expect to be able to access this repo then ",
            "check that repo and token in envvar '", token_envvar,
            "' are correct.\n",
            "The response's content was:\n", paste(httr::content(resp), collapse = "\n"),
            if (!must_work) " Staged dependencies will continue, ignoring this repository. Some packages may ",
            "not be able to be installed and its package name is assumed to match repository name."
          )
        )
      } else{
        notification_function(
          paste0(
            "Repo ", repo, " could ",
            "not be cloned. The git2r::clone error is: ", e$message,
            if (!must_work) "\nStaged dependencies will continue, ignoring this repository. Some packages may ",
            "not be able to be installed and its package name is assumed to match repository name."
          )
        )
      }

      return(NULL)
    })

    if (is.null(cloned_repo)) {
      return(list(dir = as.character(NA), ref = as.character(NA), sha = as.character(NA), accessible = FALSE))
    }

    # git automatically created local tracking branch (for master or main), checkout
    # corresponding remote branch and delete local branch, so we only have remote
    # branches
    # note: git2r::clone seems to have an argument `checkout = FALSE`, but it does not
    # seem to work (it still checks out the local branch)
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
    # prune (remove) remote branches that were deleted from remote
    git2r::config(git_repo, remote.origin.prune = "true")
    tryCatch({
      git2r::fetch(git_repo, name = get_remote_name(git_repo, repo_url), credentials = creds, verbose = verbose >= 2)
    }, error = function(cond) {
      warning("Unable to fetch from remote for ", repo_dir, " using state of repo found in cache.\n",
              "Error message when trying to fetch: ", cond$message)
    })
  }

  check_only_remote_branches(git_repo, remote_name = get_remote_name(git_repo, repo_url))

  available_refs <- available_references(repo_dir, remote_name = get_remote_name(git_repo, repo_url))
  selected_ref <- select_ref_rule(available_refs)

  if (attr(selected_ref, "type") == "branch") {
    if (!selected_ref %in% available_refs$ref[available_refs$type == "branch"]) {
      stop("ref ", selected_ref, " is unavailable for this repo")
    }

    branch <- paste0(get_remote_name(git_repo, repo_url), "/", selected_ref)
    if (verbose >= 1) {
      message(paste("   - checkout branch", branch, "in directory", repo_dir))
    }
    git2r::checkout(git_repo, branch = branch, force = TRUE)
  } else if (attr(selected_ref, "type") == "tag") {
    if (verbose >= 1) {
      message(paste("   - checkout tag", selected_ref, "in directory", repo_dir))
      git2r::branch_create(commit = git2r::commits(repo = repo_dir, ref = selected_ref, n = 1)[[1]], force = TRUE, name = paste0("staged_dep_tag_", selected_ref))
      git2r::checkout(git_repo, branch =  paste0("staged_dep_tag_", selected_ref), force = TRUE)
    }
  } else{
    stop("The selected reference should have a type attribute as 'branch' or 'tag'")
  }

  return(list(dir = repo_dir, ref = selected_ref,
              sha = get_short_sha(repo_dir), accessible = TRUE))
}

# Install the external deps required for a package
# does not install dependencies that appear in `internal_pkg_deps`
install_external_deps <- function(repo_dir, internal_pkg_deps, ...) {
  # `remotes::install_deps` only makes use of the package DESCRIPTION file via
  # `remotes:::load_pkg_description`
  # So we create a temp directory containing this file and then call this function
  repo_dir_external <- tempfile(paste0(basename(repo_dir), "_externalDeps"))
  fs::dir_create(repo_dir_external)
  fs::file_copy(file.path(repo_dir, "DESCRIPTION"),
                file.path(repo_dir_external, "DESCRIPTION"))

  # remove internal_pkg_deps from DESCRIPTION file
  desc_obj <- desc::desc(file.path(repo_dir_external, "DESCRIPTION"))
  new_deps <- desc_obj$get_deps()[!desc_obj$get_deps()$package %in% internal_pkg_deps,]
  desc_obj$set_deps(new_deps)
  desc_obj$write()

  remotes::install_deps(repo_dir_external, ...)
}


# function to get the remote name (e.g. origin) which matches
# the url given in the staged.deps yaml file
get_remote_name <- function(git_repo, repo_url) {

  # remove the https:// and .git from repo_url
  repo_url <- strsplit(repo_url, "//", fixed = TRUE)[[1]][2]
  repo_url <- gsub(".git", "", repo_url)


  remotes <- git2r::remotes(git_repo)

  for (remote in remotes) {

    target_url <- git2r::remote_url(git_repo, remote = remote)
    target_url <- gsub(":", "/", target_url, fixed = TRUE)

    #print(repo_url)
    #print(target_url)
    #print(grepl(repo_url, target_url))
    if (grepl(repo_url, target_url)) {
      return(remote)
    }
  }
  # by default return origin
  return("origin")
}


#' Install a git repository
#'
#' It adds the git SHA to the DESCRIPTION file, so that the package
#' does not need to be installed again when the same commit is already
#' installed.
#'
#' @param repo_dir directory of repo
#' @param ... Additional args passed to `remotes::install_deps`. Note `upgrade`
#'   is set to "never" and shouldn't be passed into this function.
install_repo_add_sha <- function(repo_dir,
                                 ...) {
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

  # once we have installed or failed to install the package we reset the repo
  # so that further operations on the repo do not fail by the above
  # "the git directory contains changes" check
  on.exit(git2r::reset(git2r::commits(repo_dir)[[1]], reset_type = "hard"))

  desc <- utils::modifyList(desc, metadata)
  write_dcf(source_desc, desc)

  # only install if SHA differs
  if (identical(commit_sha, get_local_sha(desc$Package))) {
    message("Skipping installation of ", repo_dir, " since same commit sha already installed")
    return(invisible(NULL))
  }

  utils::install.packages(repo_dir, repos = NULL, type = "source")
  invisible(NULL)
}


get_short_sha <- function(repo_dir) {
  substr(git2r::sha(git2r::repository_head(repo_dir)), 1, 7)
}

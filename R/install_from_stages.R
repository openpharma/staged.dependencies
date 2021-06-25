# todo: into package.R
CACHE_DIR <- path.expand("~/.staged.dependencies")
# unlink(CACHE_DIR, recursive = TRUE)
if (!dir.exists(CACHE_DIR)) {
  dir.create(CACHE_DIR)
}

STAGEDDEPS_FILENAME <- "staged_dependencies.yaml"

# DISCUSSION POINTS:
# todo: check verbose arg
# todo: replace git2r by gert
# some other todos below
# todo: rstudio addin interactive with Shiny
# todo: how to run rcmdcheck without latex?

cat_nl <- function(...) cat(paste0(paste(...), "\n"))

is_non_empty_char <- function(x) {
  length(x) == 1 && nchar(x) > 0 && is.character(x)
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
  prefix <- if (local) "local_" else ""
  file.path(CACHE_DIR, paste0(prefix, gsub("/", "_", repo, fixed = TRUE),
                              "_", digest::digest(paste0(repo, "/", host))))
}

# get currently active branch of repo in cache
get_active_branch_in_cache <- function(repo, host, local = FALSE) {
  get_current_branch(get_repo_cache_dir(repo, host, local))
}

# url for `git clone`
get_repo_url <- function(repo, host) {
  file.path(host, repo)
}

# Returns the environment variable that stores the auth token
get_authtoken_envvar <- function(host) {
  token_mapping <- getOption("staged.dependencies.token_mapping")
  if (!host %in% names(token_mapping)) {
    stop("unknown host ", host, ", please set the package option staged.dependencies.token_mapping")
  }
  token_mapping[[host]]
}

#' Clear the repository cache
#'
#' @md
#' @param pattern files to remove, see `unlink` (wildcards `*` and `?` allowed)
#' @export
clear_cache <- function(pattern = "*") {
  unlink(file.path(CACHE_DIR, pattern), recursive = TRUE)
}

# checks out the correct branch (corresponding to feature) in the repo from the remote,
# clones the repo if necessary
checkout_repo <- function(repo, host, feature, verbose = 0) {
  repo_dir <- get_repo_cache_dir(repo, host)
  creds <- git2r::cred_token(token = get_authtoken_envvar(host))
  if (!dir.exists(repo_dir)) {
    message(paste("clone", get_repo_url(repo, host), "to directory", repo_dir))

    git_repo <- git2r::clone(
      url = get_repo_url(repo, host), local_path = repo_dir, credentials = creds, progress = verbose >= 2
    )
  } else {
    message(paste("pull", get_repo_url(repo, host), "to directory", repo_dir))

    git_repo <- git2r::repository(repo_dir)
    # todo: checkout dummy branch, then prune branches that were deleted from remote
    # (by setting fetch.prune = TRUE with git2r::config)
    git2r::pull(git_repo, credentials = creds)
  }
  # this directory should only contain remote branches (+ 1 local master branch)
  available_branches <- names(git2r::branches(git_repo, flags = "remote"))
  available_branches <- setdiff(gsub("origin/", "", available_branches, fixed = TRUE), "HEAD")
  branch <- determine_branch(feature, available_branches)

  message(paste("   - checkout branch:", branch))

  git2r::checkout(git_repo, branch = branch, force = TRUE) # force = TRUE to discard any changes (which should not happen)
  if (verbose >= 1) {
    cat_nl("Checked out branch", branch, "from repo in directory", repo_dir) #todo: message()
  }
  repo_dir
}

# copies a local directory to the cache dir and commits the current state in
# that cache dir, so the SHA can be added to the DESCRIPTION file
# note: files in .gitignore are also available to the package locally
copy_local_repo_to_cachedir <- function(local_dir, repo, host, verbose = 0) {
  repo_dir <- get_repo_cache_dir(repo, host, local = TRUE)
  if (dir.exists(repo_dir)) {
    unlink(repo_dir, recursive = TRUE)
  }
  # file.copy copies a directory inside an existing directory
  if (verbose >= 1) {
    message(paste("Copying local dir", local_dir, "to cache dir", repo_dir))
  }
  fs::dir_copy(local_dir, repo_dir)
  if ((length(git2r::status(repo_dir)$staged) > 0) || length(git2r::status(repo_dir)$unstaged) > 0) {
    git2r::commit(repo_dir, paste0("committing everything for installation, copied from ", local_dir), all = TRUE)
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
    required_fields <- c("upstream_repos", "downstream_repos", "current_repo")
    if (!all(required_fields %in% names(content))) {
      stop("File ", yaml_file, "invalid, it must contain fields ", toString(required_fields))
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
  if (length(repo_and_host) == 0) {
    c()
  } else {
    paste0(repo_and_host$repo, " @ ", repo_and_host$host)
  }
}
# hash_repo_and_host(list())
# hash_repo_and_host(list(repo = "repo1", host = "host1"))
# hash_repo_and_host(list(repo = c("repo1", "repo2"), host = c("host1", "host2")))
unhash_repo_and_host <- function(hashed_repo_and_host) {
  repo_and_host <- strsplit(hashed_repo_and_host, " @ ", fixed = TRUE)
  list(
    repo = vapply(repo_and_host, `[[`, character(1), 1),
    host = vapply(repo_and_host, `[[`, character(1), 2)
  )
}
# unhash_repo_and_host(character(0))
# unhash_repo_and_host("repo1 @ host1")
# unhash_repo_and_host(c("repo1 @ host1", "repo2 @ host2"))

get_local_repo_to_dir_mapping <- function(local_repos) {
  stopifnot(is.data.frame(local_repos))
  if (nrow(local_repos) == 0) {
    list()
  } else {
    setNames(local_repos$directory, hash_repo_and_host(local_repos))
  }
}

# Recursively check out all repos to match branch determined by feature,
# starting from repos_to_process.
# When direction is upstream, it includes upstream repos
# returns the upstream and/or downstream dependency graphs
rec_checkout_repos <- function(repos_to_process, feature, direction = c("upstream"),
                               local_repos = data.frame(repo = character(0), host = character(0), directory = character(0)), verbose = 0) {
  stopifnot(
    is.list(repos_to_process)
  )
  stopifnot(all(direction %in% c("upstream", "downstream")), length(direction) >= 1)
  stopifnot(is.data.frame(local_repos), setequal(colnames(local_repos), c("repo", "host", "directory")))

  local_repo_to_dir <- get_local_repo_to_dir_mapping(local_repos)
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
      repo_dir <- checkout_repo(repo_and_host$repo, repo_and_host$host, feature, verbose = verbose)
    }

    hashed_new_repos <- c()
    if ("upstream" %in% direction) {
      # Attention: use lapply because with vapply, vector may be NULL, so assignment to
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
    hashed_repos_to_process <- union(hashed_repos_to_process, setdiff(hashed_new_repos, hashed_processed_repos))
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

# gets the currently checked out branch
get_current_branch <- function(repo_dir) {
  git2r::repository_head(git2r::repository(repo_dir))$name
}

warn_if_stageddeps_inexistent <- function(project) {
  fpath <- normalizePath(
    file.path(project, STAGEDDEPS_FILENAME),
    winslash = "/", mustWork = FALSE # output error, see below
  )
  if (!file.exists(fpath)) {
    warning("file staged_dependencies.yaml does not exist in project folder: not restoring anything")
  }
}

#' Check downstream dependencies
#'
#' It installs the downstream dependencies and their upstream dependencies,
#' and then runs `rcmdcheck` (`R CMD check`) on the downstream dependencies.
#' It runs recursively on the downstream dependencies, but this can be disabled.
#'
#' Note: It runs against the remote version of project, so the project must have
#' been pushed before.
#'
#' If A <- B (i.e. A is upstream of B), we require that A lists B as downstream
#' and B lists A as upstream. This requirement can be verified with
#' `dependency_table` by looking at the arrows (each arrow should be once dashed
#' and once solid).
#'
#' todo: check if only one lists the other
#'
#' @md
#'
#' @export
#'
#' @param dry_install_and_check whether to install upstream dependencies and run the checks;
#'   useful to see a dry-run (it however updates the cached repos!)
#' @param downstream_repos to overwrite the downstream repos to check
#' @param recursive whether to recursively check downstream dependencies of the
#'   downstream dependencies
#' @inheritParams install_upstream_deps
#' @export
#' @seealso determine_branch
#'
#' @return `data.frame` of installed packages (in installation order) and checked packages
#'
#' @examples
#' \dontrun{
#' check_downstream(project = ".", verbose = 1)
#'
#' check_downstream(
#'   project = "../stageddeps.electricity"
#' )
#' }
check_downstream <- function(project = ".", feature = NULL, downstream_repos = NULL,
                             local_repos = data.frame(repo = character(0), host = character(0), directory = character(0)),
                             recursive = TRUE, dry_install_and_check = FALSE, verbose = 0) {
  warn_if_stageddeps_inexistent(project)
  stopifnot(is.list(local_repos))

  project_branch <- get_current_branch(project)
  if (is.null(feature)) {
    feature <- project_branch
  }

  expected_project_branch <- determine_branch(
    feature, available_branches = setdiff(gsub("origin/", "", names(git2r::branches(project)), fixed = TRUE), "HEAD")
  )
  if (project_branch != expected_project_branch) {
    warning("feature ", feature, " would match ", expected_project_branch,
            ", but currently checked out branch is ", project_branch)
  }

  repo_deps_info <- get_deps_info(project)

  local_repos <- rbind(
    local_repos,
    data.frame(
      repo = repo_deps_info$current_repo$repo,
      host = repo_deps_info$current_repo$host,
      directory = normalizePath(project), stringsAsFactors = FALSE
    )
  )

  if (is.null(downstream_repos)) {
    # get downstream repos without fetching remote, todo: local upstream dependencies
    downstream_repos <- if (!recursive) {
      get_deps_info(project)$downstream_repos
    } else {
      deps <- rec_checkout_repos(
        list(repo_deps_info$current_repo), feature, verbose = verbose,
        direction = "downstream", local_repos = local_repos
      )
      hashed_downstream_nodes <- lapply(get_descendants(
        deps[["downstream_deps"]], hash_repo_and_host(repo_deps_info$current_repo)
      ), unhash_repo_and_host)
    }
  }
  stopifnot(
    is.list(downstream_repos),
    all(vapply(downstream_repos, function(x) {
      all(c("repo", "host") %in% names(x))
    }, logical(1)))
  )

  deps <- rec_checkout_repos(
    downstream_repos, feature, verbose = verbose,
    local_repos = local_repos, direction = "upstream"
  )
  install_order <- get_install_order(deps)

  local_repo_to_dir <- get_local_repo_to_dir_mapping(local_repos)
  if (verbose) {
    cat_nl("Installing packages in order: ", toString(install_order))
  }
  for (repo_and_host in install_order) {
    is_local <- hash_repo_and_host(
      list(repo = repo_and_host$repo, host = repo_and_host$host)
    ) %in% names(local_repo_to_dir)
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)
    if (hash_repo_and_host(repo_and_host) %in% lapply(downstream_repos, hash_repo_and_host)) {
      if (!dry_install_and_check) {
        rcmdcheck::rcmdcheck(repo_dir, error_on = "warning", args = Sys.getenv("RCMDCHECK_ARGS")) #todo: make an option
      } else if (verbose >= 1) {
        cat_nl("(Dry run): Skipping check of", repo_dir)
      }
    }
    if (!dry_install_and_check) {
      install_repo_add_sha(repo_dir)
    } else if (verbose >= 1) {
      cat_nl("(Dry run): Skipping installation of", repo_dir)
    }
  }

  res <- do.call(rbind, lapply(install_order, function(repo_and_host) {
    data.frame(
      repo = repo_and_host$repo,
      host = repo_and_host$host,
      checked = hash_repo_and_host(repo_and_host) %in% lapply(downstream_repos, hash_repo_and_host)
    )
  }))
  if (is.null(res)) {
    data.frame(
      repo = character(0),
      host = character(0)$host,
      checked = character(0)
    )
  }
  res
}

# returns the order in which the repos must be installed
get_install_order <- function(deps) {
  install_order <- topological_sort(deps[["upstream_deps"]])
  lapply(install_order, unhash_repo_and_host)
}

get_descendants <- function(parents_to_children, node) {
  nodes_to_process <- c(node)
  descendants <- c()
  while (length(nodes_to_process) > 0) {
    cur_node <- nodes_to_process[[1]]
    nodes_to_process <- nodes_to_process[-1]

    descendants <- c(descendants, cur_node)
    children <- parents_to_children[[cur_node]]
    nodes_to_process <- union(nodes_to_process, setdiff(children, descendants))
  }

  setdiff(descendants, node)
}

dependency_table <- function(project = ".", feature = NULL,
                             local_repos = data.frame(repo = character(0), host = character(0), directory = character(0)),
                             verbose = 0) {
  warn_if_stageddeps_inexistent(project)
  stopifnot(is.list(local_repos))

  project_branch <- get_current_branch(project)
  if (is.null(feature)) {
    feature <- project_branch
  }

  expected_project_branch <- determine_branch(
    feature, available_branches = setdiff(gsub("origin/", "", names(git2r::branches(project)), fixed = TRUE), "HEAD")
  )
  if (project_branch != expected_project_branch) {
    warning("feature ", feature, " would match ", expected_project_branch,
            ", but currently checked out branch is ", project_branch)
  }

  repo_deps_info <- get_deps_info(project)

  local_repos <- rbind(
    local_repos,
    data.frame(
      repo = repo_deps_info$current_repo$repo,
      host = repo_deps_info$current_repo$host,
      directory = normalizePath(project), stringsAsFactors = FALSE
    )
  )

  deps <- rec_checkout_repos(list(repo_deps_info$current_repo), feature,
                             direction = c("upstream", "downstream"), local_repos = local_repos,
                             verbose = verbose)
  cur_node <- hash_repo_and_host(repo_deps_info$current_repo)
  hashed_upstream_nodes <- get_descendants(deps[["upstream_deps"]], cur_node)
  hashed_downstream_nodes <- get_descendants(deps[["downstream_deps"]], cur_node)
  hashed_remaining_nodes <- setdiff(
    union(names(deps[["upstream_deps"]]), names(deps[["downstream_deps"]])),
    union(union(hashed_upstream_nodes, hashed_downstream_nodes), cur_node)
  )

  # if df is empty, don't add any type
  cbind_handle_empty <- function(df, ...) {
    col_and_vals <- list(...)
    if (nrow(df) == 0) {
      col_and_vals <- lapply(col_and_vals, function(x) character(0))
    }
    cbind(df, do.call(data.frame, c(col_and_vals, stringsAsFactors = FALSE)))
  }

  df <- rbind(
    cbind_handle_empty(data.frame(unhash_repo_and_host(cur_node), stringsAsFactors = FALSE), type = "current"),
    cbind_handle_empty(data.frame(unhash_repo_and_host(hashed_upstream_nodes), stringsAsFactors = FALSE), type = "upstream"),
    cbind_handle_empty(data.frame(unhash_repo_and_host(hashed_downstream_nodes), stringsAsFactors = FALSE), type = "downstream"),
    cbind_handle_empty(data.frame(unhash_repo_and_host(hashed_remaining_nodes), stringsAsFactors = FALSE), type = "other")
  )
  local_repo_to_dir <- get_local_repo_to_dir_mapping(local_repos)
  df$branch <- Map(function(repo, host) {
    is_local <- hash_repo_and_host(list(repo = repo, host = host)) %in% names(local_repo_to_dir)
    branch <- get_active_branch_in_cache(repo, host, local = is_local)
    if (is_local) {
      paste0("local (", branch, ")")
    } else {
      branch
    }
  }, df$repo, df$host)

  # all_unique <- function(x) {
  #   length(unique(x)) == length(x)
  # }
  # stopifnot(all_unique(df$repo))
  library(dplyr) #todo
  nodes <- df %>% mutate(
    id = hash_repo_and_host(list(repo = repo, host = host)),
    label = vapply(strsplit(repo, "/", fixed = TRUE), function(x) tail(x, 1), character(1)),
    title = paste0("<p>", repo, "<br/>", host, "<br/>", type, "<br/>", branch, "</p"),
    value = 3,
    group = type
    # color = case_when(
    #   type == "upstream" ~ "#edc987", # "orange"
    #   type == "downstream" ~ "#bad984", # "green"
    #   type == "current" ~ "#ccc916", # "yellow"
    #   TRUE ~ "grey"
    # )
  ) %>% select(id, label, title, value, group)

  adj_list_to_edge_df <- function(parents_to_children) {
    if (length(parents_to_children) == 0) {
      return(data.frame(from = character(), to = character()))
    }

    do.call(rbind, lapply(names(parents_to_children), function(node) {
      children <- parents_to_children[[node]]
      data.frame(
        from = if (length(children) > 0) node else character(0),
        to = unlist(children, recursive = FALSE), stringsAsFactors = FALSE
      )
    }))
  }

  edges <- rbind(
    cbind_handle_empty(
      adj_list_to_edge_df(deps[["upstream_deps"]]) %>% rename(to = from, from = to),
      arrows = "from", dashes = TRUE),
    cbind_handle_empty(
      adj_list_to_edge_df(deps[["downstream_deps"]]),
      arrows = "from", dashes = FALSE
    )
  )

  plot_title <- paste0("Dependency graph starting from ", repo_deps_info$current_repo$repo)
  visNetwork::visNetwork(nodes, edges, width = "100%", main = plot_title) %>%
    # topological sort
    visNetwork::visHierarchicalLayout(sortMethod = "directed", direction = "LR") %>%
    # "orange"
    visNetwork::visGroups(groupname = "upstream", color = "#edc987") %>%
    # "green"
    visNetwork::visGroups(groupname = "downstream", color = "#bad984") %>%
    # "yellow"
    visNetwork::visGroups(groupname = "current", color = "#ccc916") %>%
    # "grey"
    visNetwork::visGroups(groupname = "other", color = "#c5c9c9") %>%
    visNetwork::visLegend(
      position = "right",
      addEdges = data.frame(arrows = "from", dashes = c(TRUE, FALSE), font.vadjust = "15", label = c("upstream deps", "downstream deps"))
    ) %>%
    #visNetwork::visInteraction(zoomView = TRUE) %>%
    print()

  df
}

#' Install upstream dependencies of project corresponding to feature
#'
#' This reads the upstream dependencies for the project (recursively) and
#' installs the right branches based on the feature.
#'
#' It throws a warning if the currently checked out branch in the project
#' is not the one that would be taken based on `feature`. In particular,
#' the checked out branch should not be a remote branch.
#'
#' @md
#' @param project directory of project (for which to restore the dependencies according to feature);
#'   must be a git repository.
#' @param feature feature we want to build; inferred from the branch of the project if not provided
#' @param install_project whether to also install the current package (`project`)
#' @param dry_install whether to install or just print (useful for dry-runs, but still
#'   checks out the git repos)
#' @param verbose verbosity level, incremental;
#'   (0: None, 1: packages that get installed, 2: includes git checkout)
#'
#' @return installed packages in installation order
#'
#' @export
#' @seealso determine_branch
#'
#' @examples
#' \dontrun{
#' install_upstream_deps()
#' }
#'
install_upstream_deps <- function(project = ".", feature = NULL,
                                  local_repos = data.frame(repo = character(0), host = character(0), directory = character(0)),
                                  install_project = TRUE, dry_install = FALSE, verbose = 0) {
  warn_if_stageddeps_inexistent(project)
  stopifnot(is.list(local_repos))

  project_branch <- get_current_branch(project)
  if (is.null(feature)) {
    feature <- project_branch
  }

  expected_project_branch <- determine_branch(
    feature, available_branches = setdiff(gsub("origin/", "", names(git2r::branches(project)), fixed = TRUE), "HEAD")
  )
  if (project_branch != expected_project_branch) {
    warning("feature ", feature, " would match ", expected_project_branch,
            ", but currently checked out branch is ", project_branch)
  }

  repo_deps_info <- get_deps_info(project)

  local_repos <- rbind(
    local_repos,
    data.frame(
      repo = repo_deps_info$current_repo$repo,
      host = repo_deps_info$current_repo$host,
      directory = normalizePath(project), stringsAsFactors = FALSE
    )
  )

  deps <- rec_checkout_repos(list(repo_deps_info$current_repo), feature, direction = "upstream",
                             local_repos = local_repos, verbose = verbose)
  install_order <- get_install_order(deps)
  stopifnot(all.equal(tail(install_order, 1)[[1]], repo_deps_info$current_repo))
  if (!install_project) {
    install_order <- head(install_order, -1)
  }

  if (verbose) {
    cat_nl("Installing packages in order: ", toString(install_order))
  }
  local_repo_to_dir <- get_local_repo_to_dir_mapping(local_repos)
  for (repo_and_host in install_order) {
    is_local <- hash_repo_and_host(
      list(repo = repo_and_host$repo, host = repo_and_host$host)
    ) %in% names(local_repo_to_dir)
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)
    if (!dry_install) {
      install_repo_add_sha(repo_dir)
    } else if (verbose >= 1) {
      cat_nl("(Dry run) Skipping installation of", repo_dir)
    }
  }

  install_order
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
  git_status <- git2r::status(repo_dir)
  if ((length(git_status$staged) > 0) || (length(git_status$unstaged) > 0) || (length(git_status$untracked) > 0)) {
    # check that there are no changes (so that sha is correct)
    stop("The git directory ", repo_dir, " contains changes.")
  }

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

  utils::install.packages(repo_dir, repos = NULL, type = "source")  # returns NULL
}

#' Topologically sorts nodes so that parents are listed before all their children
#'
#' @param child_to_parents  mapping from child to its parents (upstream dependencies)
#'
#' @return vector listing parents before children
#'
#' @examples
#' topological_sort <- staged.dependencies:::topological_sort
#'
#' all(topological_sort(list(
#' n1 = c(), n2 = c("n1"), n3 = c("n2"),
#' n4 = c("n2", "n3"))) == c("n1", "n2", "n3", "n4")
#' )
#' is.null(topological_sort(list()))
#'
#' \dontrun{
#' # cycle
#' topological_sort(list(
#'   n1 = c("n2"), n2 = c("n3", "n1"), n3 = c()
#' ))
#' }
topological_sort <- function(child_to_parents) {
  # depth-first search from children to parents, then output nodes in the order of finishing times
  ordering <- c()
  visiting <- list()

  treat_node <- function(node) {
    # cat_nl("Treating node '", node, "'")
    # Sys.sleep(1)
    # print(visiting)
    if (node %in% names(visiting)) {
      stop("Node ", node, " forms part of a cycle.")
    }
    visiting[[node]] <<- TRUE
    for (parent in setdiff(child_to_parents[[node]], ordering)) {
      treat_node(parent)
    }
    visiting[[node]] <<- NULL # remove
    ordering <<- c(ordering, node)
  }

  nodes_to_process <- names(child_to_parents)
  while (length(nodes_to_process) > 0) {
    treat_node(nodes_to_process[[1]])
    nodes_to_process <- setdiff(nodes_to_process, ordering)
  }

  ordering
}

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


#' @include caching.R
NULL

# returns the order in which the repos must be installed, unhashing the dependencies
get_install_order <- function(upstream_deps) {
  stopifnot(
    is.list(upstream_deps)
  )

  install_order <- topological_sort(upstream_deps)
  lapply(install_order, unhash_repo_and_host)
}

# add the specified project to the local repos
add_project_to_local_repos <- function(project, local_repos) {
  stopifnot(
    is.data.frame(local_repos) || is.null(local_repos)
  )
  check_dir_exists(project)

  repo_deps_info <- get_yaml_deps_info(project)
  rbind(
    local_repos,
    data.frame(
      repo = repo_deps_info$current_repo$repo,
      host = repo_deps_info$current_repo$host,
      directory = normalizePath(project), stringsAsFactors = FALSE
    )
  )
}

#' Loads the config file and extracts `local_packages`
#'
#' Checks that all directories exist and are absolute paths.
#'
#' @md
#' @return local_packages
#' @export
#'
#' @examples
#' get_local_pkgs_from_config()
get_local_pkgs_from_config <- function() {
  filename <- file.path(STORAGE_DIR, CONFIG_FILENAME)
  if (file.exists(filename)) {
    content <- yaml::read_yaml(filename)
    local_pkgs <- content[["local_packages"]]
    df <- do.call(rbind,
                  lapply(local_pkgs, function(x) data.frame(x, stringsAsFactors = FALSE))
    )
    if (is.null(df)) {
      return(NULL)
    }
    stopifnot(setequal(colnames(df), c("repo", "host", "directory")))
    stopifnot(all(vapply(df$directory, function(x) {
      check_dir_exists(x, "Local package config: ")
      fs::is_absolute_path(x)
    }, logical(1))))
    df
  } else {
    NULL
  }
}

# Given a named list of packages of the form
# list(name = <<path to package>>), create a dependency graph
# using the R DESCRIPTION files, rather than the
# staged_dependencies.yaml files.
# The dependency graph is defined over the names in the list.
# The direction can be "upstream", "downstream" or both. The "upstream_deps"
# graph is the graph where edges point from a package to its upstream
# dependencies. The "downstream_deps" graph is the graph with the edge
# direction flipped.
get_true_deps_graph <- function(pkgs,
                                direction = "upstream") {

  # get the Imports, Suggests, Depends for each package
  # from the package DESCRIPTION files, filter for only
  # the internal packages
  upstream_deps <- apply(pkgs, 1,
    function(row) pkgs$package_name[pkgs$package_name %in% desc::desc_get_deps(file = row[["cache_dir"]])$package],
    simplify = FALSE
  )

  names(upstream_deps) <- pkgs$package_name

  res <- list()
  if ("upstream" %in% direction) {
    res[["upstream_deps"]] <- upstream_deps
  }
  if ("downstream" %in% direction) {
    downstream_deps <- lapply(upstream_deps, function(x) list())
    for (x in names(upstream_deps)) {
      for (y in upstream_deps[[x]]) {
        downstream_deps[[y]] <- c(downstream_deps[[y]], x)
      }
    }

    res[["downstream_deps"]] <- downstream_deps
  }

  res

}

# dep_table: dependency table as returned by `dependency_structure`
yaml_from_dep_table <- function(dep_table) {
  upstream_repos <- dep_table[dep_table$type == "upstream" & dep_table$distance == 1, c("repo", "host")]
  downstream_repos <- dep_table[dep_table$type == "downstream" & dep_table$distance == 1, c("repo", "host")]
  list(
    current_repo = list(
      repo = dep_table[dep_table$type == "current",]$repo,
      host = dep_table[dep_table$type == "current",]$host
    ),
    upstream_repos = Map(
      function(repo, host) list(repo = repo, host = host),
      upstream_repos$repo, upstream_repos$host
    ),
    downstream_repos = Map(
      function(repo, host) list(repo = repo, host = host),
      downstream_repos$repo, downstream_repos$host
    )
  )
}


#' @include caching.R
NULL


run_package_actions <- function(pkg_df, type, dry,
                                install_external_deps,
                                internal_pkg_deps,
                                check_args = NULL,
                                verbose = verbose, ...) {

  if (nrow(pkg_df) == 0) {
    message_if_verbose("No packages to process!", verbose = verbose)
    return(pkg_df)
  }

  message_if_verbose("Processing packages in order: ", toString(pkg_df$package_name), verbose = verbose)

  for (cache_dir in pkg_df$cache_dir) {

    if (!dry) {

      if ("test" %in% type) {
        # testthat::test_dir and devtools::test do not always agree
        # testthat::test_dir(file.path(repo_dir, "tests"), stop_on_failure = TRUE, stop_on_warning = TRUE)
        # stop_on_failure argument cannot be passed to devtools::test
        if (dir.exists(file.path(cache_dir, "tests"))) {
          # this does not work with legacy R packages where tests are in the inst directory
          # see devtools:::find_test_dir
          test_res <- devtools::test(cache_dir, stop_on_warning = TRUE)
          all_passed <- function(res) {
            # copied from testthat:::all_passed
            if (length(res) == 0) {
              return(TRUE)
            }
            df <- as.data.frame(res)
            sum(df$failed) == 0 && all(!df$error)
          }
          if (!all_passed(test_res)) {
            stop("Tests for package in directory ", cache_dir, " failed")
          }
        } else {
          message_if_verbose("No tests found for package in directory ", cache_dir, verbose = verbose)
        }
      }

      if ("check" %in% type) {
        rcmdcheck::rcmdcheck(cache_dir, error_on = "warning", args = check_args)
      }

      if ("install" %in% type) {
        install_repo_add_sha(cache_dir, install_external_deps = install_external_deps,
                             internal_pkg_deps = internal_pkg_deps, ...)
      }

    } else { # dry run
      message_if_verbose(cat_nl("(Dry run) Skipping", toString(type), "of", cache_dir), verbose = verbose)
    }

  }

  message("Processed packages in order: ", toString(pkg_df$package_name), verbose = verbose)
  pkg_df
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

  # order the dependencies
  install_order <- topological_sort(upstream_deps)
  upstream_deps <- upstream_deps[install_order]

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

    res[["downstream_deps"]] <- downstream_deps[rev(install_order)]
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


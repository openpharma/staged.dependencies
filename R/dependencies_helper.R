#' @include caching.R
NULL


#' Run an action on the packages
#'
#' @param pkg_df table of packages to install, e.g. returned by
#'   `dependency_table(...)$table`
#' @param action a subset of "test", "build", "check", "install";
#'  -  if test: run with devtools::test
#'  -  if build: R CMD build
#'  -  if check: `rcmdcheck` with output to command line (if no build action),
#'     otherwise `R CMD check` on tarball
#'  -  if install: R install based on remotes::install (if no build action),
#'     otherwise `R CMD INSTALL` on tarball
#' @param internal_pkg_deps packages to install (when `install_external_deps = TRUE`)
#' @param install_external_deps logical to describe whether to install
#'   external dependencies of package using `remotes::install_deps`.
run_package_actions <- function(pkg_df, action, internal_pkg_deps,
                                dry = FALSE,
                                install_external_deps = TRUE,
                                rcmd_args = NULL,
                                artifact_dir = NULL,
                                verbose = verbose, ...) {

  stopifnot(all(action %in% c("test", "build", "check", "install")))

  if (nrow(pkg_df) == 0) {
    message_if_verbose("No packages to process!", verbose = verbose)
    return(pkg_df)
  }

  if (any(c("build", "check", "install") %in% action)) {
    stop("when building a package an artifact_dir must be specified")
  }

  if (!is.null(artifact_dir)) {
    #TODO empty directories if exist?
    lapply(
      intersect(action, c("build", "install")),
      function(action) fs::dir_create(file.path(artifact_dir, paste0(action, "_logs")))
    )
  }

  message_if_verbose("Processing packages in order: ", toString(pkg_df$package_name), verbose = verbose)

  for (cache_dir in pkg_df$cache_dir) {

    if (!dry) {

      if ("test" %in% action) {
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

      package_tar <- NULL
      if ("build" %in% action) {
        withr::with_dir(artifact_dir, {
          pkg_name <- desc::desc_get_field("Package", file = cache_dir)
          system2(
            "R", args = c("CMD", "build", rcmd_args$build, cache_dir),
            stdout = file.path("build_logs", paste0(pkg_name, "_stdout.txt")),
            stderr = file.path("build_logs", paste0(pkg_name, "_stderr.txt"))
          )
          package_tar <- Sys.glob(paste0(pkg_name, "_*.tar.gz"))
        })
      }

      if ("check" %in% action) {
        # check tar.gz if it exists otherwise check the cache_dir
        if (!is.null(package_tar)) {
          withr::with_dir(artifact_dir,
            system2("R", args = c("CMD", "check", rcmd_args$check, package_tar),
                    stdout = file.path("check_logs", paste0(pkg_name, "_stdout.txt")),
                    stderr = file.path("check_logs", paste0(pkg_name, "_stderr.txt"))))
        } else {
          rcmdcheck::rcmdcheck(cache_dir, error_on = "warning", args = rcmd_args$check)
        }
      }

      if ("install" %in% action) {
        # install the tar.gz if it exists otherwise install using staged.deps
        if (!is.null(package_tar)) {
          withr::with_dir(artifact_dir, system2("R", args = c("CMD", "INSTALL", rcmd_args$install, package_tar)))
        } else {
          install_repo_add_sha(cache_dir, install_external_deps = install_external_deps,
                             internal_pkg_deps = internal_pkg_deps, ...)
        }
      }

    } else { # dry run
      message_if_verbose(cat_nl("(Dry run) Skipping", toString(action), "of", cache_dir), verbose = verbose)
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
      directory = normalize_path(project), stringsAsFactors = FALSE
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
  filename <- file.path(get_storage_dir(), CONFIG_FILENAME)
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

# Given a data.frame of internal packages (with columns package_name, cache_dir),
# create a dependency graph using the R DESCRIPTION files (rather than the
# staged_dependencies.yaml files).
# The dependency graph is defined over the packages in the data.frame. All
# other packages are external and not included.
# The graph_directions can be "upstream", "downstream" or both. The "upstream_deps"
# graph is the graph where edges point from a package to its upstream
# dependencies. They are ordered in installation order.
# The "downstream_deps" graph is the graph with the edge
# direction flipped, and is ordered in reverse installation order.
# It preserves the other columns of this data.frame.
get_true_deps_graph <- function(pkgs_df,
                                graph_directions = "upstream") {

  stopifnot(
    is.data.frame(pkgs_df),
    all(c("package_name", "cache_dir") %in% colnames(pkgs_df))
  )
  check_direction_arg(graph_directions)

  # get the Imports, Suggests, Depends for each package
  # from the package DESCRIPTION files, filter for only
  # the internal packages
  upstream_deps <- lapply(pkgs_df$cache_dir,
                          function(file) intersect(pkgs_df$package_name, desc::desc_get_deps(file)$package))
  names(upstream_deps) <- pkgs_df$package_name

  # order the dependencies
  install_order <- topological_sort(upstream_deps)
  upstream_deps <- upstream_deps[install_order]

  res <- list()
  if ("upstream" %in% graph_directions) {
    res[["upstream_deps"]] <- upstream_deps
  }
  if ("downstream" %in% graph_directions) {
    downstream_deps <- lapply(upstream_deps, function(x) c())
    for (x in names(upstream_deps)) {
      for (y in upstream_deps[[x]]) {
        downstream_deps[[y]] <- c(downstream_deps[[y]], x)
      }
    }

    res[["downstream_deps"]] <- downstream_deps[rev(install_order)]
  }

  res

}

# dep_table: dependency table as returned by `dependency_table`
# useful to create staged_dependencies.yaml file
# yaml::write_yaml(dependency_table(project = "."), file = "./staged_dependencies.yaml")
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


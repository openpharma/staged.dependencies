#' Run an action on the packages
#'
#' @md
#' @param pkg_actions sorted `data.frame` with columns `cache_dir` pointing
#' to package source, `sha`, `installable` and `actions`, processed from top to bottom in order
#'  Each `actions` is a subset of "test", "build", "check", "install";
#'  -  if test: run with `devtools::test`
#'  -  if build: `R CMD build`
#'  -  if check: `rcmdcheck` with output to command line (if no build action),
#'     otherwise `R CMD check` on tarball
#'  -  if install: R install based on `remotes::install` (if no build action),
#'     otherwise `R CMD INSTALL` on tarball
#' @param internal_pkg_deps packages to not install (when `install_external_deps = TRUE`)
#' @param dry logical, if `FALSE` (default) run the actions, if `TRUE` do not
#' @param install_external_deps logical to describe whether to install
#'   external dependencies of package using [remotes::install_deps()] (or [renv::install()] if
#'   inside an `renv` environment) .
#' @param upgrade argument passed to [remotes::install_deps()], defaults to `'never'`. Ignored
#'   if inside an `renv` environment.
#' @param rcmd_args list with names `build`, `check`,
#'   `install` which are vectors that are passed as separate arguments
#'   to the `R CMD` commands
#' @param artifact_dir directory to store log files, only when actions include
#'   `build`; action `test` only outputs to the console
#' @param ... Additional args passed to [remotes::install_deps()] Ignored
#'   if inside an `renv` environment.
#' @inheritParams argument_convention
#'
#' @keywords internal
run_package_actions <- function(pkg_actions, internal_pkg_deps,
                                dry = FALSE,
                                install_external_deps = TRUE,
                                upgrade = "never",
                                rcmd_args = NULL,
                                artifact_dir = NULL,
                                verbose = 0, ...) {
  all_actions <- unique(unlist(pkg_actions$actions))
  stopifnot(all(all_actions %in% c("test", "build", "check", "install")))
  check_verbose_arg(verbose)

  rcmdcheck_outputs <- list()

  if (nrow(pkg_actions) == 0) {
    message_if_verbose("No packages to process!", verbose = verbose)
    return(pkg_actions)
  }

  if ("build" %in% all_actions && is.null(artifact_dir)) {
    stop("when building a package an artifact_dir must be specified")
  }

  if (!is.null(artifact_dir)) {
    # TODO empty directories if exist?
    lapply(
      intersect(all_actions, c("build", "install")),
      function(action) fs::dir_create(file.path(artifact_dir, paste0(action, "_logs")))
    )
  }

  message_if_verbose("Processing packages in order: ",
    toString(pkg_actions$package_name),
    verbose = verbose
  )

  for (idx in seq_along(pkg_actions$cache_dir)) {
    cache_dir <- pkg_actions$cache_dir[idx]
    actions <- pkg_actions$actions[idx]

    if (!pkg_actions$installable[idx]) {
      message_if_verbose("skipping package ", pkg_actions$package_name[idx],
        " as it (or one of its upstream dependencies) is not accessible",
        verbose = verbose
      )
      next
    }


    # check cached sha matches expect sha
    sha <- pkg_actions$sha[idx]
    cached_sha <- get_short_sha(cache_dir)
    if (sha != cached_sha) {
      stop(
        "The SHA in ", cache_dir, ": ", cached_sha,
        " does not match the sha in the dependency_structure object: ", sha
      )
    }

    if (!dry) {
      if ("test" %in% actions[[1]]) {
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
      if ("build" %in% actions[[1]]) {
        withr::with_dir(artifact_dir, {
          pkg_name <- desc::desc_get_field("Package", file = cache_dir)
          system2(
            "R",
            args = c("CMD", "build", rcmd_args$build, cache_dir),
            stdout = file.path("build_logs", paste0(pkg_name, "_stdout.txt")),
            stderr = file.path("build_logs", paste0(pkg_name, "_stderr.txt"))
          )
          package_tar <- Sys.glob(paste0(pkg_name, "_*.tar.gz"))
        })
      }

      if ("check" %in% actions[[1]]) {
        # check tar.gz if it exists otherwise check the cache_dir
        if (!is.null(package_tar)) {
          withr::with_dir(
            artifact_dir,
            system2("R", args = c("CMD", "check", rcmd_args$check, package_tar))
          )
        } else {
          rcmdcheck_outputs[[pkg_actions$package_name[idx]]] <-
            rcmdcheck::rcmdcheck(cache_dir, error_on = "never", args = rcmd_args$check)
        }
      }

      if ("install" %in% actions[[1]]) {
        if (install_external_deps) {
          install_external_deps(cache_dir,
            internal_pkg_deps = internal_pkg_deps,
            dependencies = TRUE, upgrade = upgrade, ...
          )
        }

        # install the tar.gz if it exists otherwise install using staged.deps
        if (!is.null(package_tar)) {
          withr::with_dir(
            artifact_dir,
            system2("R",
              args = c("CMD", "INSTALL", rcmd_args$install, package_tar),
              stdout = file.path("install_logs", paste0(pkg_name, "_stdout.txt")),
              stderr = file.path("install_logs", paste0(pkg_name, "_stderr.txt"))
            )
          )
        } else {
          install_repo_add_sha(cache_dir, ...)
        }
      }
    } else { # dry run
      message_if_verbose(cat_nl("(Dry run) Skipping", toString(actions), "of", cache_dir), verbose = verbose)
    }
  }

  message_if_verbose("Processed packages in order: ", toString(pkg_actions$package_name), verbose = verbose)

  # output rcmdcheck outputs
  if (!rlang::is_empty(rcmdcheck_outputs)) {
    lapply(
      names(rcmdcheck_outputs),
      function(pkg_name) {
        print(paste("R CMD CHECK package ", pkg_name))
        print(rcmdcheck_outputs[[pkg_name]])
      }
    )
  }

  pkg_actions
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
      subdir = ".",
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
    df <- do.call(
      rbind,
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

# Given a data.frame of internal packages (with [at least] columns package_name, cache_dir),
# create a dependency graph using the R DESCRIPTION files (rather than the
# staged_dependencies.yaml files).
# The dependency graph is defined over the packages in the data.frame. All
# other packages are external and not included.
# The graph_directions can be "upstream", "downstream" or both.
# The "upstream_deps"
# list is the graph where edges point from a package to its upstream
# dependencies. They are ordered in installation order.
# The "downstream_deps" list is the graph with the edge
# direction flipped, and is ordered in reverse installation order.
# It preserves the other columns of this data.frame.
# The "external" list contains the external packages in the DESCRIPTION file,
# idocument where the names are the dependency type (e.g. Imports, Suggests,...)
get_true_deps_graph <- function(pkgs_df,
                                graph_directions = "upstream") {
  stopifnot(
    is.data.frame(pkgs_df),
    all(c("package_name", "cache_dir") %in% colnames(pkgs_df))
  )
  graph_directions <- check_direction_arg_deprecated(graph_directions)
  check_direction_arg(graph_directions)

  # get the Imports, Suggests, Depends for each package
  # from the package DESCRIPTION files, filter for only
  # the internal packages
  upstream_deps <- lapply(
    pkgs_df$cache_dir,
    function(file) {
      if (is.na(file)) {
        return(character(0))
      }
      intersect(pkgs_df$package_name, desc::desc_get_deps(file)$package)
    }
  )
  names(upstream_deps) <- pkgs_df$package_name

  # for inaccessible packages we need to get their upstream
  # dependencies from yaml files of accessible repos
  if (!all(pkgs_df$accessible)) {

    # for each accessible package
    for (idx in seq_len(nrow(pkgs_df))) {
      if (!pkgs_df$accessible[idx]) {
        next
      }

      current_package_name <- pkgs_df$package_name[idx]

      # get the downstream deps of package_name (as specified by the yaml files)
      downstream_deps <- get_yaml_deps_info(pkgs_df$cache_dir[idx])$downstream_repos

      # check if repo and host match for any inaccessible package
      for (downstream_dep in downstream_deps) {
        inaccessible_deps <- dplyr::filter(
          pkgs_df, .data$repo == downstream_dep$repo,
          .data$host == downstream_dep$host, !.data$accessible
        )

        for (package_name in inaccessible_deps$package_name) {
          upstream_deps[[package_name]] <- c(upstream_deps[[package_name]], current_package_name)
        }
      }
    }
  }


  external <- lapply(
    pkgs_df$cache_dir,
    function(file) {
      if (is.na(file)) {
        return(data.frame(type = character(0), package = character(0), version = character(0)))
      }
      df <- desc::desc_get_deps(file)
      df <- df[!df$package %in% c("R", pkgs_df$package_name), ]
      rownames(df) <- NULL
      df
    }
  )
  names(external) <- pkgs_df$package_name

  # order the dependencies
  install_order <- topological_sort(upstream_deps)
  upstream_deps <- upstream_deps[install_order]
  external <- external[install_order]

  res <- list()
  res[["external"]] <- external
  if (graph_directions %in% c("upstream", "all")) {
    res[["upstream_deps"]] <- upstream_deps
  }
  if (graph_directions %in% c("downstream", "all")) {
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
# yaml::write_yaml(dependency_table(project = "."))
yaml_from_dep_table <- function(dep_table) {
  upstream_repos <- dep_table[dep_table$type == "upstream" & dep_table$distance == 1, c("repo", "host")]
  downstream_repos <- dep_table[dep_table$type == "downstream" & dep_table$distance == 1, c("repo", "host")]
  list(
    current_repo = list(
      repo = dep_table[dep_table$type == "current", ]$repo,
      host = dep_table[dep_table$type == "current", ]$host
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


# take string x@y and split it into list(repo=x, host=y, subdir=z)
# error if multiple "@'s" in string, if no @'s then take
# host = "https://github.com"
parse_remote_project <- function(project) {
  error_message <- "Invalid project argument, should be 'repo@host'"
  if (nchar(trimws(project)) == 0) {
    stop(error_message)
  }
  split_string <- strsplit(project, "@")[[1]]
  if (length(split_string) > 2) {
    stop(error_message)
  }
  if (length(split_string) == 1) {
    split_string <- c(split_string, "https://github.com")
  }
  if (any(nchar(trimws(split_string)) == 0)) {
    stop(error_message)
  }
  return(list(repo = split_string[1], host = split_string[2], subdir = "."))
}

# filter packages according to some criteria
# pkg_df = dependency_table(...)$table
filter_pkgs <- function(pkg_df,
                        install_direction,
                        include_project = TRUE,
                        package_list = NULL,
                        distance = NULL) {
  stopifnot(
    is.data.frame(pkg_df),
    is.logical(include_project),
    is.null(distance) || (is.numeric(distance) && distance > 0)
  )
  check_direction_arg(install_direction)

  # filter by install_direction
  if (install_direction != "all") {
    pkg_names <- pkg_df$package_name[pkg_df$type %in% c("current", install_direction)]
  } else {
    pkg_names <- pkg_df$package_name
  }

  if (!include_project) {
    pkg_names <- setdiff(pkg_names, pkg_df$package_name[pkg_df$type == "current"])
  }

  # filter by dependency_packages: restrict actions to those packages
  if (!is.null(package_list)) {
    pkg_names <- intersect(pkg_names, package_list)
  }

  if (!is.null(distance)) {
    pkg_names <- intersect(pkg_names, pkg_df$package_name[pkg_df$distance <= distance, ])
  }

  return(pkg_names)
}

# helper function to compute pkg_actions data.frame for `run_package_actions`
# actions: actions to take for pkg_names, "install" action for upstream_pkgs
compute_actions <- function(pkg_df, pkg_names, actions, upstream_pkgs) {
  pkg_df <- pkg_df[pkg_df$package_name %in% c(pkg_names, upstream_pkgs), ]
  pkg_df[pkg_df$package_name %in% pkg_names, "actions"] <- list(
    rep(list(actions), times = sum(pkg_df$package_name %in% pkg_names))
  ) # outer list() to assign list elements to column
  pkg_df[pkg_df$package_name %in% upstream_pkgs, "actions"] <- "install"
  pkg_df %>%
    dplyr::arrange(.data$install_index) %>%
    dplyr::select("package_name", "cache_dir", "actions", "sha", "installable")
}


# function which takes a string in utils::available.packages$Depends|Imports|Suggests
# and outputs a vector of packages with "R" removed and version requirements removed
parse_deps_table <- function(str) {
  if (is.na(str) || nchar(str) == 0) {
    return(character(0))
  }
  # remove whitespace
  str <- gsub("\\s", "", str)
  # split by "," and remove version info within brackets
  deps <- gsub("\\s*\\([^\\)]+\\)", "", strsplit(str, ",")[[1]])
  # remove R
  deps <- deps[deps != "R"]
  return(deps)
}

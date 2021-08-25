# unlink(get_packages_cache_dir(), recursive = TRUE); dir.create(get_packages_cache_dir()) #todo

# DISCUSSION POINTS:
# todo: replace git2r by gert
# todo: cached repos, add examples

# todo: local_repos to data.frame: package name to directory: no, because this means that the package needs to be fetched from the remote first
#' TODO
#' @param project (`character`) directory of project (for which to restore the
#'   dependencies according to feature); must be a git repository.
#' @param feature (`character`) feature we want to build; inferred from the
#'   branch of the project if not provided; warning if not consistent with
#'   current branch of project
#' @export
dependency_table <- function(project = ".", feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             direction = c("upstream", "downstream"),
                             verbose = 0) {

  stopifnot(is.data.frame(local_repos) || is.null(local_repos))
  check_dir_exists(project)
  check_verbose_arg(verbose)
  check_direction_arg(direction)
  error_if_stageddeps_inexistent(project)

  if (!is.null(feature) && nchar(feature) == 0) {
    feature <- NULL
  }
  feature <- infer_feature_from_branch(feature, project)

  # take local version of project (rather than remote)
  local_repos <- add_project_to_local_repos(project, local_repos)

  repo_deps_info <- get_yaml_deps_info(project)

  # a dataframe with columns repo, host, branch, cache_dir
  internal_deps <- rec_checkout_internal_deps(
    list(repo_deps_info$current_repo), feature, direction = direction,
    local_repos = local_repos, verbose = verbose
  )

  internal_deps$package_name <- get_pkg_names_from_paths(internal_deps$cache_dir)

  if (length(internal_deps$package_name) != length(unique(internal_deps$package_name))) {
    stop("Each R package must have a unique name")
  }

  internal_deps$type <- factor("other", levels = c("current", "upstream", "downstream", "other"))
  internal_deps$distance <- as.numeric(NA)

  # deps$upstream_deps[["a"]] is vector of upstream_deps of "a"
  # deps$downstream_deps[["a"]] is a vector of downstream_deps of "a"
  # where the elements of the lists are the package names found in internal_deps
  # deps is ordered topologically
  deps <- get_true_deps_graph(internal_deps, graph_directions = c("upstream", "downstream"))

  current_pkg <- internal_deps$package_name[
    internal_deps$repo == repo_deps_info$current_repo$repo &
      internal_deps$host == repo_deps_info$current_repo$host
    ]

  internal_deps$type[internal_deps$package_name == current_pkg] <- "current"
  internal_deps$distance[internal_deps$package_name == current_pkg] <- 0

  upstream_nodes <- get_descendants_distance(deps[["upstream_deps"]], current_pkg)
  internal_deps$type[internal_deps$package_name %in% upstream_nodes$id] <- "upstream"
  internal_deps$distance[internal_deps$package_name %in% upstream_nodes$id] <-
    upstream_nodes$distance

  downstream_nodes <- get_descendants_distance(deps[["downstream_deps"]], current_pkg)
  internal_deps$type[internal_deps$package_name %in% downstream_nodes$id] <- "downstream"
  internal_deps$distance[internal_deps$package_name %in% downstream_nodes$id] <-
    downstream_nodes$distance

  # sort the table
  internal_deps <- internal_deps[order(internal_deps$type, internal_deps$distance),
                                 c("package_name", "type", "distance", "branch",
                                   "repo", "host", "cache_dir")]
  rownames(internal_deps) <- NULL

  # install_index: order in which to install packages
  internal_deps$install_index <- vapply(internal_deps$package_name,
                                        function(y) which(names(deps[["upstream_deps"]]) == y),
                                        FUN.VALUE = numeric(1))
  structure(
    list(
      project = fs::path_abs(project),
      current_pkg = current_pkg,
      table = internal_deps,
      deps = deps,
      direction = direction
    ),
    class = "dependency_structure"
  )
}


#' @export
print.dependency_structure <- function(x, ...) {
  # do not show the cache dir or install order when printing
  table <- x$table
  table$cache_dir <- NULL
  table$install_index <- NULL
  print(table)
}


#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
plot.dependency_structure <- function(x, y, ...){

  # construct visNetwork graph
  require_pkgs(c("dplyr", "visNetwork"))
  # todo: put branch below node: https://github.com/almende/vis/issues/3436
  nodes <- x$table %>% dplyr::mutate(
    id = .data$package_name,
    # label does not support html tags
    label = paste0(.data$package_name, "\n", .data$branch),
    title = paste0("<p>", .data$package_name,  "<br/>", .data$type, "<br/>", .data$branch, "</p>"),
    value = 3,
    group = .data$type
  ) %>% dplyr::select(c("id", "label", "title", "value", "group"))

  edges <- rbind(
    cbind_handle_empty(
      adj_list_to_edge_df(x$deps[["upstream_deps"]]),
      arrows = "to", listed_by = "from"
    ),
    cbind_handle_empty(
      adj_list_to_edge_df(x$deps[["downstream_deps"]]) %>% dplyr::rename(to = .data$from, from = .data$to),
      arrows = "to", listed_by = "to"
    )
  )

  get_edge_color <- function(listed_by) {
    if (setequal(listed_by, c("from", "to"))) {
      "#bad984" # green
    } else if (setequal(listed_by, c("from"))) {
      "blue"
    } else if (setequal(listed_by, c("to"))) {
      "#cc1504" # red
    } else {
      stop("Unexpected listed_by: ", listed_by)
    }
  }
  get_edge_tooltip <- function(from, to, listed_by) {
    from <- from[[1]]
    to <- to[[1]]
    if (setequal(listed_by, c("from", "to"))) {
      ""
    } else if (setequal(listed_by, c("from"))) {
      paste0("<p>", to, "<br/> does not list <br/>", from, "</p>")
    } else if (setequal(listed_by, c("to"))) {
      paste0("<p>", from, "<br/> does not list <br/>", to, "</p>")
    } else {
      stop("Unexpected listed_by: ", listed_by)
    }
  }
  edges <- edges %>% dplyr::group_by(.data$from, .data$to) %>%
    dplyr::mutate(color = get_edge_color(.data$listed_by)) %>%
    dplyr::mutate(title = get_edge_tooltip(.data$from, .data$to, .data$listed_by)) %>%
    dplyr::mutate(dashes = !setequal(.data$listed_by, c("from", "to"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyr::one_of("listed_by"))

  plot_title <- paste0("Dependency graph starting from ", x$current_pkg)
  graph <- visNetwork::visNetwork(nodes, edges, width = "100%", main = plot_title) %>%
    # topological sort
    visNetwork::visHierarchicalLayout(sortMethod = "directed", direction = "RL") %>%
    # "orange"
    visNetwork::visGroups(groupname = "upstream", color = "#edc987") %>%
    # "green"
    visNetwork::visGroups(groupname = "downstream", color = "#bad984") %>%
    # "yellow"
    visNetwork::visGroups(groupname = "current", color = "#ccc916") %>%
    # "grey"
    visNetwork::visGroups(groupname = "other", color = "#c5c9c9") %>%
    visNetwork::visOptions(highlightNearest = list(algorithm = "all")) %>%
    # display option to export as png
    visNetwork::visExport() %>%
    visNetwork::visLegend(
      position = "right",
      addEdges = data.frame(
        arrows = "from", dashes = c(FALSE, TRUE, TRUE), font.vadjust = "15",
        color = c(get_edge_color(c("from", "to")), get_edge_color(c("to")), get_edge_color(c("from"))),
        label = c("listed by both", "listed by upstream", "listed by downstream")
      )
    )
  graph
}



#' Install dependencies of project corresponding to feature
#'
#' This reads the dependencies for the project (recursively) and
#' installs the right branches based on the feature.
#' The dependencies can be upstream (by default) and downstream (to
#' install downstream packages as well).
#'
#' It throws a warning if the currently checked out branch in the project
#' is not the one that would be taken based on `feature`.
#' The checked out branch should be a local branch.
#'
#' @md
#' @param dep_structure (`dependency_structure`) output of function
#'   `dependency_table`
#' @param install_project (`logical`) whether to also install the current
#'   package (`project`)
#' @param dry_install (`logical`) dry run that lists packages that would be
#'   installed without installing; this still checks out the git repos to
#'   match `feature`
#' @param install_direction "upstream", "downstream" or both; which packages
#'   to install (according to dependency structure)
#' @param ... Additional args passed to `remotes::install_deps. Note `upgrade`
#'   is set to "never" and shouldn't be passed into this function.
#' @inheritParams rec_checkout_internal_deps
#'
#' @return installed packages in installation order
#'
#' @export
#' @seealso determine_branch
#'
#' @examples
#' \dontrun{
#' install_deps()
#'
#' # install all dependencies
#' install_deps(direction = c("upstream", "downstream"))
#'
#' remove.packages("stageddeps.food")
#' install_deps("../scratch1/stageddeps.food")
#' }
#'
install_deps <- function(dep_structure,
                         install_project = TRUE,
                         dry_install = FALSE,
                         verbose = 0,
                         install_direction = "upstream",
                         install_external_deps = TRUE,
                         dependency_packages = NULL, #TODO get this working
                         ...) {

  stopifnot(methods::is(dep_structure, "dependency_structure"))
  stopifnot(is.logical(install_project), is.logical(dry_install))

  check_verbose_arg(verbose)
  check_direction_arg(install_direction)

  if (!all(install_direction %in% dep_structure$direction)) {
    stop("Invalid install_direction argument for this dependency object")
  }

  # get the packages to install
  pkg_df <- dep_structure$table[order(dep_structure$table$install_index), ]
  # filter by install_direction
  if (length(install_direction) == 1) {
    pkg_df <- pkg_df[, pkg_df$type %in% c("current", install_direction)]
  }
  # if upstream and downstream, we also need to install the upstream deps of the
  # downstream deps, so no filtering

  if (!install_project) {
    pkg_df <- pkg_df[, pkg_df$type != "current"]
  }

  # filter by dependency_packages
  if (!is.null(dependency_packages)) {
    pkg_df <- pkg_df[pkg_df$package_name %in% dependency_packages, ]
  }


  run_package_actions(pkg_df, action = "install", dry = dry_install,
                      install_external_deps = install_external_deps,
                      internal_pkg_deps = dep_structure$table$package_name,
                      verbose = verbose, ...)
  pkg_df$package_name
}


#' Check & install downstream dependencies
#'
#' It installs the downstream dependencies and their upstream dependencies,
#' and then runs `rcmdcheck` (`R CMD check`) on the downstream dependencies.
#'
#' If A <- B (i.e. A is upstream of B), B should list A as upstream. Otherwise,
#' B will be checked, but may not pick up the right version of A (unless there
#' are other packages that require A and are processed before B).
#' If A does not list B as downstream (which can be the case when projects A and
#' B are unrelated), this will simply mean that check_downstream starting from A
#' will not check B which is desirable.
#' This requirement can be verified with `dependency_graph` by looking at the arrows.
#'
#' @md
#'
#' @export
#'
#' @param downstream_repos (`list`) to overwrite the downstream repos to check
#'   of `project`
#' @param dry_install_and_check (`logical`) whether to install upstream
#'   dependencies and check/test downstream repos; otherwise just reports
#'   what would be installed
#' @param recursive (`logical`) whether to recursively check the downstream
#'   dependencies of the downstream dependencies;
#'   ignored if `downstream_repos` is set
#' @param check_args (`list`) arguments passed to `rcmdcheck`
#' @param only_tests (`logical`) whether to only run tests (rather than checks)
#' @inheritParams install_deps
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
check_downstream <- function(dep_structure,
                             downstream_packages = NULL,
                             distance = NULL, dry_install_and_check = FALSE, check_args = NULL,
                             only_tests = FALSE,
                             verbose = 0, install_external_deps = TRUE, ...) {
  stopifnot(
    "dependency_structure" %in% class(dep_structure),
    is.null(distance) || (is.numeric(distance) && distance > 0),
    is.logical(dry_install_and_check)
  )

  check_verbose_arg(verbose)

  if (!"downstream" %in% dep_structure$direction) {
    stop("Invalid dependency table - downstream dependencies must be have been calculated")
  }

  # get the packages to test/check + install
  pkg_df <- dep_structure$table[order(dep_structure$table$install_index),]
  pkg_df <- pkg_df[pkg_df$type == "downstream", ]

  if (!is.null(downstream_packages)) {
    pkg_df <- pkg_df[pkg_df$package_name %in% downstream_packages, ]
  } else if (!is.null(distance)) {
    pkg_df <- pkg_df[pkg_df$distance <= distance, ]
  }

  action <- if (only_tests) c("test", "install") else c("check", "install")

  run_package_actions(pkg_df, action = action, dry = dry_install_and_check,
                      install_external_deps = install_external_deps,
                      internal_pkg_deps = dep_structure$table$package_name,
                      rcmd_args = list(check = check_args),
                      verbose = verbose, ...)

  pkg_df$package_name
}


#' Update existing stage_dependencies yaml file
#'
#' Using the existing stage_dependencies yaml file
#' 'graph' to define internal dependencies, update the
#' project yaml file to include to include all direct
#' (i.e. distance 1) upstream and downstream repos
#' @param dep_structure, `dep_structure` object, output of `dependency_table`
#'   function
#' @md
#' @export
update_with_direct_deps <- function(dep_structure) {
  stopifnot(methods::is(dep_structure, "dependency_structure"))

  yaml_contents <- yaml_from_dep_table(dep_structure$table)
  yaml::write_yaml(
    list(current_repo = yaml_contents$current_repo,
         upstream_repos = yaml_contents$upstream_repos,
         downstream_repos = yaml_contents$downstream_repos),
    file = file.path(dep_structure$project, STAGEDDEPS_FILENAME)
  )
}

#' Recursively discover and build, check and install
#' internal dependencies
#'
#' It discovers all dependencies starting from the repositories,
#' determines the installation order and then
#' builds, checks and installs them in order.
#'
#' @md
#' @inheritParams rec_checkout_internal_deps
#' @param steps (`character` vector) subset of "build", "check", "install";
#'   useful to skip checking for example
#' @param rcmd_args (`list`) with names `build`, `check`,
#'   `install` which are vectors that are passed as separate arguments
#'   to the `R CMD` commands
#' @param artifact_dir (`character`) directory where build
#'   tarball and logs go to
#' @return `artifact_dir` directory with log files
#' @export
#' @examples
#' \dontrun{
#' build_check_install_repos(
#'   list(list(repo = "openpharma/stageddeps.food", host = "https://github.com")),
#'   feature = "main",
#'   direction = "upstream",
#'   local_repos = data.frame(repo = "openpharma/stageddeps.food",
#'   host = "https://github.com", directory = "../scratch1/stageddeps.food/",
#'   stringsAsFactors = FALSE)
#' )
#' build_check_install_repos(
#'   list(list(repo = "openpharma/stageddeps.electricity",
#'   host = "https://github.com")),
#'   feature = "main",
#'   direction = "upstream",
#'   local_repos = data.frame(repo = "openpharma/stageddeps.electricity",
#'   host = "https://github.com",
#'   directory = "../example_ecosystem/stageddeps.electricity/",
#'   stringsAsFactors = FALSE),
#'   artifact_dir = "/tmp/test112"
#' )
#'
#' # to install all packages
#' build_check_install_repos(someArgs, steps = "install")
#' # alternatively with slightly different arguments (e.g. dry_run),
#' # also adds commit SHA
#' install_deps(someArgs, direction = c("upstream", "downstream"))
#' }
build_check_install <- function(dep_structure,
                               install_direction = c("upstream", "downstream"),
                               packages_to_process = NULL,
                               verbose = 0,
                               steps = c("build", "check", "install"),
                               rcmd_args = list(check = c("--no-manual")),
                               artifact_dir = tempfile(),
                               install_external_deps = TRUE, ...) {

  steps <- match.arg(steps, several.ok = TRUE)
  stopifnot(methods::is(dep_structure, "dependency_structure"))

  check_verbose_arg(verbose)

  if (!all(install_direction %in% dep_structure$direction)) {
    stop("Invalid install_direction argument for this dependency object")
  }

  if (!dir.exists(artifact_dir)) {
    dir.create(artifact_dir)
  }

  # get the packages to process
  pkg_df <- dep_structure$table[order(dep_structure$table$install_index), ]
  # filter by install_direction
  if (length(install_direction) == 1) {
    pkg_df <- pkg_df[, pkg_df$type %in% c("current", install_direction)]
  }
  # if upstream and downstream, we also need to install the upstream deps of the
  # downstream deps, so no filtering

  # filter by dependency_packages
  if (!is.null(packages_to_process)) {
    pkg_df <- pkg_df[pkg_df$package_name %in% packages_to_process, ]
  }

  run_package_actions(pkg_df, action = steps,
                      install_external_deps = install_external_deps,
                      internal_pkg_deps = dep_structure$table$package_name,
                      rcmd_args = rcmd_args,
                      artifact_dir = artifact_dir,
                      verbose = verbose, ...)

  return(artifact_dir)
}


#' Checks that the staged dependency yamls are consistent with
#' the dependencies listed in the DESCRIPTION files
#'
#' @md
#' @details This function explicitly checks that for all packages in the
#'   `dependency_structure` object: all upstream and downstream packages specified in each yaml
#'   file are found in the appropriate package DESCRIPTION file
#'
#' @param dep_structure `dependency_structure` object
#' @return NULL if successful. An error is thrown if inconsistencies found
#' @export
#'
#' @examples
#' \dontrun{
#' x <- dependency_table(project = ".")
#' check_yamls_consistent(x)
#' }
check_yamls_consistent <- function(dep_structure) {

  stopifnot(methods::is(dep_structure, "dependency_structure"))
  stopifnot(setequal(dep_structure$direction, c("upstream", "downstream")))

  extract_package_name <- function(repo_and_host, table) {
    table$package_name[table$repo == repo_and_host$repo & table$host == repo_and_host$host]
  }

  error_msg <- c()

  for (index in seq_len(nrow(dep_structure$table))) {
    package_name <- dep_structure$table$package_name[[index]]
    yaml_deps <- get_yaml_deps_info(unlist(dep_structure$table$cache_dir[[index]]))

    # check that packages upstream in the yaml file are upstream deps
    upstream_packages_in_yaml <- vapply(yaml_deps$upstream_repos, extract_package_name, dep_structure$table,
                                        FUN.VALUE = character(1))
    upstream_packages_in_desc <- dep_structure$deps[["upstream_deps"]][[package_name]]
    # the dep_structure$deps only lists the internal package
    error_msg <- c(error_msg, check_set_equal(
      upstream_packages_in_yaml, upstream_packages_in_desc,
      paste0("The internal upstream packages listed in the yaml don't agree with those in the DESCRIPTION files for package ", package_name, ": "),
      return_error = TRUE
    ))

    downstream_packages_in_yaml <- vapply(yaml_deps$downstream_repos, extract_package_name, dep_structure$table,
                                          FUN.VALUE = character(1))
    downstream_packages_in_desc <- dep_structure$deps[["downstream_deps"]][[package_name]]
    error_msg <- c(error_msg, check_set_equal(
      downstream_packages_in_yaml, downstream_packages_in_desc,
      paste0("The internal downstream packages listed in the yaml don't agree with those in the DESCRIPTION files for package ", package_name, ": "),
      return_error = TRUE
    ))
  }

  if (length(error_msg) > 0) {
    stop(paste(error_msg, collapse = "\n"))
  }

  return(invisible(NULL))
}

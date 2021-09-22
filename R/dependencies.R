# DISCUSSION POINTS:
# todo: replace git2r by gert
# todo: cached repos, add examples
# todo: local_repos to data.frame: package name to directory: no, because this means that the package needs to be fetched from the remote first
# todo? unlink(get_packages_cache_dir(), recursive = TRUE); dir.create(get_packages_cache_dir())


#' Create dependency structure of your package collection
#' @param project (`character`) If `project_type` is `local` then
#'   directory of project (for which to calculate the dependency structure);
#'   must be a git repository. If `project_type` is `repo@host` then should
#'   be character of the form `openpharma/stageddeps.food@https://github.com`
#'   If host is not included in the string then the default `https://github.com`
#'   is assumed.
#' @param project_type (`character`) See `project` argument
#' @param feature (`character`) feature we want to build; inferred from the
#'   branch of the project if not provided; warning if not consistent with
#'   current branch of project. If `project_type` is not `local` then this argument
#'   must be provided
#' @param local_repos (`data.frame`) repositories that should be taken from
#'   local file system rather than cloned; columns are `repo, host, directory`
#' @param direction (`character`) direction in which to discover packages
#'   either or both of "upstream" and "downstream".
#'   Note if both are chosen then the entire graph is created
#'   (i.e. upstream dependencies of downstream dependencies)
#' @param verbose (`numeric`) verbosity level, incremental;
#'   (0: None, 1: packages that get installed + high-level git operations,
#'   2: includes git checkout infos)
#' @return `dependency_structure` An S3 object with the following items:
#' \describe{
#'   \item{project}{`project` argument used to create
#'                  the object (absolute path if `project_type` is `local`}
#'   \item{project_type}{`project_type` used to create object}
#'   \item{current_pkg}{The R package name of code in the `project` directory}
#'   \item{table}{`data.frame` contain one row per r package discovered, with the
#'                following rows `package_name`, `type` (`current`, `upstream`, `downstream` or `other`),
#'                `distance` (minimum number of steps from `current_pkg`), `branch`, `repo`, `host`,
#'                `cache_dir` and `install_index` (the order to install the packages).
#'                Note `cache_dir` and `install_index` are suppressed when printing the object}
#'   \item{deps}{`list` with three elements, `upstream_deps`is the graph where edges point from a package
#'               to its upstream dependencies. They are ordered in installation order. The
#'               `downstream_deps` list is the graph with the edge direction flipped,
#'               and is ordered in reverse installation order. `external` contains the external
#'               R packages found in the description files of the internal packages. It is a dataframe
#'               of the form returned by `desc::desc_get_deps`}
#'   \item{direction}{`direction` argument used to create object}
#' }
#' @md
#' @export
#' @examples
#' \dontrun{
#'   dependency_table(verbose = 1)
#'   dependency_table(project = "openpharma/stageddeps.food@@https://github.com",
#'                    project_type = "repo@@host")
#'   x <- dependency_table(project = "path/to/project",
#'                         direction = c("upstream"))
#'   print(x)
#'   plot(x)
#' }
dependency_table <- function(project = ".",
                             project_type = c("local", "repo@host")[1],
                             feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             direction = c("upstream", "downstream"),
                             verbose = 1) {

  # validate arguments
  stopifnot(is.data.frame(local_repos) || is.null(local_repos))
  check_verbose_arg(verbose)
  check_direction_arg(direction)
  stopifnot(project_type %in% c("local", "repo@host"))

  if (project_type == "repo@host" && (is.null(feature) || nchar(feature) == 0)) {
    stop("For non-local projects the feature (branch) must be specified")
  }
  if (project_type == "local") {
    check_dir_exists(project)
    error_if_stageddeps_inexistent(project)
    # infer feature
    if (is.null(feature) || nchar(feature) == 0) {
      feature <- infer_feature_from_branch(NULL, project)
    }
  }

  if (project_type == "local") {
    # take local version of project (rather than remote)
    local_repos <- add_project_to_local_repos(project, local_repos)
    repo_deps_info <- get_yaml_deps_info(project)
    repo_to_process <- list(repo_deps_info$current_repo)
  } else {
    repo_to_process <- list(parse_remote_project(project))
  }

  # a dataframe with columns repo, host, branch, cache_dir, accessible (logical)
  internal_deps <- rec_checkout_internal_deps(
    repo_to_process, feature, direction = direction,
    local_repos = local_repos, verbose = verbose
  )

  internal_deps$package_name[internal_deps$accessible] <-
    get_pkg_names_from_paths(internal_deps$cache_dir[internal_deps$accessible])
  internal_deps$package_name[!internal_deps$accessible] <-
    unlist(lapply(strsplit(internal_deps$repo[!internal_deps$accessible], "/"), tail, 1))

  if (length(internal_deps$package_name) != length(unique(internal_deps$package_name))) {
    stop("Each R package must have a unique name")
  }

  internal_deps$type <- factor("other", levels = c("current", "upstream", "downstream", "other"))
  internal_deps$distance <- as.numeric(NA)

  # deps$upstream_deps[["a"]] is vector of upstream_deps of "a"
  # deps$downstream_deps[["a"]] is a vector of downstream_deps of "a"
  # deps$external[["a"]] is a vector of external packages in "a"s DESCRIPTION file
  # where the elements of the lists are the package names found in internal_deps
  # deps is ordered topologically

  deps <- get_true_deps_graph(internal_deps, graph_directions = c("upstream", "downstream"))

  current_pkg <- internal_deps$package_name[
    internal_deps$repo == repo_to_process[[1]]$repo &
      internal_deps$host == repo_to_process[[1]]$host
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
                                   "repo", "host", "cache_dir", "accessible")]
  rownames(internal_deps) <- NULL

  # install_index: order in which to install packages
  #internal_deps$install_index <- vapply(internal_deps$package_name,
  #                                      function(y) which(names(deps[["upstream_deps"]]) == y),
  #                                      FUN.VALUE = numeric(1))
  structure(
    list(
      project = if (project_type == "local") fs::path_abs(project) else project,
      project_type = project_type,
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
  print(table, ...)
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



#' Install dependencies of project
#'
#' Given a `dependency_structure` object, install the R packages
#'
#' @md
#' @param dep_structure (`dependency_structure`) output of function
#'   `dependency_table`; uses `dep_structure$table` to infer the packages
#'   to apply action to and infer installation order;
#'   uses `dep_structure$deps` to infer upstream dependencies
#' @param install_project (`logical`) whether to also install the current
#'   package (i.e. the package named in `dependency_structure$current_pkg`),
#'   ignored unless `install_direction = "upstream"` (because downstream
#'   deps automatically install all their upstream deps)
#' @param dry_install (`logical`) dry run that lists packages that would be
#'   installed
#' @param install_direction "upstream", "downstream" or both; which packages
#'   to install (according to dependency structure). By default this is only "upstream"
#' @param install_external_deps logical to describe whether to install
#'   external dependencies of packages using `remotes::install_deps`.
#' @param dependency_packages (`character`) An additional filter, only packages on this
#'   list will be installed (advanced usage only)
#' @param verbose verbosity level, incremental; from 0 (none) to 2 (high)
#' @param ... Additional args passed to `remotes::install_deps. Note `upgrade`
#'   is set to "never" and shouldn't be passed into this function.
#'
#' @return `data.frame` of performed actions
#'
#' @export
#' @seealso determine_branch
#'
#' @examples
#' \dontrun{
#' x <- dependency_table(project = "./path/to/project")
#'
#' install_deps(x)
#'
#' # install all dependencies
#' install_deps(x, direction = c("upstream", "downstream"))
#'
#' }
install_deps <- function(dep_structure,
                         install_project = TRUE,
                         dry_install = FALSE,
                         install_direction = "upstream",
                         install_external_deps = TRUE,
                         dependency_packages = NULL,
                         verbose = 1,
                         ...) {

  stopifnot(methods::is(dep_structure, "dependency_structure"))
  stopifnot(is.logical(install_project), is.logical(dry_install))

  if (!all(install_direction %in% dep_structure$direction)) {
    stop("Invalid install_direction argument for this dependency object")
  }

  # get the packages to install
  pkg_df <- dep_structure$table

  pkg_names <- filter_pkgs(pkg_df, install_direction,
                           include_project = install_project,
                           dependency_packages = dependency_packages)


  # we also need to install the upstream dependencies of e.g. the downstream dependencies
  # they may have been filtered out by the above
  upstream_pkgs <- get_descendants(dep_structure$deps[["upstream_deps"]], pkg_names)

  pkg_actions <- compute_actions(pkg_df, pkg_names, "install", upstream_pkgs)

  run_package_actions(pkg_actions, dry = dry_install,
                      install_external_deps = install_external_deps,
                      internal_pkg_deps = dep_structure$table$package_name,
                      verbose = verbose, ...)
  pkg_actions
}


#' Check & install downstream dependencies
#'
#' Installs downstream R packages as specified in a
#' `dependency_structure` object  and then runs
#' `rcmdcheck` (`R CMD check`) on the downstream dependencies.
#'
#' @md
#' @param downstream_packages (`vector`) additional filter to only install
#'   and check packages contained in this vector (advanced use only)
#' @param distance (`numeric`) additional filter to only install downstream
#'   packages at most this distance from the `dependency_structure$current_pkg`
#'   (advanced use only)
#' @param dry_install_and_check (`logical`) whether to install upstream
#'   dependencies and check/test downstream repos; otherwise just reports
#'   what would be installed
#' @param check_args (`list`) arguments passed to `rcmdcheck`
#' @param only_tests (`logical`) whether to only run tests (rather than checks)
#' @inheritParams install_deps
#' @inheritDotParams install_deps
#' @export
#
#' @return `data.frame` of performed actions
#' @examples
#' \dontrun{
#' x <- dependency_table(project = ".", verbose = 1)
#'
#' check_downstream(x, verbose = 1)
#' check_downstream(x, verbose = 1, only_test = TRUE, check_args = c("--no-manual"))
#' }
check_downstream <- function(dep_structure,
                             downstream_packages = NULL,
                             distance = NULL, dry_install_and_check = FALSE,
                             check_args = NULL,
                             only_tests = FALSE,
                             verbose = 1, install_external_deps = TRUE, ...) {
  stopifnot(
    methods::is(dep_structure, "dependency_structure"),
    is.logical(dry_install_and_check)
  )

  if (!"downstream" %in% dep_structure$direction) {
    stop("Invalid dependency table - downstream dependencies must be have been calculated")
  }

  # get the packages to install
  pkg_df <- dep_structure$table

  pkg_names <- filter_pkgs(pkg_df, install_direction = "downstream",
                           include_project = FALSE,
                           dependency_packages = downstream_packages,
                           distance = distance)


  # we also need to install the upstream dependencies of e.g. the downstream dependencies
  # they may have been filtered out by the above
  upstream_pkgs <- get_descendants(dep_structure$deps[["upstream_deps"]], pkg_names)

  actions <- if (only_tests) c("test", "install") else c("check", "install")
  pkg_actions <- compute_actions(pkg_df, pkg_names, actions, upstream_pkgs)

  run_package_actions(pkg_actions, dry = dry_install_and_check,
                      install_external_deps = install_external_deps,
                      internal_pkg_deps = dep_structure$table$package_name,
                      rcmd_args = list(check = check_args),
                      verbose = verbose, ...)

  pkg_actions
}


#' Update existing stage_dependencies yaml file
#'
#' Using the existing stage_dependencies yaml file
#' 'graph' to define internal dependencies, update the
#' project yaml file to include to include all direct
#' (i.e. distance 1) upstream and downstream repos
#' @param dep_structure, `dep_structure` object, output of `dependency_table`
#'   function with `project_type = "local"`
#' @md
#' @export
update_with_direct_deps <- function(dep_structure) {
  stopifnot(methods::is(dep_structure, "dependency_structure"))
  if (dep_structure$project_type != "local") {
    stop("Can only update yaml file for local projects")
  }

  yaml_contents <- yaml_from_dep_table(dep_structure$table)
  yaml::write_yaml(
    list(current_repo = yaml_contents$current_repo,
         upstream_repos = yaml_contents$upstream_repos,
         downstream_repos = yaml_contents$downstream_repos),
    file = file.path(dep_structure$project, STAGEDDEPS_FILENAME)
  )
}

#' Build, check and install internal dependencies
#'

#'
#' @md
#' @inheritParams install_deps
#' @inheritDotParams install_deps
#' @param steps (`character` vector) subset of "build", "check", "install";
#'   useful to skip checking for example
#' @param rcmd_args (`list`) with names `build`, `check`,
#'   `install` which are vectors that are passed as separate arguments
#'   to the `R CMD` commands
#' @param packages_to_process (`character`) An additional filter, only packages on
#'   this list will be considered (advanced usage only)
#' @param artifact_dir (`character`) directory to place built R packages
#'   and logs
#' @return list with entries
#'  - artifact_dir: `artifact_dir` directory with log files
#'  - pkg_actions: `data.frame` of performed actions
#' @export
#' @examples
#' \dontrun{
#'   x <- dependency_table(project = ".", verbose = 1)
#'   build_check_install(x, steps = c("build", "check"), verbose = 1)
#'   build_check_install(x, artifact_dir = "../output")
#'
#' }
build_check_install <- function(dep_structure,
                               install_direction = c("upstream", "downstream"),
                               packages_to_process = NULL,
                               dry_install = FALSE,
                               verbose = 1,
                               steps = c("build", "check", "install"),
                               rcmd_args = list(check = c("--no-manual")),
                               artifact_dir = tempfile(),
                               install_external_deps = TRUE, ...) {

  steps <- match.arg(steps, several.ok = TRUE)
  stopifnot(methods::is(dep_structure, "dependency_structure"))

  if (!all(install_direction %in% dep_structure$direction)) {
    stop("Invalid install_direction argument for this dependency object")
  }

  if (!dir.exists(artifact_dir)) {
    dir.create(artifact_dir)
  }

  # get the packages to install
  pkg_df <- dep_structure$table

  pkg_names <- filter_pkgs(pkg_df, install_direction = install_direction,
                           include_project = TRUE,
                           dependency_packages = packages_to_process)


  # we also need to install the upstream dependencies of e.g. the downstream dependencies
  # they may have been filtered out by the above
  upstream_pkgs <- get_descendants(dep_structure$deps[["upstream_deps"]], pkg_names)

  pkg_actions <- compute_actions(pkg_df, pkg_names, steps, upstream_pkgs)

  run_package_actions(pkg_actions, dry = dry_install,
                      install_external_deps = install_external_deps,
                      internal_pkg_deps = dep_structure$table$package_name,
                      rcmd_args = rcmd_args,
                      artifact_dir = artifact_dir,
                      verbose = verbose, ...)

  return(list(artifact_dir = artifact_dir, pkg_actions = pkg_actions))
}


#' Checks that the staged dependency yamls are consistent with
#' the dependencies listed in the DESCRIPTION files
#'
#' @md
#' @details This function explicitly checks that for all packages in the
#'   `dependency_structure` object: all upstream and downstream packages specified
#'   in each yaml file are found in the appropriate package DESCRIPTION file
#'
#' @param dep_structure `dependency_structure` object
#' @param skip_if_missing_yaml `logical` should checks be skipped on packages
#'   without yaml files. Default `FALSE`
#' @return NULL if successful. An error is thrown if inconsistencies found
#' @export
#'
#' @examples
#' \dontrun{
#' x <- dependency_table(project = ".")
#' check_yamls_consistent(x)
#' }
check_yamls_consistent <- function(dep_structure, skip_if_missing_yaml = FALSE) {

  stopifnot(methods::is(dep_structure, "dependency_structure"))
  stopifnot(setequal(dep_structure$direction, c("upstream", "downstream")))
  stopifnot(rlang::is_bool(skip_if_missing_yaml))

  extract_package_name <- function(repo_and_host, table) {
    table$package_name[table$repo == repo_and_host$repo & table$host == repo_and_host$host]
  }

  error_msg <- c()

  for (index in seq_len(nrow(dep_structure$table))) {
    package_name <- dep_structure$table$package_name[[index]]
    yaml_deps <- get_yaml_deps_info(unlist(dep_structure$table$cache_dir[[index]]))

    # if there is no yaml file then skip checks
    if (skip_if_missing_yaml && is.function(yaml_deps$current_repo)) {
      next
    }

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


#' List the external R packages required to be installed
#'
#'
#' @param available_packages (`data.frame`) A dataframe of the format given by
#'   `as.data.frame(utils::available.packages)`. It is unlikely this default needs to be changed;
#'   however you need to ensure the `options("repos")` contains the urls of all expected repos
#'   (e.g. Bioconductor).
#' @param from_internal_dependencies Vector chosen from `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`
#'   which fields of the DESCRIPTION file of the internal packages should be included. Default:
#'   `c("Depends", "Imports", "LinkingTo", "Suggests")`
#' @param from_external_dependencies Vector chosen from `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`
#'   which fields of the DESCRIPTION file of the internal packages should be included. Default:
#'   `c("Depends", "Imports", "LinkingTo")`
#' @inheritParams build_check_install
#' @md
#' @return A vector of 'external' R packages required to install
#'   the selected 'internal' packages. This can be used with `remotes::system_requirements`
#'   to extract the system requirements needed for your packages, see example below.
#' @examples
#' \dontrun{
#'   x <- dependency_table("openpharma/stageddeps.electricity",
#'     project_type = "repo@@host", feature = "main")
#'
#'   # get external package dependencies
#'   ex_deps <- get_all_external_dependencies(x)
#'   print(ex_deps)
#'
#'   # get system dependencies (in this case there are none)
#'   unique(unlist(lapply(ex_deps,
#'     function(pkg, ...)
#'       remotes::system_requirements(package = pkg, ...),
#'     os = "ubuntu",
#'     os_release = "20.04")
#'  ))
#' }
#' @export
get_all_external_dependencies <- function(dep_structure,
                                          available_packages = as.data.frame(utils::available.packages()),
                                          install_direction = "upstream",
                                          packages_to_process = NULL,
                                          from_internal_dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"),
                                          from_external_dependencies = c("Depends", "Imports", "LinkingTo")) {
  stopifnot(methods::is(dep_structure, "dependency_structure"))

  stopifnot(methods::is(available_packages, "data.frame"))
  stopifnot(all(c("Depends", "Suggests", "Imports", "Package") %in% colnames(available_packages)))

  match.arg(from_internal_dependencies, c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"), several.ok = TRUE)
  match.arg(from_external_dependencies, c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"), several.ok = TRUE)


  if (!all(install_direction %in% dep_structure$direction)) {
    stop("Invalid install_direction argument for this dependency object")
  }

  r_core_packages <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
  "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "translations", "utils")

  # get the packages to consider
  pkg_df <- dep_structure$table

  pkg_names <- filter_pkgs(pkg_df, install_direction = install_direction,
                           include_project = TRUE,
                           dependency_packages = packages_to_process)


  # we also need to consider the upstream dependencies of e.g. the downstream dependencies
  # they may have been filtered out by the above.
  # deps["upstream_deps"] is a topologically sorted list where dep_structure$deps["upstream_deps"]["x"]
  # are the upstream deps of package x, get_descendants gets all of the children of a list parents_to_children
  # so in this case are upstream "descendants"
  upstream_pkgs <- get_descendants(dep_structure$deps[["upstream_deps"]], pkg_names)

  # the external packages we have considered
  external_packages <- character(0)
  # and those to consider
  packages_to_consider <- unique(unlist(
    lapply(dep_structure$deps$external[names(dep_structure$deps$external) %in% c(upstream_pkgs, pkg_names)],
      function(df) df$package[df$type %in% from_internal_dependencies])
    )
  )

  while (length(packages_to_consider) > 0) {

    if (any(!packages_to_consider %in% c(available_packages$Package, r_core_packages))) {
      warning("Cannot find information about package(s) ",
        toString(packages_to_consider[!packages_to_consider %in% c(available_packages$Package, r_core_packages)]),
        " check that options('repos') contains expected repos")
    }

    # get appropriate rows of data.frame
    filtered_available_packages <- available_packages[available_packages$Package %in% packages_to_consider, ]

    # get appropriate columns
    deps <- unname(unlist(filtered_available_packages[, from_external_dependencies]))

    # parse to get new packages
    new_packages_to_consider <- unique(unlist(lapply(deps, parse_deps_table)))

    # move those that have been processed to external_packages vector and
    # update the packages to be considered for the next iteration
    external_packages <- c(external_packages, packages_to_consider)

    packages_to_consider <- setdiff(new_packages_to_consider, external_packages)
  }

  return(external_packages)

}

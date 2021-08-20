# unlink(get_packages_cache_dir(), recursive = TRUE); dir.create(get_packages_cache_dir()) #todo

# DISCUSSION POINTS:
# todo: replace git2r by gert
# todo: cached repos, add examples
# todo: rstudio addin to format doc
# todo: add project field (scope) in yaml: to restrict to projects
# todo: option to also fetch project from remote


#' @include caching.R
NULL

#' TODO
#' @export
dependency_table <- function(project = ".", feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             direction = c("upstream", "downstream"),
                             verbose = 0) {

  stopifnot(is.data.frame(local_repos) || is.null(local_repos))
  check_dir_exists(project)
  check_verbose_arg(verbose)
  error_if_stageddeps_inexistent(project)

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
  deps <- get_true_deps_graph(internal_deps, direction = c("upstream", "downstream"))

  current_repo <- internal_deps$package_name[internal_deps$repo == repo_deps_info$current_repo$repo &
                                             internal_deps$host == repo_deps_info$current_repo$host]

  internal_deps$type[internal_deps$package_name == current_repo] <- "current"
  internal_deps$distance[internal_deps$package_name == current_repo] <- 0

  upstream_nodes <- get_descendants_distance(deps[["upstream_deps"]], current_repo)
  internal_deps$type[internal_deps$package_name %in% upstream_nodes$id] <- "upstream"
  internal_deps$distance[match(upstream_nodes$id, internal_deps$package_name)] <- upstream_nodes$distance

  downstream_nodes <- get_descendants_distance(deps[["downstream_deps"]], current_repo)
  internal_deps$type[internal_deps$package_name %in% downstream_nodes$id] <- "downstream"
  internal_deps$distance[match(downstream_nodes$id, internal_deps$package_name)] <- downstream_nodes$distance

  # sort the table
  internal_deps <- internal_deps[order(internal_deps$type, internal_deps$distance),
                                 c("package_name", "type", "distance", "branch",
                                   "repo", "host", "cache_dir")]
  rownames(internal_deps) <- NULL
  internal_deps$type <- as.character(internal_deps$type)

  internal_deps$install_index <- vapply(internal_deps$package_name,
                                        function(y) which(names(deps[["upstream_deps"]]) == y),
                                        FUN.VALUE = numeric(1))
  structure(
    list(
      project = fs::path_abs(project),
      current_repo = current_repo,
      local_repos = local_repos,
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
    dplyr::select(-one_of("listed_by"))

  plot_title <- paste0("Dependency graph starting from ", x$current_repo)
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
#' @param project (`character`) directory of project (for which to restore the
#'   dependencies according to feature); must be a git repository.
#' @param feature (`character`) feature we want to build; inferred from the
#'   branch of the project if not provided; warning if not consistent with
#'   current branch of project
#' @param install_project (`logical`) whether to also install the current
#'   package (`project`)
#' @param dry_install (`logical`) dry run that lists packages that would be
#'   installed without installing; this still checks out the git repos to
#'   match `feature`
#' @param install_external_deps logical to describe whether to install
#'   external dependencies of package using `remotes::install_deps`.
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
                         install_external_deps = TRUE,
                         install_direction = "upstream",
                         ...) {

  stopifnot("dependency_structure" %in% class(dep_structure))
  stopifnot(is.logical(install_project), is.logical(dry_install))

  check_verbose_arg(verbose)

  if (!all(install_direction %in% dep_structure$direction)) {
    stop("Invalid install_direction argument for this dependency object")
  }

  # get the packages to install
  #TODO


  if (identical(install_direction, "upstream")) {
    # if installing upstream dependencies, project should appear last
    # if only direct upstream and downstream dependencies are listed in
    # the yaml. Otherwise, more packages may also get installed, so we
    # issue a warning.
    if (!isTRUE(all.equal(utils::tail(install_order, 1)[[1]], dep_structure$current_repo))){
      warning("The staged dependency yaml files of your packages imply ",
           utils::tail(install_order, 1)[[1]]$repo,
           " is an upstream dependency of ",
           dep_structure$current_repo$repo,
           "; this is not consistent with the dependencies given in the",
           " DESCRIPTION files.",
           " You can safely ignore this warning, but more ",
           "packages than necessary are installed.",
           " Use the function 'check_yamls_consistent' to find out why.")
    }
  }

  if (!install_project) {
    install_order <- Filter(function(x) !identical(x, dep_structure$current_repo), install_order)
  }

  if (verbose >= 1) {
    message("Installing packages in order: ", toString(extract_str_field(install_order, "repo")))
  }
  hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(dep_structure$local_repos)

  for (repo_and_host in install_order) {
    is_local <- hash_repo_and_host(repo_and_host) %in% names(hashed_repo_to_dir)
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)
    if (!dry_install) {
      install_repo_add_sha(repo_dir, install_external_deps = install_external_deps,
                           internal_pkg_deps = dep_structure$table$package_name, ...)
    } else if (verbose >= 1) {
      cat_nl("(Dry run) Skipping installation of ", repo_dir)
    }
  }
  if (verbose >= 1) {
    message("Installed packages in order: ", toString(extract_str_field(install_order, "repo")))
  }

  install_order
}


#' Gadget or Shiny app to select the dependencies to install
#'
#' The dependencies are obtained by traversing the upstream and downstream
#' dependencies starting from `project`.
#'
#' @md
#' @param default_feature (`character`) default feature, see also the parameter
#'   `feature` of `\link{install_deps}`
#' @param run_gadget (`logical`) whether to run the app as a gadget
#' @param run_as_job (`logical`) whether to run the installation as an RStudio job.
#' @inheritParams install_deps
#' @export
#' @return `shiny.app` or value returned by app (executed as a gadget)
#'
install_deps_app <- function(project = ".", default_feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             run_gadget = TRUE, run_as_job = TRUE,
                             verbose = 1, install_external_deps = TRUE, ...) {
  require_pkgs(c("shiny", "miniUI"))

  # take local version of project (rather than remote)
  local_repos <- add_project_to_local_repos(project, local_repos)

  app <- shiny::shinyApp(
    ui = function() {
      miniUI::miniPage(
        shiny::fillCol(
          shiny::tagList(
            shiny::textInput("feature", label = "Feature: ", value = default_feature),
            shiny::actionButton("compute_graph", "Compute graph")
          ),
          miniUI::miniContentPanel(
            visNetwork::visNetworkOutput("network_proxy_nodes", height = "400px")
          ),
          miniUI::miniContentPanel(
            shiny::tags$p("The following packages will be installed:"),
            shiny::verbatimTextOutput("nodesToInstall")
          ),
          flex = c(NA, 2, 1)
        ),
        miniUI::gadgetTitleBar(
          "Cmd + Click node to not install the node",
          right = miniUI::miniTitleBarButton("done", "Install", primary = TRUE)
        )
      )
    },
    server = function(input, output, session) {
      compute_dep_structure <- shiny::eventReactive(input$compute_graph, {
        feature <- input$feature
        if (identical(feature, "")) {
          feature <- NULL
        }
        if (verbose >= 1) {
          message("Computing dependency structure for feature ", feature, " starting from project ", project)
        }
        dependency_structure(project, feature = feature,
                             local_repos = local_repos, verbose = 2)
      },
      # do not ignore NULL to also compute initially with the default feature when
      # the button was not yet clicked
      ignoreNULL = FALSE)

      output$network_proxy_nodes <- visNetwork::renderVisNetwork({
        compute_dep_structure()$graph %>%
          visNetwork::visInteraction(multiselect = TRUE)
      })

      # todo: better solution? this only updates every 1s, so clicking on
      # okay in between takes old selected nodes
      autoInvalidate <- shiny::reactiveTimer(1000)
      shiny::observeEvent({
        autoInvalidate()
      }, {
        visNetwork::visNetworkProxy("network_proxy_nodes") %>%
          visNetwork::visGetSelectedNodes()
      })
      output$nodesToInstall <- shiny::renderText({
        paste(setdiff(
          names(compute_dep_structure()$deps[["upstream_deps"]]),
          input$network_proxy_nodes_selectedNodes
        ), collapse = "\n")
      })
      shiny::observeEvent(input$done, {
        selected_hashed_pkgs <- input$network_proxy_nodes_selectedNodes
        install_order <- get_install_order(compute_dep_structure()$deps[["upstream_deps"]])

        # take local version of project (rather than remote)
        local_repos <- add_project_to_local_repos(project, local_repos)

        # subset of repo dirs to install according to selection
        # we write the code this way so we only have to pass the repo dirs to the
        # rstudio job script
        repo_dirs_to_install <- c()
        hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
        internal_pkg_deps <- get_pkg_names_from_paths(compute_dep_structure()$internal_deps)
        for (repo_and_host in install_order) {
          if (hash_repo_and_host(repo_and_host) %in% selected_hashed_pkgs) {
            # the selected nodes are NOT installed
            next
          }
          is_local <- hash_repo_and_host(repo_and_host) %in% names(hashed_repo_to_dir)
          repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)

          repo_dirs_to_install <- c(repo_dirs_to_install, repo_dir)
        }

        if (verbose >= 1) {
          message("Installing directories in order: ", repo_dirs_to_install)
        }
        if (run_as_job) {
          # note: this uses the currently installed version of this package because
          # it spans a new R process (not the loaded version)
          args_str <- paste(deparse(repo_dirs_to_install), collapse = "\n")

          other_args <- c(list(
            install_external_deps = install_external_deps,
            internal_pkg_deps = internal_pkg_deps
          ), list(...))
          other_args_str <- paste(deparse(other_args), collapse = "\n")

          script <- glue::glue(
            "do.call(
              function(...) lapply({args_str}, staged.dependencies:::install_repo_add_sha, ...),
            {other_args_str})"
          )
          run_job(script, "install_deps_app",
                  paste0("Install selection of deps of ", basename(project)))
        } else {
          lapply(repo_dirs_to_install, install_repo_add_sha,
                 install_external_deps = install_external_deps,
                 internal_pkg_deps = internal_pkg_deps, ...)
        }
        if (verbose >= 1) {
          message("Installed directories in order: ", repo_dirs_to_install)
        }

        # calling it at the top of this reactive still finishes executing
        # the reactive (so installs), so we call it down here
        invisible(shiny::stopApp())
      })
    }
  ); app
  if (run_gadget) {
    shiny::runGadget(app, viewer = shiny::dialogViewer("Install packages"))
    # shiny::runGadget(app, viewer = browserViewer())
  } else {
    app
  }
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
check_downstream <- function(project = ".", feature = NULL, downstream_repos = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             recursive = TRUE, dry_install_and_check = FALSE, check_args = NULL,
                             only_tests = FALSE,
                             verbose = 0, install_external_deps = TRUE, ...) {
  stopifnot(
    is.data.frame(local_repos) || is.null(local_repos),
    is.logical(recursive),
    is.logical(dry_install_and_check)
  )
  check_dir_exists(project)
  check_verbose_arg(verbose)
  error_if_stageddeps_inexistent(project)

  feature <- infer_feature_from_branch(feature, project)

  # take local version of project (rather than remote)
  local_repos <- add_project_to_local_repos(project, local_repos)

  repo_deps_info <- get_yaml_deps_info(project)

  if (is.null(downstream_repos)) {
    downstream_repos <- if (!recursive) {
      get_yaml_deps_info(project)$downstream_repos
    } else {

      internal_deps <- rec_checkout_internal_deps(
        list(repo_deps_info$current_repo), feature, direction = "downstream",
        local_repos = local_repos, verbose = verbose
      )

      deps <- get_true_deps_graph(internal_deps, direction = "downstream")

      lapply(get_descendants(
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

  internal_deps <- rec_checkout_internal_deps(
    downstream_repos, feature, direction = "upstream",
    local_repos = local_repos, verbose = verbose
  )

  deps <- get_true_deps_graph(internal_deps, direction = "upstream")

  install_order <- get_install_order(deps[["upstream_deps"]])

  hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
  internal_pkg_deps <- get_pkg_names_from_paths(internal_deps)
  if (verbose >= 1) {
    message("Installing packages in order: ", toString(extract_str_field(install_order, "repo")))
  }
  for (repo_and_host in install_order) {
    is_local <- hash_repo_and_host(repo_and_host) %in% names(hashed_repo_to_dir)
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)
    if (hash_repo_and_host(repo_and_host) %in% lapply(downstream_repos, hash_repo_and_host)) {
      if (!dry_install_and_check) {
        if (only_tests) {
          # testthat::test_dir and devtools::test do not always agree
          # testthat::test_dir(file.path(repo_dir, "tests"), stop_on_failure = TRUE, stop_on_warning = TRUE)
          # stop_on_failure argument cannot be passed to devtools::test
          if (dir.exists(file.path(repo_dir, "tests"))) {
            # this does not work with legacy R packages where tests are in the inst directory
            # see devtools:::find_test_dir
            test_res <- devtools::test(repo_dir, stop_on_warning = TRUE)
            all_passed <- function(res) {
              # copied from testthat:::all_passed
              if (length(res) == 0) {
                return(TRUE)
              }
              df <- as.data.frame(res)
              sum(df$failed) == 0 && all(!df$error)
            }
            if (!all_passed(test_res)) {
              stop("Tests for package in directory ", repo_dir, " failed")
            }
          } else {
            if (verbose >= 1) {
              message("No tests found for package in directory ", repo_dir)
            }
          }
        } else {
          rcmdcheck::rcmdcheck(repo_dir, error_on = "warning", args = check_args)
        }
      } else if (verbose >= 1) {
        cat_nl("(Dry run): Would test/check ", repo_dir)
      }
    }
    if (!dry_install_and_check) {
      install_repo_add_sha(repo_dir, install_external_deps = install_external_deps,
                           internal_pkg_deps = internal_pkg_deps, ...)
    } else if (verbose >= 1) {
      cat_nl("(Dry run): Would install ", repo_dir)
    }
  }
  if (verbose >= 1) {
    message("Installed packages in order: ", toString(extract_str_field(install_order, "repo")))
  }

  df <- data.frame(
    repo = extract_str_field(install_order, "repo"),
    host = extract_str_field(install_order, "host")
  )
  df$checked <- Map(function(repo, host) {
    hash_repo_and_host(list(repo = repo, host = host)) %in% lapply(downstream_repos, hash_repo_and_host)
  }, df$repo, df$host)
  df
}


#' Update existing stage_dependencies yaml file
#'
#' Using the existing stage_dependencies yaml file
#' 'graph' to define internal dependencies, update the
#' project yaml file to include to include all direct
#' (i.e. distance 1) upstream and downstream repos
#' @inheritParams dependency_structure
#' @export
update_with_direct_deps <- function(dep_structure) {
  stopifnot("dependency_structure" %in% class(dep_structure))

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
build_check_install_repos <- function(repos_to_process, feature = "main",
                                      direction = c("upstream", "downstream"),
                                      local_repos = get_local_pkgs_from_config(),
                                      verbose = 0,
                                      steps = c("build", "check", "install"),
                                      rcmd_args = list(check = c("--no-manual")),
                                      artifact_dir = tempfile()) {
  steps <- match.arg(steps, several.ok = TRUE)
  if (!dir.exists(artifact_dir)) {
    dir.create(artifact_dir)
  }

  internal_deps <- rec_checkout_internal_deps(
    repos_to_process, feature, direction, local_repos, verbose
  )
  # we need the upstream direction (rather than variable direction) to
  # compute the installation order
  deps <- get_true_deps_graph(internal_deps, direction = "upstream")
  install_order <- get_install_order(deps[["upstream_deps"]])
  install_order_paths <- vapply(
    install_order, function(x) internal_deps[[hash_repo_and_host(x)]], character(1)
  )

  if ("build" %in% steps) {
    dir.create(file.path(artifact_dir, "build_logs"))
  }
  if ("install" %in% steps) {
    dir.create(file.path(artifact_dir, "install_logs"))
  }

  cat(paste0("Installing packages from paths ", toString(install_order_paths)), "\n")

  lapply(
    install_order_paths,
    function(pkg_source_dir) {
      withr::with_dir(artifact_dir, {
        pkg_name <- desc::desc_get_field("Package", file = pkg_source_dir)
        if ("build" %in% steps) {
          system2(
            "R", args = c("CMD", "build", rcmd_args$build, pkg_source_dir),
            stdout = file.path("build_logs", paste0(pkg_name, "_stdout.txt")),
            stderr = file.path("build_logs", paste0(pkg_name, "_stderr.txt"))
          )
          pkg_file <- Sys.glob(paste0(pkg_name, "_*.tar.gz"))
        } else {
          pkg_name
        }

        if ("check" %in% steps) {
          system2("R", args = c("CMD", "check", rcmd_args$check, pkg_file))
        }

        if ("install" %in% steps) {
          system2(
            "R", args = c("CMD", "INSTALL", rcmd_args$install, pkg_file),
            stdout = file.path("install_logs", paste0(pkg_name, "_stdout.txt")),
            stderr = file.path("install_logs", paste0(pkg_name, "_stderr.txt"))
          )
        }
      })
    }
  )

  return(artifact_dir)
}


#' Checks that the staged dependency yamls are consistent with
#' the dependencies listed in the DESCRIPTION files
#'
#' @md
#' @inheritParams rec_checkout_internal_deps
#' @export
#'
#' @examples
#' \dontrun{
#' check_yamls_consistent(
#' list(list(repo = "openpharma/stageddeps.food",
#'           host = "https://github.com")),
#' feature = "main",
#' local_repos = data.frame(
#'   repo = "openpharma/stageddeps.food",
#'   host = "https://github.com",
#'   directory = "../scratch1/stageddeps.food",
#'   stringsAsFactors = FALSE
#' )
#' )
#' }
check_yamls_consistent <- function(repos_to_process, feature,
                                       direction = c("upstream", "downstream"),
                                       local_repos = get_local_pkgs_from_config(),
                                       verbose = 0) {
  internal_deps <- rec_checkout_internal_deps(
    repos_to_process, feature, direction, local_repos, verbose
  )
  deps <- get_true_deps_graph(internal_deps, direction = c("upstream", "downstream"))
  for (hashed_repo_and_host in names(internal_deps)) {
    package_path <- internal_deps[[hashed_repo_and_host]]
    yaml_deps <- get_yaml_deps_info(package_path)

    check_set_equal(
      deps[["upstream_deps"]][[hashed_repo_and_host]],
      vapply(unname(yaml_deps[["upstream_repos"]]), hash_repo_and_host, character(1)),
      pre_msg = paste0(
        "For package '", hashed_repo_and_host,
        "':\nExpected dependencies 'x' from DESCRIPTION files vs dependencies 'y'",
        " from staged dependency yaml file:\n"
      )
    )
  }

  return(invisible(NULL))
}

# unlink(get_packages_cache_dir(), recursive = TRUE); dir.create(get_packages_cache_dir()) #todo

# DISCUSSION POINTS:
# todo: replace git2r by gert
# todo: cached repos, add examples
# todo: rstudio addin to format doc
# todo: add project field (scope) in yaml: to restrict to projects (e.g. nest only)
# todo: option to also fetch project from remote


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

  repo_deps_info <- get_deps_info(project)
  rbind(
    local_repos,
    data.frame(
      repo = repo_deps_info$current_repo$repo,
      host = repo_deps_info$current_repo$host,
      directory = normalizePath(project), stringsAsFactors = FALSE
    )
  )
}


#' Install upstream dependencies of project corresponding to feature
#'
#' This reads the upstream dependencies for the project (recursively) and
#' installs the right branches based on the feature.
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
#' @inheritParams get_internal_dependencies
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
#' }
#'
install_deps <- function(project = ".", feature = NULL,
                                  local_repos = get_local_pkgs_from_config(),
                                  direction = "upstream",
                                  install_project = TRUE, dry_install = FALSE, verbose = 0) {
  stopifnot(
    is.data.frame(local_repos) || is.null(local_repos),
    is.logical(install_project),
    is.logical(dry_install)
  )
  check_dir_exists(project)
  check_verbose_arg(verbose)
  error_if_stageddeps_inexistent(project)

  feature <- infer_feature_from_branch(feature, project)

  # take local version of project (rather than remote)
  local_repos <- add_project_to_local_repos(project, local_repos)

  repo_deps_info <- get_deps_info(project)

  internal_dependencies <- get_internal_dependencies(
    list(repo_deps_info$current_repo), feature, direction = direction,
    local_repos = local_repos, verbose = verbose
  )

  deps <- get_internal_dependencies_graph(internal_dependencies, direction = direction)

  install_order <- get_install_order(deps[["upstream_deps"]])
  if (identical(direction, "upstream")) {
    # if installing upstream dependencies, project should appear last
    stopifnot(all.equal(utils::tail(install_order, 1)[[1]], repo_deps_info$current_repo))
  }
  if (!install_project) {
    install_order <- utils::head(install_order, -1)
  }

  if (verbose >= 1) {
    message("Installing packages in order: ", toString(extract_str_field(install_order, "repo")))
  }
  hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
  for (repo_and_host in install_order) {
    is_local <- hash_repo_and_host(repo_and_host) %in% names(hashed_repo_to_dir)
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)
    if (!dry_install) {
      install_repo_add_sha(repo_dir)
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
#' @param run_as_job (`logical`) whether to run the installation as an RStudio job
#' @inheritParams install_deps
#' @export
#' @return `shiny.app` or value returned by app (executed as a gadget)
#'
install_deps_app <- function(project = ".", default_feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             run_gadget = TRUE, run_as_job = TRUE,
                             verbose = 1) {
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
        for (repo_and_host in install_order) {
          if (hash_repo_and_host(repo_and_host) %in% selected_hashed_pkgs) {
            # the selected nodes are NOT installed
            next
          }
          is_local <- hash_repo_and_host(repo_and_host) %in% names(hashed_repo_to_dir)
          repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)

          repo_dirs_to_install <- c(repo_dirs_to_install, repo_dir)
          #install_repo_add_sha(repo_dir)
        }

        if (verbose >= 1) {
          message("Installing directories in order: ", repo_dirs_to_install)
        }
        if (run_as_job) {
          # note: this uses the currently installed version of this package because
          # it spans a new R process (not the loaded version)
          args_str <- paste(deparse(repo_dirs_to_install), collapse = "\n")
          script <- glue::glue(
            "lapply({args_str}, staged.dependencies:::install_repo_add_sha)"
          )
          run_job(script, "install_deps_app",
                  paste0("Install selection of deps of ", basename(project)))
        } else {
          lapply(repo_dirs_to_install, install_repo_add_sha)
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
#'   dependencies of the downstream dependencies
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
                             verbose = 0) {
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

  repo_deps_info <- get_deps_info(project)

  if (is.null(downstream_repos)) {
    downstream_repos <- if (!recursive) {
      get_deps_info(project)$downstream_repos
    } else {

      internal_dependencies <- get_internal_dependencies(
        list(repo_deps_info$current_repo), feature, direction = "downstream",
        local_repos = local_repos, verbose = verbose
      )

      deps <- get_internal_dependencies_graph(internal_dependencies, direction = "downstream")

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

  internal_dependencies <- get_internal_dependencies(
    list(repo_deps_info$current_repo), feature, direction = "upstream",
    local_repos = local_repos, verbose = verbose
  )

  deps <- get_internal_dependencies_graph(internal_dependencies, direction = "upstream")

  install_order <- get_install_order(deps[["upstream_deps"]])

  hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
  if (verbose >= 1) {
    message("Installing packages in order: ", toString(extract_str_field(install_order, "repo")))
  }
  for (repo_and_host in install_order) {
    is_local <- hash_repo_and_host(repo_and_host) %in% names(hashed_repo_to_dir)
    repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)
    if (hash_repo_and_host(repo_and_host) %in% lapply(downstream_repos, hash_repo_and_host)) {
      if (!dry_install_and_check) {
        if (only_tests) {
          # testthat::test_dir and devtools::test do not agree for test.nest@master
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
      install_repo_add_sha(repo_dir)
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

#' Compute the dependency structure starting from project
#'
#' @md
#'
#' @param return_table_only (`logical`) whether to return a table or (table, graph, deps)
#' @inheritParams install_deps
#' @export
#' @seealso determine_branch
#'
#' @return depending on `return_table_only` (see above); either `data.frame` or a list with
#'   the following elements:
#'
#'   `df`:
#'   `data.frame` with columns `repo, host, type, branch`, where `type` is:
#'   - `current` (project),
#'   - `upstream` (of project),
#'   - `downstream` (of project),
#'   - `other` (the remaining, e.g. downstream dependencies of upstream dependencies)
#'
#'   and `branch` is the branch corresponding to `feature` and becomes `local (branch)`
#'   if package is in `local_repos`.
#'
#'   `graph`:
#'   visNetwork graph with arrows going in the direction from downstream to upstream.
#'   Arrows are solid if both upstream and downstream list each other; they
#'   are dashed if either does not list the other:
#'   - if upstream does not list downstream, blue dashed
#'   - if downstream does not list upstream, red dashed (this means that
#'     the downstream repo's yaml needs to be fixed)
#'   Tooltips give more information about nodes and edges.
#'
#'   `deps`:
#'   deps: upstream and downstream dependencies of each node
#'
#' @importFrom dplyr `%>%` mutate select rename group_by ungroup one_of
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' dependency_structure()
#' }
dependency_structure <- function(project = ".", feature = NULL,
                                 local_repos = get_local_pkgs_from_config(),
                                 return_table_only = FALSE, verbose = 0) {
  stopifnot(
    is.data.frame(local_repos) || is.null(local_repos),
    is.logical(return_table_only)
  )
  check_dir_exists(project)
  check_verbose_arg(verbose)
  error_if_stageddeps_inexistent(project)

  feature <- infer_feature_from_branch(feature, project)

  # take local version of project (rather than remote)
  local_repos <- add_project_to_local_repos(project, local_repos)

  repo_deps_info <- get_deps_info(project)

  internal_dependencies <- get_internal_dependencies(
    list(repo_deps_info$current_repo), feature, direction = c("upstream", "downstream"),
    local_repos = local_repos, verbose = verbose
  )

  deps <- get_internal_dependencies_graph(
    internal_dependencies,
    direction = c("upstream", "downstream")
  )

  hashed_cur_repo <- hash_repo_and_host(repo_deps_info$current_repo)
  hashed_upstream_nodes <- get_descendants_distance(deps[["upstream_deps"]], hashed_cur_repo)
  hashed_downstream_nodes <- get_descendants_distance(deps[["downstream_deps"]], hashed_cur_repo)
  hashed_remaining_nodes <- setdiff(
    union(names(deps[["upstream_deps"]]), names(deps[["downstream_deps"]])),
    union(union(hashed_upstream_nodes$id, hashed_downstream_nodes$id), hashed_cur_repo)
  )

  df <- rbind(
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_cur_repo),
                 distance = 0, stringsAsFactors = FALSE), type = "current"
    ),
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_upstream_nodes$id),
                 distance = hashed_upstream_nodes$distance,
                 stringsAsFactors = FALSE), type = "upstream"
    ),
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_downstream_nodes$id),
                 distance = hashed_downstream_nodes$distance,
                 stringsAsFactors = FALSE), type = "downstream"
    ),
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_remaining_nodes),
                 stringsAsFactors = FALSE), distance = as.numeric(NA), type = "other"
    )
  )
  # add checked out branch
  hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
  df$branch <- Map(function(repo, host) {
    is_local <- hash_repo_and_host(list(repo = repo, host = host)) %in% names(hashed_repo_to_dir)
    if (is_local) {
      branch <- get_active_branch_in_cache(repo, host, local = is_local)
      paste0("local (", branch, ")")
    } else {
      # HEAD is detached, so we infer the branch name from the branch rule (we cannot handle it
      # as in the previous case since a SHA can correspond to multiple branches)
      repo_dir <- get_repo_cache_dir(repo, host)
      available_branches <- names(git2r::branches(repo_dir))
      available_branches <- setdiff(gsub("origin/", "", available_branches, fixed = TRUE), "HEAD")
      branch <- determine_branch(feature, available_branches)
      # check sha of remote branch agrees with currently checked out commit (in detached HEAD mode)
      stopifnot(
        git2r::revparse_single(repo_dir, paste0("origin/", branch))$sha ==
          git2r::repository_head(repo_dir)$sha
      )
      branch
    }
  }, df$repo, df$host)

  if (return_table_only) {
    return(df)
  }

  short_repo_name <- function(repo_name) {
    # removes owner from reponame
    vapply(strsplit(repo_name, "/", fixed = TRUE), function(x) utils::tail(x, 1), character(1))
  }

  # construct visNetwork graph
  require_pkgs(c("dplyr", "visNetwork"))
  # todo: put branch below node: https://github.com/almende/vis/issues/3436
  nodes <- df %>% mutate(
    id = hash_repo_and_host(list(repo = .data$repo, host = .data$host)),
    # label does not support html tags
    label = paste0(short_repo_name(.data$repo), "\n", .data$branch),
    title = paste0("<p>", .data$repo, "<br/>", .data$host, "<br/>", .data$type, "<br/>", .data$branch, "</p"),
    value = 3,
    group = .data$type
  ) %>% select(c("id", "label", "title", "value", "group"))

  edges <- rbind(
    cbind_handle_empty(
      adj_list_to_edge_df(deps[["upstream_deps"]]),
      arrows = "to", listed_by = "from"
    ),
    cbind_handle_empty(
      adj_list_to_edge_df(deps[["downstream_deps"]]) %>% rename(to = .data$from, from = .data$to),
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
    from <- short_repo_name(unhash_repo_and_host(from[[1]])$repo)
    to <- short_repo_name(unhash_repo_and_host(to[[1]])$repo)
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
  edges <- edges %>% group_by(.data$from, .data$to) %>%
    mutate(color = get_edge_color(.data$listed_by)) %>%
    mutate(title = get_edge_tooltip(.data$from, .data$to, .data$listed_by)) %>%
    mutate(dashes = !setequal(.data$listed_by, c("from", "to"))) %>%
    ungroup() %>%
    select(-one_of("listed_by"))

  plot_title <- paste0("Dependency graph starting from ", repo_deps_info$current_repo$repo)
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
    ); graph

  list(df = df, graph = graph, deps = deps)
}

#' Get the dependency structure as a table
#'
#' The dependency structure depends on `feature` which determines which
#' repositories are checked out.
#' It starts from the project and discovers all reachable upstream and
#' downstream dependencies.
#'
#' @md
#' @inheritParams dependency_structure
#' @inherit dependency_structure return
#'
#' @export
#' @examples
#' \dontrun{
#' dependency_table()
#' }
dependency_table <- function(project = ".", feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             verbose = 0) {
  dependency_structure(
    project = project, feature = feature, local_repos = local_repos, return_table_only = TRUE, verbose = verbose
  )
}

#' Plots the dependency graph
#'
#' The dependency structure depends on `feature` which determines which
#' repositories are checked out.
#' It starts from the project and discovers all reachable upstream and
#' downstream dependencies.
#'
#' @md
#' @inheritParams dependency_structure
#' @inherit dependency_structure return
#'
#' @export
#' @examples
#' \dontrun{
#' dependency_graph()
#' }
dependency_graph <- function(project = ".", feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             verbose = 0) {
  dependency_structure(
    project = project, feature = feature, local_repos = local_repos, return_table_only = FALSE, verbose = verbose
  )$graph
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


# Given a list of packages (and their directories in the cache) of the form
# list(`repo @ host`=<<path>>) create a dependency graph using the R
# description files.
get_internal_dependencies_graph <- function(internal_dependencies,
                                            direction = "upstream") {

  # get the package names from the DESCRIPTION files
  # (they may not be the same name as the repo name)
  package_names <- vapply(internal_dependencies,
    function(repo_dir) {
      fdesc <- file.path(repo_dir, "DESCRIPTION")
      dsc <- desc::desc(fdesc)
      return(unname(dsc$get("Package")))
    },
    FUN.VALUE = character(1)
  )

  # add the hashed_repo_and_host as the names so there
  # is a mapping between
  names(package_names) <- names(internal_dependencies)

  # get the imports, suggests, depends for each package
  # from the package description files, filter for only
  # the internal packages and name in form hashed_repo_and_host
  deps <- lapply(internal_dependencies,
    function(repo_dir) {
      fdesc <- file.path(repo_dir, "DESCRIPTION")
      dsc <- desc::desc(fdesc)
      names(package_names[package_names %in% dsc$get_deps()$package])
    }
  )

  names(deps) <- names(internal_dependencies)

  res <- list()
  if ("upstream" %in% direction) {
    res[["upstream_deps"]] <- deps
  }
  if ("downstream" %in% direction) {
    downstream_deps <- list()
    for(x in names(deps)){
      downstream_deps[[x]] <- character(0)
      for(y in names(deps)){
        if(x %in% deps[[y]]){
          downstream_deps[[x]] <- c(downstream_deps[[x]], y)
        }
      }
    }
    res[["downstream_deps"]] <- downstream_deps
  }

  res

}



#' Update existing stage_dependencies yaml file
#'
#' Using the existing stage_dependencies yaml file
#' 'graph' to define internal dependencies, update the
#' project yaml file to include to include all direct
#' (i.e. distance 1) upstream and downstream repos
#' @inheritParams dependency_structure
#' @export
update_stageddeps_yaml_with_direct_deps <- function(project = ".", feature = NULL,
                                                    local_repos = get_local_pkgs_from_config(),
                                                    verbose = 0){
  dep_table <- dependency_structure(
    project = project,
    feature = feature,
    local_repos = local_repos,
    return_table_only = TRUE,
    verbose = verbose
  )

  #TODO tidy this add error handling, dry run etc.
  current_repo <- list("repo" = dep_table[dep_table$type == "current",]$repo,
                       "host" = dep_table[dep_table$type == "current",]$host)

  upstream_repos <- dep_table[dep_table$type == "upstream" & dep_table$distance == 1, c("repo", "host")]
  upstream_repos <- stats::setNames(apply(upstream_repos, 1, FUN = function(row) row = list(repo=row["repo"], host = row["host"])), NULL)

  downstream_repos <- dep_table[dep_table$type == "downstream" & dep_table$distance == 1, c("repo", "host")]
  downstream_repos <- stats::setNames(apply(downstream_repos, 1, FUN = function(row) row = list(repo=row["repo"], host = row["host"])), NULL)

  yaml::write_yaml(
    list(current_repo = current_repo,
      upstream_repos = upstream_repos,
      downstream_repos = downstream_repos), file = file.path(project, STAGEDDEPS_FILENAME))
}


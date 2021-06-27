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
#' @inheritParams rec_checkout_repos
#'
#' @return installed packages in installation order
#'
#' @export
#' @seealso determine_branch
#'
#' @examples
#' \dontrun{
#' install_deps()
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

  repo_deps_info <- get_deps_info(project)

  # take local version of project (rather than remote)
  local_repos <- rbind(
    local_repos,
    data.frame(
      repo = repo_deps_info$current_repo$repo,
      host = repo_deps_info$current_repo$host,
      directory = normalizePath(project), stringsAsFactors = FALSE
    )
  )

  deps <- rec_checkout_repos(
    list(repo_deps_info$current_repo), feature, direction = direction,
    local_repos = local_repos, verbose = verbose
  )

  install_order <- get_install_order(deps[["upstream_deps"]])
  stopifnot(all.equal(utils::tail(install_order, 1)[[1]], repo_deps_info$current_repo))
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
#' @param run_gadget whether to run the app as a gadget
#' @inheritParams install_deps
#' @export
#' @return `shiny.app` or value returned by app (executed as a gadget)
#'
install_deps_app <- function(project = ".", feature = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             run_gadget = TRUE,
                             verbose = 1) {
  require_pkgs(c("shiny", "miniUI"))

  app <- shiny::shinyApp(
    ui = function() {
      miniUI::miniPage(
        shiny::fillRow(
          shiny::textInput("feature", label = "Feature: ", value = feature),
          shiny::actionButton("compute_graph", "Compute graph")
        ),
        visNetwork::visNetworkOutput("network_proxy_nodes", height = "400px"),
        miniUI::gadgetTitleBar(
          "Cmd + Click node to not install the node",
          right = miniUI::miniTitleBarButton("done", "Install", primary = TRUE)
        ),
        shiny::tags$p("The following packages will be installed:"),
        shiny::verbatimTextOutput("nodesToInstall")
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
        dependency_structure("../stageddeps.electricity/", feature = feature,
                             local_repos = local_repos, verbose = 2)
      })

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

        hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
        for (repo_and_host in install_order) {
          if (hash_repo_and_host(repo_and_host) %in% selected_hashed_pkgs) {
            # the selected nodes are NOT installed
            next
          }
          is_local <- hash_repo_and_host(repo_and_host) %in% names(hashed_repo_to_dir)
          repo_dir <- get_repo_cache_dir(repo_and_host$repo, repo_and_host$host, local = is_local)

          install_repo_add_sha(repo_dir)
        }

        invisible(shiny::stopApp())
      })
    }
  ); app
  if (run_gadget) {
    shiny::runGadget(app, viewer = shiny::dialogViewer("Install packages"))
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
check_downstream <- function(project = ".", feature = NULL, downstream_repos = list(),
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
          testthat::test_dir(file.path(repo_dir, "tests"), stop_on_failure = TRUE, stop_on_warning = TRUE)
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

  repo_deps_info <- get_deps_info(project)

  local_repos <- rbind(
    local_repos,
    data.frame(
      repo = repo_deps_info$current_repo$repo,
      host = repo_deps_info$current_repo$host,
      directory = normalizePath(project), stringsAsFactors = FALSE
    )
  )

  deps <- rec_checkout_repos(
    list(repo_deps_info$current_repo), feature, direction = c("upstream", "downstream"),
    local_repos = local_repos, verbose = verbose
  )
  hashed_cur_repo <- hash_repo_and_host(repo_deps_info$current_repo)
  hashed_upstream_nodes <- get_descendants(deps[["upstream_deps"]], hashed_cur_repo)
  hashed_downstream_nodes <- get_descendants(deps[["downstream_deps"]], hashed_cur_repo)
  hashed_remaining_nodes <- setdiff(
    union(names(deps[["upstream_deps"]]), names(deps[["downstream_deps"]])),
    union(union(hashed_upstream_nodes, hashed_downstream_nodes), hashed_cur_repo)
  )

  df <- rbind(
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_cur_repo), stringsAsFactors = FALSE), type = "current"
    ),
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_upstream_nodes), stringsAsFactors = FALSE), type = "upstream"
    ),
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_downstream_nodes), stringsAsFactors = FALSE), type = "downstream"
    ),
    cbind_handle_empty(
      data.frame(unhash_repo_and_host(hashed_remaining_nodes), stringsAsFactors = FALSE), type = "other"
    )
  )
  # add checked out branch
  hashed_repo_to_dir <- get_hashed_repo_to_dir_mapping(local_repos)
  df$branch <- Map(function(repo, host) {
    is_local <- hash_repo_and_host(list(repo = repo, host = host)) %in% names(hashed_repo_to_dir)
    branch <- get_active_branch_in_cache(repo, host, local = is_local)
    if (is_local) {
      paste0("local (", branch, ")")
    } else {
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
    label = short_repo_name(.data$repo),
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
      list(NULL)
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
    )

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





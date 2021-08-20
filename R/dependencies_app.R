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

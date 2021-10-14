#' Gadget or Shiny app to select the dependencies to install
#'
#' The dependencies are obtained by traversing the upstream and downstream repositories
#' in the package's staged dependencies yaml files starting from `project`.
#'
#' @md
#' @param project (`character`) Should be of the format `"<<repo>>@<<host>>"`, for example
#'   `"openpharma/stageddeps.water@https://github.com"`, if `host` is not included then it
#'   is assumed to be `"https://github.com"`.
#' @param default_ref (`character`) default ref (branch/tag), see also the parameter
#'   `ref` of `\link{dependency_table}`
#' @param local_repos (`data.frame`) repositories that should be taken from
#'   local rather than cloned; columns are `repo, host, directory`
#' @param run_gadget (`logical`) whether to run the app as a gadget
#' @param run_as_job (`logical`) whether to run the installation as an RStudio job.
#' @inheritParams install_deps
#' @export
#' @return `shiny.app` or value returned by app (executed as a gadget)
#' @examples
#' \dontrun{
#'   install_deps_app("openpharma/stageddeps.food@https://github.com", default_ref = "main")
#' }
install_deps_app <- function(project, default_ref = NULL,
                             local_repos = get_local_pkgs_from_config(),
                             run_gadget = TRUE, run_as_job = TRUE,
                             verbose = 1, install_external_deps = TRUE, ...) {
  require_pkgs(c("shiny", "miniUI", "visNetwork"))

  app <- shiny::shinyApp(
    ui = function() {
      miniUI::miniPage(
        shiny::fillCol(
          shiny::tagList(
            shiny::textInput("ref", label = "Ref: ", value = default_ref),
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
        message_if_verbose("Computing dependency structure for ref ",
                           input$ref, " starting from project ", project,
                           verbose = verbose)

        dependency_table(project, project_type = "repo@host", ref = input$ref,
                         local_repos = local_repos, verbose = 2)
      },
      # do not ignore NULL to also compute initially with the default feature when
      # the button was not yet clicked
      ignoreNULL = FALSE)

      output$network_proxy_nodes <- visNetwork::renderVisNetwork({
        compute_dep_structure() %>%
          plot() %>%
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
          compute_dep_structure()$table$package_name,
          input$network_proxy_nodes_selectedNodes
        ), collapse = "\n")
      })
      shiny::observeEvent(input$done, {

        dependency_packages <- setdiff(
          compute_dep_structure()$table$package_name,
          input$network_proxy_nodes_selectedNodes
        )

        if (run_as_job) {
          # note: this uses the currently installed version of this package because
          # it spans a new R process (not the loaded version) - also note for now we
          # recreate the dep_structure object (which should be a fraction of the install time)
          # this could be changed by using the importEnv argument
          # to jobRunScript and creating an install_deps job if dep_structure already exists in env
          install_deps_job(project = project,  project_type = "repo@host", verbose = verbose,
                           create_args = list(local_repos = local_repos, ref = input$ref),
                           dependency_packages = dependency_packages,
                           install_external_deps = TRUE,
                           install_direction = c("upstream", "downstream"),
                           ...)
        } else{
          install_deps(compute_dep_structure(),
                       dependency_packages = dependency_packages,
                       verbose = verbose,
                       install_external_deps = TRUE,
                       install_direction = c("upstream", "downstream"),
                       ...)
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

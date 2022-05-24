#' Gadget or Shiny app to select the dependencies to install
#'
#' The dependencies are obtained by traversing the upstream and downstream repositories
#' in the package's staged dependencies yaml files starting from `project`.
#'
#' @md
#'
#' @param default_repo (`character`) the repository name for the dependency graph to be
#'   created for, for example, `"openpharma/stageddeps.water"`.
#'   If `NULL` this must be entered by app user and can always be changed by the user.
#' @param default_host (`character`) the host for the repository for the dependency graph to
#'   be created for by default `"https://github.com"`. If `NULL` this must be entered by app
#'   user and can always be changed by the user.
#' @param default_ref (`character`) default ref (branch/tag), see also the parameter
#'   `ref` of `\link{dependency_table}`. If `NULL` this must be entered by app user
#'   and can always be changed by the user.
#' @param renv_profile (`character`) the name of the `renv` profile of the `renv.lock` files
#'   to be included from the repos. The standard `renv.lock` file uses the default `NULL` argument here.
#' @param fallback_branch (`character`) the default branch to try to use if
#'   no other matches found
#' @param run_gadget (`logical`) whether to run the app as a gadget
#' @param run_as_job (`logical`) whether to run the installation as an RStudio job.
#' @inheritParams install_deps
#' @export
#' @return `shiny.app` or value returned by app (executed as a gadget)
#' @examples
#' \dontrun{
#'   install_deps_app("openpharma/stageddeps.food")
#' }
install_deps_app <- function(default_repo = NULL,
                             default_host = "https://github.com",
                             default_ref = "main",
                             fallback_branch = "main",
                             run_gadget = TRUE, run_as_job = TRUE,
                             verbose = 1, install_external_deps = TRUE,
                             renv_profile = NULL,
                             upgrade = "never", ...) {
  require_pkgs(c("shiny", "miniUI", "visNetwork"))

  app <- shiny::shinyApp(
    ui = function() {
      miniUI::miniPage(
        shiny::fillCol(
          miniUI::miniContentPanel(
            shiny::fillRow(
              shiny::tagList(
                shiny::textInput("ref", label = "Ref:", value = default_ref),
                shiny::textInput("repo", label = "Repo:", value = default_repo)
              ),
              shiny::tagList(
                shiny::br(),
                shiny::actionButton("compute_graph", "Compute graph"),
                shiny::br(),
                shiny::textInput("host", label = "Host:", value = default_host)
              )
            )
          ),
          miniUI::miniContentPanel(
            shiny::verbatimTextOutput("error_output"),
            visNetwork::visNetworkOutput("network_proxy_nodes", height = "400px")
          ),
          miniUI::miniContentPanel(
            shiny::tags$p("The following packages will be installed:"),
            shiny::verbatimTextOutput("nodesToInstall")
          ),
          flex = c(1, 3, 1)
        ),
        miniUI::gadgetTitleBar(
          "Cmd + Click node to not install the node",
          right = miniUI::miniTitleBarButton("done", "Install", primary = TRUE)
        )
      )
    },
    server = function(input, output, session) {

      dep_table_rv <- shiny::reactiveVal()
      error_rv <- shiny::reactiveVal()

      shiny::observeEvent(input$compute_graph, {

        dep_table_rv(NULL)
        error_rv(NULL)

        message_if_verbose("Computing dependency structure for ref ",
                           input$ref, " starting from project ", paste(input$repo, input$host, sep = "@"),
                           verbose = verbose)

        x <- tryCatch({
          if(is.null(input$ref) || input$ref == "" || is.null(input$repo) || input$repo == "" ||
             is.null(input$host) || input$host == "") {
            stop("Please enter a repo, host and ref and \nthen press 'Compute graph'")
          }
          dependency_table(project = paste(input$repo, input$host, sep = "@"),
                           project_type = "repo@host", ref = input$ref, fallback_branch = fallback_branch,
                           local_repos = NULL, renv_profile = renv_profile, verbose = 2)},
          error = function(cond){
            error_rv(paste0("Cannot create dependency graph:\n", cond$message))
            NULL
          }
        )

        if (!is.null(x)) {
          dep_table_rv(x)
        }

      },
      # do not ignore NULL to also compute initially with the default feature when
      # the button was not yet clicked
      ignoreNULL = FALSE)

      output$error_output <- shiny::renderText({
        shiny::req(error_rv())
        error_rv()
      })

      output$network_proxy_nodes <- visNetwork::renderVisNetwork({
        shiny::req(dep_table_rv())
        dep_table_rv() %>%
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
        shiny::req(dep_table_rv())
        paste(setdiff(
          dep_table_rv()$table$package_name[dep_table_rv()$table$installable],
          input$network_proxy_nodes_selectedNodes
        ), collapse = "\n")
      })
      shiny::observeEvent(input$done, {

        if (!is.null(dep_table_rv())) {


          dependency_packages <- setdiff(
            dep_table_rv()$table$package_name[dep_table_rv()$table$installable],
            input$network_proxy_nodes_selectedNodes
          )

          if (run_as_job) {
            # note: this uses the currently installed version of this package because
            # it spans a new R process (not the loaded version) - also note for now we
            # recreate the dep_structure object (which should be a fraction of the install time)
            # this could be changed by using the importEnv argument
            # to jobRunScript and creating an install_deps job if dep_structure already exists in env
            install_deps_job(project = paste(input$repo, input$host, sep = "@"),  project_type = "repo@host", verbose = verbose,
                             create_args = list(local_repos = NULL, ref = input$ref, renv_profile = renv_profile),
                             dependency_packages = dependency_packages, fallback_branch = fallback_branch,
                             install_external_deps = install_external_deps,
                             install_direction = "all",
                             upgrade = upgrade,
                             ...)
          } else{
            install_deps(dep_table_rv(),
                         dependency_packages = dependency_packages,
                         verbose = verbose,
                         install_external_deps = install_external_deps,
                         install_direction = "all",
                         upgrade = upgrade,
                         ...)
          }


          # calling it at the top of this reactive still finishes executing
          # the reactive (so installs), so we call it down here
          invisible(shiny::stopApp())
        }
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

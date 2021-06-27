# Helper functions for addins since the addin functions are invoked without arguments

install_deps_app_addin <- function() {
  install_deps_app(verbose = 1)
}

check_downstream_addin <- function() {
  check_downstream(verbose = 1, check_args = Sys.getenv("RCMDCHECK_ARGS"))
}

test_downstream_addin <- function() {
  check_downstream(verbose = 1, only_tests = TRUE)
}

install_deps_addin <- function() {
  install_deps(verbose = 1)
}

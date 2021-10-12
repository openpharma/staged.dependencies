# get_true_deps_graph ----
test_that("get_true_deps_graph works", {
  # take out "stageddeps.water", so it will be treated like an external
  # dependency (i.e. not appear in the dep graph)
  pkgs_df <- data.frame(
    package_name = c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food",
                     "stageddeps.house", "stageddeps.garden"),
    dummy_col = 11:15,
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(cache_dir = file.path(TESTS_GIT_REPOS, package_name))

  # check that the up- and downstream graphs and external deps are as expected by DESCRIPTION files
  expect_equal(
    get_true_deps_graph(pkgs_df, c("upstream", "downstream")),
    list(
      external = list(
        stageddeps.elecinfra = data.frame(type = "Suggests", package = "testthat", version = ">= 2.1.0"),
        stageddeps.electricity = data.frame(type = "Suggests", package = "testthat", version = ">= 2.1.0"),
        stageddeps.food = data.frame(type = c("Imports", "Imports", "Suggests"),
                                     package = c("desc", "SummarizedExperiment", "testthat"),
                                     version = c("*", "*", ">= 2.1.0")),
        stageddeps.house = data.frame(type = c("Imports", "Suggests"),
                                      package = c("stageddeps.water", "testthat"), # water treated as external, see above
                                      version = c("*", ">= 2.1.0")),
        stageddeps.garden = data.frame(type = c("Imports", "Suggests"),
                                       package = c("stageddeps.water", "testthat"),
                                       version = c("*", ">= 2.1.0"))
      ),
      upstream_deps = list(
        stageddeps.elecinfra = character(0),
        stageddeps.electricity = "stageddeps.elecinfra",
        stageddeps.food = "stageddeps.electricity",
        stageddeps.house = c("stageddeps.electricity", "stageddeps.food"),
        stageddeps.garden = character(0)
      ),
      downstream_deps = list(
        stageddeps.garden = c(),
        stageddeps.house = c(),
        stageddeps.food = c("stageddeps.house"),
        stageddeps.electricity = c("stageddeps.food", "stageddeps.house"),
        stageddeps.elecinfra = c("stageddeps.electricity")
      )
    )
  )

})

# get_local_pkgs_from_config ----
test_that("get_local_pkgs_from_config works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)

  with_tmp_cachedir({
    config_file <- file.path(get_storage_dir(), CONFIG_FILENAME)
    expect_null(get_local_pkgs_from_config())
    cat(c(
      "local_packages:",
      "- repo: openpharma/stageddeps.food",
      "  host: https://github.com",
      paste0("  directory: \"", fs::path_abs(repo_dir), "\"")
    ), sep = "\n", file = config_file)

    # check local_pkgs is read correctly from the yaml
    expect_equal(get_local_pkgs_from_config(), data.frame(
      repo = "openpharma/stageddeps.food", host = "https://github.com",
      directory = normalize_path(repo_dir), stringsAsFactors = FALSE
    ))
  })

  unlink(repo_dir, recursive = TRUE)
})

test_that("add_project_to_local_repos works", {
  # check that project is added to yaml
  project <- file.path(TESTS_GIT_REPOS, "stageddeps.food")
  expect_equal(
    add_project_to_local_repos(project, local_repos = NULL),
    data.frame(repo = "openpharma/stageddeps.food", host = "https://github.com",
               directory = normalize_path(project),
               stringsAsFactors = FALSE)
  )

  expect_equal(
    add_project_to_local_repos(
      project,
      local_repos = data.frame(repo = "openpharma/stageddeps.electricity", host = "https://github.com",
                 directory = normalize_path(file.path(TESTS_GIT_REPOS, "stageddeps.electricity")), stringsAsFactors = FALSE)
    ),
    data.frame(repo = c("openpharma/stageddeps.electricity", "openpharma/stageddeps.food"),
               host = c("https://github.com", "https://github.com"),
               directory = c(
                 normalize_path(file.path(TESTS_GIT_REPOS, "stageddeps.electricity")),
                 normalize_path(project)
               ),
               stringsAsFactors = FALSE)
  )
})

# yaml_from_dep_table ----
test_that("yaml_from_dep_table works", {
  # check that correct yaml is generated from dep table
  expect_equal(
    yaml_from_dep_table(data.frame(
      repo = c("U1", "U2", "U3", "U4", "D1", "D2", "O1", "C"),
      host = rep("host", 8),
      type = c("upstream", "upstream", "upstream", "upstream", "downstream", "downstream", "other", "current"),
      distance = c(1, 1, 2, 1, 3, 1, NA, 0),
      stringsAsFactors = FALSE
    )),
    list(
      current_repo = list(repo = "C", host = "host"),
      upstream_repos = list(
        U1 = list(repo = "U1", host = "host"),
        U2 = list(repo = "U2", host = "host"),
        U4 = list(repo = "U4", host = "host")
      ),
      downstream_repos = list(
        D2 = list(repo = "D2", host = "host")
      )
    )
  )
})


test_that("parse_remote_project works", {

  # repo@host
  expect_equal(parse_remote_project("x@y"),
               list(repo = "x", host = "y"))


  # missing host uses default
  expect_equal(parse_remote_project("x"),
               list(repo = "x", host = "https://github.com"))

  # more than 1 @ throws error
  expect_error(parse_remote_project("x@y@z"))

  # empty input or only whitespace repo/host throws error
  expect_error(parse_remote_project(""))
  expect_error(parse_remote_project("@"))
  expect_error(parse_remote_project(" @ "))
})

# run_package_actions ----
test_that("run_package_actions works", {
  mockery::stub(run_package_actions, 'install_repo_add_sha', function(cache_dir, ...) {
    cat(paste0("Mocking install_repo_add_sha for ", cache_dir, "\n"))
  })

  # check that install is called twice
  output <- capture.output(
    run_package_actions(
      data.frame(
        cache_dir = file.path(TESTS_GIT_REPOS, c("stageddeps.elecinfra", "stageddeps.electricity")),
        actions = c("install", "install"),
        sha = c(get_short_sha(file.path(TESTS_GIT_REPOS, c("stageddeps.elecinfra"))),
                get_short_sha(file.path(TESTS_GIT_REPOS, c("stageddeps.electricity")))),
        installable = c(TRUE, TRUE),
        stringsAsFactors = FALSE
      ),
      install_external_deps = FALSE
    ) %>% invisible()
  )
  expect_equal(
    output,
    c(
      paste0("Mocking install_repo_add_sha for ", file.path(TESTS_GIT_REPOS, "stageddeps.elecinfra")),
      paste0("Mocking install_repo_add_sha for ", file.path(TESTS_GIT_REPOS, "stageddeps.electricity"))

    )
  )

  #invalid sha
  expect_error(
    run_package_actions(
      data.frame(
        cache_dir = file.path(TESTS_GIT_REPOS, c("stageddeps.elecinfra", "stageddeps.electricity")),
        actions = c("install", "install"),
        sha = c("xxx","yyy"),
        installable = c(TRUE, TRUE),
        stringsAsFactors = FALSE
      ),
      install_external_deps = FALSE
    )

  )



})

# parse_deps_table ----
test_that("parsae_deps_table works as expected", {

  # empty/NA returns character(0)
  expect_length(parse_deps_table(""), 0)
  expect_length(parse_deps_table(NA), 0)
  expect_is(parse_deps_table(""), "character")
  expect_is(parse_deps_table(NA), "character")

  # R is removed
  expect_equal(parse_deps_table("R (>=3.3), utils"), "utils")

  # version numbers removed
  expect_equal(parse_deps_table("utils, survival (>= 1.17)"), c("utils", "survival"))

  # can handle newline/whitespace chars
  expect_equal(parse_deps_table("utils,\nsurvival (>= 1.17)"), c("utils", "survival"))
  expect_equal(parse_deps_table("utils  ,\t  survival  (>= 1.17)"), c("utils", "survival"))
})

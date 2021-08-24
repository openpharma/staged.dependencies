test_that("get_true_deps_graph works", {
  # take out "stageddeps.water", so it will be treated like an external
  # dependency (i.e. not appear in the dep graph)
  pkgs_df <- data.frame(
    package_name = c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food",
                     "stageddeps.house", "stageddeps.garden"),
    dummy_col = 11:15,
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(cache_dir = file.path(TESTS_GIT_REPOS, package_name))

  # check that the up- and downstream graphs are as expected by DESCRIPTION files
  expect_equal(
    get_true_deps_graph(pkgs_df, c("upstream", "downstream")),
    list(
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

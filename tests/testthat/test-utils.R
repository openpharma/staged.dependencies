test_that("cbind_handle_empty works", {
  expect_equal(
    cbind_handle_empty(data.frame(col1 = character(0), col2 = character(0), stringsAsFactors = FALSE),
                       col3 = "hello3", col4 = "hello4"),
    data.frame(col1 = character(0), col2 = character(0), col3 = character(0), col4 = character(0),
               stringsAsFactors = FALSE)
  )
  expect_equal(
    cbind_handle_empty(data.frame(col1 = c("h1", "h11"), col2 = c("h2", "h22"), stringsAsFactors = FALSE),
                       col3 = "hello3", col4 = "hello4"),
    data.frame(col1 = c("h1", "h11"), col2 = c("h2", "h22"), col3 = c("hello3", "hello3"), col4 = c("hello4", "hello4"),
               stringsAsFactors = FALSE)
  )
})

test_that("hash_repo_and_host works", {
  expect_null(hash_repo_and_host(list()))
  expect_equal(
    hash_repo_and_host(list(repo = "repo1", host = "host1")),
    "repo1 @ host1"
  )
  expect_equal(
    hash_repo_and_host(list(repo = c("repo1", "repo2"), host = c("host1", "host2"))),
    c("repo1 @ host1", "repo2 @ host2")
  )
})

test_that("unhash_repo_and_host works", {
  expect_equal(unhash_repo_and_host(character(0)), list(repo = character(0), host = character(0)))
  expect_equal(unhash_repo_and_host("repo1 @ host1"), list(repo = "repo1", host = "host1"))
  expect_equal(
    unhash_repo_and_host(c("repo1 @ host1", "repo2 @ host2")),
    list(repo = c("repo1", "repo2"), host = c("host1", "host2"))
  )
})

test_that("get_pkg_names_from_paths works", {
  expect_equal(
    get_pkg_names_from_paths(file.path(TESTS_GIT_REPOS, c("stageddeps.food", "stageddeps.house"))),
    c("stageddeps.food", "stageddeps.house")
  )
})

test_that("get_yaml_deps_info works", {
  expect_setequal(
    names(get_yaml_deps_info(file.path(TESTS_GIT_REPOS, "stageddeps.food"))),
    c("upstream_repos", "downstream_repos", "current_repo")
  )
})

test_that("error_if_stageddeps_inexistent works", {
  error_if_stageddeps_inexistent(file.path(TESTS_GIT_REPOS, "stageddeps.food"))

  dummy_dir <- tempfile("dummy")
  on.exit(unlink(dummy_dir, recursive = TRUE), add = TRUE)
  dir.create(dummy_dir)
  expect_error(
    error_if_stageddeps_inexistent(dummy_dir),
    regexp = "staged_dependencies.yaml does not exist", fixed = TRUE
  )
})

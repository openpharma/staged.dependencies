test_that("cbind_handle_empty works", {
  # when second df is empty
  expect_equal(
    cbind_handle_empty(data.frame(col1 = character(0), col2 = character(0), stringsAsFactors = FALSE),
      col3 = "hello3", col4 = "hello4"
    ),
    data.frame(
      col1 = character(0), col2 = character(0), col3 = character(0), col4 = character(0),
      stringsAsFactors = FALSE
    )
  )

  # when second df is non-empty
  expect_equal(
    cbind_handle_empty(data.frame(col1 = c("h1", "h11"), col2 = c("h2", "h22"), stringsAsFactors = FALSE),
      col3 = "hello3", col4 = "hello4"
    ),
    data.frame(
      col1 = c("h1", "h11"), col2 = c("h2", "h22"), col3 = c("hello3", "hello3"), col4 = c("hello4", "hello4"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("hash_repo_and_host works", {
  # null when list is empty
  expect_null(hash_repo_and_host(list()))
  # when lists contains one element
  expect_equal(
    hash_repo_and_host(list(repo = "repo1", host = "host1", subdir = "test1")),
    "repo1 @ host1 @ test1"
  )
  # when lists contain two elements
  expect_equal(
    hash_repo_and_host(list(repo = c("repo1", "repo2"), host = c("host1", "host2"), subdir = c("test1", "test2"))),
    c("repo1 @ host1 @ test1", "repo2 @ host2 @ test2")
  )
})

test_that("unhash_repo_and_host works", {
  # when empty
  expect_equal(unhash_repo_and_host(character(0)), list(repo = character(0), host = character(0), subdir = character(0)))
  # when of size 1
  expect_equal(unhash_repo_and_host("repo1 @ host1 @ test1"), list(repo = "repo1", host = "host1", subdir = "test1"))
  # when of size 2
  expect_equal(
    unhash_repo_and_host(c("repo1 @ host1 @ test1", "repo2 @ host2 @ test2")),
    list(repo = c("repo1", "repo2"), host = c("host1", "host2"), subdir = c("test1", "test2"))
  )
})

test_that("get_pkg_names_from_paths works", {
  # check that package names correctly inferred
  expect_equal(
    get_pkg_names_from_paths(file.path(TESTS_GIT_REPOS, c("stageddeps.food", "stageddeps.house"))),
    c("stageddeps.food", "stageddeps.house")
  )
})

test_that("get_yaml_deps_info works", {
  # check that returned yaml has correct fields
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

test_that("rep_with_names works", {
  expect_equal(rep_with_names("hh", c("aa", "bb")), c(aa = "hh", bb = "hh"))
  expect_equal(rep_with_names(list("hh"), c("aa", "bb")), list(aa = "hh", bb = "hh"))
  expect_equal(rep_with_names("hh", NULL), character(0))
})

test_that("setting verbosity works", {
  # These should be part of the definition of <<-
  f <- function() {
    return(verbose_level_staged.deps)
  }
  f2 <- function() return(f())
  expect_error(f())
  expect_error(f2())
  lev <- 2

  # Assignment
  verbose_staged.dependencies_set(level = lev)
  expect_identical(f(), lev)
  expect_identical(f2(), lev)
  expect_identical(verbose_level_staged.deps, lev)

  # Removal
  verbose_staged.dependencies_rm()
  expect_error(f2())
  expect_error(verbose_staged.dependencies_rm())
})

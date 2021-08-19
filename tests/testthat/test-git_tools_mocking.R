# The difference to test-git_tools.R is that this file copies a set of git repos
# into a directory. The git repos can then be used to mock git clone and git fetch.
# Does not require access to GitHub.

# directory in 'tests' directory which contains git folders as _git
tests_ecosystem_dir <- system.file("tests/testthat/test_ecosystem", package = "staged.dependencies")
# directory to which it is copied to be used by tests (with .git instead of _git folders)
ecosystem_dir <- tempfile("test_ecosystem")
# ecosystem_dir <- "../scratch1/test_ecosystem"

# ---- setup ----
setup({
  # # to prepare tests_ecosystem_dir, see below
  # unlink(tests_ecosystem_dir, recursive = TRUE)
  # dir.create(tests_ecosystem_dir)
  #
  # checkout_repo(file.path(tests_ecosystem_dir, "stageddeps.elecinfra"),
  #               "https://github.com/openpharma/stageddeps.elecinfra.git",
  #               function(...) "main", token_envvar = NULL)
  # checkout_repo(file.path(tests_ecosystem_dir, "stageddeps.electricity"),
  #               "https://github.com/openpharma/stageddeps.electricity.git",
  #               function(...) "main", token_envvar = NULL)
  # checkout_repo(file.path(tests_ecosystem_dir, "stageddeps.food"),
  #               "https://github.com/openpharma/stageddeps.food.git",
  #               function(...) "main", token_envvar = NULL)
  # checkout_repo(file.path(tests_ecosystem_dir, "stageddeps.garden"),
  #               "https://github.com/openpharma/stageddeps.garden.git",
  #               function(...) "main", token_envvar = NULL)
  # checkout_repo(file.path(tests_ecosystem_dir, "stageddeps.house"),
  #               "https://github.com/openpharma/stageddeps.house.git",
  #               function(...) "main", token_envvar = NULL)
  # checkout_repo(file.path(tests_ecosystem_dir, "stageddeps.water"),
  #               "https://github.com/openpharma/stageddeps.water.git",
  #               function(...) "main", token_envvar = NULL)
  # for (dir in list.dirs(tests_ecosystem_dir, full.names = TRUE, recursive = FALSE)) {
  #   fs::file_move(file.path(dir, ".git"), file.path(dir, "_git"))
  # }

  # the directory `tests_ecosystem_dir` contains git repositories that are used
  # for testing. Since this package is itself a git repo, we rename the `.git`
  # folders inside each directory of `tests_ecosystem_dir` into `_git`.
  # This is then copied to `ecosystem_dir` and the `_git` directories
  # are renamed to `.git`.
  copy_ecosystem_dir <- function(ecosystem_dir, tests_ecosystem_dir) {
    unlink(ecosystem_dir, recursive = TRUE)
    fs::dir_copy(tests_ecosystem_dir, ecosystem_dir)
    for (dir in list.dirs(ecosystem_dir, full.names = TRUE, recursive = FALSE)) {
      fs::file_move(file.path(dir, "_git"), file.path(dir, ".git"))
    }
  }

  copy_ecosystem_dir(ecosystem_dir, tests_ecosystem_dir)
})

# ---- checkout_repo ----
test_that("checkout_repo with mocking works", {
  # mock git clone

  # delete mock function with rm(list = c("checkout_repo"))
  mockery::stub(checkout_repo, 'git2r::clone', function(url, local_path, ...) {
    print("Mocking git2r::clone")
    existing_repo_dir <- file.path(ecosystem_dir, basename(local_path))
    stopifnot(dir.exists(existing_repo_dir))
    fs::dir_copy(existing_repo_dir, local_path)
    # assuming "main" branch exists
    git2r::checkout(local_path, branch = "main", force = TRUE)
    git2r::repository(local_path)
  })

  mockery::stub(checkout_repo, 'git2r::fetch', function(url, local_path, ...) {
    print("Mocking git2r::fetch")
    invisible(NULL)
  })

  repo_dir <- file.path(tempfile(), "stageddeps.food")
  # we use "REPLACED" to make sure it really uses the mocked function and
  # does not try to download from the URL; ideally, we would cut the internet
  # for this test (how)
  expect_output(
    checkout_repo(repo_dir,
                  "REPLACED/stageddeps.food.git",
                  function(...) "unittest_branch1", token_envvar = NULL),
    regexp = "Mocking git2r::clone", fixed = TRUE
  )

  expect_output(
    expect_error(
      checkout_repo(repo_dir,
                    "REPLACED/stageddeps.food.git",
                    function(...) "inexistantBranch", token_envvar = NULL),
      regexp = "available_branches", fixed = TRUE
    ),
    regexp = "Mocking git2r::fetch", fixed = TRUE
  )
  expect_output(
    checkout_repo(repo_dir,
                  "REPLACED/stageddeps.food.git",
                  function(...) "unittest_branch2", token_envvar = NULL),
    regexp = "Mocking git2r::fetch", fixed = TRUE
  )
  unlink(repo_dir, recursive = TRUE)

  repo_dir <- file.path(tempfile(), "stageddeps.food")
  expect_output(
    checkout_repo(repo_dir,
                  "REPLACED/stageddeps.food.git",
                  function(...) "main", token_envvar = NULL),
    regexp = "Mocking git2r::clone", fixed = TRUE
  )
  unlink(repo_dir, recursive = TRUE)

})

# ---- check_only_remote_branches ----
test_that("check_only_remote_branches works", {
  check_only_remote_branches(file.path(ecosystem_dir, "stageddeps.food"))

  # checkout local branch (after copying repo)
  repo_dir <- tempfile()
  fs::dir_copy(file.path(ecosystem_dir, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, branch = "main")
  expect_error(
    check_only_remote_branches(repo_dir),
    regexp = "origin", fixed = TRUE
  )
})

# ---- install_external_deps ----
test_that("install_external_deps works", {
  # stageddeps.food has dependencies desc, stageddeps.electricity, SummarizedExperiment
  internal_pkg_deps <- c("stageddeps.electricity")
  mockery::stub(remotes::update, "install_external_deps", function(packages, ...) {
    browser()
    expect_equal(packages, 1)
  })
  withr::with_temp_libpaths({
    install_external_deps(file.path(ecosystem_dir, "stageddeps.food"),
                          internal_pkg_deps)
  })

})

# ---- get_authtoken_envvar ----
test_that("get_authtoken_envvar works", {
  withr::with_options(list(
    staged.dependencies.token_mapping = list("https://github.com" = "GITHUB_PAT")), {
      expect_error(
        get_authtoken_envvar("github.com"),
        regexp = "unknown host", fixed = TRUE
      )
      expect_equal(get_authtoken_envvar("https://github.com"), "GITHUB_PAT")
    })
})

# ---- get_repo_url ----
test_that("get_repo_url works", {
  expect_identical(
    get_repo_url("testOrg/testRepo", "https://github.com"),
    "https://github.com/testOrg/testRepo.git"
  )
})

# ---- checkout_repo ----
test_that("checkout_repo works", {

  expect_silent({
    # check error when dir is not a git repo
    existing_dir <- tempfile()
    dir.create(existing_dir)
    expect_error(
      checkout_repo(existing_dir, "https://github.com/openpharma/stageddeps.water.git",
                    function(...) "main", token_envvar = NULL),
      regex = "not in a git repository", fixed = TRUE
    )
    unlink(existing_dir, recursive = TRUE)

    # inexistant branch to checkout
    repo_dir <- tempfile()
    expect_error(
      checkout_repo(repo_dir, "https://github.com/openpharma/stageddeps.water.git",
                    function(...) "inexistantBranch", token_envvar = NULL),
      regex = "available_branches", fixed = TRUE
    )
    # checkout existing branch
    expect_equal(
      checkout_repo(repo_dir, "https://github.com/openpharma/stageddeps.water.git",
                    function(...) "main", token_envvar = NULL),
      repo_dir
    )
    unlink(repo_dir, recursive = TRUE)

    repo_dir <- tempfile()
    withr::with_envvar(list(INCORRECT_TOKEN = "adfs"), {
      expect_error(
        checkout_repo(repo_dir, "https://github.com/inexistentOrg/inexistentRepo.git",
                      function(...) "main", token_envvar = "INCORRECT_TOKEN"),
        regexp = "Bad credentials", fixed = TRUE
      )
    })
  })

})

test_that("checkout_repo with mocking works", {
  # mock git clone

  tests_ecosystem_dir <- "../staged.dependencies/tests/testthat/test_ecosystem" #todo

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

  ecosystem_dir <- tempfile("test_ecosystem")
  # ecosystem_dir <- "../scratch1/test_ecosystem"

  copy_ecosystem_dir(ecosystem_dir, tests_ecosystem_dir)

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

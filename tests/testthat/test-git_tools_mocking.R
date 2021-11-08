# The difference to test-git_tools.R is that this file copies a set of git repos
# into a directory. The git repos can then be used to mock git clone and git fetch.
# Does not require access to GitHub.

# ---- checkout_repo ----
test_that("checkout_repo with mocking works", {
  # mock git clone

  # delete mock function with rm(list = c("checkout_repo"))
  mockery::stub(checkout_repo, 'git2r::clone', function(url, local_path, ...) {
    print("Mocking git2r::clone")
    existing_repo_dir <- file.path(TESTS_GIT_REPOS, basename(local_path))
    stopifnot(dir.exists(existing_repo_dir))
    fs::dir_copy(existing_repo_dir, local_path)
    # assuming "main" branch exists
    git2r::checkout(local_path, branch = "main", force = TRUE)

    # delete other local branches
    local_branches <- git2r::branches(repo_dir, flags = "local")
    lapply(local_branches[names(local_branches) != "main"], git2r::branch_delete)

    git2r::repository(local_path)
  })

  mockery::stub(checkout_repo, 'git2r::fetch', function(url, local_path, ...) {
    print("Mocking git2r::fetch")
    invisible(NULL)
  })

  mockery::stub(checkout_repo, 'get_remote_name', function(...) "origin")

  with_tmp_cachedir({
    repo_dir <- file.path(tempfile(), "stageddeps.food")

    # check that clone is called
    expect_output(
      checkout_repo(repo_dir,
                    "REPLACED/stageddeps.food.git",
                    function(...) structure("unittest_branch1", type = "branch"), token_envvar = NULL),
      regexp = "Mocking git2r::clone", fixed = TRUE
    )

    # check that fetch is called, raises error because it cannot checkout an inexistent branch
    expect_output(
      expect_error(
        checkout_repo(repo_dir,
                      "REPLACED/stageddeps.food.git",
                      function(...) structure("inexistantBranch", type = "branch"), token_envvar = NULL),
        regexp = "ref inexistantBranch is unavailable for this repo", fixed = TRUE
      ),
      regexp = "Mocking git2r::fetch", fixed = TRUE
    )
    # checkout an existing branch (after fetching)
    expect_output(
      checkout_repo(repo_dir,
                    "REPLACED/stageddeps.food.git",
                    function(...) structure("unittest_branch2", type = "branch"), token_envvar = NULL),
      regexp = "Mocking git2r::fetch", fixed = TRUE
    )

    unlink(repo_dir, recursive = TRUE)
  })

})

# ---- check_only_remote_branches ----
test_that("check_only_remote_branches works", {
  repo_dir <- tempfile()
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.elecinfra"), repo_dir)

  # check only remote branches exist
  expect_silent(check_only_remote_branches(repo_dir, "origin"))

  # checkout local branch, now expect an error that a local branch exists
  git2r::checkout(repo_dir, branch = "main")
  expect_error(
    check_only_remote_branches(repo_dir, "origin"),
    regexp = "remote_name", fixed = TRUE
  )
})

# See the related tests in 'test-git_tools_mocking.R' which do not require access to GitHub

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
